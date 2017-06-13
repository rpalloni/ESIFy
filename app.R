library(shiny)
library(dplyr)
library(tidyjson)
library(ggplot2)
library(stringr)

#########################################################
################## Fetch Data from API ###################
#########################################################

urlPlanned <- "https://cohesiondata.ec.europa.eu/resource/rde7-u3r9.json?$order=ms&$limit=10000"
jsonPlanned <- paste(readLines(urlPlanned), collapse="")


dfP <- data.frame(jsonPlanned %>% 
                    gather_array %>% 
                    spread_values(
                      ms = jstring("ms"),
                      cci = jstring("cci"),
                      title = jstring("title"),
                      fund = jstring("fund"),
                      priority = jstring("priority"),
                      to = jstring("to"),
                      toName =jstring("to_long"),
                      category_of_region = jstring("category_of_region"),
                      cofinancing = jnumber("eu_co_financing"),
                      total_amount = jnumber("total_amount")))

dfP$ms <- as.factor(dfP$ms)
dfP$fund <- as.factor(dfP$fund)
dfP$title <- as.factor(dfP$title)
dfP$to <- as.factor(dfP$to)


urlImplem <- "https://cohesiondata.ec.europa.eu/resource/f6wa-fhmb.json?$order=ms&$limit=50000"
jsonImplem<- paste(readLines(urlImplem), collapse="")

dfI <- data.frame(jsonImplem %>% 
                    gather_array %>% 
                    spread_values(
                      ms = jstring("ms"),
                      cci = jstring("cci"),
                      title = jstring("title"),
                      fund = jstring("fund"),
                      priority = jstring("priority"),
                      to = jstring("to"),
                      toName =jstring("to_long"),
                      category_of_region = jstring("category_of_region"),
                      cofinancing = jnumber("eu_co_financing"),
                      total_eligible_cost= jnumber("total_eligible_cost"),
                      total_eligible_expenditure=jnumber("total_eligible_expenditure")))

dfI$ms <- as.factor(dfI$ms)
dfI$fund <- as.factor(dfI$fund)
dfI$title <- as.factor(dfI$title)
dfI$to <- as.factor(dfI$to)

app <- shinyApp(

#########################################################
##################### User Interface ####################
#########################################################
# fluidPage transform the R instructions in html

frontend <- fluidPage(
  headerPanel('Visualize Cohesion Policy OpenData'),
  tags$div(class = "header", checked = NA,
           tags$p("Data on financing and achievements under the ESI Funds 2014-2020 as available at ", tags$a(href = "https://cohesiondata.ec.europa.eu/", "Open Data Portal for the ESIF")),
           tags$p("Data are available disaggregated by fund, programme, priority axis, thematic objective and category of regions."),
           tags$p("Visualization of project selection and expenditure declared as percentage of planned financing as reported by the Managing Authorities of the programmes (ERDF only)."),
           tags$hr()
           ),
  sidebarPanel(
    selectInput('ms', 'Member State', unique(dfP$ms)),
    selectInput('title', 'Operational Programme', unique(dfP$title))), # input form user
  mainPanel(
    plotOutput('pbarMS'),
    tags$hr(),
    plotOutput('pbarOP'),
    tags$hr(),
    plotOutput('pbarTO')# output for user
  ),
  tags$div(class = "footer", checked = NA,
           tags$hr(),
           tags$p("Developed by:",tags$a(href = "https://www.linkedin.com/in/roberto-palloni-6b224031/", "Roberto Palloni")))
),

#########################################################
################## Server instructions ##################
#########################################################

backend <- function(input, output, session){
  
  
  ### input functions
  
  #input$selectedDataMS <- renderUI({
   # levels(dt[,(input$ms)])
  #})
  
  #input$selectedDataOP <- renderUI({
   # levels(dt[,(input$title)])
    # selectInput('selectedDataOP', 'Operational Programme', unique(input$title[(input$ms==ms & dfP$fund=="ERDF")]))
  #})
  
  observeEvent(input$ms,{
    updateSelectInput(session, 'title', choices=unique(dfP$title[dfP$ms==input$ms & dfP$fund=="ERDF"]))
  })


  
  ### output functions
  
  output$pbarMS <- renderPlot({
    
    setMS <- input$ms
    
    MSP <- dfP %>%
      filter(ms==setMS & fund=="ERDF") %>%
      group_by(title) %>%
      summarise(sum(total_amount))
    
    MSI <- dfI %>%
      filter(ms==setMS & fund=="ERDF") %>%
      group_by(title) %>%
      summarise(sum(total_eligible_cost, na.rm=T), 
                sum(total_eligible_expenditure, na.rm=T))
    
    dt <- data.frame(MSP,MSI[-1])
    colnames(dt) <- c("Region", "PlannedOP", "SelectionOP", "ExpenditureOP")
    
    
    ## fare stesse procedure per valori MS e EU
    
    pMS <- dfP %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_amount))
    sMS <- dfI %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
    eMS <- dfI %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))
    
    pEU <- dfP %>% filter(fund=="ERDF") %>% summarise(sum(total_amount))
    sEU <- dfI %>% filter(fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
    eEU <- dfI %>% filter(fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))
    
    dt <- rbind(dt, data.frame("Region"=setMS, "PlannedOP"= as.numeric(pMS),
                               "SelectionOP"= as.numeric(sMS), "ExpenditureOP"=as.numeric(eMS)))
    dt <- rbind(dt, data.frame("Region"="EU", "PlannedOP"= as.numeric(pEU),
                               "SelectionOP"= as.numeric(sEU), "ExpenditureOP"=as.numeric(eEU)))
    ##
    
    
    dt$ratio_f <- dt$SelectionOP/dt$PlannedOP
    dt$ratio_g <- dt$ExpenditureOP/dt$PlannedOP
    dt$level <- ifelse(dt$Region == setMS, "MS",
                       ifelse(dt$Region =="EU", "EU","OP"))
    
    dt$ColSel <- ifelse(dt$level == "MS", "#42beb8",
                        ifelse(dt$level == "EU", "#48686c", "#b1dcc0"))
    
    dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31",
                        ifelse(dt$level == "EU", "#f2c400", "#f04e52"))
    
    
    # riordinare i valori per avere il matching corretto
    dt <- dt[order(-dt$ratio_f),]
    
    ggplot() +
      geom_bar(data=dt, 
               aes(x=reorder(Region,-ratio_f), y=ratio_f*100), fill = dt$ColSel,
               position = position_dodge(width=0.9), stat="identity", width=0.8) +
      geom_bar(data=dt, 
               aes(x=Region, y=ratio_g*100), fill = dt$ColExp,
               position = position_dodge(width=0.9), stat="identity", width=0.4) +
      ggtitle(paste("Rate of project selection and expenditure declared by ",setMS," Operational Programmes (% of planned financing)"))+
      xlab("Operational Programmes") + 
      ylab("Rate of project selection and expenditure declared (%)") +
      theme_classic() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(legend.position="bottom",
            legend.text=element_text(size=12),
            axis.text.x = element_text(hjust = 0.5, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(face="bold"))
  })


  output$pbarOP <- renderPlot({
    
    setCCI <- input$title
    setMS <- input$ms
    
    OPP <- dfP %>%
      filter(title==setCCI & fund=="ERDF") %>%
      group_by(priority) %>%
      summarise(sum(total_amount))
    
    OPI <- dfI %>%
      filter(title==setCCI & fund=="ERDF") %>%
      group_by(priority) %>%
      summarise(sum(total_eligible_cost, na.rm=T), 
                sum(total_eligible_expenditure, na.rm=T))
    
    dt <- data.frame(setCCI,OPP,OPI[-1])
    colnames(dt) <- c("OPname","PAcode", "PlannedPAx", "SelectionPAx", "ExpenditurePAx")
    
    pOP <- dfP %>% filter(title==setCCI & fund=="ERDF") %>% summarise(sum(total_amount))
    sOP <- dfI %>% filter(title==setCCI & fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
    eOP <- dfI %>% filter(title==setCCI & fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))
    
    pMS <- dfP %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_amount))
    sMS <- dfI %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
    eMS <- dfI %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))
    
    pEU <- dfP %>% filter(fund=="ERDF") %>% summarise(sum(total_amount))
    sEU <- dfI %>% filter(fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
    eEU <- dfI %>% filter(fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))
    
    # merge datasets
    dt <- rbind(dt, data.frame("OPname"="OP", "PAcode"="OP","PlannedPAx"= as.numeric(pOP),
                               "SelectionPAx"= as.numeric(sOP), "ExpenditurePAx"=as.numeric(eOP)))
    dt <- rbind(dt, data.frame("OPname"=setMS, "PAcode"=setMS,"PlannedPAx"= as.numeric(pMS),
                               "SelectionPAx"= as.numeric(sMS), "ExpenditurePAx"=as.numeric(eMS)))
    dt <- rbind(dt, data.frame("OPname"="EU", "PAcode"="EU","PlannedPAx"= as.numeric(pEU),
                               "SelectionPAx"= as.numeric(sEU), "ExpenditurePAx"=as.numeric(eEU)))
    
    ## rates calculation and set of colours
    dt$ratio_f <- dt$SelectionPAx/dt$PlannedPAx
    dt$ratio_g <- dt$ExpenditurePAx/dt$PlannedPAx
    dt$level <- ifelse(dt$OPname == "OP", "OP",
                       ifelse(dt$OPname == setMS, "MS",
                              ifelse(dt$OPname =="EU", "EU","PA")))
    
    dt$ColSel <- ifelse(dt$level == "MS", "#42beb8",
                        ifelse(dt$level == "EU", "#48686c", "#b1dcc0"))
    
    dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31",
                        ifelse(dt$level == "EU", "#f2c400", "#f04e52"))
    
    ## order of bars
    dt$PAcode <- as.character(dt$PAcode)
    dt <- dt[order(dt$level, dt$PAcode, decreasing = F), ]
    dt$index <- seq(1:nrow(dt))
    dt <- dt[order(dt$index, decreasing = T), ]
    
    ## plot function
    ggplot() +
      geom_bar(data=dt, 
               aes(x=reorder(PAcode, -index), y=ratio_f*100), fill = dt$ColSel,
               position = position_dodge(width=0.9), stat="identity", width=0.8) +
      geom_text(data=dt, aes(label=paste0(round(ratio_f*100,1),"%"),
                             y=ratio_f*100+(ratio_f*100)*0.03, x=PAcode ), size=3) +
      geom_bar(data=dt, 
               aes(x=PAcode, y=ratio_g*100), fill = dt$ColExp,
               position = position_dodge(width=0.9), stat="identity", width=0.4) +
      geom_text(data=dt, aes(label=paste0(round(ratio_g*100,1),"%"),
                             y=ratio_g*100+(ratio_g*100)*0.03, x=PAcode), size=3) +
      ggtitle("Rate of project selection and expenditure declared by OP Priority Axis (% of planned financing)")+
      xlab("EU, MS, OP and Priority Axes") + 
      ylab("Rate of project selection and expenditure declared (%)") +
      theme_classic() +
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      theme(legend.position="bottom",
            legend.text=element_text(size=12),
            axis.text.x = element_text(hjust = 1, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(face="bold"))
  })
  
  output$pbarTO <- renderPlot({
    
    setCCI <- input$title
    setMS <- input$ms
    
    OPI <- dfI %>%
      filter(title==setCCI & fund=="ERDF") %>%
      group_by(to) %>%
      summarise(sum(total_eligible_cost, na.rm=T), 
                sum(total_eligible_expenditure, na.rm=T))
    
    OPI <- OPI[OPI$to != 'MULTI',]
    
    
    OPP <- dfP %>%
      filter(title==setCCI & fund=="ERDF") %>%
      group_by(to) %>%
      summarise(sum(total_amount))
    
    OPP$toM <- c(rep(0, nrow(OPP)))
    OPP$to <- as.character(OPP$to)
    OPI$to <- as.character(OPI$to)
    for(i in 1:nrow(OPP)){
      if(OPP$to[i] %in% OPI$to){
        OPP$toM[i] <- OPP$to[i]
      } else {
        OPP$toM[i] <- 'MULTI'
      }
    }
    
    OPP <- OPP %>% 
      group_by(toM) %>%
      summarise(sum(`sum(total_amount)`))
    
    OPP <- OPP[OPP$toM != 'MULTI',]
    
    # MS
    
    MSI <- dfI %>%
      filter(ms==setMS & fund=="ERDF") %>%
      group_by(to) %>%
      summarise(sum(total_eligible_cost, na.rm=T), 
                sum(total_eligible_expenditure, na.rm=T))
    
    MSI <- MSI[MSI$to %in% OPI$to,]
    
    
    MSP <- dfP %>%
      filter(ms==setMS & fund=="ERDF") %>%
      group_by(to) %>%
      summarise(sum(total_amount))
    
    MSP <- MSP[MSP$to %in% OPP$toM,]
    
    
    # EU
    
    EUI <- dfI %>%
      filter(fund=="ERDF") %>%
      group_by(to) %>%
      summarise(sum(total_eligible_cost, na.rm=T), 
                sum(total_eligible_expenditure, na.rm=T))
    
    EUI <- EUI[EUI$to %in% OPI$to,]
    
    
    EUP <- dfP %>%
      filter(fund=="ERDF") %>%
      group_by(to) %>%
      summarise(sum(total_amount))
    
    EUP <- EUP[EUP$to %in% OPP$toM,]
    
    
    
    dtOP <- data.frame(setCCI,OPP[-1],OPI)
    colnames(dtOP) <- c("Name", "PlannedTO", "TOcode", "SelectionTO", "ExpenditureTO")
    dtMS <- data.frame(setMS,MSP[-1],MSI)
    colnames(dtMS) <- c("Name", "PlannedTO", "TOcode", "SelectionTO", "ExpenditureTO")
    dtEU <- data.frame('EU',EUP[-1],EUI)
    colnames(dtEU) <- c("Name", "PlannedTO", "TOcode", "SelectionTO", "ExpenditureTO")
    dt <- rbind(dtOP, dtMS, dtEU)
    
    
    ## rates calculation and set of colours
    dt$ratio_f <- dt$SelectionTO/dt$PlannedTO
    dt$ratio_g <- dt$ExpenditureTO/dt$PlannedTO
    dt$level <- ifelse(dt$Name == "OP", "OP",
                       ifelse(dt$Name == setMS, "MS",
                              ifelse(dt$Name =="EU", "EU","OP")))
    
    dt$ColSel <- ifelse(dt$level == "MS", "#42beb8",
                        ifelse(dt$level == "EU", "#48686c", "#b1dcc0"))
    
    dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31",
                        ifelse(dt$level == "EU", "#f2c400", "#f04e52"))
    
    ## order of bars
    dt <- dt[order(dt$TOcode, dt$level), ]
    dt$index <- seq(1:nrow(dt))
    
    ## plot function
    ggplot() +
      geom_bar(data=dt, 
               aes(x=reorder(TOcode,index), y=ratio_f*100, group = Name),fill = dt$ColSel,
               position = position_dodge(width=0.9), stat="identity", width=0.8) +
      geom_bar(data=dt, 
               aes(x=TOcode, y=ratio_g*100, group = Name), fill = dt$ColExp,
               position = position_dodge(width=0.9), stat="identity", width=0.4) +
      ggtitle("Rate of project selection and expenditure declared by OP Thematic Objective (% of planned financing)")+
      xlab("OP, MS, EU by comparable Thematic Objectives") + 
      ylab("Rate of project selection and expenditure declared (%)") +
      theme_classic() +
      scale_y_continuous(expand = c(0, 0)) +
      theme(legend.position="bottom",
            legend.text=element_text(size=12),
            axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(face="bold"))
    
    
  })
}
)

