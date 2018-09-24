output$pbarEU <- renderPlot({
  
  setFund <- input$fundEUp
  
  MSP <- dfP %>%
    filter(fund==setFund) %>%
    group_by(ms) %>%
    summarise(sum(total_amount, na.rm=T))
  
  MSI <- dfI %>%
    filter(fund==setFund & year==max(as.numeric(dfI$year))) %>%
    group_by(ms) %>%
    summarise(sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  
  dt <- data.frame(MSP,MSI[-1])
  colnames(dt) <- c("Region", "PlannedOP", "SelectionOP", "ExpenditureOP")
  
  
  ## fare stesse procedure per valori MS e EU
  
  pEU <- dfP %>% filter(fund==setFund) %>% summarise(sum(total_amount))
  sEU <- dfI %>% filter(fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_cost, na.rm=T))
  eEU <- dfI %>% filter(fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_expenditure, na.rm=T))
  
  dt <- rbind(dt, data.frame("Region"="EU", "PlannedOP"= as.numeric(pEU),
                             "SelectionOP"= as.numeric(sEU), "ExpenditureOP"=as.numeric(eEU)))
  ##
  
  ## rates calculation and set of colours
  dt$valueSelection <- round((dt$SelectionOP/dt$PlannedOP)*100,1)
  dt$valueExpenditure <- round((dt$ExpenditureOP/dt$PlannedOP)*100,1)
  
  dt$level <- ifelse(dt$Region =="EU", "EU","MS")
  
  dt$ColSel <- ifelse(dt$level == "MS", "#42beb8", "#48686c")
  
  dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31", "#f2c400")
  
  
  # riordinare i valori per avere il matching corretto
  dt <- dt[order(-dt$valueSelection),]
  dt$index <- seq(1:nrow(dt))
  
  ## plot function
  
  ggplot() +
    
    # selection
    geom_bar(data = dt,
             aes(x=reorder(Region, index),
                 y=valueSelection, 
                 fill = ColSel),
             position = position_dodge(width=0.9), stat="identity", width=0.8) +
    geom_text(data = dt,
              aes(label=paste0(valueSelection,"%"),
                  y=valueSelection,
                  x=Region),
              size=3,
              hjust = 0.5,
              vjust = -0.5,
              position = position_dodge(width = 1)) +
    
    # expenditure
    geom_bar(data = dt,
             aes(x=reorder(Region, index),
                 y=valueExpenditure, 
                 fill = ColExp),
             position = position_dodge(width=0.9), stat="identity", width=0.4) +
    geom_text(data = dt,
              aes(label=paste0(valueExpenditure,"%"),
                  y=valueExpenditure,
                  x=Region),
              size=3,
              hjust = 0.5,
              vjust = -0.5,
              position = position_dodge(width = 1)) +
    
    # legend, footnote
    scale_fill_manual(name = "",
                      values=c("#42beb8"="#42beb8", # MS sel
                               "#ed7d31"="#ed7d31", # MS exp
                               "#48686c"="#48686c", # EU sel
                               "#f2c400"="#f2c400"),# EU exp
                      # breaks e labels gestiscono ordine ed etichette                  
                      breaks = c("#42beb8","#ed7d31",
                                 "#48686c", "#f2c400"),
                      labels=c("MS rate of project selection","MS rate of expenditure declared",
                               "EU rate of project selection","EU rate of expenditure declared")) +
    
    
    theme_classic() +
    
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, max(dt$valueSelection, na.rm=T), by=10),
                       labels =  paste0(seq(0, max(dt$valueSelection, na.rm=T),by=10),"%")) +
    
    coord_cartesian(ylim = c(0,max(dt$valueSelection, na.rm=T)+max(dt$valueSelection, na.rm=T)*0.1), expand = T) +
    
    theme(legend.position = "bottom",
          legend.box.margin = margin(0.5, 0.5, 0.5, 0.5), # top, right, bottom, left
          legend.box.background = element_rect(colour = "white"),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(hjust = 0.5, vjust=0.4, size = 12),
          axis.text.y = element_text(size = 10),
          #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, colour = 'lightgrey'),
          plot.title = element_text(face="bold"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

})