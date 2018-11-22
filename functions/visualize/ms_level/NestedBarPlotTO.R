
output$pbarTOms <- renderPlotly({
  
  setMS <- input$msMSp
  setFund <- input$fundMSp
  
  # MS
  
  MSI <- dfI %>%
    filter(ms==setMS & fund==setFund & year==max(as.numeric(dfI$year))) %>%
    group_by(to) %>%
    summarise(sum(total_amount, na.rm=T),
              sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  
  # EU
  
  EUI <- dfI %>%
    filter(fund==setFund & year==max(as.numeric(dfI$year))) %>%
    group_by(to) %>%
    summarise(sum(total_amount, na.rm=T),
              sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  EUI <- EUI[EUI$to %in% MSI$to,]


  dtMS <- data.frame(setMS,MSI)
  colnames(dtMS) <- c("Geo", "TOcode","PlannedTO", "SelectionTO", "ExpenditureTO")
  dtEU <- data.frame('EU',EUI)
  colnames(dtEU) <- c("Geo", "TOcode","PlannedTO", "SelectionTO", "ExpenditureTO")
  dt <- rbind(dtMS, dtEU)
  
  
  ## rates calculation and set of colours
  dt$valueSelection <- round((dt$SelectionTO/dt$PlannedTO)*100,1)
  dt$valueExpenditure <- round((dt$ExpenditureTO/dt$PlannedTO)*100,1)
  
  dt$level <- ifelse(dt$Geo =="EU", "EU","MS")
  
  dt$ColSel <- ifelse(dt$level == "MS", "MS project selection", "EU project selection")
  
  dt$ColExp <- ifelse(dt$level == "MS", "MS expenditure declared", "EU expenditure declared")
  
  ## order of bars
  dt <- dt[order(dt$TOcode, dt$level), ]
  dt$index <- seq(1:nrow(dt))
  
  ## plot function
  
  ggplotly(
  ggplot() +
    
    # selection
    geom_bar(data = dt,
             aes(x=reorder(TOcode, index),
                 y=valueSelection, 
                 fill = ColSel,
                 group = level,
                 label2 = Geo),
             position = position_dodge(width = 0.9),
             stat="identity", width=0.8) +
    geom_text(data = dt,
              aes(label=paste0(valueSelection,"%"),
                  label2 = Geo,
                  y=valueSelection,
                  x=TOcode,
                  group = level),
              size=3,
              hjust = 0.5,
              vjust = -0.2,
              position = position_dodge(width = 0.9)) + # same position_dodge for perfect alignment
    
    # expenditure
    geom_bar(data = dt,
             aes(x=reorder(TOcode, index),
                 y=valueExpenditure, 
                 fill = ColExp,
                 group = level,
                 label2 = Geo),
             position = position_dodge(width = 0.9),
             stat="identity", width=0.4) +
    geom_text(data = dt,
              aes(label=paste0(valueExpenditure,"%"),
                  label2 = Geo,
                  y=valueExpenditure,
                  x=TOcode,
                  group = level),
              size=3,
              hjust = 0.5,
              vjust = -0.2,
              position = position_dodge(width = 0.9)) +
    
    # legend, footnote
    scale_fill_manual(name = "",
                      values=c("OP project selection"="#b1dcc0", # OP sel
                               "OP expenditure declared"="#f04e52", # OP exp
                               "MS project selection"="#42beb8", # MS sel
                               "MS expenditure declared"="#ed7d31", # MS exp
                               "EU project selection"="#48686c", # EU sel
                               "EU expenditure declared"="#f2c400")# EU exp
                     ) +
    
    
    theme_classic() +
    
    #scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, max(dt$valueSelection, na.rm=T), by=10),
                       labels =  paste0(seq(0, max(dt$valueSelection, na.rm=T),by=10),"%")) +
    
    coord_cartesian(ylim = c(0,max(dt$valueSelection, na.rm=T)+max(dt$valueSelection, na.rm=T)*0.1), expand = T) +
    
    theme(
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(hjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, colour = 'lightgrey'),
          plot.title = element_text(face="bold"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
  
  tooltip=c("label2", "y")
  ) %>%
    layout(
      showlegend = T,
      legend = list(
        orientation = "v", 
        x = 1, 
        y = 0.2
      )
    )

  
})