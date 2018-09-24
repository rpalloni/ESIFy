
output$pbarTOeu <- renderPlot({
  
  setFund <- input$fundEUp
  
  EUI <- dfI %>%
    filter(fund==setFund & year==max(as.numeric(dfI$year))) %>%
    group_by(to) %>%
    summarise(sum(total_amount, na.rm=T),
              sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  dtEU <- data.frame('EU',EUI)
  colnames(dtEU) <- c("Name", "TOcode","PlannedTO", "SelectionTO", "ExpenditureTO")
  dt <- dtEU
  
  
  ## rates calculation and set of colours
  dt$valueSelection <- round((dt$SelectionTO/dt$PlannedTO)*100,1)
  dt$valueExpenditure <- round((dt$ExpenditureTO/dt$PlannedTO)*100,1)
  
  
  dt$ColSel <- "#48686c"
  
  dt$ColExp <- "#f2c400"
  
  ## order of bars
  dt <- dt[order(dt$TOcode), ]
  dt$index <- seq(1:nrow(dt))
  
  ## plot function
  
  ggplot() +
    
    # selection
    geom_bar(data = dt,
             aes(x=reorder(TOcode, index),
                 y=valueSelection, 
                 fill = ColSel,
                 group = Name),
             position = position_dodge(width = 0.9),
             stat="identity", width=0.8) +
    geom_text(data = dt,
              aes(label=paste0(valueSelection,"%"),
                  y=valueSelection,
                  x=TOcode,
                  group = Name),
              size=3,
              hjust = 0.5,
              vjust = -0.2,
              position = position_dodge(width = 0.9)) + # same position_dodge for perfect alignment
    
    # expenditure
    geom_bar(data = dt,
             aes(x=reorder(TOcode, index),
                 y=valueExpenditure, 
                 fill = ColExp,
                 group = Name),
             position = position_dodge(width = 0.9),
             stat="identity", width=0.4) +
    geom_text(data = dt,
              aes(label=paste0(valueExpenditure,"%"),
                  y=valueExpenditure,
                  x=TOcode,
                  group = Name),
              size=3,
              hjust = 0.5,
              vjust = -0.2,
              position = position_dodge(width = 0.9)) +
    
    # legend, footnote
    scale_fill_manual(name = "",
                      values=c("#48686c"="#48686c", # EU sel
                               "#f2c400"="#f2c400"),# EU exp
                      # breaks e labels gestiscono ordine ed etichette                  
                      breaks = c("#48686c","#f2c400"),
                      labels=c("EU rate of project selection","EU rate of expenditure declared")) +
    
    
    theme_classic() +
    
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
          axis.text.x = element_text(hjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, colour = 'lightgrey'),
          plot.title = element_text(face="bold"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  
  
})