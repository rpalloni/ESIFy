
output$pbarTOeu <- renderPlotly({
  
  setFund <- input$fundEUp
  
  EUI <- dfI %>%
    filter(fund==setFund & year==max(as.numeric(dfI$year))) %>%
    group_by(to) %>%
    summarise(sum(total_amount, na.rm=T),
              sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  dtEU <- data.frame('EU',EUI)
  colnames(dtEU) <- c("Geo", "TOcode","PlannedTO", "SelectionTO", "ExpenditureTO")
  dt <- dtEU
  
  
  ## rates calculation and set of colours
  dt$valueSelection <- rnd(dt$SelectionTO/dt$PlannedTO)
  dt$valueExpenditure <- rnd(dt$ExpenditureTO/dt$PlannedTO)
  
  
  dt$ColSel <- "EU project selection"
  
  dt$ColExp <- "EU expenditure declared"
  
  ## order of bars
  dt <- dt[order(dt$TOcode), ]
  dt$index <- seq(1:nrow(dt))
  
  ## plot function
  
  ggplotly(
  ggplot() +
    
    # selection
    geom_bar(data = dt,
             aes(x=reorder(TOcode, index),
                 y=valueSelection, 
                 fill = ColSel,
                 group = Geo,
                 label2 = Geo),
             position = position_dodge(width = 0.9),
             stat="identity", width=0.8) +
    geom_text(data = dt,
              aes(label=paste0(valueSelection*100,"%"),
                  y=valueSelection,
                  x=TOcode,
                  group = Geo,
                  label2 = Geo),
              size=3,
              hjust = 0.5,
              vjust = -0.2,
              position = position_dodge(width = 0.9)) + # same position_dodge for perfect alignment
    
    # expenditure
    geom_bar(data = dt,
             aes(x=reorder(TOcode, index),
                 y=valueExpenditure, 
                 fill = ColExp,
                 group = Geo,
                 label2 = Geo),
             position = position_dodge(width = 0.9),
             stat="identity", width=0.4) +
    geom_text(data = dt,
              aes(label=paste0(valueExpenditure*100,"%"),
                  y=valueExpenditure,
                  x=TOcode,
                  group = Geo,
                  label2 = Geo),
              size=3,
              hjust = 0.5,
              vjust = -0.2,
              position = position_dodge(width = 0.9)) +
    
    # legend, footnote
    scale_fill_manual(name = "",
                      values=c("EU project selection"="#48686c", # EU sel
                               "EU expenditure declared"="#f2c400")# EU exp
    ) +
    
    
    theme_classic() +
    
    
    coord_cartesian() +
    
    
    scale_y_continuous(expand = c(0,0), labels = percent_format()) +
    
    expand_limits(y = max(dt$valueSelection, na.rm=T)*1.1) +
    
    theme(axis.title.x=element_blank(),
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