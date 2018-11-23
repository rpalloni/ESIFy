output$pbarEU <- renderPlotly({
  
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
  colnames(dt) <- c("Geo", "PlannedOP", "SelectionOP", "ExpenditureOP")
  
  
  ## fare stesse procedure per valori MS e EU
  
  pEU <- dfP %>% filter(fund==setFund) %>% summarise(sum(total_amount))
  sEU <- dfI %>% filter(fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_cost, na.rm=T))
  eEU <- dfI %>% filter(fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_expenditure, na.rm=T))
  
  dt <- rbind(dt, data.frame("Geo"="EU", "PlannedOP"= as.numeric(pEU),
                             "SelectionOP"= as.numeric(sEU), "ExpenditureOP"=as.numeric(eEU)))
  ##
  
  ## rates calculation and set of colours
  dt$valueSelection <- rnd(dt$SelectionOP/dt$PlannedOP)
  dt$valueExpenditure <- rnd(dt$ExpenditureOP/dt$PlannedOP)
  
  dt$level <- ifelse(dt$Geo =="EU", "EU","MS")
  
  dt$ColSel <- ifelse(dt$level == "MS", "MS project selection", "EU project selection")
  
  dt$ColExp <- ifelse(dt$level == "MS", "MS expenditure declared", "EU expenditure declared")
  
  
  # riordinare i valori per avere il matching corretto
  dt <- dt[order(-dt$valueSelection),]
  dt$index <- seq(1:nrow(dt))
  
  ## plot function
  ggplotly(
  ggplot() +
    
    # selection
    geom_bar(data = dt,
             aes(x=reorder(Geo, index),
                 y=valueSelection, 
                 fill = ColSel,
                 label2 = Geo),
             position = position_dodge(width=0.9), stat="identity", width=0.8) +
    geom_text(data = dt,
              aes(label=paste0(valueSelection*100,"%"),
                  label2 = Geo,
                  y=valueSelection,
                  x=Geo),
              size=3,
              hjust = 0.5,
              vjust = -0.5,
              position = position_dodge(width = 1)) +
    
    # expenditure
    geom_bar(data = dt,
             aes(x=reorder(Geo, index),
                 y=valueExpenditure, 
                 fill = ColExp,
                 label2 = Geo),
             position = position_dodge(width=0.9), stat="identity", width=0.4) +
    geom_text(data = dt,
              aes(label=paste0(valueExpenditure*100,"%"),
                  label2 = Geo,
                  y=valueExpenditure,
                  x=Geo),
              size=3,
              hjust = 0.5,
              vjust = -0.5,
              position = position_dodge(width = 1)) +
    
    # legend, footnote
    scale_fill_manual(name = "",
                      values=c("MS project selection"="#42beb8", # MS sel
                               "MS expenditure declared"="#ed7d31", # MS exp
                               "EU project selection"="#48686c", # EU sel
                               "EU expenditure declared"="#f2c400")# EU exp
    ) +
    
    
    theme_classic() +
    
    
    coord_cartesian() +
    
    
    
    scale_y_continuous(expand = c(0,0), labels = percent_format()) +
    
    expand_limits(y = max(dt$valueSelection, na.rm=T)*1.1) +
    
    
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    
    
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(hjust = 0.5, vjust=0.4, size = 12),
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