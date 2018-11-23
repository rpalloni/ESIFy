output$pbarEUindRate <- renderPlot({
  
  setFund <- input$fundInd
  setInd <- input$codeInd
  setTO <- input$ToInd
  
  
  setYear <- dfR %>% filter(year==max(as.numeric(dfR$year))) %>% distinct(year) %>%as.character()
  measure <- dfR %>% filter(ind_code==setInd) %>% select(measurement_unit) %>% slice(1) %>% as.character()
  ind_name <- dfR %>% filter(ind_code==setInd) %>% select(indicator_long_name) %>% slice(1) %>% as.character()
  
  MSR <- dfR %>%
    filter(fund==setFund & to==setTO & ind_code==setInd & year==setYear) %>%
    group_by(ms,to) %>%
    summarise(sum(target_value, na.rm=T),
              sum(forecast_value, na.rm=T))
  
  MSI <- dfI %>%
    filter(fund==setFund & to==setTO & year==setYear) %>%
    group_by(ms,to) %>%
    summarise(sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  dt <- merge(MSR, MSI, by=c('ms','to'))
  
  
  colnames(dt) <- c("ms", "to","Target","Indicator" , "SelectionOP", "ExpenditureOP")
  
  
  ## fare stesse procedure per valori MS e EU
  
  rEU <- dfR %>% 
    filter(fund==setFund & to==setTO & ind_code==setInd & year==setYear) %>%
    summarise(sum(forecast_value, na.rm=T))
  
  tEU <- dfR %>% 
    filter(fund==setFund & to==setTO & ind_code==setInd & year==setYear) %>%
    summarise(sum(target_value, na.rm=T))
  
  sEU <- dfI %>% 
    filter(fund==setFund & to==setTO & year==setYear) %>%
    summarise(sum(total_eligible_cost, na.rm=T))
  
  eEU <- dfI %>% 
    filter(fund==setFund & to==setTO & year==setYear) %>%
    summarise(sum(total_eligible_expenditure, na.rm=T))
  
  dt <- rbind(dt, data.frame("ms"="EU","to"= setTO, "Indicator"= as.numeric(rEU),
                             "Target"= as.numeric(tEU), "SelectionOP"= as.numeric(sEU), "ExpenditureOP"=as.numeric(eEU)))
  
  
  
  if(nrow(dt)==0){
    
    ggplot() + geom_text(aes(x = 1, y = 1, label="No Data", size = 4)) + theme_void()
    
  }else{
    
    
    ## rates calculation and set of colours
    #dt$valueSelection <- round((dt$Target),1)
    #dt$valueExpenditure <- round((dt$Indicator),1)
    
    dt$rate <- rnd(dt$Indicator/dt$Target)
    
    dt$level <- ifelse(dt$ms =="EU", "EU","MS")
    
    dt$ColSel <- ifelse(dt$level == "MS", "#42beb8", "#48686c")
    
    # dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31", "#f2c400")
    
    
    # riordinare i valori per avere il matching corretto
    dt <- dt[order(-dt$rate),]
    dt$index <- seq(1:nrow(dt))
    
    ## plot function
    
    ggplot() +
      
      # ratio
      geom_bar(data = dt,
               aes(x=reorder(ms, index),
                   y=rate, 
                   fill = ColSel),
               position = position_dodge(width=0.9), stat="identity", width=0.8) +
      geom_text(data = dt,
                aes(label=paste0(rate*100, "%"),
                    y=rate,
                    x=ms),
                size=3,
                hjust = 0.5,
                vjust = -0.5,
                position = position_dodge(width = 1)) +
    
      
      # legend, footnote
      scale_fill_manual(name = "",
                        values=c("#42beb8"="#42beb8", # MS sel
                                 #"#ed7d31"="#ed7d31", # MS exp
                                 "#48686c"="#48686c"), # EU sel
                                 #"#f2c400"="#f2c400"),# EU exp
                        # breaks e labels gestiscono ordine ed etichette                  
                        breaks = c("#42beb8",#"#ed7d31",
                                   "#48686c"), #"#ed7d31"),
                        
                        labels=c(#"MS indicator value","MS target value",
                                 "MS rate of achievement","EU rate of achievement")) +
      
      ggtitle(paste0("Indicator: ", ind_name,"\nMeasurement unit: ",measure,"\nYear: ",setYear)) +
      
      # labs(caption="*Ratios calculated only for MSs adopting the indicator and with value >0")+
      
      theme_classic() +
      
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      coord_cartesian() +
      
      
      scale_y_continuous(expand = c(0,0), labels = percent_format()) +
      
      expand_limits(y = max(dt$rate, na.rm=T)*1.1) +
      
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
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
            plot.caption=element_text(hjust = 0, vjust = 0, size = 10))
    
    
  }
  
})