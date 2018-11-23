output$pbarPA <- renderPlotly({
  
  setMS <- input$msOPp # 'DE'
  setFund <- input$fundOPp # 'ERDF'
  setCCI <- input$titleOPp # 'Nordrhein-Westfalen - ERDF'
            
            OPP <- dfP %>%
              filter(title==setCCI & fund==setFund) %>%
              group_by(priority) %>%
              summarise(sum(total_amount, na.rm=T))
            
            OPI <- dfI %>%
              filter(title==setCCI & fund==setFund & year==max(as.numeric(dfI$year))) %>%
              group_by(priority) %>%
              summarise(sum(total_eligible_cost, na.rm=T), 
                        sum(total_eligible_expenditure, na.rm=T))
            
            OPname <- rep(setCCI, nrow(OPI))
            dt <- data.frame(OPname,OPP,OPI[-1])
            colnames(dt) <- c("Geo","PAcode", "PlannedPAx", "SelectionPAx", "ExpenditurePAx")
            
            # planned, selection, expenditure
            pOP <- dfP %>% filter(title==setCCI & fund==setFund) %>% summarise(sum(total_amount))
            sOP <- dfI %>% filter(title==setCCI & fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_cost, na.rm=T))
            eOP <- dfI %>% filter(title==setCCI & fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_expenditure, na.rm=T))
            
            pMS <- dfP %>% filter(ms==setMS & fund==setFund) %>% summarise(sum(total_amount))
            sMS <- dfI %>% filter(ms==setMS & fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_cost, na.rm=T))
            eMS <- dfI %>% filter(ms==setMS & fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_expenditure, na.rm=T))
            
            pEU <- dfP %>% filter(fund==setFund) %>% summarise(sum(total_amount))
            sEU <- dfI %>% filter(fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_cost, na.rm=T))
            eEU <- dfI %>% filter(fund==setFund & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_expenditure, na.rm=T))
            
            # merge datasets
            dt <- rbind(dt, data.frame("Geo"="OP", "PAcode"="OP","PlannedPAx"= as.numeric(pOP),
                                       "SelectionPAx"= as.numeric(sOP), "ExpenditurePAx"=as.numeric(eOP)))
            dt <- rbind(dt, data.frame("Geo"=setMS, "PAcode"=setMS,"PlannedPAx"= as.numeric(pMS),
                                       "SelectionPAx"= as.numeric(sMS), "ExpenditurePAx"=as.numeric(eMS)))
            dt <- rbind(dt, data.frame("Geo"="EU", "PAcode"="EU","PlannedPAx"= as.numeric(pEU),
                                       "SelectionPAx"= as.numeric(sEU), "ExpenditurePAx"=as.numeric(eEU)))
            
            ## rates calculation and set of colours
            dt$valueSelection <- rnd(dt$SelectionPAx/dt$PlannedPAx)
            dt$valueExpenditure <- rnd(dt$ExpenditurePAx/dt$PlannedPAx)
            
            dt$level <- ifelse(dt$Geo == "OP", "OP",
                               ifelse(dt$Geo == setMS, "MS",
                                      ifelse(dt$Geo =="EU", "EU","PA")))
            
            dt$ColSel <- ifelse(dt$level == "MS", "MS project selection",
                                ifelse(dt$level == "EU", "EU project selection", "OP project selection"))
            
            dt$ColExp <- ifelse(dt$level == "MS", "MS expenditure declared",
                                ifelse(dt$level == "EU", "EU expenditure declared", "OP expenditure declared"))
            
            ## order of bars
            dt$PAcode <- as.character(dt$PAcode)
            dt$PAcode <- ifelse(is.na(dt$PAcode),"unspecified",dt$PAcode)
            dt <- dt[order(dt$level, dt$PAcode, decreasing = F), ]
            dt$index <- seq(1:nrow(dt))
            dt <- dt[order(dt$index, decreasing = T), ]
            
            ## plot function
            ggplotly(
            ggplot() +
              
              # selection
              geom_bar(data = dt,
                       aes(x=reorder(PAcode, -index),
                           y=valueSelection, 
                           fill = ColSel,
                           label2 = Geo),
                       position = position_dodge(width=0.9), stat="identity", width=0.8) +
              geom_text(data = dt,
                        aes(label=paste0(valueSelection*100,"%"),
                            label2 = Geo,
                            y=valueSelection,
                            x=PAcode),
                        size=3,
                        hjust = -0.2,
                        vjust = 0.1,
                        position = position_dodge(width = 1)) +
              
              # expenditure
              geom_bar(data = dt,
                       aes(x=reorder(PAcode, -index),
                           y=valueExpenditure, 
                           fill = ColExp,
                           label2 = Geo),
                       position = position_dodge(width=0.9), stat="identity", width=0.4) +
              geom_text(data = dt,
                        aes(label=paste0(valueExpenditure*100,"%"),
                            label2 = Geo,
                            y=valueExpenditure,
                            x=PAcode),
                        size=3,
                        hjust = -0.2,
                        vjust = 0.1,
                        position = position_dodge(width = 1)) +
              
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
              
              coord_flip(ylim = c(0,max(dt$valueSelection, na.rm=T)*1.1), expand = T) +
              

              scale_y_continuous(expand = c(0,0), labels = percent_format()) +
              
              theme(
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x = element_text(hjust = 0.5, size = 10),
                    axis.text.y = element_text(size = 10),
                    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    panel.grid.major.x = element_line(size = 0.5, colour = 'lightgrey'),
                    panel.grid.major.y = element_blank(),
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