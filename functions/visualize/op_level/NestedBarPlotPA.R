output$pbarPA <- renderPlot({
  
        setMS <- input$msOPp
        setFund <- input$fundOPp
        setCCI <- input$titleOPp # Nordrhein-Westfalen - ERDF
            
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
            colnames(dt) <- c("OPname","PAcode", "PlannedPAx", "SelectionPAx", "ExpenditurePAx")
            
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
            dt <- rbind(dt, data.frame("OPname"="OP", "PAcode"="OP","PlannedPAx"= as.numeric(pOP),
                                       "SelectionPAx"= as.numeric(sOP), "ExpenditurePAx"=as.numeric(eOP)))
            dt <- rbind(dt, data.frame("OPname"=setMS, "PAcode"=setMS,"PlannedPAx"= as.numeric(pMS),
                                       "SelectionPAx"= as.numeric(sMS), "ExpenditurePAx"=as.numeric(eMS)))
            dt <- rbind(dt, data.frame("OPname"="EU", "PAcode"="EU","PlannedPAx"= as.numeric(pEU),
                                       "SelectionPAx"= as.numeric(sEU), "ExpenditurePAx"=as.numeric(eEU)))
            
            ## rates calculation and set of colours
            dt$valueSelection <- round((dt$SelectionPAx/dt$PlannedPAx)*100,1)
            dt$valueExpenditure <- round((dt$ExpenditurePAx/dt$PlannedPAx)*100,1)
            
            dt$level <- ifelse(dt$OPname == "OP", "OP",
                               ifelse(dt$OPname == setMS, "MS",
                                      ifelse(dt$OPname =="EU", "EU","PA")))
            
            dt$ColSel <- ifelse(dt$level == "MS", "#42beb8",
                                ifelse(dt$level == "EU", "#48686c", "#b1dcc0"))
            
            dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31",
                                ifelse(dt$level == "EU", "#f2c400", "#f04e52"))
            
            ## order of bars
            dt$PAcode <- as.character(dt$PAcode)
            dt$PAcode <- ifelse(is.na(dt$PAcode),"unspecified",dt$PAcode)
            dt <- dt[order(dt$level, dt$PAcode, decreasing = F), ]
            dt$index <- seq(1:nrow(dt))
            dt <- dt[order(dt$index, decreasing = T), ]
            
            ## plot function

            ggplot() +
              
              # selection
              geom_bar(data = dt,
                       aes(x=reorder(PAcode, -index),
                           y=valueSelection, 
                           fill = ColSel),
                       position = position_dodge(width=0.9), stat="identity", width=0.8) +
              geom_text(data = dt,
                        aes(label=paste0(valueSelection,"%"),
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
                           fill = ColExp),
                       position = position_dodge(width=0.9), stat="identity", width=0.4) +
              geom_text(data = dt,
                        aes(label=paste0(valueExpenditure,"%"),
                            y=valueExpenditure,
                            x=PAcode),
                        size=3,
                        hjust = -0.2,
                        vjust = 0.1,
                        position = position_dodge(width = 1)) +
              
              # legend, footnote
              scale_fill_manual(name = "",
                                values=c("#b1dcc0"="#b1dcc0", # OP sel
                                         "#f04e52"="#f04e52", # OP exp
                                         "#42beb8"="#42beb8", # MS sel
                                         "#ed7d31"="#ed7d31", # MS exp
                                         "#48686c"="#48686c", # EU sel
                                         "#f2c400"="#f2c400"),# EU exp
                                # breaks e labels gestiscono ordine ed etichette                  
                                breaks = c("#48686c","#f2c400",
                                           "#42beb8","#ed7d31",
                                           "#b1dcc0", "#f04e52"),
                                labels=c("EU rate of project selection","EU rate of expenditure declared",
                                         "MS rate of project selection","MS rate of expenditure declared",
                                         "OP rate of project selection","OP rate of expenditure declared")) +

              
              theme_classic() +
              
              #scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
              scale_y_continuous(expand = c(0, 0),
                                 breaks = seq(0, max(dt$valueSelection, na.rm=T), by=10),
                                 labels =  paste0(seq(0, max(dt$valueSelection, na.rm=T),by=10),"%")) +
              
              coord_flip(ylim = c(0,max(dt$valueSelection, na.rm=T)+max(dt$valueSelection, na.rm=T)*0.1), expand = T) +
              
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
                    panel.grid.major.x = element_line(size = 0.5, colour = 'lightgrey'),
                    panel.grid.major.y = element_blank(),
                    plot.title = element_text(face="bold"),
                    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
            

})