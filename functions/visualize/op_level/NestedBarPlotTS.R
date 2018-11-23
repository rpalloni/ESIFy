output$pbarTS <- renderPlot({
  
  setMS <- input$msOPp
  setFund <- input$fundOPp
  setCCI <- input$titleOPp # Nordrhein-Westfalen - ERDF
  
          
        OPP <- dfP %>%
          filter(title==setCCI & fund==setFund) %>%
          group_by(priority) %>%
          summarise(sum(total_amount, na.rm=T))
        
        OPI <- dfI %>%
          filter(title==setCCI & fund==setFund) %>%
          group_by(priority, year) %>%
          summarise(sum(total_eligible_cost, na.rm=T), 
                    sum(total_eligible_expenditure, na.rm=T))
        
        # PA
        OPname <- rep(setCCI, nrow(OPI))
        dtPA <- data.frame(OPname,merge(OPP, OPI, by='priority', type='left', match='all'))
        colnames(dtPA) <- c("OPname","PAcode", "PlannedPAx", "year","SelectionPAx", "ExpenditurePAx")
        
      
        
        dtPA$valueSelection <- rnd(dtPA$SelectionPAx/dtPA$PlannedPAx)
        dtPA$valueExpenditure <- rnd(dtPA$ExpenditurePAx/dtPA$PlannedPAx)
        
        dtPA$ColSel <- ifelse(dtPA$year == "2015", "#65c3ba",
                            ifelse(dtPA$year == "2016", "#54b2a9",
                                   ifelse(dtPA$year == "2017", "#35a79c",
                                          ifelse(dtPA$year == "2018", "#009688","#83d0c9"))))
        
        dtPA$ColExp <- ifelse(dtPA$year == "2015", "#dbac98",
                            ifelse(dtPA$year == "2016", "#d29985",
                                   ifelse(dtPA$year == "2017", "#c98276",
                                          ifelse(dtPA$year == "2018", "#e35d6a","#eec1ad"))))
        
        # OP
        pOP <- dfP %>% filter(title==setCCI & fund==setFund) %>% summarise(sum(total_amount))
        iOP <- dfI %>% filter(title==setCCI & fund==setFund) %>% 
          group_by(year) %>% summarise(sum(total_eligible_cost, na.rm=T),
                                       sum(total_eligible_expenditure, na.rm=T))
        colnames(iOP) <- c("year", "SelectionPAx","ExpenditurePAx")
        
        dtOP <- data.frame("OPname"="OP", "PAcode"="OP","PlannedPAx"= as.numeric(pOP),iOP)
        
        
        dtOP$valueSelection <- rnd(dtOP$SelectionPAx/dtOP$PlannedPAx)
        dtOP$valueExpenditure <- rnd(dtOP$ExpenditurePAx/dtOP$PlannedPAx)
        
        dtOP$ColSel <- ifelse(dtOP$year == "2015", "#65c3ba",
                            ifelse(dtOP$year == "2016", "#54b2a9",
                                   ifelse(dtOP$year == "2017", "#35a79c",
                                          ifelse(dtOP$year == "2018", "#009688","#83d0c9"))))
        
        dtOP$ColExp <- ifelse(dtOP$year == "2015", "#dbac98",
                            ifelse(dtOP$year == "2016", "#d29985",
                                   ifelse(dtOP$year == "2017", "#c98276",
                                          ifelse(dtOP$year == "2018", "#e35d6a","#eec1ad"))))
      
        
        ## order of bars
        dtPA <- dtPA[order(dtPA$PAcode, -rank(dtPA$year), decreasing = T), ]
        dtPA$index <- seq(1:nrow(dtPA))
        
        year_order <- factor(dtPA$year, levels = c('2018','2017','2016','2015','2014'), ordered=T)
        
        
        ## plot function
        
        year_label <- seq(0.65,length.out=nrow(dtPA), by=0.18)
        for (i in 1:length(year_label)){
          year_label[i+5] <- year_label[i] + 1
        }
        yl <- year_label[1:nrow(dtPA)]
        
        gPA <- ggplot() +
          # selection
          geom_bar(data = dtPA,
                   aes(x=reorder(PAcode, index),
                       y=valueSelection, 
                       fill = ColSel,
                       group = year_order),
                   position = position_dodge(width=0.9), stat="identity", width=0.8) +
          geom_text(data = dtPA,
                    aes(label=paste0(valueSelection*100,"%"),
                        y=valueSelection,
                        x=PAcode,
                        group = year_order),
                    size=3,
                    hjust = -0.2,
                    vjust = 0.1,
                    position = position_dodge(width = 0.9)) +
          
          # expenditure
          geom_bar(data = dtPA,
                   aes(x=reorder(PAcode, index),
                       y=valueExpenditure, 
                       fill = ColExp,
                       group = year_order),
                   position = position_dodge(width=0.9), stat="identity", width=0.4) +
          geom_text(data = dtPA,
                    aes(label=paste0(valueExpenditure*100,"%"),
                        y=valueExpenditure,
                        x=PAcode,
                        group = year_order),
                    size=3,
                    hjust = -0.2,
                    vjust = 0.1,
                    position = position_dodge(width = 0.9)) +
          
          # legend, footnote
          scale_fill_manual(name = "",
                            values=c("#83d0c9"="#83d0c9", # 2014 sel
                                     "#65c3ba"="#65c3ba", # 2015 sel
                                     "#54b2a9"="#54b2a9", # 2016 sel
                                     "#35a79c"="#35a79c", # 2017 sel
                                     "#009688"="#009688", # 2018 sel
                                     "#eec1ad"="#eec1ad", # 2014 exp
                                     "#dbac98"="#dbac98", # 2015 exp
                                     "#d29985"="#d29985", # 2016 exp
                                     "#c98276"="#c98276", # 2017 exp
                                     "#e35d6a"="#e35d6a"), # 2018 exp
                            # breaks e labels gestiscono ordine ed etichette                  
                            breaks = c("#83d0c9",
                                       "#65c3ba",
                                       "#54b2a9",
                                       "#35a79c",
                                       "#009688",
                                       "#eec1ad",
                                       "#dbac98",
                                       "#d29985",
                                       "#c98276",
                                       "#e35d6a"),
                            labels=c("EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared")) +
          
          theme_classic() +
          
          scale_y_continuous(expand = c(0,0), labels = percent_format()) +
          
          expand_limits(y = max(dt$valueSelection, na.rm=T)*1.1) +
          
          # -0.02 and -0.04 are x-scale dependent
          coord_flip(ylim = c(-0.04,max(dtPA$valueSelection, na.rm=T)*1.1), expand = F) +
          annotate(geom = "text", x = yl , y = -0.02, label = rev(dtPA$year), size = 3) +
          
          theme(legend.position = "none",
                legend.box.margin = margin(0.5, 0.5, 0.5, 0.5), # top, right, bottom, left
                legend.box.background = element_rect(colour = "white"),
                legend.direction = "horizontal",
                legend.title = element_blank(),
                legend.text=element_text(size=10),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.x = element_text(hjust = 0.5, size = 10),
                axis.text.y = element_text(size = 10),
                axis.line.x=element_line(),
                axis.line.y=element_blank(),
                #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                panel.grid.major.x = element_line(size = 0.5, colour = 'lightgrey'),
                panel.grid.major.y = element_blank(),
                plot.title = element_text(face="bold"),
                plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
        
        
        ######################################################################################
        gOP <- ggplot() +
          # selection
          geom_bar(data = dtOP,
                   aes(x=year,
                       y=valueSelection, 
                       fill = ColSel),
                   position = position_dodge(width=0.9), stat="identity", width=0.8) +
          geom_text(data = dtOP,
                    aes(label=paste0(valueSelection*100,"%"),
                        y=valueSelection,
                        x=year),
                    size=3,
                    hjust = 0.5,
                    vjust = -0.2,
                    position = position_dodge(width = 0.9)) +
          
          # expenditure
          geom_bar(data = dtOP,
                   aes(x=year,
                       y=valueExpenditure, 
                       fill = ColExp),
                   position = position_dodge(width=0.9), stat="identity", width=0.4) +
          geom_text(data = dtOP,
                    aes(label=paste0(valueExpenditure*100,"%"),
                        y=valueExpenditure,
                        x=year),
                    size=3,
                    hjust = 0.5,
                    vjust = -0.2,
                    position = position_dodge(width = 0.9)) +
          
          # legend, footnote
          scale_fill_manual(name = "",
                            values=c("#83d0c9"="#83d0c9", # 2014 sel
                                     "#65c3ba"="#65c3ba", # 2015 sel
                                     "#54b2a9"="#54b2a9", # 2016 sel
                                     "#35a79c"="#35a79c", # 2017 sel
                                     "#009688"="#009688", # 2018 sel
                                     "#eec1ad"="#eec1ad", # 2014 exp
                                     "#dbac98"="#dbac98", # 2015 exp
                                     "#d29985"="#d29985", # 2016 exp
                                     "#c98276"="#c98276", # 2017 exp
                                     "#e35d6a"="#e35d6a"), # 2018 exp
                            # breaks e labels gestiscono ordine ed etichette                  
                            breaks = c("#83d0c9",
                                       "#65c3ba",
                                       "#54b2a9",
                                       "#35a79c",
                                       "#009688",
                                       "#eec1ad",
                                       "#dbac98",
                                       "#d29985",
                                       "#c98276",
                                       "#e35d6a"),
                            labels=c("EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared")) +
          
          
          theme_classic() +
          
          
          coord_cartesian() +
          
          
          scale_y_continuous(expand = c(0,0), labels = percent_format()) +
          
          expand_limits(y = max(dtOP$valueSelection, na.rm=T)*1.1) +


          theme(legend.position = "none",
                legend.box.margin = margin(0.5, 0.5, 0.5, 0.5), # top, right, bottom, left
                legend.box.background = element_rect(colour = "white"),
                legend.direction = "horizontal",
                legend.title = element_blank(),
                legend.text=element_text(size=10),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.line.x=element_line(),
                axis.line.y=element_blank(),
                axis.text.x = element_text(hjust = 0.5, size = 10),
                axis.text.y = element_text(size = 10),
                #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(size = 0.5, colour = 'lightgrey'),
                plot.title = element_text(face="bold"),
                plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
        
        
        grid.arrange(gOP, gPA,ncol=2, widths = c(0.3, 0.7))

})