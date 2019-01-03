output$pbarTS <- renderPlot({
  
  setMS <- input$msOPp # 'DE'
  setFund <- input$fundOPp # 'ERDF'
  setCCI <- input$titleOPp # 'Nordrhein-Westfalen - ERDF'
  
  # color factory
  cs14 = "#83d0c9"
  cs15 = "#65c3ba"
  cs16 = "#54b2a9"
  cs17 = "#35a79c"
  cs18 = "#009688"
  cs19 = "#00786c"
  
  ce14 = "#eec1ad"
  ce15 = "#dbac98"
  ce16 = "#d29985"
  ce17 = "#c98276"
  ce18 = "#e35d6a"
  ce19 = "#b54a54"
  
          
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
        Geo <- rep(setCCI, nrow(OPI))
        dtPA <- data.frame(Geo, merge(OPP, OPI, by='priority', type='left', match='all'))
        colnames(dtPA) <- c("Geo","PAcode", "PlannedPAx", "year","SelectionPAx", "ExpenditurePAx")
        
      
        dtPA$valueSelection <- rnd(dtPA$SelectionPAx/dtPA$PlannedPAx)
        dtPA$valueExpenditure <- rnd(dtPA$ExpenditurePAx/dtPA$PlannedPAx)
        
        dtPA$ColSel <- case_when(
          dtPA$year == "2015" ~ "cs15",
          dtPA$year == "2016" ~ "cs16",
          dtPA$year == "2017" ~ "cs17",
          dtPA$year == "2018" ~ "cs18",
          dtPA$year == "2019" ~ "cs19",
          TRUE ~ "cs14"
        )
        
        dtPA$ColExp <- case_when(
          dtPA$year == "2015" ~ "ce15",
          dtPA$year == "2016" ~ "ce16",
          dtPA$year == "2017" ~ "ce17",
          dtPA$year == "2018" ~ "ce18",
          dtPA$year == "2019" ~ "ce19",
          TRUE ~ "ce14"
        )

        # OP
        pOP <- dfP %>% filter(title==setCCI & fund==setFund) %>% summarise(sum(total_amount))
        iOP <- dfI %>% filter(title==setCCI & fund==setFund) %>% 
          group_by(year) %>% summarise(sum(total_eligible_cost, na.rm=T),
                                       sum(total_eligible_expenditure, na.rm=T))
        colnames(iOP) <- c("year", "SelectionPAx","ExpenditurePAx")
        
        dtOP <- data.frame("Geo"="OP", "PAcode"="OP","PlannedPAx"= as.numeric(pOP),iOP)
        
        
        dtOP$valueSelection <- rnd(dtOP$SelectionPAx/dtOP$PlannedPAx)
        dtOP$valueExpenditure <- rnd(dtOP$ExpenditurePAx/dtOP$PlannedPAx)
        
        dtOP$ColSel <- case_when(
          dtOP$year == "2015" ~ "cs15",
          dtOP$year == "2016" ~ "cs16",
          dtOP$year == "2017" ~ "cs17",
          dtOP$year == "2018" ~ "cs18",
          dtOP$year == "2019" ~ "cs19",
          TRUE ~ "cs14"
        )
        
        dtOP$ColExp <- case_when(
          dtOP$year == "2015" ~ "ce15",
          dtOP$year == "2016" ~ "ce16",
          dtOP$year == "2017" ~ "ce17",
          dtOP$year == "2018" ~ "ce18",
          dtOP$year == "2019" ~ "ce19",
          TRUE ~ "ce14"
        )
        
        
        ## priority axis right plot

        ## order of bars
        dtPA$PAcode <- as.character(dtPA$PAcode)
        dtPA$PAcode <- ifelse(is.na(dtPA$PAcode),"unspecified",dtPA$PAcode)
        dtPA <- dtPA[order(dtPA$PAcode, -rank(dtPA$year), decreasing = T), ]
        dtPA$index <- seq(1:nrow(dtPA))
        
        year_order <- factor(dtPA$year, levels = c('2019','2018','2017','2016','2015','2014'), ordered=T)
        
        
        # second level axis label
        
        year_label <- seq(0.65,length.out=nrow(dtPA), by=0.15)
        for (i in 1:length(year_label)){
          year_label[i+6] <- year_label[i] + 1
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
          
          # bars color, legend, labels
          scale_fill_manual(name = "",
                            values=c(cs14 = cs14, # dtPA$ColSel = factory color
                                     cs15 = cs15,
                                     cs16 = cs16,
                                     cs17 = cs17,
                                     cs18 = cs18,
                                     cs19 = cs19,
                                     
                                     ce14 = ce14,
                                     ce15 = ce15,
                                     ce16 = ce16,
                                     ce17 = ce17,
                                     ce18 = ce18,
                                     ce19 = ce19),
                            # breaks e labels gestiscono ordine ed etichette                  
                            breaks = c(cs14,cs15,cs16,cs17,cs18,cs19,
                                       ce14,ce15,ce16,ce17,ce18,ce19),
                            labels=c("EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared")) +
          
          theme_classic() +
          
          scale_y_continuous(expand = c(0,0), labels = percent_format()) +
          
          expand_limits(y = max(dtPA$valueSelection, na.rm=T)*1.1) +
          
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
        
        
        ## operational programme left plot
        
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
                            values=c(cs14 = cs14, # dtPA$ColSel = factory color
                                     cs15 = cs15,
                                     cs16 = cs16,
                                     cs17 = cs17,
                                     cs18 = cs18,
                                     cs19 = cs19,
                                     
                                     ce14 = ce14,
                                     ce15 = ce15,
                                     ce16 = ce16,
                                     ce17 = ce17,
                                     ce18 = ce18,
                                     ce19 = ce19),
                            # breaks e labels gestiscono ordine ed etichette                  
                            breaks = c(cs14,cs15,cs16,cs17,cs18,cs19,
                                       ce14,ce15,ce16,ce17,ce18,ce19),
                            labels=c("EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared",
                                     "MS rate of project selection","MS rate of expenditure declared",
                                     "EU rate of project selection","EU rate of expenditure declared",
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
        
        
        # assemble plots 
        grid.arrange(gOP, gPA,ncol=2, widths = c(0.3, 0.7))

})