output$pbarTS <- renderPlot({
  
  setCCI <- input$title
  setMS <- input$ms
  
OPP <- dfP %>%
  filter(title==setCCI & fund=="ERDF") %>%
  group_by(priority) %>%
  summarise(sum(total_amount))

OPI <- dfI %>%
  filter(title==setCCI & fund=="ERDF") %>%
  group_by(priority, year) %>%
  summarise(sum(total_eligible_cost, na.rm=T), 
            sum(total_eligible_expenditure, na.rm=T))

# PA
OPname <- rep(setCCI, nrow(OPI))
dtPA <- data.frame(OPname,join(OPP, OPI, by='priority', type='left', match='all'))
colnames(dtPA) <- c("OPname","PAcode", "PlannedPAx", "year","SelectionPAx", "ExpenditurePAx")


dtPA$ratio_f <- dtPA$SelectionPAx/dtPA$PlannedPAx
dtPA$ratio_g <- dtPA$ExpenditurePAx/dtPA$PlannedPAx

dtPA$ColSel <- ifelse(dtPA$year == "2015", "#88e188",
                    ifelse(dtPA$year == "2016", "#6aaf6a", "#B4EEB4"))

dtPA$ColExp <- ifelse(dtPA$year == "2015", "#FF7878",
                    ifelse(dtPA$year == "2016", "#EA4C44", "#FF9999"))

# OP
pOP <- dfP %>% filter(title==setCCI & fund=="ERDF") %>% summarise(sum(total_amount))
iOP <- dfI %>% filter(title==setCCI & fund=="ERDF") %>% 
  group_by(year) %>% summarise(sum(total_eligible_cost, na.rm=T),
                               sum(total_eligible_expenditure, na.rm=T))
colnames(iOP) <- c("year", "SelectionPAx","ExpenditurePAx")

dtOP <- data.frame("OPname"="OP", "PAcode"="OP","PlannedPAx"= as.numeric(pOP),iOP)


dtOP$ratio_f <- dtOP$SelectionPAx/dtOP$PlannedPAx
dtOP$ratio_g <- dtOP$ExpenditurePAx/dtOP$PlannedPAx

dtOP$ColSel <- ifelse(dtOP$year == "2015", "#88e188",
                    ifelse(dtOP$year == "2016", "#6aaf6a", "#B4EEB4"))

dtOP$ColExp <- ifelse(dtOP$year == "2015", "#FF7878",
                    ifelse(dtOP$year == "2016", "#EA4C44", "#FF9999"))


## order of bars
dtPA <- dtPA[order(dtPA$PAcode, -rank(dtPA$year), decreasing = T), ]
dtPA$index <- seq(1:nrow(dtPA))

year_order <- factor(dtPA$year, levels = c('2016','2015','2014'), ordered=T)


## plot function

year_label <- seq(0.7,length.out=nrow(dtPA), by=0.3)
for (i in 1:length(year_label)){
  year_label[i+3] <- year_label[i] + 1
}
yl <- year_label[1:nrow(dtPA)]

gPA <- ggplot() +
  geom_bar(data=dtPA, 
           aes(x=reorder(PAcode,index), y=ratio_f*100, group=year_order), fill = dtPA$ColSel,
           position = position_dodge(width=0.9), stat="identity", width=0.8) +
  geom_bar(data=dtPA, 
           aes(x=PAcode, y=ratio_g*100, group=year_order), fill = dtPA$ColExp,
           position = position_dodge(width=0.9), stat="identity", width=0.4) +
  geom_hline(yintercept=0) + # force the presence of the vertical line after years
  xlab("PA progress over time") + 
  ylab("Rate of project selection and expenditure declared (%)") +
  theme_classic() +
  coord_flip(ylim = c(-4, max(dtPA$ratio_f*100)),
             xlim = c(0.5, length(unique(dtPA$PAcode))+0.4),
             expand = F) +
  annotate(geom = "text", x = yl , y = -2, label = rev(dtPA$year), size = 3) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.line.y =  element_blank())


gOP <- ggplot() +
  geom_bar(data=dtOP, 
           aes(x=year, y=ratio_f*100), fill = dtOP$ColSel,
           position = position_dodge(width=0.9), stat="identity", width=0.8) +
  geom_bar(data=dtOP, 
           aes(x=year, y=ratio_g*100), fill = dtOP$ColExp,
           position = position_dodge(width=0.9), stat="identity", width=0.4) +
  xlab("OP progress over time") + 
  ylab("Rate of project selection and expenditure declared (%)") +
  theme_classic() +
  theme(plot.margin = unit(c(0,0.5,0.5,0.5), "cm"),
        axis.line.y =  element_blank())


grid.arrange(gOP, gPA,ncol=2, widths = c(0.2, 0.8))

})