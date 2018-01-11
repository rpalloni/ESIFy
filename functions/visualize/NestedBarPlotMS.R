output$pbarMS <- renderPlot({
  
  setMS <- input$ms
  
MSP <- dfP %>%
  filter(ms==setMS & fund=="ERDF") %>%
  group_by(title) %>%
  summarise(sum(total_amount, na.rm=T))

MSI <- dfI %>%
  filter(ms==setMS & fund=="ERDF" & year==max(as.numeric(dfI$year))) %>%
  group_by(title) %>%
  summarise(sum(total_eligible_cost, na.rm=T), 
            sum(total_eligible_expenditure, na.rm=T))


dt <- data.frame(MSP,MSI[-1])
colnames(dt) <- c("Region", "PlannedOP", "SelectionOP", "ExpenditureOP")


## fare stesse procedure per valori MS e EU

pMS <- dfP %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_amount))
sMS <- dfI %>% filter(ms==setMS & fund=="ERDF" & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_cost, na.rm=T))
eMS <- dfI %>% filter(ms==setMS & fund=="ERDF" & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_expenditure, na.rm=T))

pEU <- dfP %>% filter(fund=="ERDF") %>% summarise(sum(total_amount))
sEU <- dfI %>% filter(fund=="ERDF" & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_cost, na.rm=T))
eEU <- dfI %>% filter(fund=="ERDF" & year==max(as.numeric(dfI$year))) %>% summarise(sum(total_eligible_expenditure, na.rm=T))


dt <- rbind(dt, data.frame("Region"=setMS, "PlannedOP"= as.numeric(pMS),
                           "SelectionOP"= as.numeric(sMS), "ExpenditureOP"=as.numeric(eMS)))
dt <- rbind(dt, data.frame("Region"="EU", "PlannedOP"= as.numeric(pEU),
                           "SelectionOP"= as.numeric(sEU), "ExpenditureOP"=as.numeric(eEU)))
##


dt$ratio_f <- dt$SelectionOP/dt$PlannedOP
dt$ratio_g <- dt$ExpenditureOP/dt$PlannedOP
dt$level <- ifelse(dt$Region == setMS, "MS",
                   ifelse(dt$Region =="EU", "EU","OP"))

dt$ColSel <- ifelse(dt$level == "MS", "#42beb8",
                    ifelse(dt$level == "EU", "#48686c", "#b1dcc0"))

dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31",
                    ifelse(dt$level == "EU", "#f2c400", "#f04e52"))


# riordinare i valori per avere il matching corretto
dt <- dt[order(-dt$ratio_f),]

ggplot() +
  geom_bar(data=dt, 
           aes(x=reorder(Region,-ratio_f), y=ratio_f*100), fill = dt$ColSel,
           position = position_dodge(width=0.9), stat="identity", width=0.8) +
  geom_bar(data=dt, 
           aes(x=Region, y=ratio_g*100), fill = dt$ColExp,
           position = position_dodge(width=0.9), stat="identity", width=0.4) +
  ggtitle(paste("Rate of project selection and expenditure declared by ",setMS," Operational Programmes (% of planned financing)"))+
  xlab("Operational Programmes") + 
  ylab("Rate of project selection and expenditure declared (%)") +
  theme_classic() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(hjust = 0.5, size = 8),
        panel.border = element_blank(),
        plot.title = element_text(face="bold"))

})