output$pbarOP <- renderPlot({
  
  setCCI <- input$title
  setMS <- input$ms
  

OPP <- dfP %>%
  filter(title==setCCI & fund=="ERDF") %>%
  group_by(priority) %>%
  summarise(sum(total_amount))

OPI <- dfI %>%
  filter(title==setCCI & fund=="ERDF") %>%
  group_by(priority) %>%
  summarise(sum(total_eligible_cost, na.rm=T), 
            sum(total_eligible_expenditure, na.rm=T))

OPname <- rep(setCCI, nrow(OPI))
dt <- data.frame(OPname,OPP,OPI[-1])
colnames(dt) <- c("OPname","PAcode", "PlannedPAx", "SelectionPAx", "ExpenditurePAx")

pOP <- dfP %>% filter(title==setCCI & fund=="ERDF") %>% summarise(sum(total_amount))
sOP <- dfI %>% filter(title==setCCI & fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
eOP <- dfI %>% filter(title==setCCI & fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))

pMS <- dfP %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_amount))
sMS <- dfI %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
eMS <- dfI %>% filter(ms==setMS & fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))

pEU <- dfP %>% filter(fund=="ERDF") %>% summarise(sum(total_amount))
sEU <- dfI %>% filter(fund=="ERDF") %>% summarise(sum(total_eligible_cost, na.rm=T))
eEU <- dfI %>% filter(fund=="ERDF") %>% summarise(sum(total_eligible_expenditure, na.rm=T))

# merge datasets
dt <- rbind(dt, data.frame("OPname"="OP", "PAcode"="OP","PlannedPAx"= as.numeric(pOP),
                           "SelectionPAx"= as.numeric(sOP), "ExpenditurePAx"=as.numeric(eOP)))
dt <- rbind(dt, data.frame("OPname"=setMS, "PAcode"=setMS,"PlannedPAx"= as.numeric(pMS),
                           "SelectionPAx"= as.numeric(sMS), "ExpenditurePAx"=as.numeric(eMS)))
dt <- rbind(dt, data.frame("OPname"="EU", "PAcode"="EU","PlannedPAx"= as.numeric(pEU),
                           "SelectionPAx"= as.numeric(sEU), "ExpenditurePAx"=as.numeric(eEU)))

## rates calculation and set of colours
dt$ratio_f <- dt$SelectionPAx/dt$PlannedPAx
dt$ratio_g <- dt$ExpenditurePAx/dt$PlannedPAx
dt$level <- ifelse(dt$OPname == "OP", "OP",
                   ifelse(dt$OPname == setMS, "MS",
                          ifelse(dt$OPname =="EU", "EU","PA")))

dt$ColSel <- ifelse(dt$level == "MS", "#42beb8",
                    ifelse(dt$level == "EU", "#48686c", "#b1dcc0"))

dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31",
                    ifelse(dt$level == "EU", "#f2c400", "#f04e52"))

## order of bars
dt$PAcode <- as.character(dt$PAcode)
dt <- dt[order(dt$level, dt$PAcode, decreasing = F), ]
dt$index <- seq(1:nrow(dt))
dt <- dt[order(dt$index, decreasing = T), ]

## plot function
ggplot() +
  geom_bar(data=dt, 
           aes(x=reorder(PAcode, -index), y=ratio_f*100), fill = dt$ColSel,
           position = position_dodge(width=0.9), stat="identity", width=0.8) +
  geom_text(data=dt, aes(label=paste0(round(ratio_f*100,1),"%"),
                         y=ratio_f*100+(ratio_f*100)*0.03, x=PAcode ), size=3) +
  geom_bar(data=dt, 
           aes(x=PAcode, y=ratio_g*100), fill = dt$ColExp,
           position = position_dodge(width=0.9), stat="identity", width=0.4) +
  geom_text(data=dt, aes(label=paste0(round(ratio_g*100,1),"%"),
                         y=ratio_g*100+(ratio_g*100)*0.03, x=PAcode), size=3) +
  ggtitle("Rate of project selection and expenditure declared by OP Priority Axis (% of planned financing)")+
  xlab("EU, MS, OP and Priority Axes") + 
  ylab("Rate of project selection and expenditure declared (%)") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(hjust = 1, size = 8),
        panel.border = element_blank(),
        plot.title = element_text(face="bold"))

})