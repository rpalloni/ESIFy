
output$pbarTO <- renderPlot({
  
  setCCI <- input$title
  setMS <- input$ms
  
  # Nordrhein-Westfalen - ERDF

OPI <- dfI %>%
  filter(title==setCCI & fund=="ERDF" & year==max(as.numeric(dfI$year))) %>%
  group_by(to) %>%
  summarise(sum(total_eligible_cost, na.rm=T), 
            sum(total_eligible_expenditure, na.rm=T))



OPI <- OPI[OPI$to != 'MULTI',]


OPP <- dfP %>%
  filter(title==setCCI & fund=="ERDF") %>%
  group_by(to) %>%
  summarise(sum(total_amount))




OPP$toM <- rep(0, nrow(OPP))
OPP$to <- as.character(OPP$to)
OPI$to <- as.character(OPI$to)
for(i in 1:nrow(OPP)){
  if(OPP$to[i] %in% OPI$to){
    OPP$toM[i] <- OPP$to[i]
  } else {
    OPP$toM[i] <- 'MULTI'
  }
}

OPP <- OPP %>% 
  group_by(toM) %>%
  summarise(sum(`sum(total_amount)`))

OPP <- OPP[OPP$toM != 'MULTI',]

# MS

MSI <- dfI %>%
  filter(ms==setMS & fund=="ERDF" & year==max(as.numeric(dfI$year))) %>%
  group_by(to) %>%
  summarise(sum(total_eligible_cost, na.rm=T), 
            sum(total_eligible_expenditure, na.rm=T))

MSI <- MSI[MSI$to %in% OPI$to,]


MSP <- dfP %>%
  filter(ms==setMS & fund=="ERDF") %>%
  group_by(to) %>%
  summarise(sum(total_amount))

MSP <- MSP[MSP$to %in% OPP$toM,]


# EU

EUI <- dfI %>%
  filter(fund=="ERDF" & year==max(as.numeric(dfI$year))) %>%
  group_by(to) %>%
  summarise(sum(total_eligible_cost, na.rm=T), 
            sum(total_eligible_expenditure, na.rm=T))

EUI <- EUI[EUI$to %in% OPI$to,]


EUP <- dfP %>%
  filter(fund=="ERDF") %>%
  group_by(to) %>%
  summarise(sum(total_amount))

EUP <- EUP[EUP$to %in% OPP$toM,]



dtOP <- data.frame(setCCI,OPP[-1],OPI)
colnames(dtOP) <- c("Name", "PlannedTO", "TOcode", "SelectionTO", "ExpenditureTO")
dtMS <- data.frame(setMS,MSP[-1],MSI)
colnames(dtMS) <- c("Name", "PlannedTO", "TOcode", "SelectionTO", "ExpenditureTO")
dtEU <- data.frame('EU',EUP[-1],EUI)
colnames(dtEU) <- c("Name", "PlannedTO", "TOcode", "SelectionTO", "ExpenditureTO")
dt <- rbind(dtOP, dtMS, dtEU)


## rates calculation and set of colours
dt$ratio_f <- dt$SelectionTO/dt$PlannedTO
dt$ratio_g <- dt$ExpenditureTO/dt$PlannedTO
dt$level <- ifelse(dt$Name == "OP", "OP",
                   ifelse(dt$Name == setMS, "MS",
                          ifelse(dt$Name =="EU", "EU","OP")))

dt$ColSel <- ifelse(dt$level == "MS", "#42beb8",
                    ifelse(dt$level == "EU", "#48686c", "#b1dcc0"))

dt$ColExp <- ifelse(dt$level == "MS", "#ed7d31",
                    ifelse(dt$level == "EU", "#f2c400", "#f04e52"))

## order of bars
dt <- dt[order(dt$TOcode, dt$level), ]
dt$index <- seq(1:nrow(dt))

## plot function
ggplot() +
  geom_bar(data=dt, 
           aes(x=reorder(TOcode,index), y=ratio_f*100, group = Name),fill = dt$ColSel,
           position = position_dodge(width=0.9), stat="identity", width=0.8) +
  geom_bar(data=dt, 
           aes(x=TOcode, y=ratio_g*100, group = Name), fill = dt$ColExp,
           position = position_dodge(width=0.9), stat="identity", width=0.4) +
  ggtitle("Rate of project selection and expenditure declared by OP Thematic Objective (% of planned financing)")+
  xlab("OP, MS, EU by comparable Thematic Objectives") + 
  ylab("Rate of project selection and expenditure declared (%)") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(hjust = 1, size = 12),
        panel.border = element_blank(),
        plot.title = element_text(face="bold"),
        plot.margin = unit(c(0,0.5,2,0.5), "cm"))

})