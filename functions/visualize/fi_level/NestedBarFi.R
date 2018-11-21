##############################################
############### Visualization ################
##############################################
library(scales)
library(stringr)
library(ggplot2)
library(gtable)
library(grid) #grobTree
library(gridExtra) #grid.arrange

############## Committment/Payment to FI by MS ############

dfp <- data.frame("MS"=opcfi1$country,"OP_com"=opcfi1$amt_com,"OP_pay"=opcfi8$amt_paid)

dfp$ColCom <- "#b1dcc0"
dfp$ColPay <- "#f04e52"

dfp <- dfp[order(dfp$OP_com, decreasing = T),]
dfp$index <- seq(1:nrow(dfp))

ggplot() +
  geom_bar(data=dfp,
           aes(x=reorder(MS,index),y=OP_com/1000000, fill=ColCom),
           position = position_dodge(width = 0.9), stat="identity", width = 0.8) +
  geom_text(data=dfp,
            aes(label=ifelse(is.na(OP_com), paste0("N/A"), paste0(round(OP_com/1000000,1))),
                y=ifelse(is.na(OP_com),1, OP_com/1000000+50),x=MS),size=3) +
  
  geom_bar(data=dfp,
           aes(x=MS,y=OP_pay/1000000, fill=ColPay),
           position = position_dodge(width = 0.9), stat="identity", width = 0.4) +
  geom_text(data=dfp,
            aes(label=ifelse(is.na(OP_pay), paste0("N/A"), paste0(round(OP_pay/1000000,1))),
                y=ifelse(is.na(OP_pay),1, OP_pay/1000000+50),x=MS),size=3) +
  
  ylab("EUR million") +
  scale_fill_manual(name="", 
                    values = c("#b1dcc0"="#b1dcc0", "#f04e52"="#f04e52"),
                    breaks = c("#b1dcc0","#f04e52"),
                    labels = c("OP commitments", "OP payments")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,max(dfp$OP_com/1000000,na.rm=T)+max(dfp$OP_com/1000000,na.rm=T)*0.1), expand = T)+
  scale_x_discrete(labels=function(x) str_wrap(x, width=10))+
  scale_y_continuous(expand =c(0,0),
                     breaks = seq(0, max(dfp$OP_com/1000000,na.rm=T)+max(dfp$OP_com/1000000,na.rm=T)*0.1, 
                                  by=ifelse(max(dfp$OP_com/1000000,na.rm=T)>500,500,100)),
                     labels = paste0(comma(seq(0, max(dfp$OP_com/1000000,na.rm=T)+max(dfp$OP_com/1000000,na.rm=T)*0.1, 
                                               by=ifelse(max(dfp$OP_com/1000000,na.rm=T)>500,500,100))))) +
  
  theme(legend.position = "bottom",
        legend.box.margin = margin(0.5,0.5,0.5,0.5),
        legend.box.background = element_rect(colour = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = 0.5, size =8),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(size = 0.5, colour = "lightgrey"),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


############## Committment/Payment to FinRec by MS ############

sfr2bis <- as.data.frame(dt %>% filter(type_fin_instr %in% c("Fund of funds specific fund","Specific fund", "Direct management")) %>% 
                           group_by(country) %>%
                           summarise(amt_com_rec=sum_na_rm(op_amt_com_fin_rec)))

dfp <- data.frame("MS"=sfr2bis$country,"OP_com"=sfr2bis$amt_com,"OP_pay"=sfr8$amt_paid_rec)

dfp$ColCom <- "#42beb8"
dfp$ColPay <- "#ed7d31"

dfp <- dfp[order(dfp$OP_com, decreasing = T),]
dfp$index <- seq(1:nrow(dfp))

ggplot() +
  geom_bar(data=dfp,
           aes(x=reorder(MS,index),y=OP_com/1000000, fill=ColCom),
           position = position_dodge(width = 0.9), stat="identity", width = 0.8) +
  geom_text(data=dfp,
            aes(label=ifelse(is.na(OP_com), paste0("N/A"), paste0(round(OP_com/1000000,1))),
                y=ifelse(is.na(OP_com),10, OP_com/1000000+20),x=MS),size=3) +
  
  geom_bar(data=dfp,
           aes(x=MS,y=OP_pay/1000000, fill=ColPay),
           position = position_dodge(width = 0.9), stat="identity", width = 0.4) +
  geom_text(data=dfp,
            aes(label=ifelse(is.na(OP_pay), paste0("N/A"), paste0(round(OP_pay/1000000,1))),
                y=ifelse(is.na(OP_pay),10, OP_pay/1000000+10),x=MS),size=3) +
  
  ylab("EUR million") +
  scale_fill_manual(name="", 
                    values = c("#42beb8"="#42beb8", "#ed7d31"="#ed7d31"),
                    breaks = c("#42beb8","#ed7d31"),
                    labels = c("FI commitments", "FI payments")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,max(dfp$OP_com/1000000,na.rm=T)+max(dfp$OP_com/1000000,na.rm=T)*0.1), expand = T)+
  scale_x_discrete(labels=function(x) str_wrap(x, width=10))+
  scale_y_continuous(expand =c(0,0),
                     breaks = seq(0, max(dfp$OP_com/1000000,na.rm=T)+max(dfp$OP_com/1000000,na.rm=T)*0.1, 
                                  by=ifelse(max(dfp$OP_com/1000000,na.rm=T)>500,100,50)),
                     labels = paste0(comma(seq(0, max(dfp$OP_com/1000000,na.rm=T)+max(dfp$OP_com/1000000,na.rm=T)*0.1, 
                                               by=ifelse(max(dfp$OP_com/1000000,na.rm=T)>500,100,50))))) +
  
  theme(legend.position = "bottom",
        legend.box.margin = margin(0.5,0.5,0.5,0.5),
        legend.box.background = element_rect(colour = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = 0.5, size =8),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(size = 0.5, colour = "lightgrey"),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


############## Distribution of FI values ############

dfp <- data.frame(opcfi4, "amt_paid"=opcfi14$amt_paid)

dfp <- dfp %>% gather(variable, amount, 3:4)

dfp <- as.data.frame(dfp %>% filter(amount<300000000)) # also removes NA

ggplot() +
  geom_boxplot(data=dfp,
               aes(y=amount/1000000, x=variable, group=variable, fill=variable), 
               position=position_dodge(width = 0.9), show.legend = F) +  
  geom_jitter(data=dfp,
              aes(y=amount/1000000, x=variable,group=variable, color=type_fin_instr),
              position=position_jitterdodge(dodge.width=0.9), size=2) +
  theme_classic() +
  coord_flip(ylim=c(0,max(dfp$amount/1000000,na.rm=T)+max(dfp$amount/1000000,na.rm=T)*0.1), expand = T)+
  scale_x_discrete(labels=c("OP committments", "OP payments"))+
  scale_y_continuous(expand =c(0,0),
                     breaks = seq(0, max(dfp$amount/1000000,na.rm=T)+max(dfp$amount/1000000,na.rm=T)*0.1, 
                                  by=ifelse(max(dfp$amount/1000000,na.rm=T)>500,100,50)),
                     labels = paste0(comma(seq(0, max(dfp$amount/1000000,na.rm=T)+max(dfp$amount/1000000,na.rm=T)*0.1, 
                                               by=ifelse(max(dfp$amount/1000000,na.rm=T)>500,100,50))))) +
  scale_fill_manual(values=c("#ededed", "#ededed")) +
  ylab("EUR million") +
  theme(legend.position = "bottom",
        legend.box.margin = margin(0.5,0.5,0.5,0.5),
        legend.box.background = element_rect(colour = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_blank(),
        axis.text.x = element_text(hjust = 0.5, size =10),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_line(size = 0.5, colour = "lightgrey"),
        panel.grid.major.y = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


############## Distribution of FI values ############

dfp <- as.data.frame(dt %>% filter(type_fin_instr %in% c("Fund of funds specific fund","Specific fund", "Direct management")) %>% 
                       group_by(fi_name_id, type_fin_instr) %>%
                       summarise(amt_com_rec=sum_na_rm(op_amt_com_fin_rec),
                                 amt_paid_rec=sum_na_rm(op_amt_paid_fin_rec)))

dfp <- dfp %>% gather(variable, amount, 3:4)

dfp <- as.data.frame(dfp %>% filter(amount<100000000)) # also removes NA

ggplot() +
  geom_boxplot(data=dfp,
               aes(y=amount/1000000, x=variable, group=variable, fill=variable), 
               position=position_dodge(width = 0.9), show.legend = F) +  
  geom_jitter(data=dfp,
              aes(y=amount/1000000, x=variable,group=variable, color=type_fin_instr),
              position=position_jitterdodge(dodge.width=0.9), size=2) +
  theme_classic() +
  coord_flip(ylim=c(0,max(dfp$amount/1000000,na.rm=T)+max(dfp$amount/1000000,na.rm=T)*0.1), expand = T)+
  scale_x_discrete(labels=c("FI committments", "FI payments"))+
  scale_y_continuous(expand =c(0,0),
                     breaks = seq(0, max(dfp$amount/1000000,na.rm=T)+max(dfp$amount/1000000,na.rm=T)*0.1, 
                                  by=ifelse(max(dfp$amount/1000000,na.rm=T)>500,10,5)),
                     labels = paste0(comma(seq(0, max(dfp$amount/1000000,na.rm=T)+max(dfp$amount/1000000,na.rm=T)*0.1, 
                                               by=ifelse(max(dfp$amount/1000000,na.rm=T)>500,10,5))))) +
  scale_fill_manual(values=c("#ededed", "#ededed")) +
  ylab("EUR million") +
  theme(legend.position = "bottom",
        legend.box.margin = margin(0.5,0.5,0.5,0.5),
        legend.box.background = element_rect(colour = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_blank(),
        axis.text.x = element_text(hjust = 0.5, size =10),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_line(size = 0.5, colour = "lightgrey"),
        panel.grid.major.y = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


############## Committment/Payment to FI by type ############

nf <- as.data.frame(n_fi %>% group_by(type_fin_instr) %>% summarise(nf=sum_na_rm(nFI)))
fi_fr <- as.data.frame(dt %>% group_by(type_fin_instr) %>%
                         summarise(amt_com_fi=sum_na_rm(op_amt_com_fi),
                                   amt_paid_fi=sum_na_rm(op_amt_paid_fi),
                                   amt_com_fr=sum_na_rm(op_amt_com_fin_rec),
                                   amt_paid_fr=sum_na_rm(op_amt_paid_fin_rec)))

dfp <- cbind(nf, fi_fr[,-1])

dfp[3,3]<-NA
dfp[3,4]<-NA

dfp_c <- dfp %>% gather(Committment, valueCommittment, c("amt_com_fi", "amt_com_fr"))
dfp_p <- dfp %>% gather(Payment, valuePayment, c("amt_paid_fi", "amt_paid_fr"))


dfp_c$level <- ifelse(substr(dfp_c$Committment,9,10) == "fi", "fi", "fr")

dfp_p$level <- ifelse(substr(dfp_p$Payment,10,11) == "fi", "fi","fr")


dfp_c$level <- factor(dfp_c$level,levels = c("fi","fr"), ordered = T)
dfp_p$level <- factor(dfp_p$level,levels = c("fi","fr"), ordered = T)

dfp_com <- dfp_c[,c(1,2,6,7)]

dfp_paid <- dfp_p[,c(1,2,6,7)]


dfp_com$ColCom <- ifelse(dfp_com$level == "fi", "#b1dcc0","#42beb8")
dfp_paid$ColPay <- ifelse(dfp_paid$level == "fi", "#f04e52","#ed7d31")

dfp_com <- dfp_com[order(dfp_com$type_fin_instr, dfp_com$level), ]
dfp_com$index <- seq(1:nrow(dfp_com))

dfp_paid <- dfp_paid[order(dfp_paid$type_fin_instr, dfp_paid$level), ]
dfp_paid$index <- seq(1:nrow(dfp_paid))


ggplot() +
  
  # selection
  geom_bar(data=dfp_com, 
           aes(x=reorder(type_fin_instr, index), y=valueCommittment/1000000, group = level, fill = dfp_com$ColCom),
           position = position_dodge(width=0.9), stat="identity", width=0.8) +
  
  geom_text(data=dfp_com, aes(label=ifelse(is.na(valueCommittment/1000000),paste0("N/A"),paste0(round(valueCommittment/1000000,1))),
                              y=ifelse(is.na(valueCommittment),
                                       100, 
                                       valueCommittment/1000000+300), x=type_fin_instr, group=level),
            size=3,hjust = 0.5, position = position_dodge(width = 1)) +
  
  # expenditure
  geom_bar(data=dfp_paid, 
           aes(x=reorder(type_fin_instr, -index), y=valuePayment/1000000, group = level, fill = dfp_paid$ColPay),
           position = position_dodge(width=0.9), stat="identity", width=0.4) +
  
  geom_text(data=dfp_paid, aes(label=ifelse(is.na(valuePayment),paste0("N/A"),paste0(round(valuePayment/1000000,1))),
                               y=ifelse(is.na(valuePayment),
                                        100,
                                        valuePayment/1000000+200), x=type_fin_instr, group=level),
            size=3,hjust = 0.5,  position = position_dodge(width = 1)) +
  
  # legend
  # values associa "livello variabile fill" = "colore"
  scale_fill_manual(name = "", values=c("#42beb8"="#42beb8",
                                        "#ed7d31"="#ed7d31",
                                        "#b1dcc0"="#b1dcc0",
                                        "#f04e52"="#f04e52"),
                    
                    # breaks e labels gestiscono ordine ed etichette                  
                    breaks = c("#b1dcc0", "#f04e52","#42beb8","#ed7d31"),
                    labels=c("OP committments","OP payments","FI committments","FI payments")) +
  theme_classic() +
  coord_cartesian(ylim = c(0,max(dfp_com$valueCommittment/1000000, na.rm=T)+max(dfp_com$valueCommittment/1000000, na.rm=T)*0.1), expand = T) +  
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, max(dfp_com$valueCommittment/1000000, na.rm=T)+max(dfp_com$valueCommittment/1000000, na.rm=T)*0.1, 
                                  by=ifelse(max(dfp_com$valueCommittment/1000000, na.rm=T)>500,1000,20)),
                     labels =  paste0(comma(seq(0, max(dfp_com$valueCommittment/1000000, na.rm=T)+max(dfp_com$valueCommittment/1000000, na.rm=T)*0.1,
                                                by=ifelse(max(dfp_com$valueCommittment/1000000, na.rm=T)>500,1000,20))))) +
  ylab("EUR million") +
  theme(legend.position = "bottom",
        legend.box.margin = margin(0.5, 0.5, 0.5, 0.5), # top, right, bottom, left
        legend.box.background = element_rect(colour = "white"),
        legend.title = element_blank(),
        legend.text=element_text(size=10),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=10),
        axis.line.y=element_blank(),
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, colour = 'lightgrey'),
        plot.title = element_text(face="bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


#######################################################################

dtab <- data.frame("year"=c(2015,2016), "nMS"=c(21,24), "nOP"=c(64,103), "nEX"=c(111,221), "nFI"=c(125,243))

# division of the page as table
tabPla <- gtable(unit(c(0.95), "npc"), # one horizontal sections with a small white space
                 unit(c(0.20,0.20,0.20,0.20,0.20), "npc")) # five vertical sections with different heights
# gtable_show_layout(tabsel)


tabPla <- gtable_add_grob(tabPla,
                          grobTree(rectGrob(gp=gpar(fill="#003399", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob("At 31.12.2016",
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "white"))),1,1) # hjust: text L-R position; col: text color

tabPla <- gtable_add_grob(tabPla,
                          grobTree(rectGrob(gp=gpar(fill="#dbdbdb", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nMS[2]," Member State"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),2,1)

tabPla <- gtable_add_grob(tabPla,
                          grobTree(rectGrob(gp=gpar(fill="#c6c6c6", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nOP[2]," OPs concerned"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),3,1)

tabPla <- gtable_add_grob(tabPla,
                          grobTree(rectGrob(gp=gpar(fill="#dbdbdb", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nEX[2]," FI ex-ante complete"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),4,1)

tabPla <- gtable_add_grob(tabPla,
                          grobTree(rectGrob(gp=gpar(fill="#c6c6c6", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nFI[2]," FIs"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),5,1)


# division of the page as table
tabSel <- gtable(unit(c(0.95), "npc"), # one horizontal sections with a small white space
                 unit(c(0.20,0.20,0.20,0.20,0.20), "npc")) # five vertical sections with different heights
# gtable_show_layout(tabsel)


tabSel <- gtable_add_grob(tabSel,
                          grobTree(rectGrob(gp=gpar(fill="#003399", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob("At 31.12.2015",
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "white"))),1,1)

tabSel <- gtable_add_grob(tabSel,
                          grobTree(rectGrob(gp=gpar(fill="#dbdbdb", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nMS[1]," Member State"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),2,1)

tabSel <- gtable_add_grob(tabSel,
                          grobTree(rectGrob(gp=gpar(fill="#c6c6c6", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nOP[1]," OPs concerned"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),3,1)

tabSel <- gtable_add_grob(tabSel,
                          grobTree(rectGrob(gp=gpar(fill="#dbdbdb", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nEX[1]," FI ex-ante complete"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),4,1)


tabSel <- gtable_add_grob(tabSel,
                          grobTree(rectGrob(gp=gpar(fill="#c6c6c6", alpha=0.8, col="#FFFFFF")), # alpha: transparency level; col: border color
                                   textGrob(paste0(dtab$nFI[1]," FIs"),
                                            hjust = 0.5, gp = gpar(fontsize = 18, col = "black"))),5,1)



info<-grid.arrange(arrangeGrob(tabPla), arrangeGrob(tabSel), ncol=2, heights=c(1,0), widths=c(1,1), newpage=FALSE)