# Tutorial: http://shiny.rstudio.com/tutorial/
# Case study: https://www.rstudio.com/products/shiny/shiny-user-showcase/

library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(RSocrata)

#########################################################
################## Load Data from API ###################
#########################################################

urlPlanned <- "https://cohesiondata.ec.europa.eu/resource/rde7-u3r9.json"
dfP <- read.socrata(urlPlanned)

dfP$ms <- as.factor(dfP$ms)
dfP$fund <- as.factor(dfP$fund)
dfP$title <- as.factor(dfP$title)
dfP$to <- as.factor(dfP$to)
dfP$total_amount <- as.numeric(dfP$total_amount)


urlImplem <- "https://cohesiondata.ec.europa.eu/resource/f6wa-fhmb.json"
dfI <- read.socrata(urlImplem)

dfI$ms <- as.factor(dfI$ms)
dfI$fund <- as.factor(dfI$fund)
dfI$title <- as.factor(dfI$title)

dfI$total_eligible_cost <- as.numeric(dfI$total_eligible_cost)
dfI$total_eligible_expenditure <- as.numeric(dfI$total_eligible_expenditure)

# different TOs label system for the two sources
dfI$to <- ifelse(nchar(as.character(dfI$to))==1,paste0("0",as.character(dfI$to)),as.character(dfI$to))
dfI$to <- as.factor(dfI$to)