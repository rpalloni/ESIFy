library(RSocrata)
library(shiny)
library(ggplot2)
library(gridExtra)
library(stringr)
library(scales)
# library(plyr) # join
library(dplyr)
library(DT) # dataTableOutput
library(shinycssloaders) # add spinner when output is loading

#########################################################
################## Load Data from API ###################
#########################################################

# # https://cohesiondata.ec.europa.eu/dataset/ESIF-2014-2020-FINANCES-PLANNED-DETAILS/e4v6-qrrq
urlPlanned <- "https://cohesiondata.ec.europa.eu/resource/rde7-u3r9.json"
dfP <- read.socrata(urlPlanned)

dfP$ms <- as.factor(dfP$ms)
dfP$fund <- as.factor(dfP$fund)
dfP$title <- as.factor(dfP$title)
dfP$to <- as.factor(dfP$to)
dfP$total_amount <- as.numeric(dfP$total_amount)

dfP <- dfP %>% arrange(ms, fund, title) # evita che i dati dei menu vengano caricati non in ordine

# https://cohesiondata.ec.europa.eu/EU-Level/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52
urlImplem <- "https://cohesiondata.ec.europa.eu/resource/f6wa-fhmb.json"
dfI <- read.socrata(urlImplem)

dfI$ms <- as.factor(dfI$ms)
dfI$fund <- as.factor(dfI$fund)
dfI$title <- as.factor(dfI$title)

dfI$total_amount <- as.numeric(dfI$total_amount)
dfI$total_eligible_cost <- as.numeric(dfI$total_eligible_cost)
dfI$total_eligible_expenditure <- as.numeric(dfI$total_eligible_expenditure)

dfI <- dfI %>% arrange(ms, fund, title)

# different TOs label system for the two sources
dfI$to <- ifelse(nchar(as.character(dfI$to))==1,paste0("0",as.character(dfI$to)),as.character(dfI$to))
dfI$to <- as.factor(dfI$to)


######## Indicators ########

urlInd <- "https://cohesiondata.ec.europa.eu/resource/2q3n-nr7n.json"
dfR <- read.socrata(urlInd)


dfR$ms <- as.factor(dfR$ms)
dfR$fund <- as.factor(dfR$fund)
dfR$title <- as.factor(dfR$title)

# dfR$implemented_nominator <- as.numeric(dfR$implemented_nominator)
# dfR$implemented_denominator <- as.numeric(dfR$implemented_denominator)
dfR$target_value <- as.numeric(dfR$target_value)
dfR$forecast_value <- as.numeric(dfR$forecast_value)

dfR <- dfR %>% arrange(ms, fund, to, ind_code)

# different TOs label system for the two sources
dfR$to <- ifelse(nchar(as.character(dfR$to))==1,paste0("0",as.character(dfR$to)),as.character(dfR$to))
dfR$to <- as.factor(dfR$to)

#########################################################
################## Load Meta from API ###################
#########################################################

urlMetadata <- "http://cohesiondata.ec.europa.eu/api/views/metadata/v1/f6wa-fhmb"
update <- substr(readLines(urlMetadata)[8],22,31)

urlMetadata <- "http://cohesiondata.ec.europa.eu/api/views/metadata/v1/2q3n-nr7n"
update_ind <- substr(readLines(urlMetadata)[8],22,31)

###
