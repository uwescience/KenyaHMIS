#### This program looks for all the indicators found in reports 705 and unifies the values
#### It outputs a dictionnary for the indicators to be used for analysis 

## Author : Grégoire Lurton
## Date   : July 2014

setwd("J:/Project/abce/ken/HMIS/data")

library(stringr)
library(plyr)

Data705A <- read.csv("705AData.csv" , sep = '\t' , header = TRUE , row.names = NULL)
Data705B <- read.csv("705BData.csv" , sep = '\t' , header = TRUE , row.names = NULL)

indicatorsA <- data.frame(Indicator = unique(as.character(Data705A$Indicator)))
indicatorsB <- data.frame(Indicator = unique(as.character(Data705B$Indicator)))

length(unique(indicatorsA$Indicator))
indicatorsA$new <- str_trim(indicatorsA$Indicator)
indicatorsA$new <- str_replace_all(indicatorsA$Indicator, "  ", " ")
indicatorsA$new  <- tolower(indicatorsA$Indicator)
length(unique(indicatorsA$new))

length(unique(indicatorsB$Indicator))
indicatorsB$new <- str_trim(indicatorsB$Indicator)
indicatorsB$new <- str_replace_all(indicatorsB$Indicator, "  ", " ")
indicatorsB$new  <- tolower(indicatorsB$Indicator)
indicatorsB$new[indicatorsB$Indicator == "Confirmed Malaria "] <- "confirmed malaria"
length(unique(indicatorsB$new))

sort(unique(indicatorsA$new))
sort(unique(indicatorsB$new))

indicatorsB$new[indicatorsB$Indicator == "prepared by; juliana ngoa---dhrio"] <- ""

write.csv(indicatorsA , 'DicoIndicators705A.csv', row.names = FALSE)
write.csv(indicatorsB , 'DicoIndicators705B.csv', row.names = FALSE)
