#### This program looks for all the indicators found in reports 105 and unifies the values
#### It outputs a dictionnary for the indicators to be used for analysis 

## Author : Grégoire Lurton
## Date   : July 2014

setwd("J:/Project/abce/ken/HMIS/data")

library(stringr)
library(plyr)

Data105 <- read.csv("105Data.csv" , sep = '\t' , header = TRUE , row.names = NULL)

indicators <- data.frame(Indicator = unique(as.character(Data105$Indicator)))
  
length(unique(indicators$Indicator))
indicators$new <- str_trim(indicators$Indicator)
indicators$new <- str_replace_all(indicators$new, "  ", " ")
indicators$new  <- tolower(indicators$new)
length(unique(indicators$new))

sort(unique(indicators$new))

write.csv(indicators , 'DicoIndicators105.csv' , row.names = FALSE )