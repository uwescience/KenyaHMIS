#### This program looks at the data relevant to Malaria analysis, and compares data from reports 705s and report 105
#### To ensure that the data from these two sources is consistant

## Author : Grégoire Lurton
## Date   : July 2014


library(sqlshare)
library(plyr)
library(ggplot2)
library(zoo)
library(maptools)
library(stringr)
library(reshape2)
library(lme4)

setwd("J:/Project/abce/ken/HMIS")


zones <- readShapePoly("data/pfprInZones.shp")
MalariaDataPed <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705ADataClean]
                                 WHERE Indicator=\'confirmed malaria\' OR Indicator=\'clinical malaria\' OR Indicator = \'typhoid fever\'OR Indicator =  \'Pneumonia\' AND isnumeric(Value) = 1;')
MalariaDataAdult <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705BDataClean]
                                 WHERE Indicator=\'confirmed malaria\' OR Indicator=\'clinical malaria\' OR Indicator = \'typhoid fever\'OR Indicator =  \'Pneumonia\' AND isnumeric(Value) = 1;')

Malaria105 <- fetch.data.frame('select District , Value , Year , Month , Level , Indicator
                                 FROM [grlurton@washington.edu].[105DataClean]
                                 WHERE Indicator=\'total number of inpatient malaria cases\' 
                                        OR Indicator=\'number of under five years treated for malaria\' 
                                        OR Indicator = \'number of patients over five years treated for malaria\'
                                        OR Indicator =  \'number of malaria inpatient deaths\' 
                                        OR Indicator =  \'number of llitn distributed to pregnant women\' 
                                        OR Indicator= \'number of llitns distributed to children under 5 years\' 
                                        OR Indicator=\'number of children over five years treated for malaria\' 
                                        OR Indicator= \'number of < 1 deaths occuring at facility\'
                                        AND isnumeric(Value) = 1;')


MalariaDataAdult$Cohort <- "> 5 years"
MalariaDataPed$Cohort <- "< 5 years"

MalariaData <- rbind(MalariaDataAdult , MalariaDataPed)
MalariaData$CalMonth <- as.Date(as.yearmon(paste(MalariaData$Month , MalariaData$Year  , sep = '-') , "%B-%Y"))
MalariaData <- subset(MalariaData, !is.na(Value) & !is.na(CalMonth) & District %in% zones@data$District)

Malaria105$CalMonth <- as.Date(as.yearmon(paste(Malaria105$Month , Malaria105$Year  , sep = '-') , "%B-%Y"))
Malaria105 <- subset(Malaria105 , !is.na(Value) & !is.na(CalMonth) & District %in% zones@data$District)

Malaria105$Cohort[Malaria105$Indicator == "number of under five years treated for malaria"] <- "< 5 years"
Malaria105$Cohort[Malaria105$Indicator == "number of patients over five years treated for malaria"] <- "> 5 years"

Malaria105$Indicator <- as.character(Malaria105$Indicator)
Malaria105$Indicator[Malaria105$Indicator == "number of under five years treated for malaria"] <-
  Malaria105$Indicator[Malaria105$Indicator == "number of patients over five years treated for malaria"] <-
  "treated malaria"

MalariaCases105 <- subset(Malaria105 , Indicator == "treated malaria")
MalariaCases705 <- subset(MalariaData , Indicator == "confirmed malaria")
####Summing Malaria cases accross each data set

Summed705 <- ddply(MalariaCases705 , .(District , CalMonth , Indicator) , 
                   function(x){
                     data.frame(totalCases = sum(x$Value , na.rm = TRUE))
                   },
                   .progress = 'text')


Summed105 <- ddply(MalariaCases105 , .(District , CalMonth , Indicator) , 
                   function(x){
                     data.frame(totalCases = sum(x$Value , na.rm = TRUE))
                   }, .progress = 'text')

trimmed705 <- ddply(Summed705 , .(District) , 
                         function(x){
                           subset(x , !(totalCases %in% boxplot.stats(totalCases)$out))
                         })

trimmed105 <- ddply(Summed105 , .(District) , 
                         function(x){
                           subset(x , !(totalCases %in% boxplot.stats(totalCases)$out))
                         })


corMatrix <- merge(trimmed105[,-3] , trimmed705[,-3] , by = c("District", 'CalMonth') , all = FALSE)


corrMal705vs105 <- ddply(corMatrix , .(District) , 
                         function(x){
                           data.frame(corr = cor(x$totalCases.x , x$totalCases.y))
                         }
                         )

corrMal705vs105 <- subset(corrMal705vs105 , !is.na(corr))

nValues <- ddply(Summed105 , .(District) , 
                 function(x){
                   data.frame(Nobs = nrow(x))
                 }
                 )


corrMal705vs105 <- merge(corrMal705vs105 , nValues)

corrMal705vs105$District <- factor(as.character(corrMal705vs105$District) , 
                                   levels = as.character(corrMal705vs105$District)[order(corrMal705vs105$corr)])

qplot(data = corrMal705vs105 , x = District , y = corr , geom = 'bar' , stat = 'identity' , fill = Nobs) +
  coord_flip() + theme_bw() + ylab('Correlation between Clinical Malaria in 705 and Treated Malaria in 105') +
  scale_fill_gradient(low="grey" , high = "dark red")

goodCorrDist <- as.character(corrMal705vs105$District[corrMal705vs105$corr >= 0.7])
medCorrDist <- as.character(corrMal705vs105$District[corrMal705vs105$corr < 0.7 & corrMal705vs105$corr >= 0.5])
badCorrDist <- as.character(corrMal705vs105$District[corrMal705vs105$corr < 0.5 & corrMal705vs105$corr >= 0])
negCorrDist <- as.character(corrMal705vs105$District[corrMal705vs105$corr < 0])






####Looking at it

NValDist <- ddply(Summed105 , .(District) , function(x) nrow(x[x$Value != 0 ,]))
SufficientDataDist <- subset(NValDist , V1 > 21)

Summed105 <- subset(Summed105 , District %in% SufficientDataDist$District)
Summed105$District <- as.character(Summed105$District)

qplot(data = Summed105[Summed105$Indicator != "treated malaria" ,] , x = CalMonth , y = Value , col = Indicator , geom = 'line') +
  facet_wrap(~District , scales = "free")
ComparePlot <- rbind(data.frame(subset(Summed105 ,
                                       Indicator == "treated malaria"), 
                                Source = "105 Report") ,
                     data.frame(subset(MalariaData , 
                                       Indicator %in% c('clinical malaria' , 'confirmed malaria') ,
                                       select = c(Indicator , CalMonth , District , Value , Cohort)
                     ) , Source = "705 Reports")
)

ComparePlot <- subset(ComparePlot , !is.na(CalMonth) & !is.na(Value))

ggplot(data = ComparePlot , aes(x = CalMonth , y = Value , colour = Indicator , linetype = Cohort) )+ 
  geom_line() +
  facet_wrap(~District , scales = "free_y") + theme_bw()