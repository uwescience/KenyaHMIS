#### This program compares the Pfpr levels in the different zones for which we have data
#### to the diagnostic of malaria, both clinic or confirmed, in the facilities data


## Author : Grégoire Lurton
## Date   : July 2014

library(maptools)
library(sqlshare)
library(zoo)
library(plyr)
library(ggplot2)

setwd("J:/Project/abce/ken/HMIS")

pfprZones <- readShapePoly("data/pfprInZones.shp")

pal <- colorRampPalette(c('white' , 'lightblue', 'darkred'))

par(mfrow = c(1,2))
plot(pfprZones , col = pal(50)[as.numeric(cut(pfprZones$pfpr,breaks = 50))])
title(main = 'Distribution of pfpr')
plot(pfprZones , col = pal(50)[cut(pfprZones$ghapfprpop,breaks = 50)])
title(main = 'Nbr Malaria infected people')
par(mfrow = c(1,1))

MalariaDataPed <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705ADataClean]
                                 WHERE Indicator=\'confirmed malaria\' OR Indicator=\'clinical malaria\' AND isnumeric(Value) = 1;')
MalariaDataAdult <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705BDataClean]
                                 WHERE Indicator=\'confirmed malaria\' OR Indicator=\'clinical malaria\' AND isnumeric(Value) = 1;')

MalariaDataAdult$Cohort <- "> 5 years"
MalariaDataPed$Cohort <- "< 5 years"

MalariaData <- rbind(MalariaDataAdult , MalariaDataPed)
MalariaData$CalMonth <- as.Date(as.yearmon(paste(MalariaData$Month , MalariaData$Year  , sep = '-') , "%B-%Y"))


TotalCases <- ddply(MalariaData , .(District , CalMonth) , function(x){
  data.frame(TotConf  = sum(x$Value[x$Indicator == 'confirmed malaria'] , na.rm = TRUE),
             TotClin = sum(x$Value[x$Indicator == 'clinical malaria'] , na.rm = TRUE))})

TotalCases <- subset(TotalCases , TotConf != 0 & TotClin != 0)

TotalCases <- merge(TotalCases , pfprZones@data , all.y = FALSE , by.x = 'District' , by.y = 'District')

MalariaStat <- ddply(TotalCases ,.(District), function(x){
  data.frame(meanConf  = mean(x$TotConf  , na.rm = T) , 
             medianConf = median(x$TotConf, na.rm = T) ,
             meanClin  = mean(x$TotClin, na.rm = T) , 
             medianClin = median(x$TotClin, na.rm = T))})



DataPlot    <- merge(MalariaStat, pfprZones@data , all.y = FALSE , by.x = 'District' , by.y = 'District')

par(mfrow = c(2,2))
plot(order(DataPlot$ghapfprpop) , order(DataPlot$medianClin) ,
     xlab = 'Number people with pfp (Rank)' , 
     ylab = 'Mean number clinical malaria (Rank)')
plot(order(DataPlot$pfpr) , order(DataPlot$medianClin / DataPlot$population),
     xlab = 'Pfpr (Rank)' , 
     ylab = 'Mean rate clinical malaria (Rank)')
plot(order(DataPlot$ghapfprpop) , order(DataPlot$medianConf),
     xlab = 'Number people with pfp (Rank)' , 
     ylab = 'Mean number confirmed malaria (Rank)')
plot(order(DataPlot$pfpr) , order(DataPlot$medianConf / DataPlot$population),
     xlab = 'Pfpr (Rank)' , 
     ylab = 'Mean rate confirmed malaria (Rank)')

summary()


ggplot(data = DataPlot ,
       aes(x = pfpr , y = 1000 * 12 * meanClin /(population)  , 
           #size= population  ,
           col = population , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="dark green" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate clinical malaria')

ggplot(data = DataPlot ,
       aes(x = pfpr , y = 1000 * 12 * meanConf /(population)  , 
           #size= population  ,
           col = population , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="dark green" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate confirmed malaria') 


ggplot(data = DataPlot ,
       aes(x = medianClin , y = medianConf  , 
           #size= population  ,
           col = population )) + 
  geom_point() +
  theme_bw() + scale_colour_gradient(low="dark green" , high = "red") +
  xlab('Number Clinical Malaria') + ylab('Incidence rate clinical malaria')


fitData <- subset(TotalCases , !is.na(CalMonth))
ConfModel <- glm(TotConf ~ ghapfprpop  + months(CalMonth), data = fitData , family = quasipoisson )
plot(ConfModel)
par(mfrow = c(1,1))
plot(fitData$TotConf , fitted(ConfModel))
