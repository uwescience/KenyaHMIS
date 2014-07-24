#### This program compares the Pfpr levels in the different zones for which we have data
#### to the diagnostic of malaria, both clinic or confirmed, in the facilities data


## Author : Grégoire Lurton
## Date   : July 2014

library(maptools)
library(sqlshare)
library(zoo)
library(plyr)
library(ggplot2)
library(MASS)

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


TotalCases <- merge(TotalCases , pfprZones@data , all.y = FALSE , by.x = 'District' , by.y = 'District')

###What does the data look like ?

ggplot(data = TotalCases ,
       aes(x = ghapfprpop , y = TotClin)) + 
  geom_point() +
  geom_point(aes(x = ghapfprpop , y = TotConf) , col = "red" )+
  theme_bw() +
  ylab('Number of cases in hospital reports (red  = confirmed)') + xlab('Number of people with Pfp')

##Deleting outliers

TotalCases <- subset(TotalCases , TotClin <40000)

ggplot(data = TotalCases ,
       aes(x = ghapfprpop , y = TotClin)) + 
  geom_point() +
  geom_point(aes(x = ghapfprpop , y = TotConf) , col = "red" )+
  theme_bw() +
  ylab('Number of cases in hospital reports (red  = confirmed)') + xlab('Number of people with Pfp')

##0 Values are considered missing data...

TotalCases <- subset(TotalCases , !(TotConf == 0 & TotClin == 0))

MalariaStat <- ddply(TotalCases ,.(District), function(x){
  data.frame(meanConf  = mean(x$TotConf  , na.rm = T) , 
             medianConf = median(x$TotConf, na.rm = T) ,
             meanClin  = mean(x$TotClin, na.rm = T) , 
             medianClin = median(x$TotClin, na.rm = T))})

####Ranking of 

DataPlot    <- merge(MalariaStat, pfprZones@data , all.y = FALSE , by.x = 'District' , by.y = 'District')

ggplot(data = DataPlot ,
       aes(x = pfpr , y = 1000 * 12 * meanClin /(population)  , 
           #size= population  ,
           col = population , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="dark green" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate clinical malaria')

ggplot(data = DataPlot ,
       aes(x = pfpr , y = 1000 * 12 * meanConf /(population)  , ,
           col = population , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="dark green" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate confirmed malaria') 


fitData <- subset(TotalCases , !is.na(CalMonth))
ClinModel <- glm(round(TotClin) ~ ghapfprpop  + months(CalMonth), data = fitData , family = quasipoisson )
ConfModel <- glm(round(TotConf) ~ log(ghapfprpop)  + months(CalMonth), data = fitData , family = quasipoisson )

par(mfcol = c(4,2))

plot(fitData$TotClin , fitted(ClinModel),
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Clinical - pfpr Model', sub = 'model : OverDisp Poisson' ,
     xlim = c(0,max(fitData$TotClin) ) , ylim = c(0,max(fitData$TotClin) ) )
lines(x = c(0,max(fitData$TotClin) ), y = c(0,max(fitData$TotClin ) ) , col = 'red')

plot(fitData$TotConf , fitted(ConfModel) ,
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Confirmed - pfpr Model', sub = 'model : OverDisp Poisson' ,
     xlim = c(0,max(fitData$TotConf) ) , ylim = c(0,max(fitData$TotConf) ) )
lines(x = c(0,max(fitData$TotConf) ), y = c(0,max(fitData$TotConf ) ) , col = 'red')

ClinModelNB <- glm.nb(round(TotClin) ~ log(ghapfprpop)  + months(CalMonth), data = fitData )
ConfModelNB <- glm.nb(round(TotConf) ~ log(ghapfprpop)  + months(CalMonth), data = fitData )


plot(fitData$TotClin , fitted(ClinModelNB),
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Clinical - pfpr Model' , sub = 'model : Negative Binomial' ,
     xlim = c(0,max(fitData$TotClin) ) , ylim = c(0,max(fitData$TotClin) ) )
lines(x = c(0,max(fitData$TotClin) ), y = c(0,max(fitData$TotClin ) ) , col = 'red')

plot(fitData$TotConf , fitted(ConfModelNB),
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Clinical - pfpr Model', sub = 'model : Negative Binomial' , 
     xlim = c(0,max(fitData$TotConf) ) , ylim = c(0,max(fitData$TotConf) ) )
lines(x = c(0,max(fitData$TotConf) ), y = c(0,max(fitData$TotConf ) ) , col = 'red')


###Models with district instead of pfprpop

ClinModelDist <- glm(round(TotClin) ~ District  + months(CalMonth), data = fitData , family = quasipoisson )
ConfModelDist <- glm(round(TotConf) ~ District  + months(CalMonth), data = fitData , family = quasipoisson )

plot(fitData$TotClin , fitted(ClinModelDist),
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Clinical - District Model' , sub = 'model : OverDisp Poisson' ,
     xlim = c(0,max(fitData$TotClin) ) , ylim = c(0,max(fitData$TotClin) ) )
lines(x = c(0,max(fitData$TotClin) ), y = c(0,max(fitData$TotClin ) ) , col = 'red')

plot(fitData$TotConf , fitted(ConfModelDist),
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Confirmed - District Model' , sub = 'model : OverDisp Poisson' ,
     xlim = c(0,max(fitData$TotConf) ) , ylim = c(0,max(fitData$TotConf) ) )
lines(x = c(0,max(fitData$TotConf) ), y = c(0,max(fitData$TotConf ) ) , col = 'red')




ClinModelDistNB <- glm.nb(round(TotClin) ~ District  + months(CalMonth), data = fitData)
ConfModelDistNB <- glm.nb(round(TotConf) ~ District  + months(CalMonth), data = fitData)

plot(fitData$TotClin , fitted(ClinModelDistNB),
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Clinical - District Model' , sub = 'model : Negative Binomial' ,
     xlim = c(0,max(fitData$TotClin) ) , ylim = c(0,max(fitData$TotClin) ) )
lines(x = c(0,max(fitData$TotClin) ), y = c(0,max(fitData$TotClin ) ) , col = 'red')

plot(fitData$TotConf , fitted(ConfModelDistNB),
     xlab = 'Actual data' , ylab = 'Fitted data' , main = 'Confirmed - District Model' , sub = 'model : Negative Binomial' ,
     xlim = c(0,max(fitData$TotConf) ) , ylim = c(0,max(fitData$TotConf) ) )
lines(x = c(0,max(fitData$TotConf) ), y = c(0,max(fitData$TotConf ) ) , col = 'red')