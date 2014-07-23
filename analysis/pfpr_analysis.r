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

MalariaCases <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705BDataComplete]
                                 WHERE Indicator=\'confirmed malaria \' OR Indicator = \'clinical malaria\' AND isnumeric(Value) = 1;')

MalariaCases$Month <- as.yearmon(paste(MalariaCases$Month , MalariaCases$Year  , sep = '-') , "%B-%Y")

MalariaStat <- ddply(MalariaCases , .(District) , function(x){
  data.frame(meanConf  = mean(x$Value[x$Indicator == 'confirmed malaria ']  , na.rm = T) , 
             medianConf = median(x$Value[x$Indicator == 'confirmed malaria '], na.rm = T) ,
             meanClin  = mean(x$Value[x$Indicator == 'clinical malaria'], na.rm = T) , 
             medianClin = median(x$Value[x$Indicator == 'clinical malaria'], na.rm = T))})
CountCases <- ddply(MalariaCases , .(District , Month) , function(x){
  data.frame(sumConf  = sum(x$Value[x$Indicator == 'confirmed malaria '] , na.rm = TRUE),
             sumClin = sum(x$Value[x$Indicator == 'clinical malaria'] , na.rm = TRUE))})

####A enlever
##################################################################
formatNames <- function(x){
  library(stringr)
  tolower(str_trim(as.character(x)))
}

MalariaStat$District <- formatNames(MalariaStat$District)
CountCases$District <- formatNames(CountCases$District)
##################################################################


DataPlot    <- merge(MalariaStat, pfprZones@data , all.y = FALSE , by.x = 'District' , by.y = 'District')
SumDataPlot <- merge(CountCases, pfprZones@data , all.y = FALSE , by.x = 'District' , by.y = 'District')


par(mfrow = c(2,2))
plot(order(DataPlot$ghapfprpop) , order(DataPlot$meanClin) ,
     xlab = 'Number people with pfp (Rank)' , 
     ylab = 'Mean number clinical malaria (Rank)')
plot(order(DataPlot$pfpr) , order(DataPlot$meanClin / DataPlot$population),
     xlab = 'Pfpr (Rank)' , 
     ylab = 'Mean rate clinical malaria (Rank)')
plot(order(DataPlot$ghapfprpop) , order(DataPlot$meanConf),
     xlab = 'Number people with pfp (Rank)' , 
     ylab = 'Mean number confirmed malaria (Rank)')
plot(order(DataPlot$pfpr) , order(DataPlot$meanConf / DataPlot$population),
     xlab = 'Pfpr (Rank)' , 
     ylab = 'Mean rate confirmed malaria (Rank)')

ggplot(data = DataPlot ,
       aes(x = pfpr , y = 1000 * 12 * meanClin /(population)  , 
           #size= population  ,
           col = pfpr , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="blue" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate clinical malaria') 

ggplot(data = DataPlot ,
       aes(x = pfpr , y = 1000 * 12 * meanConf /(population)  , 
           #size= population  ,
           col = pfpr , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="blue" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate confirmed malaria') 
