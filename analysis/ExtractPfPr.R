##Basic code adapted from Tom Fleming and Nick Graetz

require(raster)
require(sp)
require(maptools)
library(rgdal)
library(RColorBrewer)
library(sqlshare)
library(plyr)
library(splines)
library(MASS)
library(stringr)
library(ggplot2)
library(zoo)

gpclibPermit()

setwd("J:/Project/abce/ken/HMIS")

# INITIAL CROP of raw pfpr to country. 
popData <- raster("J://WORK/01_covariates/02_inputs/environmental/data/GPW/full_ts_1980_2015/glp2010ag.asc")
pfprData <- raster("J://Project/Coverage/Malaria Intervention Coverage/Map Making/nick/map2010.asc")
#pfprData <- raster("addata/pfpr/map2010.asc")

KenyaPop <- crop(popData,zones)
Kenyapfpr <- crop(pfprData,KenyaPop)

save.image(file = 'addata/CroppedPfpr.rdata')

formatNames <- function(x){
  tolower(str_trim(as.character(x)))
}

iso <- as.character(commandArgs()[3])
zones <- readShapePoly("data/zones.shp")

ghapfprpop <- c()
population <- c()

for (District in zones$districts){
  print(District)
  Shap <- zones[zones$districts == District , ]
  DistPop <- crop(KenyaPop , Shap)
  DistPfpr <- crop(Kenyapfpr , DistPop)
  DistPop <- crop(DistPop , DistPfpr)
  print(paste('rows -' , nrow(DistPfpr) , "x" , nrow(DistPop) ,sep = ' '))
  print(paste('cols -' , ncol(DistPfpr) , "x" , ncol(DistPop) ,sep = ' '))
  # The division by 25 after (c,fact=5) ensures that the disaggregated cells sum to the original cell that they 
  ##were split from.  Not necessary if you're just interested in prevalence.
  # The disaggregation factor can be found using nrow(pfprcrop)/nrow(c) or ncol(pfprcrop)/ncol(c)
  cdisag <- disaggregate(DistPop,fact=5)
  print(ncol(cdisag))
  pw_pfpr <- overlay(cdisag,DistPfpr,fun=function(x,y){return(x*y/25)})
  ##Mask the values that are not exactly in the shapefile
  crop <- setValues(pw_pfpr, NA)
  myshp.r <- rasterize(Shap , crop)
  pfpr.masked <- mask(x = pw_pfpr , mask = myshp.r)
  ##Same for population
  crop <- setValues(cdisag, NA)
  population.masked <- mask(x = cdisag/25 , mask = myshp.r)
  # Extract total pfpr population
  ghapfprpop <- c(ghapfprpop , extract(pfpr.masked,Shap,fun='sum',na.rm=TRUE))
  population <- c(population , extract(population.masked , Shap , fun = 'sum' , na.rm=TRUE))
}
  
pfpr <- data.frame(District = zones$districts , ghapfprpop , population , pfpr = ghapfprpop / population)
#qplot(x = pfpr$District , y= as.numeric(pfpr$population) )+ coord_flip()

MapPfpr <- zones
MapPfpr@data <- merge(pfpr , zones@data , by.x  = 'District' , by.y = 'districts' , all = TRUE)

colors <- brewer.pal(4, "Oranges")
pal <- colorRampPalette(c('white' , 'lightblue', 'darkred'))

par(mfrow = c(1,2))
plot(MapPfpr , col = pal(50)[as.numeric(cut(MapPfpr$pfpr,breaks = 50))])
title(main = 'Distribution of pfpr')
plot(MapPfpr , col = pal(50)[cut(MapPfpr$ghapfprpop,breaks = 50)])
title(main = 'Nbr Malaria infected people')
par(mfrow = c(1,1))


MalariaCases <- fetch.data.frame('select District , Value , Year , Month 
                                  FROM [grlurton@washington.edu].[705BDataComplete]
                                  WHERE Indicator=\'confirmed malaria\' AND isnumeric(Value) = 1;')

#MalariaCases <- CleanMonths(MalariaCases)
MalariaCases$Month <- as.yearmon(paste(MalariaCases$Month , MalariaCases$Year  , sep = '-') , "%B-%Y")

MalariaStat <- ddply(MalariaCases , .(District) , function(x){data.frame(mean  = mean(x$Value) , median = median(x$Value))})
CountCases <- ddply(MalariaCases , .(District , Month) , function(x){data.frame(sum  = sum(x$Value , na.rm = TRUE))})

MalariaStat$District <- formatNames(MalariaStat$District)
CountCases$District <- formatNames(CountCases$District)

DataPlot <- merge(MalariaStat, pfpr , all.y = FALSE , by.x = 'District' , by.y = 'District')
SumDataPlot <- merge(CountCases, pfpr , all.y = FALSE , by.x = 'District' , by.y = 'District')

pdf("pfprdef.pdf", width=14.85, height=10.5)
ggplot(data = DataPlot ,
      aes(x = pfpr , y = 1000 * 12 * mean /(population)  , 
          #size= population  ,
          col = pfpr , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="blue" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate') 

ggplot(data = SumDataPlot ,
      aes(x = pfpr , y = 1000 * 12 * sum /(population)   , label = District)) + 
 # geom_text() +
  geom_point() + geom_smooth(method="rlm") +
  theme_bw() + #scale_colour_gradient(low="blue" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate') 

ggplot(data = DataPlot ,
       aes(x = ghapfprpop , y = mean  , size= population  ,col = pfpr , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="blue" , high = "red")

  
  qplot(data = DataPlot ,
       x = ghapfprpop , y = median ,  
        size = 2) +
    xlab('pfpr * Population') + ylab('Median of confirmed Malaria Cases') +
    theme_bw() + scale_colour_brewer(palette="Set1")
dev.off()

