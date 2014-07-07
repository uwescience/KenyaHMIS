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
gpclibPermit()

iso <- as.character(commandArgs()[3])

# INITIAL CROP of raw pfpr to country. 
Kenya <- readShapePoly(paste0("Shapefiles/Districts/kenya_districts98.shp"))
Kenya$DISTRICT <- formatNames(Kenya$DISTRICT)
Kenya$DISTRICT[Kenya$DISTRICT == 'mt.elgon'] <- 'mt. elgon'

popData <- raster("J://WORK/01_covariates/02_inputs/environmental/data/GPW/full_ts_1980_2015/glp2010ag.asc")
#pfprData <- raster("J://Project/Coverage/Malaria Intervention Coverage/Map Making/nick/map2010.asc")
pfprData <- raster("C://Users/grlurton/Documents/KenyaHMIS/pfpr/map2010.asc")

KenyaPop <- crop(popData,Kenya)
Kenyapfpr <- crop(pfprData,KenyaPop)


pdf("pfprplot.pdf", width=4, height=4)
plot(Kenyapfpr)
plot(Kenya , add = TRUE)
title(main = 'pfpr in Kenya')
dev.off()

pdf("pfprDistrict", width=4, height=4)
plot(pfpr.masked)
title(main = 'District Number of clinical pfpr cases')
dev.off()

library(stringr)
ghapfprpop <- c()
population <- c()
for (Dist in 1:length(unique(FinalDataMap$DistNameShape))){
  District <- unique(FinalDataMap$DistNameShape)[Dist]
  if(District %in% formatNames(Kenya$DISTRICT)){
    print(District)
    Shap <- Kenya[formatNames(Kenya$DISTRICT) == District , ]
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
    ghapfprpop[Dist] <- extract(pfpr.masked,Shap,fun='sum',na.rm=TRUE)
    population[Dist] <- extract(population.masked , Shap , fun = 'sum' , na.rm=TRUE)
  }
  else print(paste(District , "not in available data" , sep = " "))
}
  
pfpr <- data.frame(District = unique(FinalDataMap$DistNameShape) , ghapfprpop , population , pfpr = ghapfprpop / population)
qplot(x = pfpr$District , y= pfpr$population , geom = 'bar')+ coord_flip()

MapPfpr <- Kenya
MapPfpr@data <- merge(pfpr , Kenya@data , by.x  = 'District' , by.y = 'DISTRICT' , all = TRUE)

colors <- brewer.pal(4, "Oranges")
pal <- colorRampPalette(c('white' , 'lightblue', 'darkred'))

par(mfrow = c(1,2))
plot(MapPfpr , col = pal(50)[as.numeric(cut(MapPfpr$pfpr,breaks = 50))])
title(main = 'Distribution of pfpr')
plot(MapPfpr , col = pal(50)[cut(MapPfpr$ghapfprpop,breaks = 50)])
title(main = 'Nbr Malaria infected people')
par(mfrow = c(1,1))


MalariaCases <- fetch.data.frame('select DistNameShape as District , Value , Level , Year , Month
                                  FROM [grlurton@washington.edu].[Rep705BCleanDistrict]
                                  WHERE Indicator=\'Confirmed Malaria\' AND isnumeric(Value) = 1;')

MalariaCases <- CleanMonths(MalariaCases)
MalariaCases$Month <- as.yearmon(paste(MalariaCases$Month , MalariaCases$Year  , sep = '-') , "%B-%Y")

MalariaStat <- ddply(MalariaCases , .(District) , function(x){data.frame(mean  = mean(x$Value) , median = median(x$Value))})
CountCases <- ddply(MalariaCases , .(District , Month) , function(x){data.frame(sum  = sum(x$Value , na.rm = TRUE))})

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

