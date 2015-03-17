#### This programs extract pfpr rates and number of cases from raster files from MAP project and
#### It outputs a shapefile of the zones defined from DHIS orgunits with data and pfpr and population


## Author : Gr?goire Lurton
##          Cropping part of the code adapted from Tom Fleming and Nick Graetz
## Date   : July 2014


require(raster)
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
library(shapefiles)

gpclibPermit()

setwd("J:/Project/abce/ken/HMIS")

# INITIAL CROP of raw pfpr to country. 
popData <- raster("J://WORK/01_covariates/02_inputs/environmental/data/GPW/full_ts_1980_2015/glp2010ag.asc")
pfprData <- raster("J://Project/Coverage/Malaria Intervention Coverage/Map Making/nick/map2010.asc")
#pfprData <- raster("addata/pfpr/map2010.asc")

KenyaPop <- crop(popData,zones)
Kenyapfpr <- crop(pfprData,KenyaPop)

save.image(file = 'addata/CroppedPfpr.rdata')
#load(file = 'addata/CroppedPfpr.rdata')
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
  ## The division by 25 (fact=5) ensures that the disaggregated cells sum to the 
  ## original cell that they were split from. 
  ## The disaggregation factor can be found using 
      ##nrow(DistPfpr)/nrow(DistPop) or ncol(DistPfpr)/ncol(DistPop)
  cdisag <- disaggregate(DistPop,fact=5)
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

zones@data <- merge(pfpr , zones@data , by.x  = 'District' , by.y = 'districts' , all = TRUE)

write.csv(pfpr , file = "addata/pfprInZones.csv")
writeOGR(zones, "data", "pfprInZones" , driver="ESRI Shapefile")
