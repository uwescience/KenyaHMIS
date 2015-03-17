library(ggplot2)
require(raster)
require(maptools)
library(rgdal)
library(sqlshare)
library(plyr)
library(splines)
library(MASS)
library(stringr)
library(zoo)
library(shapefiles)

data <- read.csv('C://users/grlurton/Desktop/extracted_data/final_malaria_confirmed.csv')
data_positive <- read.csv('C://users/grlurton/Desktop/extracted_data/final_malaria_positive.csv')


table(data_positive$datasets_name)

data_plot <- subset(data , datasets_name != 'AWP Monthly Service Delivery' &
                      datasets_name != 'MOH 705 A Outpatient summary < 5 years' , 
                    select = c(county , value , availability_rate , 
                               period , county_ID))

qplot(data = data_plot , x = availability_rate , y = value) +
  facet_wrap(~county , scales = 'free_y')

qplot(data = data_positive , x = availability_rate , y = value) +
  facet_wrap(~county , scales = 'free_y')

data_plot_fin <- subset(data_plot , !(county == 'Embu County' & 
                           availability_rate < 0.07 & value > 1000) &
                          !(county == 'Embu County' & value > 1500)&
                          !(county == 'Mandera County' & value > 400)&
                          !(county == 'Lamu County' & value > 900)&
                          !(county == 'Kilifi County' & value > 20000))

qplot(data = data_plot_fin , x = availability_rate , 
      y = value) +
  facet_wrap(~county , scales = 'free_y')

data_plot_fin$complete <- data_plot_fin$value / data_plot_fin$availability_rate

data_plot_fin$year <- substr(data_plot_fin$period , 1 , 4) 

yearly <- ddply(data_plot_fin , .(county_ID , year) , 
                function(x) mean(x$complete, na.rm = TRUE))

qplot(data = yearly , x = year , 
      y = V1) +
  facet_wrap(~county_ID , scales = 'free_y')


## Get PfPR Data

##########

formatNames <- function(x){
  tolower(str_trim(as.character(x)))
}

iso <- as.character(commandArgs()[3])

zones_orig <- readShapePoly("J://Project/dhis/kenya/extracted_data/map_polygons.shp")
zones <- zones_orig[zones_orig@data$org_unit_I %in% data$county_ID,]

colnames(zones@data) <- 'districts'

# INITIAL CROP of raw pfpr to country. 
popData <- raster("J://WORK/01_covariates/02_inputs/environmental/data/GPW/full_ts_1980_2015/glp2010ag.asc")
pfprData <- raster("J://Project/Coverage/Malaria Intervention Coverage/Map Making/nick/map2010.asc")
#pfprData <- raster("addata/pfpr/map2010.asc")

KenyaPop <- crop(popData,zones)
Kenyapfpr <- crop(pfprData,KenyaPop)

#####

ghapfprpop <- c()
population <- c()

sum_nona <- function(x){
  sum(x , na.rm = TRUE)
}

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
  ghapfprpop <- c(ghapfprpop , extract(pfpr.masked,Shap,fun=sum_nona,na.rm=TRUE))
  population <- c(population , extract(population.masked , 
                                       Shap , fun = sum_nona , na.rm=TRUE))
}

pfpr <- data.frame(districts = zones$districts , ghapfprpop , population , 
                   pfpr = ghapfprpop / population)





########################


zones@data <- merge(pfpr , zones@data , by.x  = 'District' , by.y = 'districts' , 
                    all = TRUE , sort = FALSE )
zones@data <- c('county' , 'ghapfprpop' , 'population' , 'pfpr')


test <- merge(zones@data , yearly , by.x  = 'District' , by.y = 'county_ID' , 
              sort = FALSE)

test$cov <- test$V1 / test$ghapfprpop
test$popcov <- test$V1 / test$population

test <- subset(test , year == 2014 & cov  <100)

table(test$cov)


test_map <- zones
test_map@data <- merge(test_map@data , test , by = 'District' , 
                       sort = FALSE)
