#### This Program takes the districts scraped from DHIS orgunits and matches them as well as possible
#### with the names in the excel data. 
#### It then gets the GPS coordinates of these zones from the DHIS2 webapp and produces a shapefile
#### for the zones in this data.


## Author : Grégoire Lurton
## Date   : July 2014

library(sqlshare)
library(stringr)
library(plyr)
library(RCurl)
library(XML)
library(sp)
library(maptools)
library(shapefiles)


setwd("J:/Project/abce/ken/HMIS")

dhis_orgunits <- read.csv('addata/dhis_orgunits.csv' , header = TRUE)

data_districts <-  
  as.character(fetch.data.frame("select distinct lower(ltrim(rtrim(District))) as District from [grlurton@washington.edu].[705BDataComplete]")$District)

data_districts <- data.frame(districts = data_districts)

##Function to format the names of zones
formatNames <- function(x){
  tolower(str_trim(as.character(x)))
}

dhis_orgunits$child_name <- formatNames(dhis_orgunits$child_name)
data_districts$districts <- formatNames(data_districts$districts)

dhis_orgunits$child_name <- gsub(" sub county" , "", dhis_orgunits$child_name)
dhis_orgunits$child_name <- gsub(" sub-county" , "", dhis_orgunits$child_name)
dhis_orgunits$child_name <- gsub(" subcounty" , "", dhis_orgunits$child_name)
dhis_orgunits$child_name <- gsub(" s.c." , "", dhis_orgunits$child_name)
dhis_orgunits$child_name <- gsub(" county" , "", dhis_orgunits$child_name)

dhis_orgunits$child_name[dhis_orgunits$child_name == "murang'a south"] <- "muranga south"
dhis_orgunits$child_name[dhis_orgunits$child_name == "gucha south"] <- "south gucha"
dhis_orgunits$child_name[dhis_orgunits$child_name == "mt elgon"] <- "mt. elgon"
dhis_orgunits$child_name[dhis_orgunits$child_name == "taita taveta"] <- "taita"
dhis_orgunits$child_name[dhis_orgunits$child_name == "elgeyo-marakwet"] <- "marakwet"
dhis_orgunits$child_name[dhis_orgunits$child_name == "nandi north (mosop)"] <- "mosop"
dhis_orgunits$child_name[dhis_orgunits$child_name == "thika town"] <- "thika"
dhis_orgunits$child_name[dhis_orgunits$child_name == "pokot north"] <- "north pokot"
dhis_orgunits$child_name[dhis_orgunits$child_name == "tharaka nithi"] <- "tharaka"
dhis_orgunits$child_name[dhis_orgunits$child_name == "loitokitok"] <- "loitoktok"
dhis_orgunits$child_name[dhis_orgunits$child_name == "kenyatta national hospital"] <- "knh"

length(unique(data_districts$districts[data_districts$districts %in% dhis_orgunits$child_name]))
length(unique(data_districts$districts[data_districts$districts %in% dhis_orgunits$child_name])) / 176
data_districts$districts[!(data_districts$districts %in% dhis_orgunits$child_name)]

SimplifyDists <- function(x){
  unique(as.character(x$child_adress))[1]
}

simple_dhis_data <- ddply(dhis_orgunits[!is.na(dhis_orgunits$child_name),] , 
                          .(child_name) , 
                          SimplifyDists , .progress = 'text')
colnames(simple_dhis_data) <- c("District" , "url")

districtsTable <- merge(data_districts , simple_dhis_data , by.x = 'districts' , by.y = 'District' , all.x = TRUE , all.y = FALSE)

##Return GPS coordinates as a string
getGPS <- function(url){
  Page<-getURL(url,userpwd="grlurton:Glurton29",
               ssl.verifypeer = FALSE , httpauth = 1L)
  ParsedPage <- xmlParse(Page)
  root <- xmlRoot(ParsedPage)
  unitcoordinates <- xmlValue(root[["coordinates"]])
  unitcoordinates <- substr(unitcoordinates , 4 , nchar(unitcoordinates) - 3)
  unitcoordinates
}

## Returns the GPS coordinates in tabular format
tabGPS <- function(string_coordinates){
  string_coordinates <- substr(string_coordinates , 2 , nchar(string_coordinates) - 1)
  tabCoord <- data.frame(unlist(strsplit(string_coordinates , "],[" , fixed = TRUE)))
  splitcoord <- unlist(strsplit(as.character(tabCoord[,1]) , "," , fixed = TRUE))
  coord <- data.frame(long = as.numeric(splitcoord[2*(1:(length(splitcoord)/2))-1]) ,
                      lat = as.numeric(splitcoord[2*(1:(length(splitcoord)/2))]) )
  if (!is.na(coord$long) & !is.na(coord$lat)){
    if (coord$long[1] != coord$long[nrow(coord)]) coord$long <- NA
  }
  coord
}

Coordinates <- ddply(districtsTable[!is.na(districtsTable$url) , ] , .(districts) ,
                     function(x) {print(x$districts)
                                  tabGPS(getGPS(paste(x$url , "xml" , sep = '.')))} ,
                     .progress = 'text')

##Taking out non valid coordinates + coordinates from Kentatta National Hospital
CoordPolygons <- subset(Coordinates , !is.na(long) & !is.na(lat) & districts != "knh")

##Creating data frame to be included in the shapefile
ShapeData <- data.frame(districts = unique(CoordPolygons$districts) , country = "Kenya")

Shapefile <- convert.to.shapefile(CoordPolygons, ShapeData, "districts", 5)
write.shapefile(Shapefile, "data/zones", arcgis=T)

Shapefile <- readShapePoly("data/zones.shp")
Kenya <- readShapePoly('addata/Shapefiles/Districts//kenya_districts98.shp')

plot(Kenya , col = "red")
plot(Shapefile , add = TRUE , col = "grey")