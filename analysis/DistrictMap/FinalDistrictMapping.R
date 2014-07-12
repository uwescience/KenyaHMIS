#### This program maps the names of the zones for which the data is recorded in the original HMIS Data to an existing division of Kenya Territory
#### It outputs a dictionnary for the zones in the HMIS Data that are matched to zones in a given shapefile + specific GPS coordinates for some zones

## Author : Grégoire Lurton
## Date   : July 2014

setwd("J:/Project/abce/ken/HMIS/")

library(sp)
library(RColorBrewer)
library(sqlshare)
library(stringr)
library(raster)
library(maptools)
library(rgdal)
library(plyr)
library(ggplot2)

##Function to format the names of zones
formatNames <- function(x){
  tolower(str_trim(as.character(x)))
}

##Loading shapefile for Kenya Districts
Kenya <- readShapePoly('addata/Shapefiles/Districts//kenya_districts98.shp')
Kenya$DISTRICT <- formatNames(Kenya$DISTRICT)

##Getting list of zones in the data from reports 705B, uploaded in sqlshare
DataDistricts <-  
  as.character(fetch.data.frame("select distinct lower(ltrim(rtrim(District))) as District from [grlurton@washington.edu].[705BDataComplete]")$District)

##Processing Names as to have all combinations possible of composed names
DataNames <- data.frame(NameOrig = DataDistricts , NameUnif = DataDistricts ,  stringsAsFactors = FALSE)
n <- nrow(DataNames)
cardin <- c('north' , 'south' , 'east' , 'west' , 'central')
for (i in 1:n){
  NameSplit <- unlist(strsplit(DataNames$NameUnif[i] , split = ' '))
  lNameSplit <- length(NameSplit)
  if(NameSplit[1] %in% cardin){
    lcrop <- nchar(NameSplit[1])
    permut <- paste(substr(DataNames$NameUnif[i] , lcrop+2,nchar(DataNames$NameUnif[i])) ,
                    NameSplit[1] , sep = ' ')
    DataNames[n + 1 ,] <- c(DataNames$NameOrig[i] , permut)
    n <- n + 1
  }
  if(NameSplit[lNameSplit] %in% cardin){
    lcrop <- nchar(NameSplit[lNameSplit])
    permut <- paste(NameSplit[lNameSplit] ,
                    substr(DataNames$NameUnif[i] , 1,nchar(DataNames$NameUnif[i]) - lcrop -1) , 
                    sep = ' ')
    DataNames[n + 1 ,] <- c(DataNames$NameOrig[i] , permut)
    n <- n + 1
  }
}
rm(i , lcrop , n , lNameSplit , NameSplit , permut)

##Finding Exact correspondances between Shapefile and Data
DataNames$NameUnif[DataNames$NameUnif == 'garbatula'] <- 'garbatulla'
DataNames$NameUnif[DataNames$NameUnif == 'loitoktok'] <- 'loitokitok'
DataNames$NameUnif[DataNames$NameUnif == 'knh'] <- 'kenyatta'
Kenya$DISTRICT[Kenya$DISTRICT == 'mt.elgon'] <- 'mt. elgon'

FindExactMatch <- function(data , ShapeNames){
  data$Unit <- ''
  for(i in 1:length(ShapeNames)){
    name <- ShapeNames[i]
    if(name %in% data$NameUnif){
      data$Unit[data$NameUnif == name] <- 'District'
      data$NameShape[data$NameUnif == name] <- ShapeNames[i] 
    }
  }
  return(data)
}

DataNames <- FindExactMatch(DataNames , Kenya$DISTRICT)

##Add simplified Districts
SimplifyDist <- data.frame(NameUnif = grep(paste(cardin,collapse="|"), 
                                           DataNames$NameUnif , value=TRUE) ,
                           NameSimple = str_trim(gsub(pattern = paste(cardin,collapse="|"), 
                                                      replacement = '' ,
                                                      x = grep(paste(cardin,collapse="|"), DataNames$NameUnif , value=TRUE) )
                                                 )
                           )

for(i in 1:nrow(SimplifyDist)){
  name <- SimplifyDist$NameUnif[i]
  if (as.character(SimplifyDist$NameSimple[i]) %in% Kenya$DISTRICT){
    DataNames$NameShape[DataNames$NameUnif == name & as.character(DataNames$Unit) != 'District'] <- as.character(SimplifyDist$NameSimple[i])
    DataNames$Unit[DataNames$NameUnif == name & DataNames$Unit != 'District'] <- 'Splitted District'
    DataNames$Unit[DataNames$NameUnif == name & DataNames$Unit == 'District'] <- 'Simple or Splitted District'
  }
  else DataNames$Unit[DataNames$NameUnif == name & DataNames$Unit != 'District'] <- 'Splitted Zone'
}

DataNames$Unit[DataNames$NameOrig == 'meru south'] <- 'District'
DataNames$NameShape[DataNames$NameOrig == 'meru south'] <- 'meru south'

DataNames$Unit[DataNames$NameOrig == 'west pokot'] <- 'District'
DataNames$NameShape[DataNames$NameOrig == 'west pokot'] <- 'west pokot'

##Loading facilities
facilities <- read.csv('addata/KenyaFacilitiesGPS.csv')
facilities <- subset(facilities , Geolocation != '' , select = c(F_NAME , Province , District , Division , Facility.Type , Geolocation))
colnames(facilities) <- c('FacilityName' , 'FacilityProvince' , 'FacilityDistrict' ,'FacilityDivision' , 'FacilityType' , 'Geolocation')

facilities$Geolocation <- as.character(facilities$Geolocation)
facilities$Geolocation <- substr(facilities$Geolocation , 2 , nchar(facilities$Geolocation) - 1)
ExtractGeolocation <- unlist(strsplit(facilities$Geolocation , ", "))
facilities$Longitude <- as.numeric(ExtractGeolocation[seq(1, length(ExtractGeolocation) - 1 , 2)])
facilities$Latitude  <- as.numeric(ExtractGeolocation[seq(2, length(ExtractGeolocation) , 2)])

##Match Data and Facilities based on Name
for(i in 1:nrow(DataNames)){
  facilities$FacilityType <- as.numeric(as.character(facilities$FacilityType))
  name <- DataNames$NameUnif[i]
  test <- grep(name , tolower(facilities$FacilityName[facilities$FacilityType <= 2]))
  print(test)
  if(length(test) == 1){ ##If only One facility answers the search, she is designated
    DataNames$FacilityName[i] <- as.character(facilities$FacilityName[facilities$FacilityType <= 2][test])
    DataNames$FacilLong[i]    <- facilities$Longitude[facilities$FacilityType <= 2][test]
    DataNames$FacilLat[i]     <- facilities$Latitude[facilities$FacilityType <= 2][test]
    if (DataNames$Unit[i] == ''){
      DataNames$Unit[i] <- 'Facility'
    }
    else{
      DataNames$Unit[i] <- 'Facility or District'
    }
  }
  if(length(test) > 1){ ##If only multiple facilities answers the search, We take the first for now
    DataNames$FacilityName[i] <- as.character(facilities$FacilityName[facilities$FacilityType <= 2][test][1])
    DataNames$FacilLong[i] <- facilities$Longitude[facilities$FacilityType <= 2][test][1]
    DataNames$FacilLat[i] <- facilities$Latitude[facilities$FacilityType <= 2][test][1]
    if (DataNames$Unit[i] == ''){
      DataNames$Unit[i] <- 'Multiple Facilities'
    }
    else{
      DataNames$Unit[i] <- 'Multiple Facilities or District'
    }
  }
  if(length(test) == 0){
    DataNames$FacilityName[i] <- 'Not Found'
    DataNames$FacilLong[i] <- 'Not Found'
    DataNames$FacilLat[i] <- 'Not Found'
  }
}

## Simplifying the whole Data
# Name|DistNameShape|Level|Long|Lat
FinalDataMap <- data.frame(Name = character(length(unique(DataNames$NameOrig))) ,
                           FacilityName = character(length(unique(DataNames$NameOrig))) ,
                           DistNameShape = character(length(unique(DataNames$NameOrig))) ,
                           Level = character(length(unique(DataNames$NameOrig))) ,
                           Longitude = numeric(length(unique(DataNames$NameOrig))) ,
                           Latitude = numeric(length(unique(DataNames$NameOrig))) ,
                           stringsAsFactors = FALSE
                           )

for(i in 1:length(unique(DataNames$NameOrig))){
  name <- unique(DataNames$NameOrig)[i]
  if(nrow(DataNames[DataNames$NameOrig == name ,]) == 1){
    FinalDataMap$Name[i] <- unique(DataNames$NameOrig)[i]
    FinalDataMap$DistNameShape[i] <- DataNames$NameShape[DataNames$NameOrig == name]
    FinalDataMap$FacilityName[i] <- DataNames$FacilityName[DataNames$NameOrig == name]
    FinalDataMap$Level[i] <- DataNames$Unit[DataNames$NameOrig == name]
    FinalDataMap$Longitude[i] <- DataNames$FacilLong[DataNames$NameOrig == name]
    FinalDataMap$Latitude[i]  <- DataNames$FacilLat[DataNames$NameOrig == name]
  }
  if(nrow(DataNames[DataNames$NameOrig == name ,]) > 1){
    if (length(unique(DataNames$Unit[DataNames$NameOrig == name])) == 1){
      FinalDataMap$Name[i] <- unique(DataNames$NameOrig)[i]
      FinalDataMap$DistNameShape[i] <- DataNames$NameShape[DataNames$NameOrig == name][1]
      FinalDataMap$FacilityName[i] <- DataNames$FacilityName[DataNames$NameOrig == name][1]
      FinalDataMap$Level[i] <- DataNames$Unit[DataNames$NameOrig == name][1]
      FinalDataMap$Longitude[i] <- DataNames$FacilLong[DataNames$NameOrig == name][1]
      FinalDataMap$Latitude[i]  <- DataNames$FacilLat[DataNames$NameOrig == name][1]
    }
    else print(DataNames[DataNames$NameOrig == name ,])
  }
}

FinalDataMap$Level[FinalDataMap$Name == 'westlands'] <- ''
table(FinalDataMap$Level)

##Imputing District to facilities based on their location 
##Here we give location in the shapefile we use
coordinates(facilities) = ~Latitude+Longitude
facilities$DistrictShape <- over(facilities, Kenya)
facilities <- data.frame(facilities)
colnames(facilities)[9] <- 'ShapeDistrict'

for (name in FinalDataMap$FacilityName[FinalDataMap$Level %in% c('Facility' , 'Multiple Facilities')] ){
  print(name)
  FinalDataMap$DistNameShape[FinalDataMap$FacilityName == name] <-facilities$ShapeDistrict[facilities$FacilityName == name]
}


##Rechercher les noms des donnnées dans divison; location et sub-location de la bdd facilities



FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("east pokot")] <- "baringo"
FinalDataMap$Level[FinalDataMap$Name %in% c("buret")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("buret" , "sotik")] <- "bomet"
FinalDataMap$Level[FinalDataMap$Name %in% c("buret" , "sotik")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("bunyala" , "butula")] <- "busia"
FinalDataMap$Level[FinalDataMap$Name %in% c("bunyala")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("keiyo")] <- "elgeyo"
FinalDataMap$Level[FinalDataMap$Name %in% c("keiyo")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("fafi" , "ijara" , "lagdera")] <- "garissa"
FinalDataMap$Level[FinalDataMap$Name %in% c("fafi" , "ijara" , "lagdera")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("ndhiwa")] <- "homa_bay"
FinalDataMap$Level[FinalDataMap$Name %in% c("ndhiwa")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("khwisero" , "lugari" , "matete")] <- "kakamega"
FinalDataMap$Level[FinalDataMap$Name %in% c("khwisero" , "lugari" , "matete")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("kipkelion")] <- "kericho"
FinalDataMap$Level[FinalDataMap$Name %in% c("kipkelion")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("garbatula")] <- "isiolo"
FinalDataMap$Level[FinalDataMap$Name %in% c("garbatula")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("athi river" , "matungu" , "mwala")] <- "machakos"
FinalDataMap$Level[FinalDataMap$Name %in% c("athi river" , "matungu", "mwala")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("kibwezi"  , "nzaui")] <- "makueni"
FinalDataMap$Level[FinalDataMap$Name %in% c("kibwezi" , "nzaui")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("chalbi" )] <- "marsabit"
FinalDataMap$Level[FinalDataMap$Name %in% c("chalbi" )] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("imenti north" , "imenti south" , "maara")] <- "meru"
FinalDataMap$Level[FinalDataMap$Name %in% c("imenti north" , "imenti south" , "maara")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("nyatike")] <- "migori"
FinalDataMap$Level[FinalDataMap$Name %in% c("nyatike")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("kilindini")] <- "mombasa"
FinalDataMap$Level[FinalDataMap$Name %in% c("kilindini")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("dagoretti" , "embakasi" , "kasarani" ,
                                                    "langata"   , "makadara" , "starehe" , "westlands")] <- "nairobi"
FinalDataMap$Level[FinalDataMap$Name %in% c("dagoretti", "embakasi" , "kasarani" , "langata" , 
                                            "makadara", "starehe" , "westlands")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("manga" , "marani" , "masaba")] <- "kisii"
FinalDataMap$Level[FinalDataMap$Name %in% c("manga" , "marani" , "masaba")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("igembe")] <- "nyambene"
FinalDataMap$Level[FinalDataMap$Name %in% c("igembe")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("tinderet")] <- "nandi"
FinalDataMap$Level[FinalDataMap$Name %in% c("tinderet")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("borabu")] <- "nyamira"
FinalDataMap$Level[FinalDataMap$Name %in% c("borabu")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("ruiru")] <- "thika"
FinalDataMap$Level[FinalDataMap$Name %in% c("ruiru")] <- "Splitted District"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("taita")] <- "taita taveta"
FinalDataMap$Level[FinalDataMap$Name %in% c("taita")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("trans nzoia east" , "trans nzoia west" , "kwanza")] <- "trans-nzoia"
FinalDataMap$Level[FinalDataMap$Name %in% c("trans nzoia east" , "trans nzoia west")] <- "Splitted District"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("eldoret east" , "eldoret west")] <- "uasin gishu"
FinalDataMap$Level[FinalDataMap$Name %in% c("eldoret east"  , "eldoret west")] <- "Unclear"

FinalDataMap$DistNameShape[FinalDataMap$Name %in% c("emuhaya" , "hamisi")] <- "vihiga"
FinalDataMap$Level[FinalDataMap$Name %in% c("emuhaya")] <- "Unclear"


Kenya$DISTRICT
sort(FinalDataMap$Name[is.na(FinalDataMap$DistNameShape)])

table(FinalDataMap$Level)

write.csv(FinalDataMap , 'DistrictMapping.csv' , row.names = FALSE)
