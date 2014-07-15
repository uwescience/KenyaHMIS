#### 

## Author : Grégoire Lurton
## Date   : July 2014

library(sqlshare)
library(stringr)
library(plyr)

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

getGPS <- function(){
  
}
