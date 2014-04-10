#### This proram gets the list of the files in the data we got from Kenya, and 
#### analyses the names of the different files to get the type of report and an indication of the 
#### date it has been produced

library(plyr)


PathDistrictData <- "J://LIMITED_USE/PROJECT_FOLDERS/KEN/ART_ABCE/HMIS/Districts"
  #"C://Users/grlurton/Documents/Kenya HIS Data 2008-2011/Districts"
  
  #

##Extracting list files in district data

ListDistricts <- list.files(PathDistrictData)
ListDirectory <- list.dirs(PathDistrictData)

TotalList <- data.frame(District = character(), Files = character())

for (dist in ListDirectory[-1]){
  District = unlist(strsplit(dist , "/"))[9]
  Files = list.files(dist)
  if (length(Files) > 0){
    ListFilesDist <- data.frame(District = District , Files = Files)
    TotalList <- rbind(TotalList , ListFilesDist)
  }
}

rm(ListFilesDist , dist)

TotalList$Files <- as.character(TotalList$Files)
TotalList$IdFile <- paste("ID" , seq(1:nrow(TotalList)) , sep = "")

TotalList$Path <- paste(PathDistrictData , TotalList$District , TotalList$Files , sep = "/")


##Finding years from the files names

substr74 <- substr(TotalList$Files , nchar(TotalList$Files) - 7 , nchar(TotalList$Files)-4 )
TotalList$Year[substr74 %in% c("2008" , "2009" , "2010" , "2011")] <- substr74[substr74 %in% c("2008" , "2009" , "2010" , "2011")] 

substr85 <- substr(TotalList$Files , nchar(TotalList$Files) - 8 , nchar(TotalList$Files)-5 )
TotalList$Year[substr85 %in% c("2008" , "2009" , "2010" , "2011")] <- substr85[substr85 %in% c("2008" , "2009" , "2010" , "2011")] 

TotalList$Year[is.na(TotalList$Year)] <- "No Date (yet)"

####Extracting Type of report from files names

ClassifyReport <- function(ReportType , patterns , data){
  FoundPatterns <- character()
  for (ONEpattern in patterns){
    FoundPatterns <- c(FoundPatterns , grep(ONEpattern , data$Files , value = TRUE , ignore.case = TRUE))
  }
  data$ReportType[data$Files %in% FoundPatterns] <- ReportType
  data$ReportType
}

rm(ONEpattern)


TotalList$ReportType <- ClassifyReport("105" , c("105") , TotalList)
TotalList$ReportType <- ClassifyReport("705A" , c("705A" , "705 A") , TotalList)
TotalList$ReportType <- ClassifyReport("705B" , c("705B" , "705 B") , TotalList)
TotalList$ReportType <- ClassifyReport("710" , c("710") , TotalList)
TotalList$ReportType <- ClassifyReport("711B" , c("711B") , TotalList)
TotalList$ReportType <- ClassifyReport("717" , c("717") , TotalList)
TotalList$ReportType <- ClassifyReport("718" , c("718") , TotalList)

TotalList$ReportType <- ClassifyReport("Analysis" , c("Analysis.xls") , TotalList)
TotalList$ReportType <- ClassifyReport("Hospital Administrative Statistics" , c("Hospital Adm" ,
                                                                                "ADMIN STATISTICS" ,
                                                                                "ADMINISTRATIVE STATISTICS") ,
                                       TotalList)
TotalList$ReportType <- ClassifyReport("Immunisation" , "IMMUNISATION" , TotalList)
TotalList$ReportType <- ClassifyReport("Inpatient Mortality and Morbidity" ,
                                       c("Morbidity and Mortality inpatient" ,
                                         "INPATIENT MORBIDITY AND MORTALITY" ,
                                         "IN-PATIENT MORBIDITY AND MORTALITY" ,
                                         "IN PATIENT MORBIDITY AND MORTALITY",
                                         "In-patient%20Morbidity%26Mortality" ,
                                         "In-patient Morbidity&M" ,
                                         "INPATIENT MORBIDITY & MORTALITY" ,
                                         "INPATIENT MORBIDITY&M" ,
                                         "INPATIENT MORBIDITY MORTALITY" ,
                                         "INPT MORB&MORT" ,
                                         "IN PATIENT MORB&MORT" ,
                                         "INPT MORB &MORT" ,
                                         "IP MORB &MORT"),
                                       TotalList)
TotalList$ReportType <- ClassifyReport("Inpatient Administrative Statistics" ,
                                       c("Inpatient Adm" , "In patient Adm") , TotalList)
TotalList$ReportType <- ClassifyReport("Service Delivery" , c("service delivery" ,
                                                              "Service Delvery" ,
                                                              "SERVICE DELIVERLY") , TotalList)
TotalList$ReportType <- ClassifyReport("Workload" , "WORKLOAD" , TotalList)

addmargins(table(TotalList$ReportType , TotalList$Year))