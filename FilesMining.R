#### This proram gets the list of the files in the data we got from Kenya, and 
#### analyses the names of the different files to get the type of report and an indication of the 
#### date it has been produced

library(plyr)

PathDistrictData <- "J://LIMITED_USE/PROJECT_FOLDERS/KEN/ART_ABCE/HMIS/Districts"

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

####Extracting Type of report from files names

ClassifyReport <- function(ReportType , patterns , data){
  FoundPatterns <- character()
  for (ONEpattern in patterns){
    FoundPatterns <- c(FoundPatterns , grep(ONEpattern , data$Files , value = TRUE , ignore.case = TRUE))
  }
  data$ReportType[data$Files %in% FoundPatterns] <- ReportType
  data$ReportType
}

###Getting type of report from file nanmes

TotalList$ReportType <- ClassifyReport("105 - Service Delivery Summary" , c("105") , TotalList)
TotalList$ReportType <- ClassifyReport("705A - Outpatient Summary <5" , c("705A" , "705 A") , TotalList)
TotalList$ReportType <- ClassifyReport("705B - Outpatient Summary >5" , c("705B" , "705 B") , TotalList)
TotalList$ReportType <- ClassifyReport("710 - Immunization Summary" , c("710") , TotalList)
TotalList$ReportType <- ClassifyReport("711B - RH, TB, Malaria, HIV & Chanis Summary" , c("711B") , TotalList)
TotalList$ReportType <- ClassifyReport("717 - Service Statistics" , c("717") , TotalList)
TotalList$ReportType <- ClassifyReport("718 - Inpatient Mortality and Morbidity" , c("718") , TotalList)

addmargins(table(TotalList$ReportType))

write.csv(TotalList , "TotalList.csv")

##Comparison with metadata

windowsMeta <- read.csv("C:/Users/grlurton/Documents/KenyaHIS/python/WindowsMetadata.csv" , header = FALSE)
colnames(windowsMeta) <-  c("Path" , "Author" , "Modifier" , "DateCreated" , "DateSaved")
windowsMeta$Path <- gsub( pattern = ":/" , replacement = "://" , x = windowsMeta$Path)

TotalList <- merge(TotalList , windowsMeta , all = T)

table(TotalList$Author,TotalList$ReportType)

table(TotalList$ReportType[TotalList$Author == "Not working"])/ table(TotalList$ReportType)
##We have a significantly higher number of non working files for 718... May be worth a look

###Names are considered authors of a report type if they are authors of more than 10% of these reports

IdentifyAuthor <- function(ReportData){
  NReports <- nrow(ReportData)
  ReportData$Author <- as.character(ReportData$Author)
  ReportData$DateCreated <- as.character(ReportData$DateCreated)
  DistAuthors <- data.frame(table(ReportData$Author , ReportData$DateCreated) / NReports > 0.1)
  colnames(DistAuthors) <- sort(unique(as.character(ReportData$DateCreated)))
  CoordAuthor <- which(DistAuthors == TRUE , arr.ind = TRUE)
  Author <- rownames(DistAuthors)[CoordAuthor[,1]]
  DateCreation <- colnames(DistAuthors)[CoordAuthor[,2]]
  data.frame(Author  , DateCreation)
}

ListForExtract <- TotalList[!is.na(TotalList$ReportType) & !is.na(TotalList$Author) , ]

MetadataTrace <- ddply(ListForExtract  , .(ReportType) , IdentifyAuthor)

MetadataTrace