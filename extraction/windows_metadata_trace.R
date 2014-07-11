#### This program outputs a metadata profile for the different type of reports in Kenya HMIS
#### Based on the windows metadata extracted with python script get_windows_metadata.py 

## Author : Grégoire Lurton
## Date   : July 2014

library(plyr)

setwd("J:/Project/abce/ken/HMIS/data")

####Extracting Type of report from files names
ClassifyReport <- function(ReportType , patterns , data){
  FoundPatterns <- character()
  for (ONEpattern in patterns){
    FoundPatterns <- c(FoundPatterns , grep(ONEpattern , data$Path , value = TRUE , ignore.case = TRUE))
  }
  data$ReportType[data$Path %in% FoundPatterns] <- ReportType
  data$ReportType
}

###Getting type of report from file names
windowsMeta <- read.csv("WindowsMetadata.csv" , header = TRUE)

windowsMeta$ReportType <- ClassifyReport("105 - Service Delivery Summary" , c("105") , windowsMeta)
windowsMeta$ReportType <- ClassifyReport("705A - Outpatient Summary <5" , c("705A" , "705 A") , windowsMeta)
windowsMeta$ReportType <- ClassifyReport("705B - Outpatient Summary >5" , c("705B" , "705 B") , windowsMeta)
windowsMeta$ReportType <- ClassifyReport("710 - Immunization Summary" , c("710") , windowsMeta)
windowsMeta$ReportType <- ClassifyReport("711B - RH, TB, Malaria, HIV & Chanis Summary" , c("711B") , windowsMeta)
windowsMeta$ReportType <- ClassifyReport("717 - Service Statistics" , c("717") , windowsMeta)
windowsMeta$ReportType <- ClassifyReport("718 - Inpatient Mortality and Morbidity" , c("718") , windowsMeta)

###Getting date of report from file names

library(stringr)

windowsMeta$YearReport <- str_extract( windowsMeta$Path , c("2008|2009|2010|2011|2012"))



###Names are considered main authors of a report type if they are authors of more than 10% of these reports
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

ListForExtract <- windowsMeta[!is.na(windowsMeta$ReportType) & !is.na(windowsMeta$Author) , ]

MetadataTrace <- ddply(ListForExtract  , .(ReportType) , IdentifyAuthor)

MetadataTrace

write.csv(MetadataTrace , "WindowsMetadataTrace.csv" , row.names = FALSE)
write.csv(windowsMeta , "WindowsMetadata.csv" , row.names = FALSE)
