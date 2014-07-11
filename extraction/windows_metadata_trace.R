#### This program outputs a metadata profile for the different type of reports in Kenya HMIS
#### Based on the windows metadata extracted with python script get_windows_metadata.py 
#### It also corrects the date of reports using saving dates and extracts district from path of files

## Author : Grégoire Lurton
## Date   : July 2014

library(plyr)
library(ggplot2)
library(stringr)
library(zoo)

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

windowsMeta$YearReport <- str_extract( windowsMeta$Path , c("2008|2009|2010|2011|2012"))

windowsMeta$DateSaved <- as.Date(as.character(windowsMeta$DateSaved))

##The reports that have been saved in the beginning of a given year most certainly have data about the previous year
##Two modifications have to happen :
## *

months <- c("Jan" , "Feb" , "Mar" , "Apr" , "May" , "Jun" , "Jul" , "Aug" , "Sep" , "Oct" , "Nov" , "Dec")

windowsMeta$yearSave <- format(windowsMeta$DateSaved , "%Y")
windowsMeta$monthSave <- format(windowsMeta$DateSaved , "%b")
windowsMeta$monthSave <- factor(windowsMeta$monthSave , levels = months)


windowsMeta$yearSave[windowsMeta$yearSave < 2008] <- NA
windowsMeta$yearSave[windowsMeta$yearSave > 2012] <- NA

qplot(data = windowsMeta , x = yearSave , y = YearReport , geom = "jitter" , col = monthSave) + 
  facet_wrap(~ReportType)

windowsMeta$monthSave <- as.character(windowsMeta$monthSave)

###For reports saved in beginning of year

StartYearSave <- c("705A - Outpatient Summary <5" , "705B - Outpatient Summary >5" , "710 - Immunization Summary")
ChangePreviousYear <- windowsMeta$monthSave %in% c("Jan" , "Feb" , "Mar") & is.na(windowsMeta$YearReport) & 
  windowsMeta$ReportType %in% StartYearSave

windowsMeta$yearCorrect[ChangePreviousYear] <- as.numeric(windowsMeta$yearSave[ChangePreviousYear])- 1

WrongYear <- windowsMeta$ReportType %in% StartYearSave & windowsMeta$yearSave < windowsMeta$YearReport
windowsMeta$yearCorrect[WrongYear] <- "pb with year"

windowsMeta$yearCorrect[ChangePreviousYear == FALSE & WrongYear == FALSE & !is.na(WrongYear)] <- 
  windowsMeta$YearReport[ChangePreviousYear == FALSE & WrongYear == FALSE & !is.na(WrongYear)]

qplot(data = windowsMeta , x = yearSave , y = yearCorrect , geom = "jitter" , col = monthSave) + 
  facet_wrap(~ReportType)

###For reports saved in mid-year

StartYearSave <- c("105 - Service Delivery Summary")
table(windowsMeta$monthSave[windowsMeta$ReportType %in% StartYearSave])

GetYear <- windowsMeta$ReportType %in% StartYearSave & windowsMeta$monthSave %in% c("Jul" , "Aug" ,"Sep" , "Oct")

windowsMeta$yearCorrect[GetYear] <- windowsMeta$yearSave[GetYear]

sum(is.na(windowsMeta$yearCorrect))

qplot(data = windowsMeta , x = yearSave , y = yearCorrect , geom = "jitter" , col = monthSave) + 
  facet_wrap(~ReportType)

###Getting the district names

getDistrict <- function(x){
  unlist(strsplit(as.character(x$Path) , "/"))[8]
}


windowsMeta <- merge(windowsMeta , ddply(windowsMeta , .(Path) , getDistrict))
colnames(windowsMeta)[11] <- 'District'

#################################
#####Defining the metadata trace#
#################################

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
