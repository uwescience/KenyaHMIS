#### This program extracts relevant information from metadata of the excel files
#### from Kenyan HMIS to correct available data, or add some data points

## Author : Grégoire Lurton
## Date   : July 2014


library(ggplot2)
library(scales)
library(zoo)

setwd("J:/Project/abce/ken/HMIS/data")

metadata <- read.csv("WindowsMetadata.csv")

metadata$DateSaved <- as.Date(as.character(metadata$DateSaved))

months <- c("Jan" , "Feb" , "Mar" , "Apr" , "May" , "Jun" , "Jul" , "Aug" , "Sep" , "Oct" , "Nov" , "Dec")
date <- as.yearmon(metadata$DateSaved)
metadata$SaveYear <- format(date , "%Y")
metadata$Month <- format(date , "%b")
metadata$Month <- factor(metadata$Month , levels  = months)

dataPlot <- subset(metadata , DateSaved >'2008/01/01' & !is.na(ReportType) & 
                     !(ReportType %in% c('718 - Inpatient Mortality and Morbidity' , '711B - RH, TB, Malaria, HIV & Chanis Summary ',
                                         '717 - Service Statistics '))
)

qplot(data = dataPlot , x = Month , fill = as.character(YearReport)) +
  facet_grid(ReportType~SaveYear)
