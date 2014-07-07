datasaves <- read.csv("WindowsMetadata2.csv")


library(ggplot2)
library(scales)
library(zoo)

dates <- as.Date(as.character(datasaves$DateSaved) , format = "%m/%d/%Y")

datasaves$DateSaved <- dates
datasaves <- subset(datasaves , DateSaved  < '2012/01/01' & DateSaved > '2007/01/01')

date <- as.yearmon(datasaves$DateSaved)
datasaves$SaveYear <- format(date , "%Y")
datasaves$Month <- format(date , "%b")
datasaves <- CleanMonths(datasaves)
datasaves$Month <- factor(datasaves$Month , levels  = months)


dataPlot <- subset(datasaves , DateSaved >'2008/01/01' & !is.na(ReportType) & 
                     !(ReportType %in% c('718 - Inpatient Mortality and Morbidity' , '711B - RH, TB, Malaria, HIV & Chanis Summary ',
                                         '717 - Service Statistics '))
)

qplot(data = dataPlot , x = Month , fill = SaveYear) +
  facet_grid(ReportType~SaveYear)





