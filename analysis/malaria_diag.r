library(sqlshare)
library(plyr)
library(ggplot2)
library(zoo)
library(maptools)
library(stringr)
library(reshape2)

setwd("J:/Project/abce/ken/HMIS")


zones <- readShapePoly("data/pfprInZones.shp")
MalariaData <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705ADataComplete]
                                 WHERE Indicator=\'confirmed malaria\' OR Indicator=\'clinical malaria\' OR Indicator = \'typhoid fever\'OR Indicator =  \'Pneumonia\' AND isnumeric(Value) = 1;')

months <- c('january' , 'february' , 'march' , 'april' , 'may' , 'june' , 'july' , 'august' , 'september' , 
            'october' , 'november' , 'december')

CleanMonths <- function(x){
  x$Month <- as.character(x$Month)
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'apr'] <- 'april'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'aug'] <- 'august'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'dec'] <- 'december'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'feb'] <- 'february'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'jan'] <- 'january'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'jul'] <- 'july'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'jun'] <- 'june'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'may'] <- 'may'
  x$Month[tolower(substr(x$Month , 1 ,2)) == 'ma' & x$Month != 'may'] <- 'march'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'nov'] <- 'november'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'oct'] <- 'october'
  x$Month[tolower(substr(x$Month , 1 ,3)) == 'sep'] <- 'september'
  x$Month[!(x$Month %in% months)] <- NA
  x$Month <- factor(x$Month , ordered(months))
  x
}

formatNames <- function(x){
  tolower(str_trim(as.character(x)))
}

MalariaData <- CleanMonths(MalariaData)
MalariaData$CalMonth <- as.Date(as.yearmon(paste(MalariaData$Month , MalariaData$Year  , sep = '-') , "%B-%Y"))

MalariaData$District <- formatNames(MalariaData$District)

MalariaData <- subset(MalariaData , !is.na(Value) & District %in% zones@data$districts & District != "west pokot")

MalariaData <- merge(MalariaData , zones@data , by.x = "District" , by.y = 'District' , all.y = FALSE )

MalariaData$Rate <- MalariaData$Value / MalariaData$population

qplot(data = MalariaData , x = CalMonth , y = Value , col = Indicator , geom = "line") +
  facet_wrap(~District)

LargeMalariaValue <- dcast(MalariaData , CalMonth + District ~ Indicator , value.var = "Value" ,fun.aggregate = function(x) max(x , na.rm = TRUE))
colnames(LargeMalariaValue) <- c("CalMonth","District","clinMal","ConfMal","pneumonia","typhoid")

forFit <- subset(LargeMalariaRate , !is.na(pneumonia) & !is.na(clinMal) 
                 & is.finite(clinMal) & !is.nan(clinMal))

library(lme4)
fit <- lm(pneumonia ~ clinMal:District, data = forFit)

LargeMalariaRate <- dcast(MalariaData , CalMonth + District ~ Indicator , value.var = "Rate" ,fun.aggregate = function(x) max(x , na.rm = TRUE))
colnames(LargeMalariaRate) <- c("CalMonth","District","clinMal","ConfMal","pneumonia","typhoid")

qplot(data = LargeMalariaRate ,x = clinMal, y = pneumonia , col = District)+
  facet_wrap(~District)


ratio_clinic_confirm <- ddply(MalariaData , .( CalMonth , District) ,
                              function(x){
                                print(x$District)
                                if (length(x$Value[x$Indicator == "confirmed malaria"]) > 0 &
                                  length(x$Value[x$Indicator == "clinical malaria"]) > 0 ){
                                    ratio <- x$Value[x$Indicator == "confirmed malaria"][1] / x$Value[x$Indicator == "clinical malaria"][1] 
                                    data.frame(ratio , 
                                               clinic = x$Value[x$Indicator == "clinical malaria"][1])
                                  }
                                })


ratio_clinic_confirm <- subset(ratio_clinic_confirm , ratio <= 1)
qplot(data = ratio_clinic_confirm , x = clinic , y = ratio)+
  facet_wrap(~District)
qplot(data = ratio_clinic_confirm , x = CalMonth , y = ratio , col = clinic)+
  facet_wrap(~District)
