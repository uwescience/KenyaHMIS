library(sqlshare)
library(plyr)
library(ggplot2)
library(zoo)
library(maptools)
library(stringr)
library(reshape2)
library(lme4)

setwd("J:/Project/abce/ken/HMIS")


zones <- readShapePoly("data/pfprInZones.shp")
MalariaDataPed <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705ADataClean]
                                 WHERE Indicator=\'confirmed malaria\' OR Indicator=\'clinical malaria\' OR Indicator = \'typhoid fever\'OR Indicator =  \'Pneumonia\' AND isnumeric(Value) = 1;')
MalariaDataAdult <- fetch.data.frame('select District , Value , Year , Month , Indicator
                                 FROM [grlurton@washington.edu].[705BDataClean]
                                 WHERE Indicator=\'confirmed malaria\' OR Indicator=\'clinical malaria\' OR Indicator = \'typhoid fever\'OR Indicator =  \'Pneumonia\' AND isnumeric(Value) = 1;')

Malaria105 <- fetch.data.frame('select District , Value , Year , Month , Level , Indicator
                                 FROM [grlurton@washington.edu].[105DataClean]
                                 WHERE Indicator=\'total number of inpatient malaria cases\' 
                                        OR Indicator=\'number of under five years treated for malaria\' 
                                        OR Indicator = \'number of patients over five years treated for malaria\'
                                        OR Indicator =  \'number of malaria inpatient deaths\' 
                                        OR Indicator =  \'number of llitn distributed to pregnant women\' 
                                        OR Indicator= \'number of llitns distributed to children under 5 years\' 
                                        OR Indicator=\'number of children over five years treated for malaria\' 
                                        OR Indicator= \'number of < 1 deaths occuring at facility\'
                                        AND isnumeric(Value) = 1;')


MalariaDataAdult$Cohort <- "> 5 years"
MalariaDataPed$Cohort <- "< 5 years"

MalariaData <- rbind(MalariaDataAdult , MalariaDataPed)
MalariaData$CalMonth <- as.Date(as.yearmon(paste(MalariaData$Month , MalariaData$Year  , sep = '-') , "%B-%Y"))
Malaria105$CalMonth <- as.Date(as.yearmon(paste(Malaria105$Month , Malaria105$Year  , sep = '-') , "%B-%Y"))


ratio_clinic_confirm <- ddply(MalariaData , .( CalMonth , District , Cohort) ,
                              function(x){
                                print(x$District)
                                if (length(x$Value[x$Indicator %in% c("confirmed malaria")]) > 0 &
                                  length(x$Value[x$Indicator == "clinical malaria"]) > 0 ){
                                    ratio <- x$Value[x$Indicator %in% c("confirmed malaria")][1] / x$Value[x$Indicator == "clinical malaria"][1] 
                                    data.frame(ratio , 
                                               clinic = x$Value[x$Indicator == "clinical malaria"][1])
                                  }
                                })

ratio_clinic_confirm <- subset(ratio_clinic_confirm , ratio <= 1 & clinic < 55000)
meanRatio <- ddply(ratio_clinic_confirm , .(District) , function(x) mean(x$ratio))
colnames(meanRatio)[2] <- "mean"
meanRatio$District <- ordered(meanRatio$District , levels = meanRatio$District[order(meanRatio$mean)])


ratio_clinic_confirm <- merge(meanRatio , ratio_clinic_confirm)

qplot(data = ratio_clinic_confirm , x = clinic , y = ratio , col = Cohort)+
  facet_wrap(~District) + theme_bw() +
  geom_hline(aes(yintercept = mean))
  
qplot(data = ratio_clinic_confirm , x = CalMonth , y = ratio , col = Cohort)+
  facet_wrap(~District) + theme_bw()+
  geom_hline(aes(yintercept = mean))

Summed105 <- ddply(Malaria105 , .(District , CalMonth , Indicator) , 
                         function(x) sum(x$Value , na.rm = T) , .progress = 'text')
colnames(Summed105)[4] <- 'Value'


Summed105$Cohort[Summed105$Indicator == "number of under five years treated for malaria"] <- "< 5 years"
Summed105$Cohort[Summed105$Indicator == "number of patients over five years treated for malaria"] <- "> 5 years"

Summed105$Indicator <- as.character(Summed105$Indicator)
Summed105$Indicator[Summed105$Indicator == "number of under five years treated for malaria"] <-
  Summed105$Indicator[Summed105$Indicator == "number of patients over five years treated for malaria"] <-
  "treated malaria"

ComparePlot <- rbind(data.frame(subset(Summed105 ,
                                       Indicator == "treated malaria"), 
                                Source = "105 Report") ,
                     data.frame(subset(MalariaData , 
                            Indicator %in% c('clinical malaria' , 'confirmed malaria') ,
                            select = c(Indicator , CalMonth , District , Value , Cohort)
                            ) , Source = "705 Reports")
                     )

ComparePlot <- subset(ComparePlot , !is.na(CalMonth) & !is.na(Value))

ggplot(data = ComparePlot , aes(x = CalMonth , y = Value , colour = Indicator , linetype = Cohort) )+ 
  geom_line() +
  facet_wrap(~District , scales = "free_y") + theme_bw()
  



fit <- lmer(ratio ~ log(clinic)*District + Cohort + (1|District), data = ratio_clinic_confirm)

plot(ratio_clinic_confirm$ratio  , fitted(fit))
lines(x = c(0,1) , y = c(0,1) , col = 'red')


LargeMalariaValue <- dcast(MalariaData , CalMonth + District ~ Indicator , value.var = "Value" ,fun.aggregate = function(x) max(x , na.rm = TRUE))
colnames(LargeMalariaValue) <- c("CalMonth","District","clinMal","ConfMal","pneumonia","typhoid")

forFit <- subset(LargeMalariaRate , !is.na(pneumonia) & !is.na(clinMal) 
                 & is.finite(clinMal) & !is.nan(clinMal))


fit <- lm(pneumonia ~ clinMal:District, data = forFit)

LargeMalariaRate <- dcast(MalariaData , CalMonth + District ~ Indicator , value.var = "Rate" ,fun.aggregate = function(x) max(x , na.rm = TRUE))
colnames(LargeMalariaRate) <- c("CalMonth","District","clinMal","ConfMal","pneumonia","typhoid")

qplot(data = LargeMalariaRate ,x = clinMal, y = pneumonia , col = District)+
  facet_wrap(~District)
