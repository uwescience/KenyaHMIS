#### This program looks at how malaria diagnosis is made in facilities and how it varies between facilities.
#### Additional question is how this may finally act upon evaluation of care in facilities

## Author : Grégoire Lurton
## Date   : July 2014


library(sqlshare)
library(plyr)
library(ggplot2)
library(zoo)
library(maptools)
library(stringr)
library(reshape2)
library(lme4)
library(MASS)

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

###Select relevant data

SelectData <- function(data){
  subset(data , !is.na(Value) &  District %in% zones@data$District)
}

MakeDate <- function(x){
  x$CalMonth <- as.Date(as.yearmon(paste(x$Month , x$Year  , sep = '-') , "%B-%Y"))
  subset(x , !is.na(CalMonth))
}

Data705 <- SelectData(MakeDate(MalariaData))
Data105 <- SelectData(MakeDate(Malaria105))




MalariaData$CalMonth <- as.Date(as.yearmon(paste(MalariaData$Month , MalariaData$Year  , sep = '-') , "%B-%Y"))
Malaria105$CalMonth <- as.Date(as.yearmon(paste(Malaria105$Month , Malaria105$Year  , sep = '-') , "%B-%Y"))

MalariaData <- merge(MalariaData , zones@data)

### Computing the confirmation ratio

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


ratio_clinic_confirm <- merge(meanRatio , ratio_clinic_confirm )
ratio_clinic_confirm <- merge(ratio_clinic_confirm, zones@data)

qplot(data = ratio_clinic_confirm , x = clinic , y = ratio , col = Cohort)+
  facet_wrap(~District) + theme_bw() +
  geom_hline(aes(yintercept = mean)) + 
  xlab('Number of clinical diagnosis') + ylab('Confirmation ratio')
  
qplot(data = ratio_clinic_confirm , x = CalMonth , y = ratio , col = Cohort)+
  facet_wrap(~District) + theme_bw()+
  geom_hline(aes(yintercept = mean))+ 
  xlab('Time') + ylab('Confirmation ratio')

ratio_clinic_confirm$Month <- months(ratio_clinic_confirm$CalMonth)

ratio_clinic_confirm <- subset(ratio_clinic_confirm, !is.na(Month))

qplot(data = ratio_clinic_confirm , x = pfpr , y = ratio)+
  theme_bw() + facet_wrap(~Month)



#####Modelisation

##Model 1 : what influences the confirmation ratio
##Would be nice to have other covariates. other infections ? resources ? types of facilties ? 
##total number of admissions ?

ratio_clinic_confirm$District <- factor(ratio_clinic_confirm$District, ordered =  FALSE)

modelRatio <- lmer(ratio ~ log(population) + District + Month + (1|Month), 
            data = ratio_clinic_confirm )

plot(fitted(modelRatio) , ratio_clinic_confirm$ratio)
lines(x = c(0,max(ratio_clinic_confirm$ratio) ), y = c(0,max(ratio_clinic_confirm$ratio) ) , col = 'red')



##Model 2 : does high clinical diagnosis / low confirmation influence the 
## diagnostic of other conditions like pneumonia or typhoid

LargeMalaria <- dcast(MalariaData , CalMonth + District ~ Indicator , value.var = "Value" ,
                           fun.aggregate = function(x) max(x , na.rm = TRUE))
colnames(LargeMalariaValue) <- c("CalMonth","District","clinMal","ConfMal","pneumonia","typhoid")

 <- merge(LargeMalariaValue , ratio_clinic_confirm ,by  = c("District" , "CalMonth"))

forFit <- subset(data , !is.na(pneumonia) & !is.na(clinMal) 
                 & is.finite(clinMal) & !is.nan(clinMal))

fitPneumo <- glm(pneumonia ~ ratio*pfpr , data = forFit , offset = log(population) ,
           family = quasipoisson)
summary(fitPneumo)

fitTyphoid <- glm(typhoid ~ ratio*pfpr + ratio:District, data = forFit , offset = log(population) ,
                 family = quasipoisson)

fitTyphoidNB <- glm.nb(typhoid ~ ratio*pfpr + log(population)+ pneumonia , data = forFit)

fitTyphoidNB <- glm.nb(typhoid ~ ratio*pfpr + ratio:District + log(population) , data = forFit)

summary(fitTyphoidNB)

summary(forFit$typhoid)

par(mfrow =c(2,1))
plot(fitted(fitTyphoid) , forFit$typhoid)
lines(x = c(0,max(forFit$typhoid) ), y = c(0,max(forFit$typhoid) ) , col = 'red')
plot(fitted(fitTyphoidNB) , forFit$typhoid)
lines(x = c(0,max(forFit$typhoid) ), y = c(0,max(forFit$typhoid) ) , col = 'red')
