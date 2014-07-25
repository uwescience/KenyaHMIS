#### This program looks at the data relevant to Malaria analysis, and compares data from reports 705s and report 105
#### To ensure that the data from these two sources is consistant

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

setwd("J:/Project/abce/ken/HMIS")


##Loading and formatting data

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

Malaria105$Cohort[Malaria105$Indicator == "number of under five years treated for malaria"] <- "< 5 years"
Malaria105$Cohort[Malaria105$Indicator == "number of patients over five years treated for malaria"] <- "> 5 years"

Malaria105$Indicator <- as.character(Malaria105$Indicator)
Malaria105$Indicator[Malaria105$Indicator == "number of under five years treated for malaria"] <-
  Malaria105$Indicator[Malaria105$Indicator == "number of patients over five years treated for malaria"] <-
  "treated malaria"

###Select relevant data

SelectData <- function(data , indicator){
  subset(data , !is.na(Value) &  District %in% zones@data$District & Indicator == indicator)
}

MakeDate <- function(x){
  x$CalMonth <- as.Date(as.yearmon(paste(x$Month , x$Year  , sep = '-') , "%B-%Y"))
  subset(x , !is.na(CalMonth))
}

Data705 <- SelectData(MakeDate(MalariaData) ,"clinical malaria")
Data105 <- SelectData(MakeDate(Malaria105) ,  "treated malaria")

####Summing and trimming Malaria cases accross each data set

SumData <- function(data){
  ddply(data , .(District , CalMonth , Indicator) , 
        function(x){
          data.frame(totalCases = sum(x$Value , na.rm = TRUE))
        },
        .progress = 'text')
}

Summed705 <- SumData(Data705)
Summed105 <- SumData(Data105)

TrimData <- function(data){
  ddply(data , .(District) , 
        function(x){
          subset(x , !(totalCases %in% boxplot.stats(totalCases)$out))
        })
}

trimmed705 <-  TrimData(Summed705)
trimmed105 <-  TrimData(Summed105)

#### Looking at correlation between the two series

corMatrix <- merge(trimmed105[,-3] , trimmed705[,-3] , by = c("District", 'CalMonth') , all = FALSE)
colnames(corMatrix)[3:4] <- c('TreatedCases' , 'ClinicalCases')

MakeCorrMatrix <- function(data){
  Corr <- ddply(corMatrix , .(District) ,
                function(x){
                  data.frame(corr = cor(x$TreatedCases , x$ClinicalCases))
                }
                )
  subset(Corr , !is.na(corr))  
}

CorrByDistrict <- MakeCorrMatrix(corMatrix)

nValues <- ddply(corMatrix , .(District) , 
                 function(x){
                   data.frame(Nobs = nrow(x))
                 }
                 )

CorrByDistrict <- merge(CorrByDistrict , nValues)


##ordering districts for plotting
CorrByDistrict$District <- factor(as.character(CorrByDistrict$District) , 
                                   levels = as.character(CorrByDistrict$District)[order(CorrByDistrict$corr)])

qplot(data = CorrByDistrict , x = District , y = corr , geom = 'bar' , stat = 'identity' , fill = Nobs) +
  coord_flip() + theme_bw() + ylab('Correlation between Clinical Malaria in 705 and Treated Malaria in 105') +
  scale_fill_gradient(low="grey" , high = "dark red")


## Looking at what these series look like

CorrByDistrict$LevCorr[CorrByDistrict$corr >= 0.7] <- "[0.7 , 1]"
CorrByDistrict$LevCorr[CorrByDistrict$corr < 0.7 & CorrByDistrict$corr >= 0.5] <- "[0.5 - 0.7["
CorrByDistrict$LevCorr[CorrByDistrict$corr < 0.5 & CorrByDistrict$corr >= 0] <- "[0 - 0.5["
CorrByDistrict$LevCorr[CorrByDistrict$corr < 0] <- "[-1 , 0]"

table(CorrByDistrict$LevCorr)

SummedCompile <- merge(rbind(Summed105 , Summed705) , CorrByDistrict , all.x = FALSE)

SummedCompile$District <- factor(as.character(SummedCompile$District) , 
                                levels = as.character(SummedCompile$District)[order(SummedCompile$corr)])

ggplot(data = SummedCompile,aes(x=CalMonth, y = totalCases)) + 
  geom_rect(data = SummedCompile ,aes(fill = LevCorr),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.3) +
  geom_line(aes(linetype = Indicator)) + 
  facet_wrap( ~ District , scales = 'free_y') + 
  scale_fill_brewer(palette="Greens")
