library(sqlshare)
library(ggplot2)
library(plyr)

Data710Complete <- fetch.data.frame("select * from [grlurton@washington.edu].[Rep710Total]")
Data710Incoherent <- fetch.data.frame("select * from [grlurton@washington.edu].[TbIncoherent710Saves]")
Data705AComplete <- fetch.data.frame("select * from [grlurton@washington.edu].[Rep705ATotal]")
Data705BComplete <- fetch.data.frame("select * from [grlurton@washington.edu].[Rep705BTotal]")

##Checking that files with incoherent save dates are files with non important information

##Do they have more missing values ?

PathIncoherent <- unique(Data710Incoherent$Path)

distInco <- table(is.na(Data710Incoherent$Value))
distNorm <- table(is.na(Data710Complete$Value[!(Data710Complete$Path %in% PathIncoherent)]))

prop.test(c(distInco[2] , distNorm[2]) , c(sum(distInco) , sum(distNorm)) )

##Yes

##Do they have values that are close from other values (ie pure copy paste)

## Do we have outliers somewhere

CheckOutlier <- function(data , indicator1 , indicator2 , district){
  data  <- subset(data , Indicator1 == indicator1 & Indicator2 == indicator2  & District == district & !is.na(data$Value))
  data$Value <- as.numeric(data$Value)
  print(nrow(data))
  mean  <- mean( data$Value , na.rm=TRUE)
  sd    <- sd(data$Value)
  out <- data$Value > mean + 3*sd | data$Value < mean - 3*sd 
  print(mean)
  print(table(out))
  data[out , ]
}


CheckOutlier(Data710Complete , 'measles' , 'under 1 year' , 'Athi River' )
CheckOutlier(Data710Complete , 'measles' , 'above 1 year' , 'Athi River' )

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

CreateDate <- function(x){
  x$date <- as.Date(paste(28,substr(x$Month , 1 ,3) , x$Year  , sep = '-') ,'%d-%b-%Y')
  x
}

CreateTimeVariables <- function(x){
  CreateDate(CleanMonths(x))
}


Data710Complete   <- CreateTimeVariables(Data710Complete)
Data705AComplete  <- CreateTimeVariables(Data705AComplete)
Data705BComplete  <- CreateTimeVariables(Data705BComplete)

dataPlot <- subset(Data710Complete , !is.na(Value))
dataPlot$District <- as.character(dataPlot$District)

qplot(data = dataPlot[dataPlot$Indicator1 == 'measles' & dataPlot$Indicator2 %in% c('under 1 year' , 'above 1 year') ,] , 
      x = date ,  y = Value , geom = 'line' , col = Indicator2) +
  facet_wrap(~District ,  scales = 'free_y') +
  theme_bw() + scale_colour_brewer(palette="Set1") +
  scale_x_date(breaks = date_breaks("1 year") , 
               labels = date_format("%y") ,
               minor_breaks = "3 months") +
  xlab('Date') + ylab('Number of Measles Vaccinations') + labs(col = 'Age group')

qplot(data = dataPlot[dataPlot$Indicator1 == 'Vitamin A Supplement',] , 
      x = date ,  y = Value , geom = 'line' , col = Indicator2) +
  facet_wrap(~District ,  scales = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  scale_x_date(breaks = date_breaks("1 year") , 
               labels = date_format("%y") ,
               minor_breaks = "3 months") +
  xlab('Date') + ylab('Number Vitamin A Supplementation') + labs(col = 'Age group')


qplot(data = dataPlot[(dataPlot$Indicator1 == 'measles' | dataPlot$Indicator1 == 'opv1' | dataPlot$Indicator1 == 'opv0') &
                        dataPlot$Indicator2 %in% c('under 1 year'),] , 
      x = date , y = Value , col = Indicator1 , geom = 'line')+
  facet_wrap(~District , scales = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  scale_x_date(breaks = date_breaks("1 year") , 
               labels = date_format("%y") ,
               minor_breaks = "3 months") +
  xlab('Year') + ylab('Number of <1 children vaccined') + labs(col = 'Vaccine')

qplot(data = dataPlot[(dataPlot$Indicator1 == 'measles' | dataPlot$Indicator1 == 'dpt+hepb+hib1' | 
                         dataPlot$Indicator1 == 'dpt+hepb+hib2' | dataPlot$Indicator1 == 'dpt+hepb+hib3') &
                        dataPlot$Indicator2 %in% c('under 1 year'),] , 
      x = date , y = Value , col = Indicator1 , geom = 'line')+
  facet_wrap(~District , scales = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  scale_x_date(breaks = date_breaks("1 year") , 
               labels = date_format("%y") ,
               minor_breaks = "3 months") +
  xlab('Year') + ylab('Number of <1 children vaccined') + labs(col = 'Vaccine')


##Logistics

qplot(data = dataPlot[dataPlot$Indicator1 == 'measles' & 
                        dataPlot$Indicator2 %in% c('[b]dose received within the month' , 'under 1 year'),] ,
      x = date , y = Value , col = Indicator2 , geom = 'line' ) +
  facet_wrap(~District , scales = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  scale_x_date(breaks = date_breaks("1 year") , 
               labels = date_format("%y") ,
               minor_breaks = "3 months") +
  xlab('Year') + ylab('Number of <1 children vaccined & Doses') + labs(col = 'Indicator')


##Plotting some 705A

data705APlot <- subset(Data705AComplete , !is.na(Value))

qplot(data = data705APlot[data705APlot$Indicator == 'Measles' , ] , x = date , y = Value , geom = 'line') +
  facet_wrap(~District , scale = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  scale_x_date(breaks = date_breaks("1 year") , 
               labels = date_format("%y") ,
               minor_breaks = "3 months") +
  xlab('Year') + ylab('Diagnosed Measles Cases')


qplot(data = data705APlot[data705APlot$Indicator ==  'Confirmed Malaria', ] , 
      x = Month , y = Value , col = Year) +
  facet_wrap(~District , scale = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  xlab('Month') + ylab('Confirmed Malaria Cases') +
  scale_x_discrete(labels = substr(unique(sort(data705APlot$Month)) , 1 , 1))


###Recuperation des donnees sur la malaria

getConfirmedMalariaData <- function(x){
  subset(x , Indicator ==  'Confirmed Malaria' )
}

MalariaDataChildren <- getConfirmedMalariaData(data705APlot)
MalariaDataAdults <- getConfirmedMalariaData(data705BPlot)

MalariaTotal <- data.frame(rbind(MalariaDataChildren , MalariaDataAdults) ,
                           AgeGroup = c(rep('Children' , nrow(MalariaDataChildren)) ,
                                        rep('Adults' , nrow(MalariaDataAdults))))

ggplot(data = MalariaTotal, aes(x = Month , y = Value , group = AgeGroup , col = Year , shape = AgeGroup)) +
  geom_point() +
  facet_wrap(~District , scale = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  xlab('Month') + ylab('Confirmed Malaria Cases')+
  scale_x_discrete(labels = substr(unique(sort(data705APlot$Month)) , 1 , 1))
ggsave(filename = 'MalariaCasesRaw.pdf', width= 29.7 , height= 21)

ggplot(data = MalariaTotal, aes(x = Month , y = Value , group = AgeGroup , col = AgeGroup) ) + stat_smooth() +
  facet_wrap(~District , scale = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  xlab('Month') + ylab('Confirmed Malaria Cases')+
  scale_x_discrete(labels = substr(unique(sort(data705APlot$Month)) , 1 , 1))
ggsave(filename = 'MalariaCasesSmoothed.pdf', width= 29.7 , height= 21)

ggplot(data = MalariaTotal, aes(x = date , y = Value , group = AgeGroup , col = AgeGroup) ) + 
  geom_line() + geom_smooth(col = 'black') +
  facet_wrap(~District , scale = 'free_y')+
  theme_bw() + scale_colour_brewer(palette="Set1") +
  xlab('Date') + ylab('Confirmed Malaria Cases')
ggsave(filename = 'MalariaCasesTrend.pdf', width= 29.7 , height= 21)


table(MalariaTotal$Value)

NatMeasles <- ddply(subset(data705APlot , Indicator == 'Measles') , .(date) , function(x) data.frame(Cases = sum(x$Value) , Reports= nrow(x) , nmoy  = sum(x$Value) / nrow(x)))
qplot(data = NatMeasles , x = as.Date(date) , y = Cases , geom = 'histogram' , stat = 'identity' ,
      fill = Reports) +
  theme_bw() + 
  ylim(c(0 , 550)) + 
  scale_fill_gradient(low="white" , high = 'red') + labs(fill = 'N Reports') +
  scale_x_date(breaks = date_breaks('1 year') , 
               labels= date_format('%Y')) +
  xlab('Date') + ylab('Number of reported measles cases')


qplot(data = NatMeasles , x = as.Date(date) , y = nmoy , geom = 'histogram' , stat = 'identity' ,
      fill = Reports) +
  theme_bw() + 
  ylim(c(0 , 10)) + 
  scale_fill_gradient(low="white" , high = 'red') + labs(fill = 'N Reports') +
  scale_x_date(breaks = date_breaks('1 year') , 
               labels= date_format('%Y')) +
  xlab('Date') + ylab('Mean number of reported measles cases')


