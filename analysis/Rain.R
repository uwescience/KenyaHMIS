coordinates(facilities) = ~Latitude+Longitude
palette(value=  brewer.pal(12,"Set3"))

##Rain Data
rain <- read.csv('Rainfall/RainKenya.txt' , sep = ',')
stations <- read.csv('Rainfall/StationsList.csv')
stations <- subset(stations , LON > -200000)
stations$LON <- stations$LON / 1000
stations$LAT <- stations$LAT / 1000
coordinates(stations) = ~ LON+LAT


##put the weather stations in the right district
stations$District <- over(stations , Kenya)
point.df=data.frame(stations)
KenStations <- subset(point.df , !is.na(DISTRICT))
coordinates(KenStations) = ~ LON+LAT
plot(Kenya)
points(facilities)
points(KenStations , col = 'red')


RainKenya <- base::merge(stations , rain , all.x = FALSE , all.y = TRUE , by.x = 'USAF' , by.y = 'STN...')
RainKenya$Date <- as.Date(as.character(RainKenya$YEARMODA) , "%Y%m%d")
RainKenya$PRCPt <- str_trim(as.character(RainKenya$PRCP))
RainKenya$Rain <- as.numeric(substr(as.character(RainKenya$PRCPt) , 1 , 4))
RainKenya$RainFlag <- substr(as.character(RainKenya$PRCPt) , 5 , 5)
RainKenya$Rain[RainKenya$Rain == 99.9] <- NA

##For now we drop the data with flags
RainKenya <- subset(RainKenya , RainFlag == 'G')
RainKenya$Rain[RainKenya$RainFlag != 'G'] <- NA


dtaPlot <- subset(RainKenya , !is.na(Date) & !is.na(TEMP))

qplot(data = RainKenya , x = Date , y = Rain , geom = 'line') +
  facet_wrap(~DISTRICT)

##Aggregate data monthly
library(zoo)
RainKenya$month <- as.yearmon(RainKenya$Date)
sumMonthRain <- ddply(RainKenya , .(DISTRICT , month) , function(x) sum(x$Rain , na.rm = TRUE))
meanMonthRain <- ddply(RainKenya , .(DISTRICT , month) , function(x) mean(x$Rain , na.rm = TRUE))

qplot(data = sumMonthRain , x = as.Date(month) , y = V1 , geom = 'line') +
  facet_wrap(~DISTRICT)

##Rain and Malaria
RainMalar <- merge(sumMonthRain , SumDataPlot , by.x = c('DISTRICT' , 'month') , by.y = c('District' , 'Month') , all = TRUE)

qplot(data = RainMalar , x = as.Date(month) , y = 1000*V1 , geom = 'line') +
  geom_line(data = RainMalar , aes(x = as.Date(month) , y = sum) , col = 'red') +
  facet_wrap(~DISTRICT , scales='free_y')
