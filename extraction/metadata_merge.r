#### This program merges metadata profiles from different reports and merges them 
#### output are the total datasets with all data and metadata with corresponding datasets
#### It also cleans and standardizes month names and district names

#### This part was originally made in sqlshare but ended up with problems in the 105 because of reporting period 
#### If solution found, may be moved back

## Author : Grégoire Lurton
## Date   : July 2014

library(stringr)

setwd("J:/Project/abce/ken/HMIS/data")

data105 <- read.csv('105Data.csv' , sep = "\t")
data705A <- read.csv('705AData.csv' , sep = "\t")
data705B <- read.csv('705BData.csv' , sep = "\t")
data710 <- read.csv('710Data.csv' , sep = "\t")

WindowsMeta <- read.csv('WindowsMetadata.csv')

###Functions for standardizing months names and district names

formatNames <- function(x){
  tolower(str_trim(as.character(x)))
}

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

PrepareData <- function(x){
  x <- CleanMonths(x)
  x$District <- formatNames(x$District)
  x 
}

### integration of metadata + cleaning of data

data105 <- PrepareData(subset(merge(data105, WindowsMeta , by = "Path") , 
                  select = c(Path , Indicator , District , Level , Month , yearCorrect , Value )))
colnames(data105)[6] <- "Year"

##Changing year number for second half of year for 105 report because they are made from july to june
data105$Year <- as.numeric(as.character(data105$Year))
data105$Year[data105$Month %in% 
               c("july" , "august" , "september" , "october" , "november" , "december")] <-
  data105$Year[data105$Month %in% 
                 c("july" , "august" , "september" , "october" , "november" , "december")] - 1
print("105 clean")

data705A <- PrepareData(subset(merge(data705A, WindowsMeta , by = "Path") , 
                  select = c(Path , Indicator , District , Month , yearCorrect , Value )))
colnames(data705A)[5] <- "Year"
print('705A clean')


data705B <- PrepareData(subset(merge(data705B, WindowsMeta , by = "Path") , 
                   select = c(Path , Indicator , District , Month , yearCorrect , Value )))
colnames(data705B)[5] <- "Year"
print('705B clean')

data710 <- PrepareData(subset(merge(data710, WindowsMeta , by = "Path") , 
                   select = c(Path , Sheet , Indicator1 , Indicator2 , 
                              District , Month , yearCorrect , Value )))
colnames(data710)[7] <- "Year"
print('710 clean')

print('Saving Clean files')
print('Saving 105')
write.table(data105  , '105Data.csv' , sep = "\t" , row.names = FALSE , quote = FALSE)
print('Saving 705A')
write.table(data705A  , '705AData.csv' , sep = "\t" , row.names = FALSE , quote = FALSE)
print('Saving 705B')
write.table(data705B  , '705BData.csv' , sep = "\t" , row.names = FALSE , quote = FALSE)
print('Saving 710')
write.table(data710  , '710Data.csv' , sep = "\t" , row.names = FALSE , quote = FALSE)

q()