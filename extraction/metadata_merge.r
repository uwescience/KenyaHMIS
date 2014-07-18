#### merges metadata profiles from different reports and merges them with corresponding datasets
#### output are the total datasets with all data and metadata

#### This part was originally made in sqlshare but ended up with problems in the 105. 
#### If solution found, may be moved back

## Author : Grégoire Lurton
## Date   : July 2014

setwd("J:/Project/abce/ken/HMIS/data")

data105 <- read.csv('105Data.csv' , sep = "\t")
data705A <- read.csv('705AData.csv' , sep = "\t")
data705B <- read.csv('705BData.csv' , sep = "\t")
data710 <- read.csv('710Data.csv' , sep = "\t")

WindowsMeta <- read.csv('WindowsMetadata.csv')

data105 <- subset(merge(data105, WindowsMeta , by = "Path") , 
                  select = c(Path , Indicator , District , Level , Month , yearCorrect , Value ))
colnames(data105)[6] <- "Year"
data105$Year[data105$Month %in% c("")]

data705A <- subset(merge(data705A, WindowsMeta , by = "Path") , 
                  select = c(Path , Indicator , District , Month , yearCorrect , Value ))
colnames(data705A)[5] <- "Year"
data705B <- subset(merge(data705B, WindowsMeta , by = "Path") , 
                   select = c(Path , Indicator , District , Month , yearCorrect , Value ))
colnames(data705B)[5] <- "Year"
data710 <- subset(merge(data710, WindowsMeta , by = "Path") , 
                   select = c(Path , Sheet , Indicator1 , Indicator2 , 
                              District , Month , yearCorrect , Value ))
colnames(data710)[7] <- "Year"

write.table(data105  , '105Data.csv' , sep = "\t" , row.names = FALSE)
write.table(data705A  , '705AData.csv' , sep = "\t" , row.names = FALSE)
write.csv(data705B  , '705BData.csv' , sep = "\t" , row.names = FALSE)
write.csv(data710  , '710Data.csv' , sep = "\t" , row.names = FALSE)