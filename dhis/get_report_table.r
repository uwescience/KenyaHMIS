library(RCurl)
library(XML)
library(ggplot2)


url<-"https://hiskenya.org/api/reportTables/iFgWOvKg7sW/data.csv"

response<-getURL(url,userpwd="grlurton:Glurton29",httpauth = 1L, header=FALSE,ssl.verifypeer = FALSE)

data<-noquote(response)
mydata<-read.table(textConnection(data),sep=",",header=T)


head(mydata)

qplot(data = mydata , x = Period.ID , y = Confirmed.Malaria)
