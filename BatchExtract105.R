require(gdata)

MonthId <- c("Jan" , "Feb" , "Mar" , "Apr" , "May" , "Jun" , "Jul" , "Aug" , "Sep" , "Oct" , "Nov" , "Dec")

source("Extract105.r")
source("Exclusions.R")

data105 <- data.frame(Indicators = character() , Values = numeric() , Month = character() , 
                      Level = character() , District = character() , Year = numeric() , IdFile = character()) 

List105 <- subset(TotalList , (ReportType == "105" | ReportType == "Service Delivery") & 
                    !(IdFile %in% exclusion))

###Introduire des numeros de fichiers pour tracer lesquels sont extractibles

i = 1
NbreFiles <- nrow(List105) - i + 1
timeStart <- Sys.time()
plot(x = seq(1,NbreFiles) , y = seq(1,800 , length.out= NbreFiles ) , col = "white")


while ( i < nrow(List105)){
  path <- List105$Path[i]
  district <- List105$District[List105$Path == path]
  year <-List105$Year[List105$Path == path]  
  IdFile <- List105$IdFile[List105$Path == path]
  dataExtract <- data.frame(Indicators = character() , Values = numeric() , Month = character() , 
                            Level = character()) 
  dataExtract <- Extract105(path , dataExtract)
  dataExtract$District <- rep(district , nrow(dataExtract) )
  dataExtract$Year <- rep(year , nrow(dataExtract) )
  dataExtract$IdFile <- rep(IdFile , nrow(dataExtract) ) 
  data105 <- rbind(data105 , dataExtract)

  timeLeft <- (NbreFiles - i)*difftime(Sys.time() , timeStart  , units = "mins") / (i)
  points(x = i , y = timeLeft , col = "black")
  
  i <- i + 1
}

badIndic <- unique(data105$Indicators)[tabulate(data105$Indicators) > 34000]

data105S <- subset(data105 , Indicators %in% badIndic)

write.table(data105S , "C://Users/grlurton/Google Drive/ABCE/KenyaHIS/data105.csv" , sep="\t")
