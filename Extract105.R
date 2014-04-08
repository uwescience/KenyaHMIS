Extract105 <- function(path , data105){
  sheetnames <- sheetNames(path , perl = "C:/Perl64/bin/perl5.18.2.exe")
  MonthSheetNames <- unique(grep(paste(MonthId,collapse="|"), sheetnames, value=TRUE , ignore.case = TRUE))
  for (sheet in MonthSheetNames){
    print(sheet)
    data <- read.xls(path , sheet=sheet , perl = "C:/Perl64/bin/perl5.18.2.exe" , 
                     header = FALSE , na.strings = c("NA" , "#DIV/0!" , ""))
    n <- nrow(data)
    if (data[1,1] == "No of Expected reports in the district" & !is.na(data[1,1])){
      variables <- data[7:n,2]
      for(column in 3:7){
        columData <- data[7:n,column]
        month <- rep(sheet , length(columData))
        level <- rep(data[5,column] , length(columData))
        dataMonth <- data.frame(Indicators = variables , Values = columData , Month = month , Level = level )
        data105 <- rbind(data105 , dataMonth)
      }
    }
  }
  data105
}