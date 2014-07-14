library(RCurl)
library(XML)

kenya<-'https://hiskenya.org/api/organisationUnits/HfVjCurKxh2.xml'

pagesToRead <- c(kenya)
pagesRead <- c()
orgUnits <- data.frame(unitName =  character() , unitLevel =  character() ,
                       child_adress = character() , child_name = character())
count <- 0

##Function to extract children from a given page + adress of children
getChildren <- function(url){
  Page<-getURL(url,userpwd="grlurton:Glurton29",
               ssl.verifypeer = FALSE , httpauth = 1L)
  ParsedPage <- xmlParse(Page)
  root <- xmlRoot(ParsedPage)
  unitName <- xmlGetAttr(root , "name")
  unitLevel <- xmlGetAttr(root , "level")
  if(length(root[['children']]) > 0){
    child_adress <- xmlSApply(root[['children']] , xmlGetAttr , "href")
    child_name <- xmlSApply(root[['children']] , xmlGetAttr , "name")
  }
  else{
    child_adress <- child_name <- NA
  } 
  children <- data.frame(unitName , unitLevel , child_adress , child_name)
  children
}

while (length(pagesToRead) > 0){
  extractOrg <- data.frame(unitName =  character() , unitLevel =  character() ,
                           child_adress = character() , child_name = character())
  for (url in pagesToRead){
    print(url)
    out <- getChildren(url)
    extractOrg <- rbind(extractOrg , out)
  }
  child_adress <- paste(as.character(extractOrg$child_adress[!is.na(extractOrg$child_adress)]) ,
                       "xml" , sep = '.')
  pagesRead <- c(pagesRead , pagesToRead)
  count <- count + length(pagesRead)
  pagesToRead <- c(pagesToRead , child_adress)
  pagesToRead <- pagesToRead[!(pagesToRead %in% pagesRead )]
  print(paste("pages to Read" , length(pagesToRead) , sep = " "))
  print(paste("count =" , count , "; percent done" , count / 12664 , sep = " " ))
}