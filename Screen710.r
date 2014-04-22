######## Research and importation of reports 710 - Immunization Summary
#######################################################################

#### Load Previous work

windowsMeta <- read.csv("C:/Users/grlurton/Documents/KenyaHIS/python/WindowsMetadata.csv" , header = FALSE)
colnames(windowsMeta) <-  c("Path" , "Author" , "Modifier" , "DateCreated" , "DateSaved")
windowsMeta$Path <- gsub( pattern = ":/" , replacement = "://" , x = windowsMeta$Path)

TotalList <- read.csv("TotalList.csv")

Metadata <- read.csv("MetadataTrace.csv")

ListFiles <- merge(windowsMeta , TotalList)

#### Get important parameters

ReportImportType <- "710 - Immunization Summary"

AuthorFile <- as.character(Metadata$Author[Metadata$ReportType ==  ReportType])
DateCreationFile <- as.character(Metadata$DateCreation[Metadata$ReportType ==  ReportType])

#### Identifty those who don't have the right metadata pattern

NameNoMeta <- subset(ListFiles  , (Author !=  AuthorFile | DateCreated != DateCreationFile) &
                   ReportType == ReportImportType)

#### Test them with report structure

#### Identify those with proper metadata pattern but not proper name

MetaNoName <- subset(ListFiles  , Author ==  AuthorFile & DateCreated == DateCreationFile &
                       ReportType != ReportImportType)

#### Test them with report structure

#### 