#### This program looks for all the indicators found in reports 710 and unifies the values
#### It outputs a dictionnary for the indicators to be used for analysis 

## Author : Grégoire Lurton
## Date   : July 2014

setwd("J:/Project/abce/ken/HMIS/data")

library(stringr)
library(plyr)

Data710 <- read.csv("710Data.csv" , sep = '\t' , header = TRUE , row.names = NULL)

indicators <- ddply(Data710 , .(Indicator1) , 
                    function(x){
                      data.frame(unique(as.character(x$Indicator2)))
                    }
                    )

colnames(indicators) <- c('Indicator1' , 'Indicator2')

length(unique(indicators$Indicator1))
indicators$new <- str_trim(indicators$Indicator1)
indicators$new <- str_replace_all(indicators$new, "  ", " ")
indicators$new  <- tolower(indicators$new)
length(unique(indicators$new))

indicators$new[str_replace_all(indicators$new, " ", "") == 'pneumococal1'] <- "pneumococcal 1" 
indicators$new[str_replace_all(indicators$new, " ", "") == 'pneumococal2'] <- "pneumococcal 2"
indicators$new[str_replace_all(indicators$new, " ", "") == 'pneumococal3'] <- "pneumococcal 3"
indicators$new[indicators$new %in% c("pneumococal" , "pneumococcal")] <- "pneumococcal"


VitASuppl <- c("vitamin a (supplemental)" , "vitamin a (supplementation)" , "vitamin a supplemental)")
indicators$new[indicators$new %in% VitASuppl] <- 'Vitamin A Supplement'

TriVal1 <- c("dpt/hep b/ hib1" ,  "dpt/hep+hib1")
indicators$new[indicators$new %in% TriVal1] <- 'dpt+hepb+hib1'
TriVal2 <- c("dpt/hep b/ hib2" ,  "dpt/hep+hib2")
indicators$new[indicators$new %in% TriVal2] <- 'dpt+hepb+hib2'
TriVal3 <- c("dpt/hep b/ hib3" ,  "dpt/hep+hib3")
indicators$new[indicators$new %in% TriVal3] <- 'dpt+hepb+hib3'
TriVal <- c("dpt-hib- hep b",  "dpt+hib+hep b" )
indicators$new[indicators$new %in% TriVal] <- 'dpt+hepb+hib'

fic <- c("fully immunized child (fic)" ,"fully immunized children (fic)"  )
indicators$new[indicators$new %in% fic] <- 'fully immunized children'

opv0 <- c("opv 0"  , "opv(birth dose)" )
indicators$new[indicators$new %in% opv0] <- 'opv0'

tetantrauma <- c("tetanus toxoid for                          trauma" , "tetanus toxoid for trauma" ,"t.t .for trauma"  )
indicators$new[indicators$new %in% tetantrauma] <- "tetanus toxoid for trauma" 

vitATher <- c("(therapeutic)"   , "vitamin a" , "vitamin a (therapeutic)" , "vitamin a (theraputic)")
indicators$new[indicators$new %in% vitATher] <- "Vitamin A therapeutic" 

vitA100 <- c( "vitamin a-100,000 iu" , "vitamin a 100,000 iu" )
indicators$new[indicators$new %in% vitA100] <- "vitamin a 100,000 iu" 
length(unique(indicators$new))


length(unique(indicators$Indicator2))
indicators$new2 <- str_trim(indicators$Indicator2)
indicators$new2 <- str_replace_all(indicators$new2, "  ", " ")
indicators$new2  <- tolower(indicators$new2)
length(unique(indicators$new2))

dcapRem <- c("[d]caps remaining at the end of the month"  ,  "[d]caps remainingat the end of the month")
indicators$new2[indicators$new2 %in% dcapRem] <- "[d]caps remaining at the end of the month"

fcapUse <- c("[f]number of caps used during the month"  , "[f]number of caps during the month")
indicators$new2[indicators$new2 %in% fcapUse] <- "[f]number of caps during the month"

fstdose <- c("1.0" , "1st dose")
indicators$new2[indicators$new2 %in% fstdose] <- "1st dose"
snddose <- c("2.0"  , "2nd dose" )
indicators$new2[indicators$new2 %in% snddose] <- "2nd dose" 
trddose <- c("3.0"  , "3d dose" , "3rd dose"  )
indicators$new2[indicators$new2 %in% trddose] <- "3rd dose"  
fthdose <- c("4.0"  , "4th dose"  )
indicators$new2[indicators$new2 %in% fthdose] <- "4th dose"  
ffthdose <- c("5.0"  , "5th dose"  )
indicators$new2[indicators$new2 %in% ffthdose] <- "5th dose"  

lss1yr100 <- c("6-11 months (100,000 iu)"  ,  "6-11 months (100,000iu)" , "6-12 months (100,000iu)","6 months to 12 months (100,000 iu)")
indicators$new2[indicators$new2 %in% lss1yr100] <- "6-11 months (100,000 iu)" 

ab1yr200 <- c("above 1 year (200,000 iu)" , "above 1 year (200,000iu)" , "above 1yr (200,000iu)")
indicators$new2[indicators$new2 %in% ab1yr200] <- "above 1 year (200,000 iu)"

ab1yrmin5200 <- c("12-59 months (200,000iu)"  , "12-59 months (200,000 iu)" )
indicators$new2[indicators$new2 %in% ab1yrmin5200] <- "12-59 months (200,000 iu)" 

adultTb <- c("adult ( tb, cc )" , "adult (e.g from tb clinic, ccc)" , "adult (e.g. from tb clinic, ccc)" , "adult (eg from tb clinic, ccc )")
indicators$new2[indicators$new2 %in% adultTb] <- "adult (tb, ccc)" 

fullProtect <- c("fully procted"  , "fully protected")
indicators$new2[indicators$new2 %in% fullProtect] <- "fully protected" 

measles <- c('measle' , 'measles')
indicators$new2[indicators$new2 %in% measles] <- "measles" 

subset(indicators , new2 %in% c("birth dose (within 2 weeks)" , "within 2 weeks" ))
birthDose <- c("birth dose (within 2 weeks)" , "within 2 weeks")
indicators$new2[indicators$new2 %in% birthDose] <- "birth dose" 

subset(indicators , new2 %in% c("above 1 year"  , "above 1 year (200,000 iu)" ) , select = c(new , new2))
##The two are not equivalent

subset(indicators , new2 %in% c("lactating mothers"  ,  "lactating mothers (200,000iu)"  ) , select = c(new , new2))
lactMoth <- c("lactating mothers"  ,  "lactating mothers (200,000iu)"  )
indicators$new2[indicators$new2 %in% lactMoth] <- "lactating mothers" 

subset(indicators , new2 %in% c("opv"  ,  "opv for 30 days") , select = c(new , new2))
opv <- c("opv"  ,  "opv for 30 days"  )
indicators$new2[indicators$new2 %in% opv] <- "opv" 

subset(indicators , new2 %in% c("yellow fever"  ,  "yellow fever 30 days") , select = c(new , new2))
yfever <- c("yellow fever"  ,  "yellow fever 30 days")
indicators$new2[indicators$new2 %in% yfever] <- "yellow fever" 

subset(indicators , new2 %in% c("under 1 year (100,000 iu)" ,  "under 1 year"   ) , select = c(new , new2))
#The two are not equivalent

subset(indicators , new2 %in% c( "12-59 months (200,000 iu)",  "above 1 year (200,000 iu)"  ) , select = c(new , new2))
Ab1Yr200 <- c( "12-59 months (200,000 iu)",  "above 1 year (200,000 iu)"  )
indicators$new2[indicators$new2 %in% Ab1Yr200] <- "above 1 year (200,000 iu)" 

####Now checking for empty indicator1
length(unique(indicators$new2))
table(indicators$new , indicators$new2)
sort(unique(indicators$new2))

out <- subset(indicators , select = c(Indicator1 , Indicator2 , new , new2))
colnames(out) <- c('Indicator1Orig' , 'Indicator2Orig' , 'Indicator1New' , 'Indicator2New')

write.csv(out , 'DicoIndicators710.csv', row.names = FALSE)