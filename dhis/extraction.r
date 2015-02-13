library("dhisextractr")
library(RCurl)
library(XML)
library(plyr)

extracted_content <- extract_dhis_content(base_url = 'https://hiskenya.org' , 
                                          userID = 'grlurton' , password = 'Glurton29')

time_deb <- Sys.time()

extracted_kenya <- extract_all_data(base_url = 'https://hiskenya.org' , userID = 'grlurton' , 
                                    password = "Glurton29" ,
                                    deb_period = '2009-01-01' , end_period = '2015-01-01' ,
                                    org_units = extracted_content[[4]] ,
                                    data_sets = extracted_content[[1]] )

time_end <- Sys.time()


write.shapefile(out[[2]], '../../../Desktop/ken', arcgis=T)
Shapefile <- readShapePoly('../../../Desktop/ken.shp')




plot(Shapefile , col = 'grey')
plot(out[[1]] , add = TRUE)
