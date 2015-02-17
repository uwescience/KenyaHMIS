library(dhisextractr)
library(RCurl)
library(XML)
library(plyr)
library(maptools)
library(shapefiles)

setwd('J://Project/dhis/kenya/extracted_data')

extracted_content <- extract_dhis_content(base_url = 'https://hiskenya.org' , 
                                          userID = 'grlurton' , password = 'Glurton29')

data_sets <- extracted_content[[1]]
data_elements <- extracted_content[[2]]
data_categories <- extracted_content[[3]]
org_units <- extracted_content[[4]]
org_units_description <- extracted_content[[5]]
org_units_groups <- extracted_content[[6]]
org_units_data_sets <- extracted_content[[7]]

time_deb <- Sys.time()

extracted_kenya <- extract_all_data(base_url = 'https://hiskenya.org' , userID = 'grlurton' , 
                                    password = "Glurton29" ,
                                    deb_period = '2009-01-01' , end_period = '2015-01-01' ,
                                    org_units = org_units ,
                                    data_sets = data_sets )
time_end <- Sys.time()

### Writing Output ###

write.csv(data_sets , 'data_sets.csv')
write.csv(data_elements , 'data_elements.csv')
write.csv(data_categories , 'data_categories.csv')
write.csv(org_units , 'org_units.csv')
write.csv(org_units_data_sets , 'org_units_data_sets.csv')
write.csv(org_units_groups, 'org_units_groups.csv')
write.csv(org_units_description, 'org_units_description.csv')

write.csv(extracted_kenya , 'data_kenya.csv')

save.image('kenya_initial_extraction.rdata')

### Making shapefiles ###

shapefiles <- extract_geolocalisation(org_units_description)

write.shapefile(shapefiles[[1]], 'map_points', arcgis=T)
write.shapefile(shapefiles[[2]], 'map_polygons', arcgis=T)
