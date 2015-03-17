library(rgdal)
library(plyr)

zones@data$id = rownames(zones@data)
zones.points = fortify(zones , region="id")
zones.df = join(zones.points, zones@data, by="id")

zones.df <- merge(zones.df , pfpr , by = 'districts' , all.x = TRUE) 
ggplot(zones.df)+ 
  aes(long,lat,group=group , fill = pfpr) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient()


ggplot(zones.df)+ 
  aes(long,lat,group=group , fill = ghapfprpop) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient()


ggplot(zones.df)+ 
  aes(long,lat,group=group , fill = population) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient()



zones.df <- merge(zones.df , yearly , by.x = 'districts' , by.y = 'county_ID')
ggplot(zones.df)+ 
  aes(long,lat,group=group , fill = population) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient() + 
  facet_wrap(~year)

###

zones.df$diag_coverage <- zones.df$V1 / zones.df$ghapfprpop

## Take Out Outliers
zones.df$diag_coverage[zones.df$diag_coverage > 1] <- 1

plot0 <- ggplot(zones.df)+ 
  aes(long,lat,group=group , fill = pfpr) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient(name="Pfpr") + 
  ggtitle(" Plasmodium falciparum parasite rate \n at County Level") +
  theme() + xlab('') + ylab('')


plot1 <- ggplot(zones.df[zones.df$year == 2014 , ])+ 
  aes(long,lat,group=group , fill = V1) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient(low="red" , high = 'green' , 
                      name="Confirmed \n Malaria cases") + 
  ggtitle("Monthly number of confirmed malaria cases \n at County Level  (2014)") +
  theme() + xlab('') + ylab('')

plot2 <- ggplot(zones.df[zones.df$year == 2014 , ])+ 
  aes(long,lat,group=group , fill = ghapfprpop) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient(low="red" , high = 'green' , 
                      name="Number of \n Malaria cases") + 
  ggtitle("Number of malaria cases \n at County Level") +
  theme() + xlab('') + ylab('')

plot3 <- ggplot(zones.df[zones.df$year == 2014 , ])+ 
  aes(long,lat,group=group , fill = diag_coverage*100) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient(low="red" , high = 'green'  , 
                      name="% of confirmed \n malaria case")+ 
  ggtitle("% of confirmed malaria case \n at County Level  (2014)")  + 
  xlab('') + ylab('')


library("gridExtra")
grid.arrange(plot0 , plot2 , plot1 , plot3 , ncol = 2)
