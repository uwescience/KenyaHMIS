



pal <- colorRampPalette(c('white' , 'lightblue', 'darkred'))

par(mfrow = c(1,2))
plot(MapPfpr , col = pal(50)[as.numeric(cut(MapPfpr$pfpr,breaks = 50))])
title(main = 'Distribution of pfpr')
plot(MapPfpr , col = pal(50)[cut(MapPfpr$ghapfprpop,breaks = 50)])
title(main = 'Nbr Malaria infected people')
par(mfrow = c(1,1))


MalariaCases <- fetch.data.frame('select District , Value , Year , Month 
                                 FROM [grlurton@washington.edu].[705BDataComplete]
                                 WHERE Indicator=\'confirmed malaria\' AND isnumeric(Value) = 1;')

#MalariaCases <- CleanMonths(MalariaCases)
MalariaCases$Month <- as.yearmon(paste(MalariaCases$Month , MalariaCases$Year  , sep = '-') , "%B-%Y")

MalariaStat <- ddply(MalariaCases , .(District) , function(x){data.frame(mean  = mean(x$Value) , median = median(x$Value))})
CountCases <- ddply(MalariaCases , .(District , Month) , function(x){data.frame(sum  = sum(x$Value , na.rm = TRUE))})

MalariaStat$District <- formatNames(MalariaStat$District)
CountCases$District <- formatNames(CountCases$District)

DataPlot <- merge(MalariaStat, pfpr , all.y = FALSE , by.x = 'District' , by.y = 'District')
SumDataPlot <- merge(CountCases, pfpr , all.y = FALSE , by.x = 'District' , by.y = 'District')

pdf("pfprdef.pdf", width=14.85, height=10.5)
ggplot(data = DataPlot ,
       aes(x = pfpr , y = 1000 * 12 * mean /(population)  , 
           #size= population  ,
           col = pfpr , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="blue" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate') 

ggplot(data = SumDataPlot ,
       aes(x = pfpr , y = 1000 * 12 * sum /(population)   , label = District)) + 
  # geom_text() +
  geom_point() + geom_smooth(method="rlm") +
  theme_bw() + #scale_colour_gradient(low="blue" , high = "red") +
  xlab('Pfpr') + ylab('Incidence rate') 

ggplot(data = DataPlot ,
       aes(x = ghapfprpop , y = mean  , size= population  ,col = pfpr , label = District)) + 
  geom_text() +
  theme_bw() + scale_colour_gradient(low="blue" , high = "red")


qplot(data = DataPlot ,
      x = ghapfprpop , y = median ,  
      size = 2) +
  xlab('pfpr * Population') + ylab('Median of confirmed Malaria Cases') +
  theme_bw() + scale_colour_brewer(palette="Set1")
dev.off()