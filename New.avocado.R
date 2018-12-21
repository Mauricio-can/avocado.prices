
#using avocado data for the past 10 years to visualize any trends 
data = read.csv("~/Downloads/Avocado.data.csv")
data$Total.Bags[data$Total.Bags == "0"] <- NA


#seperating data into different years to do individual analysis' of each year

avo.2018 = data[data$Year=="2018",]


avo.2017 = data[data$Year=="2017",]

avo.2016 = data[data$Year=="2016",]

avo.2015 = data[data$Year=="2015",]

avo.2014 = data[data$Year=="2014",]

avo.2013 = data[data$Year=="2013",]

avo.2012 = data[data$Year=="2012",]

avo.2011 = data[data$Year=="2011",]

avo.2010 = data[data$Year=="2010",]

avo.2009 = data[data$Year=="2009",]

avo.2008 = data[data$Year=="2008",]

#taking the averages of the entire year 

avo.2018.avg.price = mean(avo.2018$AveragePrice)
avo.2018.avg.vol = mean(avo.2018$Total.Volume)
avo.2018.max.price = max(avo.2018$AveragePrice)

avo.2017.avg.price = mean(avo.2017$AveragePrice)
avo.2017.avg.vol = mean(avo.2017$Total.Volume)
avo.2017.max.price = max(avo.2017$AveragePrice)

avo.2016.avg.price = mean(avo.2016$AveragePrice)
avo.2016.avg.vol = mean(avo.2016$Total.Volume)
avo.2016.max.price = max(avo.2016$AveragePrice)

avo.2015.avg.price = mean(avo.2015$AveragePrice)
avo.2015.avg.vol = mean(avo.2015$Total.Volume)
avo.2015.max.price = max(avo.2015$AveragePrice)

avo.2014.avg.price = mean(avo.2014$AveragePrice)
avo.2014.avg.vol = mean(avo.2014$Total.Volume)
avo.2014.max.price = max(avo.2014$AveragePrice)

avo.2013.avg.price = mean(avo.2013$AveragePrice)
avo.2013.avg.vol = mean(avo.2013$Total.Volume)
avo.2013.max.price = max(avo.2013$AveragePrice)

avo.2012.avg.price = mean(avo.2012$AveragePrice)
avo.2012.avg.vol = mean(avo.2012$Total.Volume)
avo.2012.max.price = max(avo.2012$AveragePrice)

avo.2011.avg.price = mean(avo.2011$AveragePrice)
avo.2011.avg.vol = mean(avo.2011$Total.Volume)
avo.2011.max.price = max(avo.2011$AveragePrice)

avo.2010.avg.price = mean(avo.2010$AveragePrice)
avo.2010.avg.vol = mean(avo.2010$Total.Volume)
avo.2010.max.price = max(avo.2010$AveragePrice)

avo.2009.avg.price = mean(avo.2009$AveragePrice)
avo.2009.avg.vol = mean(avo.2009$Total.Volume)
avo.2009.max.price = max(avo.2009$AveragePrice)


avo.2008.avg.price = mean(avo.2008$AveragePrice)
avo.2008.avg.vol = mean(avo.2008$Total.Volume)
avo.2008.max.price = max(avo.2008$AveragePrice)

#creating a new data frame for the new avg data
avo.data = data.frame("year" = c(2018:2008), "avg.price" = c(avo.2018.avg.price,avo.2017.avg.price,avo.2016.avg.price,avo.2015.avg.price,avo.2014.avg.price,avo.2013.avg.price,avo.2012.avg.price,avo.2011.avg.price,avo.2010.avg.price,avo.2009.avg.price,avo.2008.avg.price))

avo.data$maxprice = c(avo.2018.max.price,avo.2017.max.price,avo.2016.max.price,avo.2015.max.price,avo.2014.max.price,avo.2013.max.price,avo.2012.max.price, avo.2011.max.price, avo.2010.max.price,avo.2009.max.price,avo.2008.max.price)
avo.data$avg.volume = c(avo.2018.avg.vol,avo.2017.avg.vol,avo.2016.avg.vol,avo.2015.avg.vol,avo.2014.avg.vol,avo.2013.avg.vol,avo.2012.avg.vol,avo.2011.avg.vol,avo.2010.avg.vol,avo.2009.avg.vol, avo.2008.avg.vol)
avo.data$max.month = c(9,10,11,8,7,10,6,8,12,7,8)



#graphing the prices

library(ggplot2)

ggplot(avo.data, aes('year')) + 
  geom_line(aes(y = 'maxprice', colour = "maxprice")) + 
  geom_line(aes(y = 'avg.price', colour = "avg.price"))
require(lattice)
avo.graoh = xyplot(avg.price + maxprice  ~ year, data=avo.data, type = c('l','l'), col = c("blue", "red"), auto.key = TRUE, main = "U.S. Avocado Prices Over 10 Years")
avo.graoh

#tried to see any correlation between month and parameters

model2 = lm(Month ~ Total.Volume + Total.Bags , data=data)
summary(model2)




