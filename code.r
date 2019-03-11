# import data set
data <- read.csv("C:/Users/HP/Desktop/Dojo Task/Data.csv",header = TRUE,sep = ",")

# setting up data, also getting latitude and longitude to pin point data
# for getting accurate results

library(zipcode)
data(zipcode)
nrow(zipcode)
head(zipcode)
library(zipcode)
data(zipcode)
somedata = data.frame(postal = c(98007, 98290, 98065, 98801, 98104))
somedata
somedata$zip = clean.zipcodes(somedata$postal)
somedata
# getting lattitude and longitude
data(zipcode)
somedata = merge(somedata, zipcode, by.x='zip', by.y='zip')
somedata
# ggplot to get a show to a map ;)
town <- readOGR(dsn = "C:/Users/HP/Desktop/Dojo Task/Dataset/geo_map", layer = "SPD_BEATS_WGS84")
plot(town)
library(zipcode)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
data(zipcode)

#Seattle map
library("rgdal")
town <- readOGR(dsn = "C:/Users/HP/Desktop/Dojo Task/Dataset/geo_map", layer = "SPD_BEATS_WGS84")
plot(town)


#Plotting Distance
fm <- read.csv("C:/Users/HP/Desktop/Dojo Task/Data.csv",header = TRUE,sep = ",")
data(zipcode)
fm$Facility.Area.Zipcode<- clean.zipcodes(fm$Facility.Area.Zipcode)
#size by zip
fm.zip<-aggregate(data.frame(count=fm$ï..Facility.ID),list(zip=somedata$zip,county=somedata$city),length)
fm<- merge(fm.zip, zipcode, by='zip')
# joined connections
ggplot(fm,aes(longitude,latitude)) +
  geom_polygon(data=somedata,aes(x=somedata$longitude,y=somedata$latitude),color='red',fill=NA,alpha=1)+
  geom_point(aes(color = count),size=.2,alpha=.25) +
  xlim(-123,-119)+ylim(46,48)

# dotted plot
ggplot(fm,aes(longitude,latitude)) +
  geom_point(data = somedata, aes(x=somedata$longitude,y=somedata$latitude))
town <- readOGR(dsn = "C:/Users/HP/Desktop/Dojo Task/Dataset/geo_map", layer = "SPD_BEATS_WGS84")

#Data Preparation/Analysis

library(VIM)
aggr(somedata)

#clustering


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
par(mfrow = c(1, 3))
hist(somedata$latitude, col = 'gray')
hist(somedata$longitude, ylim = c(0, 10), col = 'gray')
plot(somedata$latitude, somedata$longitude, asp = 1)

set.seed(123)
two <- kmeans(somedata, 2)
three <- kmeans(somedata, 3)
three

two$centers
#cluster results
clus <- cbind(somedata, clus2 = two$cluster,
              clus3 = three$cluster)
head(clus)

#clustering visualization

par(mfrow = c(1, 2))
plot(clus$longitude, clus$latitude, col = two$cluster, asp = 1,
     pch = two$cluster, main = "Sites for two kiosks",
     xlab = "Longitude",  ylab = "Latitude")
points(two$centers[ ,2], two$centers[ ,1], pch = 23,
       col = 'maroon', bg = 'lightblue', cex = 3)
text(two$centers[ ,2], two$centers[ ,1], cex = 1.1, 
     col = 'black', attributes(two$centers)$dimnames[[1]])

plot(clus$longitude, clus$latitude, col = three$cluster, asp = 1,
     pch = three$cluster, main = "Sites for three kiosks",
     xlab = "Longitude",  ylab = "Latitude")
points(three$centers[ ,2], three$centers[ ,1],
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[ ,2], three$centers[ ,1], cex = 1.1,
     col = 'black', attributes(three$centers)$dimnames[[1]])

#clus continue

hybrid <- cbind(clus, hybrid_shape = rep(0, dim(clus)[1]))

for (e in 1:dim(hybrid[1])[1]) {
  if (hybrid[e, 3] == hybrid[e, 4]) {
    hybrid[e, 5] <- hybrid[e, 3]
  }
  if (hybrid[e, 3] != hybrid[e, 4]) {
    hybrid[e, 5] <- hybrid[e ,3] + 15
  }
}     

plot(hybrid$longitude, hybrid$latitude, col = two$cluster,
     main = "Hybrid: Two-cluster kiosks in three-cluster locations", pch = hybrid$hybrid_shape, cex = 1.1,
     xlab = "Longitude",  ylab = "Latitude", asp = 1)
points(three$centers[1:2, 2], three$centers[1:2, 1],
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[1:2, 2], three$centers[1:2, 1], cex = 1.1,
     col = 'black', attributes(two$centers)$dimnames[[1]])


















