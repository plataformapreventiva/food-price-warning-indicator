############ Interpolate air temperature maps for Estonia (12/05/2010 & 19/05/2010) and ###############
############create thematic map of differences #################
install.packages("sp")

library(ggmap)

# geode weather staions
est_weather_stat <- geocode(c('Zacatecas'))
est_weather_stat <- geocode(c('Harku', 'Jogeva', 'Johvi', 'Kihnu', 'Kunda', 'Kuusiku', 'Narva-Joesuu', 'Laane-Nigula', 'Pakri', 'Parnu', 'Ristna', 'Ruhnu', 'Sorve', 'Toravere', 'Tiirikoja', 'Turi', 'Valga', 'Viljandi', 'Vilsandi', 'Virtsu', 'Vaike-Maarja', 'Voru'))
est_weather_stat <- geocode(c('Harku, Estonia', 'Jogeva, Estonia', 'Johvi, Estonia', 'Kihnu, Estonia', 'Kunda, Estonia', 'Kuusiku, Estonia', 'Narva-Joesuu, Estonia', 'Laane-Nigula, Estonia', 'Pakri, Estonia', 'Parnu, Estonia', 'Ristna, Estonia', 'Ruhnu, Estonia', 'Sorve, Estonia', 'Toravere, Estonia', 'Tiirikoja, Estonia', 'Turi, Estonia', 'Valga, Estonia', 'Viljandi, Estonia', 'Vilsandi, Estonia', 'Virtsu, Estonia', 'Vaike-Maarja, Estonia', 'Voru'))

# create data vector with the names of weather stations
est_weather_stat_list <- c('Harku', 'Jogeva', 'Johvi', 'Kihnu', 'Kunda', 'Kuusiku', 'Narva-Joesuu', 'Laane-Nigula', 'Pakri', 'Parnu', 'Ristna', 'Ruhnu', 'Sorve', 'Toravere', 'Tiirikoja', 'Turi', 'Valga', 'Viljandi', 'Vilsandi', 'Virtsu', 'Vaike-Maarja', 'Voru')
# merge station names & coordinates
est_weather_stat_2 <- cbind(est_weather_stat_list, est_weather_stat)

estonia_air_temperature <- read.csv("C:/ANTO/loengud/GIS Maps and Spatial Analyses for Urban Planning/data/interpolate/estonia_air_temperature.csv", sep=";")
estonia_air_temperature_2 <- cbind(est_weather_stat_2, estonia_air_temperature)
write.csv(estonia_air_temperature_2, file="estonia_air_temperature_2.csv")


#### idw interpolation###
# based on example from http://rpubs.com/adam_dennett/10873

library(ggplot2) # start needed libraries
library(gstat)
library(sp)
library(maptools)
estonia_air_temperature_2_test <- estonia_air_temperature_2 # duplicate air temp. data file
estonia_air_temperature_2_test$x <- estonia_air_temperature_2_test$lon # define x & y as longitude and latitude
estonia_air_temperature_2_test$y <- estonia_air_temperature_2_test$lat
coordinates(estonia_air_temperature_2_test) = ~x + y #set spatial coordinates to create a Spatial object
plot(estonia_air_temperature_2_test)
plot(estonia_air_temperature_2_test)
x.range <- as.numeric(c(21.76, 28.21)) # map extent
y.range <- as.numeric(c(57.45, 59.72))
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1), y = seq(from = y.range[1], to = y.range[2], by = 0.1)) # expand points to grid
coordinates(grd) <- ~x + y 
gridded(grd) <- TRUE
plot(grd, cex = 1.5)
points(estonia_air_temperature_2_test, pch = 1, col = "red", cex = 1)
idw <- idw(formula = may12 ~ 1, locations = estonia_air_temperature_2_test, newdata = grd) # apply idw model for the data
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("long", "lat", "var1.pred") # give names to the modelled variables
# plot results
ggplot() + geom_tile(data = idw.output, aes(x = long, y = lat, fill=var1.pred)) + geom_point(data=estonia_air_temperature_2, aes(x=lon, y=lat), shape=21, colour="red")
#add estonian contour:
est_contour <- readShapePoly("C:/ANTO/loengud/GIS Maps and Spatial Analyses for Urban Planning/data/population_in_municipalities_2011_wgs84.shp")
est_contour <- fortify(est_contour, region = "name")

ggplot() +
  geom_tile(data = idw.output, alpha=0.75, aes(x = long, y = lat, fill=round(var1.pred, 0))) +
  geom_point(data=estonia_air_temperature_2, aes(x=lon, y=lat), shape=21, colour="red") + 
  scale_fill_gradient(low="cyan", high="orange") + 
  geom_path(data=est_contour, aes(long, lat, group=group), colour="grey")
