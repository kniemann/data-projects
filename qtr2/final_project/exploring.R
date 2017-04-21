require(httr)
require(logging)
require(RSQLite)
require(plyr)
require(dplyr)
require(bitops)
require(stringr)
#install.packages("ggmap")
require(ggmap)
require(maptools)
require(gpclib)


#install.packages("rmongodb")
require("rmongodb")

#mongo stuff
#install.packages("jsonlite")
require(jsonlite)
setwd('C:/Users/kevin/Google Drive/datasci/final_project')


#again
## create the empty data frame
mongo = mongo.create(host = "localhost")
activities = data.frame(stringsAsFactors = FALSE)

## create the namespace
DBNS = "mydb.activities"

## create the cursor we will iterate over, basically a select * in SQL
cursor = mongo.find(mongo, DBNS)

## create the counter
i = 1

## iterate over the cursor
while (mongo.cursor.next(cursor)) {
  # iterate and grab the next record
  tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
  # make it a dataframe
  tmp.df = as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
  # bind to the master dataframe
  activities = rbind.fill(activities, tmp.df)
  # to print a message, uncomment the next 2 lines cat('finished game ', i,
  # '\n') i = i +1
}
activities$start_latitude <- as.numeric(activities$start_latitude)
activities$start_longitude <- as.numeric(activities$start_longitude)
activities$end_latlng1 <- as.numeric(activities$end_latlng1)
activities$end_latlng2 <- as.numeric(activities$end_latlng2)

unique(activities$athlete.id)
#617/866 are unique


map <- get_map(location = 'Seattle', zoom = 11)
ggmap(map)

#Try this with scaled size vs jitter
coords = data.frame(activities$start_latitude,activities$start_longitude)
coords = ddply(coords,.(activities.start_latitude,activities.start_longitude),nrow)

mapSized <- ggmap(map) + geom_point(aes(x = activities.start_longitude , y = activities.start_latitude, size = V1), data = coords, alpha = .5, color="Red")
mapSized


#Jitter map
mapPoints <- ggmap(map) + geom_jitter(aes(x = start_longitude , y = start_latitude), data = activities, alpha = .5, color="Red") + geom_jitter(aes(x = end_latlng1 , y = end_latlng2), data = activities, alpha = .5, color="Blue")
mapPoints

#end points
mapPoints <- ggmap(map) + geom_jitter(aes(x = route_start_lng , y = route_start_lat), data = activities, alpha = .5, color="Blue") + stat_density2d(aes(x = route_start_lng , y = route_start_lat),color="red", data = activities)

mapPoints

DecodeLineR <- function(encoded) {

  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0
  
  while(index <= len) {
    
    shift <- result <- 0
    
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;
    
    shift <- result <- b <- 0
    
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng
    
    array[df.index,] <- c(lat = lat * 1e-05, lng = lng * 1e-5)
    df.index <- df.index + 1
  }
  
  ret <- data.frame(array[1:df.index - 1,])
  names(ret) <- c("lat", "lng")
  return(ret)
}

activities$route_start_lat = NA
activities$route_start_lng = NA
activities$route_furthest_lat = NA
activities$route_furthest_lng = NA

#Get start lat/lng from polyline
#Then get furthest point lat/lng (distance, not necessarily turn around point)
for(i in 1:nrow(activities)) {
   result <- tryCatch({
    route_line = DecodeLineR(activities$map.summary_polyline[i])
    activities$route_start_lat[i] = as.numeric(route_line$lat[1])
    activities$route_start_lng[i] = as.numeric(route_line$lng[1])
    max_dist = which.max(distm(c(route_line$lng[1],route_line$lat[1]), data.frame(route_line$lng, route_line$lat), fun=distHaversine))
    
    activities$route_furthest_lat[i] = as.numeric(route_line$lat[max_dist])
    activities$route_furthest_lng[i] = as.numeric(route_line$lng[max_dist])
    
  }, warning = function(war) {
    
    # warning handler picks up where error was generated
    print(paste("Decode warning:  ",war))
  }, error = function(err) {
    
    # error handler picks up where error was generated
    print(paste("Decode error:  ",err))
  }, finally = {
  }) # END tryCatch
}

#line
mapLine <- ggmap(map) + geom_point(aes(x = route_start_lng , y = route_start_lat), data = activities, alpha = .5, color="Red")+ geom_point(aes(x = route_furthest_lng , y = route_furthest_lat), data = activities, alpha = .5, color="Blue")+ geom_segment(data=activities, aes(x = route_start_lng, y = route_start_lat, xend = route_furthest_lng , yend = route_furthest_lat), color="yellow", alpha = .3) 
mapLine

download.file("https://data.seattle.gov/download/2mbt-aqqx/application/zip",
              destfile = "Neighborhoods.zip")
unzip("Neighborhoods.zip", exdir = ".")



# unzip, and load tools
gpclibPermit()
# read data into R
neighborhoods <- readShapeSpatial('Neighborhoods.shp',
                              proj4string = CRS("+proj=longlat +datum=WGS84"))

neighborhoods <- rgdal::readOGR('Neighborhoods.shp', layer ='Neighborhoods')


lu <- data.frame()
lu <- rbind(lu, neighborhoods@data)
lu$id <- as.character(lu$OBJECTID)
lu$S_HOOD <- as.character(lu$S_HOOD)
# Merge lu (LookUp) into polygons,
data <- tidy(neighborhoods)
neighborhoods = full_join(lu, data, by = "id")

ggmap(map) +  geom_polygon(aes(x = long, y = lat, group = group), data = neighborhoods,
                           colour = 'white', fill = 'black', alpha = .4 , size = .3) + geom_text(aes(label = id, x = long, y = lat), data= neighborhoods)

start = c(activities$route_start_lat[1], activities$route_start_lng[1])
point.in.polygon(activities$route_start_lng[1], activities$route_start_at[1], neighborhoods$long, neighborhoods$lat)

p = c(activities$route_start_lng[1], activities$route_start_lat[1])


neighborhoods %>% 
  split(.$id) %>% 
  sapply(function(x) point.in.polygon(p[1], p[2], x$long, x$lat) > 0) %>%  
  names(.)[.]



# Tidy merged data
region@data <- select(region@data, -NAME.x)
colnames(region@data) <- c("code", "name", "country")

# Ensure shapefile row.names and polygon IDs are sensible
row.names(region) <- row.names(region@data)
region <- spChFIDs(region, row.names(region))




# convert to a data.frame for use with ggplot2/ggmap and plot
data <- tidy(neighborhoods)

dataProjected@data$id <- rownames(dataProjected@data)

ggmap(map) +  geom_polygon(aes(x = long, y = lat, group = group), data = data,
                          colour = 'white', fill = 'black', alpha = .4 , size = .3)


qmap('texas', zoom = 6, maptype = 'satellite') +
  geom_polygon(aes(x = long, y = lat, group = group), data = data,
               colour = 'white', fill = 'black', alpha = .4, size = .3)


attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,vs), 
                    FUN=mean, na.rm=TRUE)
print(aggdata)
detach(mtcars)
