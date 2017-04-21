#Load required dependencies
require(httr)
require(logging)
require(RSQLite)
require(plyr)
require(bitops)
require(stringr)
require(ggmap)
require(rmongodb)
require(jsonlite)
require(geosphere)
require(broom)
require(dplyr)
setwd('C:/Users/kevin/Google Drive/datasci/final_project')

get_data = function() {
  loginfo('Retrieving data from Mongo.')
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
  mongo.destroy(mongo)
  return (activities)
}
#Get start lat/lng from polyline
#Then get furthest point lat/lng (distance, not necessarily turn around point)
add_polyline_data = function(activities) {
  loginfo('Decoding and adding polyline data.')
  activities$route_start_lat = NA
  activities$route_start_lng = NA
  activities$route_furthest_lat = NA
  activities$route_furthest_lng = NA
  #loop through activities
  for(i in 1:nrow(activities)) {
    result <- tryCatch({
      #call decode function
      route_line = DecodeLineR(activities$map.summary_polyline[i])
      activities$route_start_lat[i] = as.numeric(route_line$lat[1])
      activities$route_start_lng[i] = as.numeric(route_line$lng[1])
      #get furtherest point from start
      max_dist = which.max(distm(c(route_line$lng[1],route_line$lat[1]), data.frame(route_line$lng, route_line$lat), fun=distHaversine))
      
      activities$route_furthest_lat[i] = as.numeric(route_line$lat[max_dist])
      activities$route_furthest_lng[i] = as.numeric(route_line$lng[max_dist])
      
    }, warning = function(war) {
      
      # warning handler picks up where error was generated
      loginfo(paste("Decode warning:  ",war))
    }, error = function(err) {
      
      # error handler picks up where error was generated
      loginfo(paste("Decode error:  ",err))
    }, finally = {
    }) # END tryCatch
    
  }
  return (activities)
}
#This function decodes the polyline map data
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
#Create the map with lines from start to furthest point.
map_lines = function(activities) {
  loginfo('Mapping lines.')
  #line
  map <- get_map(location = 'Seattle', zoom = 11)
  mapLine <- ggmap(map) + geom_point(aes(x = route_start_lng , y = route_start_lat), 
                                     data = activities, alpha = .5, color="Red")+ geom_point(aes(x = route_furthest_lng , y = route_furthest_lat), 
                                     data = activities, alpha = .5, color="Blue")+ geom_segment(data=activities, aes(x = route_start_lng, y = route_start_lat, 
                                     xend = route_furthest_lng , yend = route_furthest_lat), color="yellow", alpha = .3) + 
                                      labs(title="Plot of start to furthest point",x ="Longitude", y = "Latitude")
  mapLine
}
map_start_points = function(activities) {
  loginfo('Mapping starting points.')
  #start points
  #density
  map <- get_map(location = 'Seattle', zoom = 11)
  mapPoints <- ggmap(map) + geom_point(aes(x = route_start_lng , y = route_start_lat), data = activities, alpha = .5, 
                                       color="Blue") + stat_density2d(aes(x = route_start_lng , y = route_start_lat),
                                        color="red", data = activities) + labs(title="Density of start points"
                                                                               ,x ="Longitude", y = "Latitude")
  mapPoints
}
#Read neighborhood shapefile (from https://data.seattle.gov/download/2mbt-aqqx/application/zip)
read_neighborhoods = function() {
  loginfo('Reading neighborhood shapefile.')
  neighborhoods <- rgdal::readOGR('Neighborhoods.shp', layer ='Neighborhoods')
  return(neighborhoods)
}
#Create the neighborhood lookup table from the shapefile
get_lookup = function(neighborhoods) {
  loginfo('Creating lookup df.')
  lookup <- data.frame()
  lookup <- rbind(lookup, neighborhoods@data)
  lookup$id <- as.character(lookup$OBJECTID)
  lookup$S_HOOD <- as.character(lookup$S_HOOD)
  
  return (lookup)
}
#Create a join on the neighborhood shapefile and the lookup table
join_neighborhoods = function(neighborhoods, activities) {
  loginfo('creating neighborhood data.')
  # Merge lookup (LookUp) into polygons,
  data <- tidy(neighborhoods)
  neighborhoods = full_join(lookup, data, by = "id")
  #map <- get_map(location = 'Seattle', zoom = 11)
  #mapPoints <- ggmap(map) + geom_point(aes(x = route_start_lng , y = route_start_lat), data = activities, alpha = .5, color="Blue") + geom_polygon(aes(x = long, y = lat, group = group), data = neighborhoods, colour = 'red', fill = 'black', alpha = .2 , size = .3)
  #mapPoints
  return (neighborhoods)
}
#Create a map with the neighborhoods overlayed.
map_neighborhoods = function(neighborhoods, activities) {
  map <- get_map(location = 'Seattle', zoom = 11)
  mapPoints <- ggmap(map) + geom_point(aes(x = route_start_lng , y = route_start_lat), data = activities, 
                  alpha = .5, color="Blue") + geom_polygon(aes(x = long, y = lat, group = group), data = neighborhoods
                  , colour = 'red', fill = 'black', alpha = .2 , size = .3) +
                  labs(title="Plot of starting points by neighborhood",x ="Longitude", y = "Latitude")
  mapPoints
  return (mapPoints)
}

#Classifies each activity with a neighborhood.
classify_neighborhoods = function(activities,neighborhood_data,lookup) {
  loginfo('Classifying activities.')
  #loop through all activities
  for(i in 1:nrow(activities)) {
    #P = start point
    p = c(activities$route_start_lng[i], activities$route_start_lat[i])
    #Get ID of neighborhood
    id = neighborhood_data %>% 
      split(.$id) %>% 
      sapply(function(x) point.in.polygon(p[1], p[2], x$long, x$lat) > 0) %>%  
      names(.)[.]
    id = as.numeric(id)
    #lookup ID and then assign actual neighborhood from lookup table
    if(length(id) > 0) {
      activities$neighborhood[i] = lookup$S_HOOD[id+1]
    }
  }
  return (activities)
}
#This creates the linear model and performs the testing.
do_linear_model = function(activities) {
  loginfo('Creating and testing linear model.')
  activities$average_watts = as.numeric(activities$average_watts)
  #Scale the watts from 0-max to 0-1.
  max = max(as.numeric(activities$average_watts), na.rm = TRUE)
  activities$average_watts_rating = as.numeric(activities$average_watts/max)
  activities$average_speed = as.numeric(activities$average_speed)
  
  # Split into train/test set
  train_ind = sample(1:nrow(activities), round(0.8*nrow(activities)))
  train_set = activities[train_ind,]
  test_set = activities[-train_ind,]
  
  #Perform logistic regression
  train_lm = glm(formula = average_watts_rating ~ neighborhood, family = binomial(link = "logit"), 
      data = train_set)
  
  #Do predictions
  test_predictions = predict(train_lm, newdata = test_set, type="response")

  #Create results table
  results = test_set["neighborhood"]
  results$pred = as.numeric(test_predictions)
  results$actual = as.numeric(test_set["average_watts_rating"]$average_watts_rating)
  results$error = abs(results$actual-results$pred)
  #Aggregate predictions by neighborhood
  agg = aggregate(results, by = list(results$neighborhood), FUN = mean )
  #Plot of results
  qplot(x= pred, y = actual, data =agg, na.rm = TRUE) + scale_y_continuous( limits = c(0,.6), expand = c(0,0) ) + scale_x_continuous( limits = c(0,.6), expand = c(0,0) ) +
    labs(title="Plot of predicted vs actual watts (each point is neighborhood)",x ="Predicted watts", y = "Actual watts")
  loginfo(paste('Mean prediction error', round(mean(results$error, na.rm =  TRUE), digits = 5)))
  #Create one way ANOVA. Look at P value
  fit = aov(pred ~ actual, data = agg)
  loginfo(paste('Reject null hypthesis, prediction is not the same as actual dataset p<.05',summary(fit)))
}



if(interactive()) {
  basicConfig(level='DEBUG')
  addHandler(writeToFile, file="final_project.log",level='FINEST') 
  loginfo('Starting Final Project run.')
  activities = get_data()
  activities = add_polyline_data(activities)
  map_1 = map_lines(activities)
  map_1
  map_2 = map_start_points(activities)
  map_2
  neighborhood_data = read_neighborhoods()
  lookup = get_lookup(neighborhood_data)
  
  neighborhood_data_join = join_neighborhoods(neighborhood_data, activities)
  map_3 = map_neighborhoods(neighborhood_data_join,activities)
  map_3
  activities = classify_neighborhoods(activities,neighborhood_data_join,lookup)
  
  do_linear_model(activities)

  loginfo('Finished Final Project run.')
}
