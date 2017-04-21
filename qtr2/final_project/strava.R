#Data Science Final project quarter 2
#install.packages("httr")
require(httr)
require(logging)
require(RSQLite)

nwLat = 47.734145
seLat = 47.48172
nwLong = -122.459696
seLong = -122.224433


#Retrieve data from Strava REST API and write to database
retrieveData = function(startCount, endCount) {
	auth = 0
  #retrieve data. Should start where left off.
  #Iterate from most current to previous
  for(i in startCount:endCount) {
  #for(i in 10000:9500) {
    gc()
    url = paste("https://www.strava.com/api/v3/activities/",i, sep="")

    authV = c("Bearer <key here>","Bearer <key here?")
    if(auth == authV[1]) {
      auth = authV[2]
    } else if(auth == authV[2]) {
      auth = authV[1]
    } else {
      auth = authV[1]
    }
    #act <- GET(url, add_headers(Authorization = "Bearer <strava id>"))
	logdebug(paste('Using auth',auth))
    act <- GET(url, add_headers(Authorization = auth))                

    result <- content(act, as = "parsed")
    #logdebug(paste('Received Response from request',i))
    #Positive response
    if(act$status_code == "200") {
        if(!(is.null(result$end_latlng))){
          
          #Get Activity location endpoint
          endLat = as.numeric(result$end_latlng[1])
          endLong = as.numeric(result$end_latlng[2])
          
          #Check if ends within Seattle limits (roughly) and is a ride
          if(endLat < nwLat & endLat > seLat & endLong > nwLong & endLong < seLong & result$type == "Ride") {
            #add info
            startLat = as.numeric(result$start_latlng[1])
            startLong = as.numeric(result$start_latlng[2])
            # Write to SQLite DB
            if(is.null(result$average_temp)) {result$average_temp = NA}
            df = data.frame(result$id, result$start_date_local, endLat,endLong,startLat,startLong,as.character(result$commute),result$distance,result$moving_time,result$elapsed_time,result$average_speed,result$average_temp)
            loginfo(paste('Match found! Committing to database',result$id))
            dbWriteTable(con, "Strava", df, overwrite=FALSE, append = TRUE)
            
          } else {
            logdebug(paste('Not within Seattle limits or not a ride',endLat, endLong,result$type))
          }
        } else {
          logdebug(paste('No lat/long provided.'))
        }
    } else if(act$status_code == "403") {
      logerror(paste('Error received:',act$status_code, i))
      Sys.sleep(300)      
    } else {
      logwarn(paste('Warning received:',act$status_code, i))
    }
    usage = strsplit(as.character(act$headers$`x-ratelimit-usage`), ",")
    shortLimit = as.numeric(usage[[1]][1])
    longLimit = as.numeric(usage[[1]][2])
    if(shortLimit >= 597) {
      logwarn(paste('Close to 15 minute request limit (sleep 5 minutes)',shortLimit))
      Sys.sleep(300)
    }
    if(longLimit >= 29975) {
      logwarn(paste('Close to daily limit (sleep 60 min)',longLimit))
      Sys.sleep(3600)
    }
  }
  
}



  logReset()
  basicConfig(level='DEBUG')
  #setwd('C:/temp')
  addHandler(writeToFile, file="~/strava.log",level='FINEST') 
  logdebug(paste('Starting app.'))
  sql_db_name = 'strava.db'
  con = dbConnect(SQLite(), dbname=sql_db_name)
  #Get current activity if possible
  db = dbReadTable(con, "Strava")
  start = (min(db$result.id)-1)
  end = start - 1000000
  #retrieve data. Should start where left off.
  retrieveData(start, end)
  dbDisconnect(con)
