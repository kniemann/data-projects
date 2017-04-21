#Data Science Final project quarter 2

#This class gets the activity from the sql lite database,
#fetches it from Strava and records it to MongoDB.

#install.packages("httr")
require(httr)
require(logging)
require(RSQLite)
require(rmongodb)

#Retrieve data from Strava REST API and write to database
retrieveData = function(activityListArg) {
  auth = 0
  
  #Iterate through activity list
  for(i in 1:length(activityListArg)) {
    url = paste("https://www.strava.com/api/v3/activities/",activityListArg[i], sep="")
    loginfo(activityListArg[i])
    authV = c("Bearer <Strava ID>","Bearer <Strava ID>")
    if(auth == authV[1]) {
      auth = authV[2]
    } else if(auth == authV[2]) {
      auth = authV[1]
    } else {
      auth = authV[1]
    }
    #act <- GET(url, add_headers(Authorization = "Bearer <Strava ID>"))
    logdebug(paste('Using auth',auth))
    act <- GET(url, add_headers(Authorization = auth))                
    
    result <- content(act, as = "parsed")
    #Positive response
    if(act$status_code == "200") {
      if(!(is.null(result$end_latlng))){
        
          #write result to mongo here:
          bson = mongo.bson.from.list(result)
          mongo.insert(mongo, "mydb.activities", bson)
        
        
        #} else {
          #logdebug(paste('Not within Seattle limits or not a ride',endLat, endLong,result$type))
        #}
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


setwd('C:/Users/kevin/Google Drive/datasci/final_project')
logReset()
basicConfig(level='DEBUG')
#setwd('C:/temp')
addHandler(writeToFile, file="~/sql_to_mongo.log",level='FINEST') 
logdebug(paste('Starting sql to mongo script'))
sql_db_name = 'strava.db'
mongo = mongo.create(host = "localhost")
con = dbConnect(SQLite(), dbname=sql_db_name)
#Get current activity if possible
db = dbReadTable(con, "Strava")
dbDisconnect(con)
activityList = db$result.id
#retrieve data from list
retrieveData(activityList)

