require(RSQLite)
setwd('C:/Users/kevin/Google Drive/datasci/final_project')
sql_db_name = 'strava.db'
readCon = dbConnect(SQLite(), dbname=sql_db_name)
if(isS4(readCon )){
  dbReadTable(readCon , "Strava")
}

dbDisconnect(readCon)