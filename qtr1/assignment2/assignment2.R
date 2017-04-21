#Review and execute the following to pull Bureau of Labor Statistics data into two vectors:
  
bls.rates <- readLines(con=url("http://download.bls.gov/pub/time.series/la/la.data.1.CurrentS"))
bls.areas <- readLines(con=url("http://download.bls.gov/pub/time.series/la/la.area"))
#Load Unemployment rate data

#Load unemployment rates
rates <- data.frame(
  Area  = substr(bls.rates,  4, 18),
  Code  = substr(bls.rates, 19, 20),
  Year  = substr(bls.rates, 32, 35),
  Month = substr(bls.rates, 37, 39),
  Rate  = gsub("P","0",substr(bls.rates, 49, 52))
)
#Load area information

areas <- data.frame(
  Area  = substr(bls.areas,  3, 17),
  Name  = do.call(rbind, strsplit(substr(bls.areas, 19, 90),"\t0"))[,1]
)
#Filter results by State, record type, and year

rates.filter <- substr(rates$Area,0,2)  == 'ST' & 
  as.character(rates$Code)             == '03' &
  as.numeric(as.character(rates$Year)) >= 1980 &
  as.numeric(as.character(rates$Rate)) <= 100

areas.filter <- substr(areas$Area,0,2)  == 'ST'

rates.states <- rates[rates.filter,]
areas.states <- areas[areas.filter,]

unemp <- merge(rates.states,areas.states,by="Area")
#Create a new data frame showing unemployment by State by months:
unemp_st_mn <- unemp[order(unemp$Name,unemp$Month),]
unemp_st_mn
write.table(unemp_st_mn, "c:/users/kevin/desktop/6a.txt", sep="\t")
 # Modify unemployment rate data frame to show real month abbreviation: (i.e. 'Jan', 'Feb')
unemp_realmonth <- unemp_st_mn
unemp_realmonth <- as.numeric(gsub("M01", "Jan", as.matrix(unemp_realmonth)))
unemp_realmonth

unemp_realmonth$Month <- gsub("M01", "Jan", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M02", "Feb", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M03", "Mar", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M04", "Apr", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M05", "May", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M06", "Jun", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M07", "Jul", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M08", "Aug", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M09", "Sep", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M10", "Oct", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M11", "Nov", unemp_realmonth$Month)
unemp_realmonth$Month <- gsub("M12", "Dec", unemp_realmonth$Month)


unemp_st_mn
mean(as.numeric(unemp_st_mn$Rate))
mean(unemp_st_mn$Rate[unemp_st_mn$Year=="1980"])
mean(unemp_st_mn$Rate)

#Create a new data frame and store the average unemployment rate for all States by Year: (i.e. National average unemployment rate by Year)

