##--------------------------------------------
##
## Test Farm-Subsidies Data set
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 3
##
## Datasets located:
##
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi
##
##   Need:
##
##    -2011 Farm Payment File (27MB) txt file
##    -State and County Code List (374KB) xls file (probably convert this to csv)
##
##--------------------------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)
require(ggplot2)
require(plyr)
require(maps)
require(data.table)

##----Hypotheses to test-----
#
#  Test these two things:
#
#    1.  Does our sample equally represent all 50 states?
#
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
#
#     Note- you can find the farms per state in census data.


#Write production R code to do answer two questions: 






#You can optionally test if weighting by acreage/state works too. Spoiler: the answer is no for all of them.  The extra files you might need are on the github site. (except for the large subsidy file)
#Write a text file describing your outcomes, p-values, and how those p-values are interpreted via the null-hypothesis.



#

trim = function (x) gsub("^\\s+|\\s+$", "", x)

##-----Declare Functions Here-----

retrieveData = function() {
  loginfo((paste('Retrieving data.')),logger="data_logger")
  
  ##-----Read in the data-----
  setwd('C:/temp')
  farmData = read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";", header=FALSE, stringsAsFactors=FALSE)
  
  ##----Trim Whitespaces-----
  farmData = as.data.frame(apply(farmData,2,trim), stringsAsFactors=FALSE)
  
  names(farmData) = c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  ##------Read State/County File-----
  setwd('C:/Users/kevin/Google Drive/datasci/hw3')
  county_state_codes = read.csv("foia_state_county_codes.csv", stringsAsFactors=FALSE)
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together----
  farmData = merge(farmData, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
  

  #farmCountByState = as.data.frame(table(farmData$ST))
  
  uniqueCust <- data.table(farmData)
  uniqueCust[, count := uniqueN(cust_num), by=ST]
  
  farmCountByState = unique(data.frame(uniqueCust$ST,uniqueCust$count))

  farmSpendingByState = aggregate(as.numeric(farmData$amount), list(state=farmData$ST), sum, na.rm=TRUE)
  farmCountByState = rename(farmCountByState, c("uniqueCust.ST"="state"))
  farmCountByState = rename(farmCountByState, c("uniqueCust.count"="Freq"))
  farmSpendingCountMerged = merge(farmCountByState,farmSpendingByState, by=c("state"), all.x=TRUE)
  farmSpendingCountMerged = rename(farmSpendingCountMerged, c("x"="Spending"))
  
  FarmsAcresByState = read.csv("FarmsAcresByState.csv", stringsAsFactors=FALSE)
  FarmsPerState = read.csv("FarmsPerState.csv", stringsAsFactors=FALSE)
  FarmsAcresByState$FarmsE3 = NULL
  
  FarmsPerState = rename(FarmsPerState, c("State"="state"))
  FarmsAcresByState = rename(FarmsAcresByState, c("State"="state"))
  
  farmSpendingCountMerged = merge(farmSpendingCountMerged,FarmsAcresByState,by=c("state"), all.x=TRUE)
  farmSpendingCountMerged = merge(farmSpendingCountMerged,FarmsPerState,by=c("state"), all.x=TRUE)
  return (farmSpendingCountMerged)
}
#just for fun.. map the farms by how well over/under represented they are.
mapFarms = function() {
  states <- map_data("state")
  #plot all states with ggplot
  
  states$state = state.abb[match(states$region,tolower(state.name))]
  mapData = merge(farmSpending, states, sort = FALSE, by="state")
  mapData <- mapData[order(mapData$order), ]
  
  qplot(long, lat, data = mapData, group = group, fill = ActualRepresentation, geom = "polygon")
  
  
}

#(1) Do the states receive equal # or $ representation in the 2011 farm subsidies? and 
equalRepresentation = function() {
  
  loginfo((paste('Using alpha value .05')),logger="data_logger")
  loginfo((paste('Null hypothesis is that all states have an equal number of farms receiving compensation')),logger="data_logger")
  if (chisq.test(farmSpending$Freq)$p.value < alpha) {
    loginfo((paste('Reject null hypothesis for farm count')),logger="data_logger")
    
  } else {
    loginfo((paste('Accept null hypothesis')),logger="data_logger")
  }
  loginfo((paste('Null hypothesis is that all states are receiving an equal amount of compensation')),logger="data_logger")
  if (chisq.test(farmSpending$Spending)$p.value < alpha) {
    loginfo((paste('Reject null hypothesis for spending')),logger="data_logger")
    
  } else {
    loginfo((paste('Accept null hypothesis')),logger="data_logger")
  }
}

#(2) Do the states receive subsidies as weighted by # of farms/state?  You can answer both with a goodness-of-fit test (chi-squared test).  

equalRepresentationWeighted = function() {
  
  loginfo((paste('Null hypothesis is that all states are receiving an equal amount of compensation when weighted by the number of actual farms')),logger="data_logger")
  if (chisq.test(farmSpending$Spending,p = farmSpending$weight)$p.value < alpha) {
    loginfo((paste('Reject null hypothesis for spending weighted by farm')),logger="data_logger")
    
  } else {
    loginfo((paste('Accept null hypothesis')),logger="data_logger")
  }
}



##----Run Main Function Here----
if(interactive()){
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')
  
  
  farmSpending = retrieveData()
  
  ##-----Probably do some data exploration----
  
  ##----Perform a test for equal representation-----
  farmSpending$SpendingPerFundedFarm = (farmSpending$Spending / farmSpending$Freq)
  #meanSpendPerFarm = mean(farmSpending$SpendingPerFarm)
  stdDevFarmSpending = sd(farmSpending$SpendingPerFundedFarm)
  #loginfo((paste('Test 1: There is not equal representation in spending, the standard deviation of spending per farm among the 50 states',stdDevFarmSpending)),logger="data_logger")
  
  #46. unequal representation. some state spend more per farm.
  
  ##----Access the farms/state data-----
  
  ##----Derive the weights for each state----
  
    #Weight is a percentage of total farms
  farmSpending$Weight = (farmSpending$Farms/ sum(farmSpending$Farms))
  ##----Perform a test for equal repreentation by farms/state-----
  farmSpending$EqualRepresentation = sum(farmSpending$Spending) * farmSpending$Weight
  farmSpending$ActualRepresentation = (farmSpending$Spending - farmSpending$EqualRepresentation)
  ##----Output Results----
  # Acceptable to print output, log output, save in DB, or write to file. Your choice.
  sdActualRepresentation = sd(farmSpending$ActualRepresentation)
  #loginfo((paste('Test 2: There is not equal representation weighted by number of farms per state, the standard deviation of the difference between equal representation and actual representation is',sdActualRepresentation)),logger="data_logger")
  
  #Actual answers
  #logs output to console
  alpha = .05
  farmSpending$weight = farmSpending$Farms/sum(farmSpending$Farms)
  equalRepresentation()
  equalRepresentationWeighted()
  
  
  
  #map of farms
  mapFarms()
}
