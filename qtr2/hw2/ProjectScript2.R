##---------------------------
## Methods for Data Analysis
## Course Project
##
## Prerequisite: Run setwd(folder), where folder contains the following files:
##   cleanedTweets.csv
##   sfCrimeData.csv
##   profanityTerms.csv (please note, this file is not safe for work, and is by nature highly offensive)
##
## All files can be downloaded at:
## https://
##
##---------------------------

require(twitteR)
require(devtools)
require(httr)
require(logging)
require(dplyr)
require(stringi)
require(ggplot2)

#----Load functions----

loadSanFrancisco <- function(filename="sfCrimeData.csv")
{
  sanFranciscoCrime <- read.csv(filename, stringsAsFactors=F, header=T)
  sanFranciscoCrime$Category <- factor(sanFranciscoCrime$Category)
  sanFranciscoCrime$PdDistrict <- factor(sanFranciscoCrime$PdDistrict)
  sanFranciscoCrime$Descript <- factor(sanFranciscoCrime$Descript)
  sanFranciscoCrime$DayOfWeek <- factor(sanFranciscoCrime$DayOfWeek)
  sanFranciscoCrime$Resolution <- factor(sanFranciscoCrime$Resolution)
  
  return(sanFranciscoCrime)
}

loadTweets <- function(filename="cleanedTweets.csv")
{
  tweets <- read.csv(filename, stringsAsFactors=F, header=T, quote = "")
  
  return(tweets)
}

loadProfanityTerms <- function(filename="profanityTerms.csv")
{
  profanity <- read.csv(filename, stringsAsFactors=F, header=T)
  
  return(profanity)
}

#----Utility functions----
createRegex <- function(terms)
{
  return(paste("\\b(", paste(terms, collapse="|"), ")\\b", sep=""))
}

assignGridCells <- function(points, minLatitude, maxLatitude, minLongitude, maxLongitude, nrow, ncol)
{
  # Remove the lower bound on the first cell, so that the minimum points are included in that cell.
  latStep <- (maxLatitude - minLatitude)/nrow
  latitudes <- c(-Inf, seq(minLatitude+latStep, maxLatitude, by=latStep))
  
  longStep <- (maxLongitude - minLongitude)/ncol
  longitudes <- c(-Inf, seq(minLongitude+longStep, maxLongitude, by=longStep))
  
  griddedPoints <- points
  griddedPoints$Column <- cut(points$Longitude, breaks=longitudes, labels=1:ncol)
  griddedPoints$Row <- cut(points$Latitude, breaks=latitudes, labels=1:nrow)
  
  return(griddedPoints)
}

calculateCountGrid <- function(griddedPoints)
{
  grid <- griddedPoints %>%
            group_by(Row, Column) %>%
            summarise(Latitude=min(Latitude), 
                      Longitude=min(Longitude), 
                      Count=length(Row), 
                      Proportion=Count/length(griddedPoints$Latitude))
  
  return(grid)
}

plotAllPoints <- function(crime, tweets, profaneTweets)
{
  toPlot <- data.frame(Type="Crime", Latitude=crime$Latitude, Longitude=crime$Longitude)
  toPlot <- rbind(toPlot, data.frame(Type="Tweet", Latitude=tweets$Latitude, Longitude=tweets$Longitude))
  toPlot <- rbind(toPlot, data.frame(Type="ProfaneTweet", Latitude=profaneTweets$Latitude, Longitude=profaneTweets$Longitude))

  return(ggplot(toPlot, aes(x=Longitude, y=Latitude)) + 
           geom_point(alpha=0.05) + 
           stat_density2d(color="red") + 
           facet_wrap( ~ Type))
}

#----Imported functions----

# Function copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#----Investigation script----

if(interactive())
{
  basicConfig()
  addHandler(writeToFile, file="testing.log", level='DEBUG')
  
  if(!exists("sanFranciscoCrimeSetupComplete"))
  {
    loginfo("Loading San Francisco crime statistics")
    centerLatitude <- 37.783333
    centerLongitude <- -122.416667
    
    sanFranciscoCrime <- loadSanFrancisco()
    sanFranciscoCrimeSetupComplete <- TRUE
  }
  
  # Interested only in the most recent crimes. Clean up the columns, converting X and Y to Longitude and Latitude, respectively
  loginfo("Selecting 2015 crimes")
  sanFranciscoCrime2015 <- sanFranciscoCrime[grep("2015",sanFranciscoCrime$Dates),]
  sanFranciscoCrime2015$Latitude <- sanFranciscoCrime2015$Y
  sanFranciscoCrime2015$Y <- NULL
  sanFranciscoCrime2015$Longitude <- sanFranciscoCrime2015$X
  sanFranciscoCrime2015$X <- NULL
  
  loginfo("Plotting crimes, use crimeDotPlot")
  
  crimeDotPlot <- ggplot(sanFranciscoCrime2015, aes(x=Longitude, y=Latitude)) + geom_point(alpha=0.05, color="red") + ggtitle("Crime locations in San Francisco, 2015-01-01 through 2015-05-13")
  
  print(crimeDotPlot)
  
  loginfo("Loading tweet archive")
  if(!exists("tweetSetupComplete"))
  {
    tweets <- loadTweets()
    tweetSetupComplete <- T
  }
  
  loginfo("Geoboxing tweets to crime area")
  
  localTweets <- tweets[tweets$Latitude > min(sanFranciscoCrime2015$Latitude) &
                        tweets$Longitude > min(sanFranciscoCrime2015$Longitude) &
                        tweets$Latitude < max(sanFranciscoCrime2015$Latitude) &
                        tweets$Longitude < max(sanFranciscoCrime2015$Longitude),]
  
  loginfo("Plotting tweets, use tweetDotPlot")
  tweetDotPlot <- ggplot(localTweets, aes(x=Longitude, y=Latitude)) + 
                    geom_point(alpha=0.05, color="blue") + 
                    ggtitle("Tweet locations in San Francisco, 2015-08-11 through 2015-08-23")
  
  print(tweetDotPlot)
  
  loginfo("Loading profanity terms")
  
  # Profanity scraped from http://www.noswearing.com/dictionary
  profanityTerms <- loadProfanityTerms()

  profanityPattern <- createRegex(profanityTerms$Term)

  # It will be useful to bucketize both the crimes and tweets into a consistent grid over the space,
  # so do that now.
  
  gridMinLatitude <- min(sanFranciscoCrime2015$Latitude)
  gridMaxLatitude <- max(sanFranciscoCrime2015$Latitude)
  gridMinLongitude <- min(sanFranciscoCrime2015$Longitude)
  gridMaxLongitude <- max(sanFranciscoCrime2015$Longitude)
  gridNumCols <- 20
  gridNumRows <- 20
  
  loginfo("Gridding crimes and tweets into 20x20 grids")

  griddedSanFranciscoCrime2015 <- assignGridCells(sanFranciscoCrime2015, gridMinLatitude, gridMaxLatitude, gridMinLongitude, gridMaxLongitude, gridNumRows, gridNumCols)
  crimeMask <- unique(paste(griddedSanFranciscoCrime2015$Row, griddedSanFranciscoCrime2015$Column))
  
  # There are areas within the box not represented by the crime information. For those
  # areas, do not consider tweets.
  
  griddedLocalTweets <- assignGridCells(localTweets, gridMinLatitude, gridMaxLatitude, gridMinLongitude, gridMaxLongitude, gridNumRows, gridNumCols)
  localCells <- paste(griddedLocalTweets$Row, griddedLocalTweets$Column)
  griddedLocalTweets <- griddedLocalTweets[localCells %in% crimeMask,]
  
  loginfo("Finding tweets containing profanity")
  griddedProfaneTweets <- griddedLocalTweets[grep(profanityPattern, griddedLocalTweets$Text, ignore.case = T),]

  sanFranciscoCrime2015Grid <- calculateCountGrid(griddedSanFranciscoCrime2015)
  sanFranciscoCrime2015TilePlot <- ggplot(sanFranciscoCrime2015Grid, aes(x=Column, y=Row, fill=Count)) + 
                                    geom_tile() + 
                                    ggtitle("Tiled San Francisco crimes\n2015-01-01 through 2015-05-13")
  print(sanFranciscoCrime2015TilePlot)
  
  localTweetGrid <- calculateCountGrid(griddedLocalTweets)
  localTweetTilePlot <- ggplot(localTweetGrid, aes(x=Column, y=Row, fill=Count)) + 
                          geom_tile() + 
                          ggtitle("Tiled San Francisco tweets\n2015-08-11 through 2015-08-23")
  print(localTweetTilePlot)
  
  profaneTweetGrid <- calculateCountGrid(griddedProfaneTweets)

  # This tends not to have all rows and columns represented, so force the issue a bit
  # and ensure all values, 1:20 are shown
  profaneTweetTilePlot <- ggplot(profaneTweetGrid, aes(x=as.numeric(Column), y=as.numeric(Row), fill=Count)) + 
    geom_tile() + 
    xlab("Column") +
    ylab("Row") +
    ggtitle("Tiled San Francisco tweets containing profanity\n2015-08-11 through 2015-08-23") +
    xlim(1,20) +
    ylim(1,20)
  
  print(profaneTweetTilePlot)
  
  loginfo("Use sanFranciscoCrime2015TilePlot, localTweetTilePlot, and profaneTweetTilePlot")
  
  loginfo("Plotting each point for visual comparison, use bigMapPlot")
  
  bigMapPlot <- plotAllPoints(griddedSanFranciscoCrime2015, griddedLocalTweets, griddedProfaneTweets) + ggtitle("All crimes, tweets, and profane tweets")
  print(bigMapPlot)

  # Collect the above into a data frame, relating the counts within each grid cell
  
  tiledTweetCountData <- merge(sanFranciscoCrime2015Grid[,c("Row","Column","Count","Proportion")], 
                               localTweetGrid[,c("Row","Column","Count","Proportion")], 
                               by=c("Row", "Column"),
                               all.x=T)
  tiledTweetCountData$CrimeCount <- tiledTweetCountData$Count.x
  tiledTweetCountData$Count.x <- NULL
  tiledTweetCountData$TweetCount <- tiledTweetCountData$Count.y
  tiledTweetCountData$Count.y <- NULL
  tiledTweetCountData$CrimeProportion <- tiledTweetCountData$Proportion.x
  tiledTweetCountData$Proportion.x <- NULL
  tiledTweetCountData$TweetProportion <- tiledTweetCountData$Proportion.y
  tiledTweetCountData$Proportion.y <- NULL
  
  tiledTweetCountData <- merge(tiledTweetCountData, 
                     profaneTweetGrid[,c("Row","Column","Count","Proportion")], 
                     by=c("Row", "Column"), 
                     all.x=T)
  tiledTweetCountData$ProfaneTweetCount <- tiledTweetCountData$Count
  tiledTweetCountData$Count <- NULL
  tiledTweetCountData$ProfaneTweetProportion <- tiledTweetCountData$Proportion
  tiledTweetCountData$Proportion <- NULL

  tiledTweetCountData$TweetCount[is.na(tiledTweetCountData$TweetCount)] <- 0
  tiledTweetCountData$ProfaneTweetCount[is.na(tiledTweetCountData$ProfaneTweetCount)] <- 0

  tiledTweetCountData$TweetProportion[is.na(tiledTweetCountData$TweetProportion)] <- 0
  tiledTweetCountData$ProfaneTweetProportion[is.na(tiledTweetCountData$ProfaneTweetProportion)] <- 0

  loginfo("Plotting CrimeCount vs TweetCount, and CrimeCount vs ProfaneTweetCount")
  crimeCountTweetCountPlot <- ggplot(tiledTweetCountData, aes(x=TweetCount, y=CrimeCount)) + 
                                geom_point(alpha=0.25) + 
                                ggtitle("Crime count and tweet count in bucketized data")
  
  crimeCountProfaneTweetCountPlot <- ggplot(tiledTweetCountData, aes(x=ProfaneTweetCount, y=CrimeCount)) + 
                                      geom_point(alpha=0.25) + 
                                      ggtitle("Crime count and profane tweet count in bucketized data")
  
  loginfo("Use crimeCountTweetCountPlot and crimeCountProfaneTweetCountPlot")
  
  print(crimeCountTweetCountPlot)
  print(crimeCountProfaneTweetCountPlot)
  
  # Try fitting linear models to the tiled data, relating overall tweets to crime counts,
  # profane tweets to crime counts, and both individually.
  
  loginfo("Modeling tweet and crime counts")
  loginfo("Use tweetModelPlot and profaneTweetModelPlot for plots")
  loginfo("Use summary(tweetAndProfaneTweetModel) for combined model summary")
  
  # Unclear how best to plot this, but p-value of tweetcount, while still significant, is
  # not nearly as low as the profane tweet count.
  tweetAndProfaneTweetModel <- glm(CrimeCount ~ TweetCount + ProfaneTweetCount, data=tiledTweetCountData)
  print(summary(tweetAndProfaneTweetModel))
  
  tweetModel <- glm(CrimeCount ~ TweetCount, data=tiledTweetCountData)
  tweetModelPredictions <- data.frame(TweetCount=0:max(tiledTweetCountData$TweetCount))
  tweetModelPredictions$CrimeCount <- predict(tweetModel, newdata=tweetModelPredictions)
  tweetModelPlot <- ggplot(tiledTweetCountData, aes(x=TweetCount, y=CrimeCount)) + 
                      geom_point(alpha=0.25) + 
                      geom_line(data=tweetModelPredictions, color="blue") + 
                      ggtitle("CrimeCount from TweetCount\nPrediction in blue")
  
  profaneTweetModel <- glm(CrimeCount ~ ProfaneTweetCount, data=tiledTweetCountData)
  profaneTweetModelPredictions <- data.frame(ProfaneTweetCount=0:max(tiledTweetCountData$ProfaneTweetCount))
  profaneTweetModelPredictions$CrimeCount <- predict(profaneTweetModel, newdata=profaneTweetModelPredictions)
  profaneTweetModelPlot <- ggplot(tiledTweetCountData, aes(x=ProfaneTweetCount, y=CrimeCount)) + 
                            geom_point(alpha=0.25) + 
                            geom_line(data=profaneTweetModelPredictions, color="blue") + 
                            ggtitle("CrimeCount from ProfaneTweetCount\nPrediction in blue")
  
  # Are there specific profane terms that are much more associated to crime than the rest?

  # loginfo("Counting and gridding individual terms in tweets")
    
  # crimesAndTerms <- gridEachTerm(tiledTweetCountData, profaneTweets, profanityTerms)
  
  # crimesAndTermsModel <- glm(CrimeCount ~ . -Row -Column, data=crimesAndTerms)
  
  # While the model shows some statistical significance (p < 0.05) on certain terms, investigation shows
  # that there really is only a single observation of these terms. That the observation aligns
  # with the crimes is interesting, but this isn't enough to go on. It seems more correct to look at the
  # aggregate of where any profanity exists, rather than a subset of terms when the subsets are so small;
  # potentially with a longer time range of tweets, this approach would be worthwhile.
  
  # We can also look at whether the number of profane users in a cell is a better indicator of the arrests
  # in that cell. To do this, first get the grid of all users in our sample. Within each cell, count each
  # user that tweeted in that cell once. Do the same for users that exist in the profanity tweets data.
  
  loginfo("Calculating grids counting unique users and profane users")
  
  griddedUsers <- griddedLocalTweets %>%
                    group_by(Row, Column) %>%
                    summarise(UserCount=length(unique(ScreenName)))
  
  griddedProfaneUsers <- griddedProfaneTweets  %>%
                          group_by(Row, Column) %>%
                          summarise(ProfaneUserCount=length(unique(ScreenName)))
  
  tiledUserData <- sanFranciscoCrime2015Grid[,c("Row","Column","Count")]
  names(tiledUserData) <- c("Row","Column","CrimeCount")

  tiledUserData <- merge(tiledUserData, griddedUsers, by=c("Row","Column"), all.x=T)
  tiledUserData$UserCount[is.na(tiledUserData$UserCount) == T] <- 0
  tiledUserData <- merge(tiledUserData, griddedProfaneUsers, by=c("Row","Column"), all.x=T)
  tiledUserData$ProfaneUserCount[is.na(tiledUserData$ProfaneUserCount) == T] <- 0
  
  loginfo("Plotting unique users, use uniqueUsersTilePlot and uniqueProfaneUsersTilePlot")
  uniqueUsersTilePlot <- ggplot(tiledUserData, aes(x=Column,y=Row,fill=UserCount)) + 
                          geom_tile() + 
                          ggtitle("Unique twitter users\n2015-08-11 through 2015-08-23")
  
  print(uniqueUsersTilePlot)
  
  uniqueProfaneUsersTilePlot <- ggplot(tiledUserData, aes(x=Column,y=Row,fill=ProfaneUserCount)) + 
                                  geom_tile() + 
                                  ggtitle("Unique twitter users tweeting profanity\n2015-08-11 through 2015-08-23")
  
  print(uniqueProfaneUsersTilePlot)
}