install.packages("twitteR")
install.packages("devtools")
library(twitteR)
library(devtools)
#devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
library(httr)
setwd('C:/Users/Kevin/Google Drive/datasci/hw2')
twit_cred = read.csv('twitter_cred.csv', stringsAsFactors=FALSE)

TWITTER_CONSUMER_KEY = twit_cred$TWITTER_CONSUMER_KEY
TWITTER_CONSUMER_SECRET = twit_cred$TWITTER_CONSUMER_SECRET
TWITTER_ACCESS_TOKEN = twit_cred$TWITTER_ACCESS_TOKEN
TWITTER_ACCESS_SECRET = twit_cred$TWITTER_ACCESS_SECRET

setup_twitter_oauth(TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET,
                    TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET)
ds <- searchTwitter('#datascience', locale=NULL, geocode=NULL,n=500)