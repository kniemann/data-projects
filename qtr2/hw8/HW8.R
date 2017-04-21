#Kevin Niemann
#HW 8

#Install lib
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("e1071")
# install.packages("caret")
# install.packages("RTextTools")
# install.packages("topicmodels")
# install.packages("slam")
# install.packages("RKEA")
# install.packages("stringdist")
# install.packages("textir")
# install.packages("openNLP")
# install.packages("openNLPdata")

##-----Load Libraries-----
library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(caret)
library(RTextTools)
library(topicmodels)
library(slam)
library(RKEA)

library(stringdist)

# If you cannot install stringdist, download the zip here:
# https://cran.rstudio.com/bin/windows/contrib/3.2/stringdist_0.9.3.zip

library(textir)
library(openNLP)
library(openNLPdata)


setwd('C:/temp')

# Load scraped beer reviews
reviews = read.csv("beer_reviews.csv", stringsAsFactors = FALSE)
reviews$date = as.Date(reviews$date, format = "%Y-%m-%d")

str(reviews)
range(reviews$date)

# Normalize Data:
# Change to lower case:
reviews$review = tolower(reviews$review)

# Remove punctuation
# Better to take care of the apostrophe first
reviews$review = sapply(reviews$review, function(x) gsub("'", "", x))
# Now the rest of the punctuation
reviews$review = sapply(reviews$review, function(x) gsub("[[:punct:]]", " ", x))

# Remove numbers
reviews$review = sapply(reviews$review, function(x) gsub("\\d","",x))

# Remove extra white space, so we can split words by spaces
reviews$review = sapply(reviews$review, function(x) gsub("[ ]+"," ",x))

# Remove non-ascii
reviews$review = iconv(reviews$review, from="latin1", to="ASCII", sub="")

# remove stopwords
stopwords()
my_stops = as.character(sapply(stopwords(), function(x) gsub("'","",x)))
my_stops = c(my_stops, "beer", "pour", "pours", "poured", "glass",
             "thank", "bottle", "bottles", "glasses", "thanked", "tulip",
             "great", "good", "nice", "really", "thanks", "like",
             "pint", "snifter", "just", "leaves", "served")
reviews$review = sapply(reviews$review, function(x){
  paste(setdiff(strsplit(x," ")[[1]],my_stops),collapse=" ")
})# Wait a minute for this to complete

# Remove extra white space again:
reviews$review = sapply(reviews$review, function(x) gsub("[ ]+"," ",x))

# Stem words:
reviews$review_stem = sapply(reviews$review, function(x){
  paste(setdiff(wordStem(strsplit(x," ")[[1]]),""),collapse=" ")
})

# Remove empty/short reviews:
sum(nchar(reviews$review_stem)<15)
reviews = reviews[nchar(reviews$review_stem)>15,]



##-----Text Corpus-----
# We have to tell R that our collection of reviews is really a corpus.
review_corpus = Corpus(VectorSource(reviews$review_stem))

# Build a Document Term Matrix
# Terms        Docs
#            ... 32 33 34 35 36 37 38 39 ...
# drink        0  0  1  1  1  0  1  2  1  0
# balanc       0  0  0  0  0  0  1  0  0  0
# exept        0  0  0  1  0  0  0  0  0  0
# hop          0  0  0  0  0  0  0  0  0  0
# sweet        0  0  1  0  0  0  0  0  0  0
# ...

# The following takes another minute to run.
review_document_matrix = TermDocumentMatrix(review_corpus)
review_term_matrix = DocumentTermMatrix(review_corpus)

# These two matrices are transposes of each other
dim(review_term_matrix)
dim(review_document_matrix)

# Too large to write out, so look at a part of it
inspect(review_term_matrix[1:5,1:5])

# Too large and too sparse, so we remove sparse terms:
review_term_small = removeSparseTerms(review_term_matrix, 0.995) # Play with the % criteria, start low and work up
dim(review_term_small)

# Look at frequencies of words across all documents
word_freq = sort(colSums(as.matrix(review_term_small)))

# Most common:
tail(word_freq, n=10)

# Least Common:
head(word_freq, n=10)

reviews$normalized_rating = reviews$rating/5

#Convert to matrix
review_matrix = as.matrix(review_term_small)

# Convert to Data Frame
review_frame = as.data.frame(review_matrix)

# Use only the most common terms
which_cols_to_use = which(colSums(review_matrix) > 10)

review_frame = review_frame[,which_cols_to_use]

# Convert to factors:
review_frame = as.data.frame(lapply(review_frame, as.factor))

# Add the response
review_frame$rating = reviews$normalized_rating

# Split into train/test set
train_ind = sample(1:nrow(review_frame), round(0.8*nrow(review_frame)))
train_set = review_frame[train_ind,]
test_set = review_frame[-train_ind,]

#### Logistic regression
review_lm = glm(rating ~ ., family=binomial(logit), data=train_set)
test_predictions = predict(review_lm, newdata = test_set, type="response")

results = test_set["rating"]
results$pred = as.numeric(test_predictions)
#Rating
summary(results$rating, na.rm= TRUE)
sd(results$rating, na.rm= TRUE)
#Pred
summary(results$pred, na.rm=TRUE)
sd(results$pred, na.rm=TRUE)
plot(results)

# Summary
summary(review_lm)