# Capstone project HW 5/24
#Final Project–Part 2: Due by Lesson 09
#
#Construct initial model, post on Kaggle to set your team baseline
#Screenshot of position on leaderboard
#Compare with multiple classifiers, model settings
#Opportunity for feature selection, feature transformation such as binning or clustering, etc.
#Describe the data cleaning, transformation steps you selected and describe the why and how.  Run at least two to three new model evaluations, submit at least one new submission to Kaggle contests site
#
#Explanation:
#
#Our group (team 5) decided to each work on an initial separate type of algorithim for this turn in. Whichever achieved the best
#result would be tuned as a group to maximize our result. I am working primarily with random forests. I chose this because of the
#simplicity and performance of the model. While it can run with all features (no feature selection), I did do normalization of the features.
#Any features which normalized to N/A (very close to zero) were dropped from both the training and test sets. This is because that feature
#wasn't helpful in predicting the number. I split the data 80/20 into training/test set to allow faster tweaking of model paramaters
#than uploading to Kaggle.
#The best upload performed at 95.7% accuracy. Screenshot is attached.
#I'm investigating using a neural network with hyper parameter tuning/cross validation to improve the model performance.
#
#
#
#
#

library(randomForest)
library(readr)
library(dplyr)
library(caret)


setwd("C:/capstone")

set.seed(3434)

numTrain <- 42000
numTrees <- 25

train <- read_csv("train.csv")
test <- read_csv("test.csv")

# Local test
# Split into train/test set
# TODO use cross validation
train_ind = sample(1:nrow(train), round(0.8*nrow(train)))
train_set = train[train_ind,]
test = train[-train_ind,]

#Actual
#rows <- sample(1:nrow(train), numTrain)
#labels <- as.factor(train[rows,1])
#train <- train[rows,-1]

#Local
labels <- as.factor(train_set[,1])
train_unlb <- train_set[-1]
test_unlb <- test[-1]

#Scale
train_unlb <- scale(train_unlb)
test_unlb <- scale(test_unlb)

#Drop columns where n/a from both train and test set

train_unlb <- train_unlb[,colSums(is.na(train_unlb))<nrow(train_unlb)]
test_unlb <- test_unlb[ , colnames(test_unlb) %in% colnames(train_unlb)]

test_unlb <- test_unlb[,colSums(is.na(test_unlb))<nrow(test_unlb)]
train_unlb <- train_unlb[ , colnames(train_unlb) %in% colnames(test_unlb)]




#test_unlb <- test_unlb[,-train_unlb]
print(ncol(test_unlb))
print(ncol(train_unlb))




#Drop columns where min/max is .01?
#train_unlb <- train_unlb[,colSums(is.na(train_unlb))<nrow(train_unlb)]



#Local
rf <- randomForest(train_unlb, labels, xtest=test_unlb, ntree=numTrees,na.action=na.omit)
predictions <- data.frame(ImageId=1:nrow(test_unlb), Label=levels(labels)[rf$test$predicted])
head(predictions)

test$prediction = predictions$Label

result <- test[,c(1,786)]

count <- result[result$label==result$prediction,]

#Accuracy
print("training set accuracy:")
nrow(count)/nrow(test)


#Actual
#rf <- randomForest(train, labels, xtest=test, ntree=numTrees, importance = TRUE)
#predictions <- data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
#head(predictions)

write_csv(predictions, "results.csv")

#Uploads
#Run 1: 0.93514 25 trees, 10000 train
#Run 2: 0.95729 42k trained (all)
#Run 3: 