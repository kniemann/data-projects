##--------------------------------------------
##
## Kevin Niemann
## Chicago Diabetes Homework (Lecture 4)
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------


setwd('C:/Users/kevin/Google Drive/datasci/hw4/')

data = read.csv('ChicagoDiabetesData.csv', stringsAsFactors = FALSE)

data_sums = apply(data[-1],2,sum)

hospitalizations = data_sums[grepl('Hospitalizations', names(data))]
admit_rate = data_sums[grepl('Crude.Rate.[0-9]+$', names(data), perl = TRUE)]

plot(hospitalizations, admit_rate)
admitLM = lm(admit_rate ~ hospitalizations)
abline(admitLM)
#Summary of linear model
summary(admitLM)

#Diff

hospitalizations_diff = diff(hospitalizations)
admit_rate_diff = diff(admit_rate)

plot(hospitalizations_diff, admit_rate_diff)

diffLM = lm(hospitalizations_diff ~ admit_rate_diff)
abline(diffLM)
#Summary of diff linear model
summary(diffLM)


