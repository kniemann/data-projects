# Lesson 4
# Simple Logistic Regression
# 1
# Problem: Estimate the probability of a vehicle being fitted with
#          a manual transmission if it has a 120hp engine and weights 2800 lbs.

#a Create general linear model
# (transmission type (am) by the horsepower (hp) and weight (wt))
am.glm <- glm(formula = am ~ hp + wt  
             , data   = mtcars 
             , family = binomial
             ) 

#b Create test data
testcar = data.frame(hp=120, wt=2.8)

#c Predict match using model
#help(predict.glm) 
predict(am.glm, testcar, type="response")

###########
# For an automobile with 120hp engine and 2800 lbs weight,
# the probability of it being fitted with a manual transmission is about 64%. 
###########

# Significance test
summary(am.glm)


## 2 - Logistic Regression Example
#a Load sample data
data(mtcars)
dat <- subset(mtcars, select=c(mpg, am, vs))

#  vs = outcome variable
# mpg = continuous predictor
#  am = Cateorical predictor

#b Perform the logisitic regression
logr_vm <- glm(vs ~ mpg, data=dat, family=binomial)
logr_vm <- glm(vs ~ mpg, data=dat, family=binomial(link="logit"))

logr_vm
summary(logr_vm)

# Plot results
#a simple plot
plot(dat$mpg, dat$vs)
curve(predict(logr_vm, data.frame(mpg=x), type="response"), add=TRUE)

#b
library(ggplot2)
ggplot(dat, aes(x=mpg, y=vs)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)






