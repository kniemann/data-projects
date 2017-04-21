###################################
# DS 250 - Lesson 4
###################################
#1

  # Probability Mass Function
  # x11()
  x <- seq(-4.5,4.5,.1)
  normdensity <- dnorm(x,mean=0,sd=1)
  plot(x,normdensity,type="l")

  # Normal distribution functions
  pnorm(27.4, mean=50, sd=20)
  pnorm(27.4, 50, 20)
  
  x <- seq(-3,3,0.1)
  plot(x=x, y=dnorm(x, mean=0, sd=1), type='l')
  
  x <- rnorm(1000, mean=100, sd=15)
  hist(x, probability=TRUE)
  xx <- seq(min(x), max(x), length=100)
  lines(xx, dnorm(xx, mean=100, sd=15))

###################################
# 2. Power distribution functions

  samples <- rlnorm(100, meanlog=0, sdlog=1)
  
  par(fig=c(0,1,0,0.35))
  boxplot(samples, horizontal=T, bty="n", xlab="log-normal distribution")
  par(fig=c(0,1,0.25,1), new=T)
  
  s <- seq(0,max(samples),0.1)
  d <- dlnorm(s, meanlog=0, sdlog=1)
  
  hist(samples, prob=T, main="", col=gray(0.9), ylim=c(0,max(d)))
  lines(density(samples), lty=2)
  curve(dlnorm(x, meanlog=0, sdlog=1), lwd=2, add=T)
  rug(samples)

###################################
# 3. Random distribution functions

# install.packages("mosaic")

require(mosaic)
rflip(10)

do(3) * rflip(10)

random.flips <- do(100) * rflip(10)
tally(~heads, data=random.flips)

histogram(~ heads, data=random.flips, width=1)

###################################
# Cumulative Distribution Function
# X11()
X = rnorm(100) # X is a sample of 100 normally distributed random variables
P = ecdf(X)    # P is a function giving the empirical CDF of X
P(0.0)         # This returns the empirical CDF at zero (should be close to 0.5)

plot(P)        # Draws a plot of the empirical CDF (see below)


###################################
# 4. LAB - Statistical Analysis - Diabetes Population

diab <- read.csv("c:/temp/lab1_diabetes.csv",header=TRUE)
colnames(diab) <- c("x","gender","race","labs","meds","diags","insulin")

names(diab)
dim(diab)
summary(diab$labs)

mean(diab$labs) 
var(diab$labs)
median(diab$labs)

table(diab$gender)
(table(diab$gender)/54745)*100 # Convert to percentages

barplot(table(diab$gender))

table(diab$gender,diab$insulin)
mosaicplot(table(diab$gender,diab$insulin))



