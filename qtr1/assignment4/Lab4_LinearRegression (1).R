# Lesson 4
# Simple Linear Regression

## 1 - Create variable sets
# Response variable
rate <- c(9.34 ,   8.50  ,  7.62  ,  6.93  ,  6.60)

# Explanatory variable
year <- c(2000 ,   2001  ,  2002  ,  2003 ,   2004)

# Plot variables
plot(year,rate
    ,main="Commercial Banks Interest Rate for 4 Year Car Loan"
    ,sub="http://www.federalreserve.gov/releases/g19/20050805/"
  )

# Correlation
cor(year,rate)

# Linear Model
fit <- lm(rate ~ year) # (Response ~ Explanatory)
attributes(fit)

# Plot regression
plot(year,rate
     ,main="Commercial Banks Interest Rate for 4 Year Car Loan"
     ,sub="http://www.federalreserve.gov/releases/g19/20050805/"
)
abline(fit) # Plot regression line

# F-Test
summary(fit)

