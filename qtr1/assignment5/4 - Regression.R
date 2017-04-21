###################################
# DS 250 - Lesson 4
###################################
# 1. Normal distribution functions

load("c:/temp/mlb11.RData")
head(mlb11,3)

cor(mlb11$runs, mlb11$at_bats)

plot_ss(x = mlb11$at_bats, y = mlb11$runs)
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

# Linear Model
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

# Linearity Test
plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals) # adds diagonal line to the normal prob plot



