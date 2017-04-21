#

# Normal Distributions

heart <- read.table("c:/temp/processed.cleveland.data.txt",sep=",",header=FALSE)
colnames(heart) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
head(heart)

range(heart$num)

mheart <- subset(heart, heart$sex == 1)
fheart <- subset(heart, heart$sex == 0)

mcholmean <- mean(mheart$chol)
mcholsd   <- sd(mheart$chol)

hist(mheart$chol, probability = TRUE)
x <- 126:353
y <- dnorm(x = x, mean = mcholmean, sd = mcholsd)
lines(x = x, y = y, col = "blue")

qqnorm(mheart$chol)
qqline(mheart$chol)

# Probability

sim_norm <- rnorm(n = length(mheart$chol), mean = mcholmean, sd = mcholsd)
1 - pnorm(q = 260, mean = mcholmean, sd = mcholsd)

sum(mheart$chol > 260) / length(mheart$chol)



