###################################
# DS 250 - Lesson 4
###################################
# 1. Probability functions
load("c:/temp/kobe.RData")
head(kobe,3)

kobe$basket[1:9]

kobe_streak <- calc_streak(kobe$basket)  # Custom function
barplot(table(kobe_streak))

######################################
# Independent Sampling
outcomes <- c("heads", "tails")
sample(outcomes, size = 1, replace = TRUE)

do(3) * sample(outcomes, size = 1, replace = TRUE)

######################################
# Coin flipping
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)

sim_fair_coin
table(sim_fair_coin)

sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2, 0.8))
sim_unfair_coin
table(sim_unfair_coin)

######################################
# Sample Comparisons
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))

table(sim_basket)
table(kobe$basket)

sim_streak <- calc_streak(sim_basket)  # Custom function
barplot(table(sim_streak))

