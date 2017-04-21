###################################
# DS 250 - Lesson 4
###################################
# 1. Normal distribution functions
body <- read.table("c:/temp/body.dat.txt", header=FALSE)
names(body) <- c("biac.diam", "pelvic.bredth", "bitro.diam", "chest.dep", "chest.diam",
                 "elbow.diam", "wrist.diam", "knee.diam", "ankle.diam", "shoulder", "chest",
                 "waist", "navel", "hip", "thigh", "bicep", "forearm", "knee", "calf", "ankle.min",
                 "wrist.min", "age", "weight", "height", "gender")
head(body,3)

# Simple linear regression
# Expand plot window!
par(mfrow = c(2, 2))
plot(body$weight ~ body$hip, main = "weight v hip")
plot(body$weight ~ body$chest, main = "weight v chest")
plot(body$weight ~ body$wrist.diam, main = "weight v wrist.diam")
plot(body$weight ~ body$thigh, main = "weight v thigh")
par(mfrow = c(1, 1))

# Multiple-linear regression
lm_chest_height <- lm(weight ~ chest + height, data = body)
summary(lm_chest_height)

body$sex[body$gender == 1] <- "male"
body$sex[body$gender == 0] <- "female"
table(body$sex)

lm_sex_thigh <- lm(weight ~ sex + thigh, data = body)
summary(lm_sex_thigh)

lm_full <- lm(weight ~ shoulder + chest + waist + navel + hip + thigh + bicep +
                       forearm + knee + calf, data = body)
summary(lm_full)

# Plot variables
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm_full)




