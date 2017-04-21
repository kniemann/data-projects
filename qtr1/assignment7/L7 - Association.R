###########################
# DS250 Lesson 7
# Association Rules
###########################

rm(list = ls())

# load association mining libraries
# install.packages("arules")
# install.packages("arulesViz")

library("arules")
library("arulesViz")

# load transaction data. 
trans = read.transactions(file="c:/temp/arules/transaction-dataset.txt" , format="basket", sep=" ");
summary(trans)

# extract association rule from transaction data. 
rules <- apriori(trans, parameter=list(support=0.01, confidence=0.5))

length(rules)
inspect(head(sort(rules , by="lift")))

#scatter plot 
plot(rules, measure=c("support" , "confidence") , shading="lift")
plot(rules, measure=c("support" , "lift") , shading="confidence")

#matrix plot
plot(rules, method="matrix" , measure="lift" )
plot(rules, method="matrix3D" ,measure=c("lift", "confidence"))

#graph plot
plot(rules, method="graph" );

# grouped matrix chart
plot(rules, method="grouped")

#Parallel coordinates
plot(rules, method="paracoord")

