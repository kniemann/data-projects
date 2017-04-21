############################
# DS250
#
# Association Rules
# Titanic Survivors
############################

# Load data

load("c:/temp/titanic.raw.rdata")
str(titanic.raw)

# Find association rules with default settings
library(arules)
rules <- apriori(titanic.raw)
inspect(rules)

# set rhs=c("Survived=No", "Survived=Yes") in appearance to make sure
# that only "Survived=No" and "Survived=Yes" will appear in the rhs of rules.

rules <- apriori(titanic.raw,
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                 default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

# Visualizing Rules
library(arulesViz)
plot(rules)

# Visualizing results graph
plot(rules, method="graph", control=list(type="items"))

# Visualize parallel coordinates
plot(rules, method="paracoord", control=list(reorder=TRUE))


