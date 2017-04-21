
## Download and install the package
#install.packages("igraph")


## Load package
library(igraph)

# load termDocMatrix
load("C:/temp/termDocMatrix.rdata")

# inspect part of the matrix
termDocMatrix[5:10,1:20]

termDocMatrix <- as.matrix(termDocMatrix)

# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1

# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]

# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")

# remove loops
g <- simplify(g)

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# Plot
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

##
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA

egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam

# plot the graph in layout1
plot(g, layout=layout1)
