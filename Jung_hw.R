##### Read in the data #####

setwd("/Users/jaeheejung/Dropbox/Wash U/Network_analysis/hw")
load("nigeria.rda")
ls()

head(nigeria)
summary(nigeria)
str(nigeria)
unique(nigeria$year)
unique(nigeria$conflict)

##### Turn event dataset into matrix #####

actors <- unique(c(as.character(nigeria$sender), as.character(nigeria$receiver)))
actors <- intersect(as.character(nigeria$sender), as.character(nigeria$receiver))
conflictMat <- matrix(0, ncol = length(actors), nrow = length(actors),
                  dimnames = list(actors, actors))
conflict <- nigeria[nigeria$conflict == 1,]
for(i in 1:nrow(conflict)){
  conflictMat[as.character(conflict[i,]$sender), as.character(conflict[i,]$receiver)] <- 1
}
conflictMat[1:3,1:3]

##### Most influential actor in the network #####

library(igraph)
g <- graph_from_adjacency_matrix(conflictMat, mode='directed',diag=FALSE)
# Check degree centrality because node with many edges can mean power to initiate conflict.
which.max(igraph::degree(g)) # Nigerian police

##### Run blockmodel with varying k #####

library(sna)
conflictNet <- network(conflictMat)
eclusts <- equiv.clust(conflictNet)
plot(eclusts,hang=-1) # 2, 3, or 6 clusters seem reasonable.

conflictBlock2 <- blockmodel(conflictNet, eclusts, k=2)
bm2 <- data.frame(conflictBlock2$block.membership,conflictBlock2$plabels)
bm2 <- bm2[match(conflictBlock2$glabels,bm2[,2]),]
conflictBlock3 <- blockmodel(conflictNet, eclusts, k=3)
bm3 <- data.frame(conflictBlock3$block.membership,conflictBlock3$plabels)
bm3 <- bm3[match(conflictBlock3$glabels,bm3[,2]),]
conflictBlock6 <- blockmodel(conflictNet, eclusts, k=6)
bm6 <- data.frame(conflictBlock6$block.membership,conflictBlock6$plabels)
bm6 <- bm6[match(conflictBlock6$glabels,bm6[,2]),]

##### Choose k through cross validation #####

outdegree <- sna::degree(conflictNet, cmode='outdegree')

dat <- data.frame(outdegree,bm2=bm2[,1],bm3=bm3[,1],bm6=bm6[,1])

set.seed(1120)
library(caret)
train_control <- trainControl(method="cv",number=10)
mod2 <- train(outdegree~bm2,data=dat, trControl=train_control)
print(mod2)
mod3 <- train(outdegree~bm3,data=dat, trControl=train_control)
print(mod3)
mod6 <- train(outdegree~bm6,data=dat, trControl=train_control)
print(mod6)
# mod2 seems best

##### Plot #####

colVec <- c("red","blue")
bcols <- colVec[bm2[,1]]
plot(conflictNet, displaylabels=T, vertex.cex=2, label.cex=1, edge.col=rgb(150,150,150,100,maxColorValue=255),label.pos=5,vertex.col=bcols)

##### Cross-sectional ERGM #####

library(statnet)
m1 <- ergm(conflictNet ~ edges + mutual + odegree(1))
summary(m1) # If there is one more reciprocal tie/conflict in the network, then the baseline probability of a tie/conflict increases. Having one more node with an out-degree of 1 does not affect the baseline probability of a tie/conflict.
mcmc.diagnostics(m1)

