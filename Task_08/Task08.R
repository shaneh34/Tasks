setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_08")

library(maps)
library(ape)
library(phytools)

text.string <- "(((((((cow, pig), whale),(bat,(lemur, human))),(robin, iguana)), coelacanth), (gold_fish, trout)),shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame= "circle", bg="white", cex=1)
#Question 1: More closely related to human
vert.tree
#Question 2: No they are no branch lengths.
tree<- read.tree(text="(((A,B), (C,D)),E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg= "lightblue", cex=1)
nodelabels(frame="circle", bg="white", cex=1)

tree$edge

AnolisTree<- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))

par(las=1)
hist(AnolisTree$edge.length, col="black", border="white", main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))

tipEdges<- which(AnolisTree$edge[,2] <= Ntip (AnolisTree))
Lengths <- AnolisTree$edge.length[tipEdges]
names(Lengths) <-AnolisTree$tip.label[AnolisTree$edge[tipEdges,2]]
names(Lengths) [which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

?plot.phylo

#Question 3
plot(AnolisTree, cex=0.25, offset=1, show.tip.label=FALSE)

#Question 4
plot(AnolisTree, type="fan")

#Question 5
plot(AnolisTree,cex=0.25,tip.color='red')

#Question 6
Lengths
edgelabels(tree$edge.length)

#Question 7
shortest<- "noblei"
AnolisTree2<- drop.tip(AnolisTree, shortest)
plot(AnolisTree2, cex=0.25)


ltt(AnolisTree)
abline(0,1,lwd=2,col='red',lty=2)

#Question 9: Theh line never goes down.  The slope is not always the same. The slope suggests that as time goes on the genetic relatedness differences between all individuals decreases.

#Question 10
fit.bd(AnolisTree, rho=0.2)