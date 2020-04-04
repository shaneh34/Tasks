setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_09")

#functions from phytools library
library(phytools)

#read phylogeny into R
tree <- read.tree("http://www.phytools.org//Cordoba2017//data//Anolis.tre")
plot(tree, type="fan")
#Question 1: 82 tips and 162 branch lengths. 
tiplabels(frame="circle", bg='lightblue', cex=1)
tree$tip.label
head(tree)
edgelabels(tree$edge.length, bg="black", col="white", font=2)
#read data
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
#Question2: Data is an object that contains species with the value of its snout-vent length.
#the dimesnions of this object is one column and 100 rows.
head(data)
dim(data)

#Vector Conversion
svl <- setNames(data$svl, rownames(data))
#let us estimate how large the ancestors were
Ancestors <- fastAnc(tree, svl[tree$tip.label], vars=TRUE, CI=TRUE)
#Question 3: Where are the estimated values stored? What is the CI95 element?
#Question 4: Where are two assumptions made in the estimation of the ancestral states using fastAnc?

#Plot
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)

obj<- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))

#Addition of fossils
fossilData<-data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("aliniger", "aliniger", "occultus", "christophei", "cristatellus", "occultus"), tip2=c("chlorocyanus", "coelestinus", "monticola", "cybotes", "angusticeps", "angusticeps"))
#Question 5: 
