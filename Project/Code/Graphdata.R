Input <- "C:\\Users\\harle\\Evolution\\Tasks\\Project\\Data"
setwd(Input)

Graphdata <-read.csv(file.choose(), header=TRUE)
Graphdata
PercentWolfDNA <- Graphdata$WOLF.DNA
Heterozygosity <- Graphdata$RELATIVE.FST
plot (PercentWolfDNA, Heterozygosity, xlab="Percent of Wolf DNA", ylab="Coyote Heterozygosity")
abline(lm(Heterozygosity~PercentWolfDNA), col="red")
Regression <- lm(Heterozygosity~PercentWolfDNA)
Regression

#To do list
#1 Run a regression analysis to look for correlation.
#2 Create a phylogeny tree of the coyotes with the data.