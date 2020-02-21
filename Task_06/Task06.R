setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_06")
library("learnPopGen")
#install.packages("coala")
#install.packages("phytools")
library("coala")
#install.packages("ape")
#install.packages("maps")

library("phytools")
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2)+
  feat_mutation(10) +
  feat_recombination(10) +
  sumstat_trees() +
  sumstat_nucleotide_div()
stats <- simulate(model, nsim=1)
Diversity <- stats$pi
Nloci <- length(stats$trees)

t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Age1 <- max(nodeHeights(t1))

t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

compare.chronograms(t1, t2)

t1_1<- read.tree(text=stats$trees[[1]][1])
t1_2<- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)

for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for(n in 1:ntrees) { 
    if(locus==1 && n==1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo( outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
model3 <-coal_model(10, 50)+
  feat_mutation(par_prior("theta", sample.int(100, 1)))+
  sumstat_nucleotide_div()
stats <- simulate(model3, nsim=40)	
mean_pi<- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi,theta)
abline(lm(mean_pi ~ theta))

#Coalescent and Questions
coalescent.plot()
coalescent.plot(n=3, ngen=50, colors=NULL)
coalescent.plot(n=20, ngen=50, colors=NULL)
coalescent.plot(n=100, ngen=50, colors=NULL)
coalescent.plot(n=5, ngen=50, colors=NULL)
coalescent.plot(n=7, ngen=50, colors=NULL)
#Question 1: You modify allele number by changing n. My simulations begin with 3, 20, and 100 alleles.
#Question 2: 3 alleles went to fixation in approxiamately 5 generations.  20 alleles simulation went to fixation at 20 generations. 100 allele simulation never reached fixation. On average, it seems that it takes a little more than one generation per allele number in order to reach fixation.
#question 3: The average numberof offspring is one.  The variance is zero to three.
#Question 4: Fitness does not play a role.  The simulation is based on just the effects of drift.
#Question 5: The most common ancestor is usually not alive for generation zero, if focal locus means the one that goes to fixation.  This is true especially for populations with smaller alleles.
#Question 6: Because each person has two loci per individual that are inherited separately.
#Question 7: No they do not match.