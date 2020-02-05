setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_04")
trueMean1 <- 5
trueSD1 <-5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm (1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample (population1, Size)
Sample2 <- sample (population2, Size)
#The two samples are different in population.
boxplot (Sample1, Sample2)
Sample1
Sample2
source ("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder ("grandma_mom")
MatGrandpa <- makeFounder ("grandpa_mom")
PatGrandma <- makeFounder ("grandma_da")
PatGrandpa <- makeFounder ("grandpa_da")
Alan <- makeBaby (PatGrandma, PatGrandpa)
Brenda <- makeBaby (MatGrandma, MatGrandpa)
Focus <- makeBaby (Brenda, Alan)
#Fifty percent DNA should be the same when comparing Focus to Brenda
ToMom <- length (grep("mom", Focus))/length (Focus)
#Focus may share roughly 25 percent of her DNA with each grandparent.  These numbers did not match my expectations.
ToMomMom <- length (grep ( "grandma_mom", Focus ))/ length (Focus)
ToMomDad <- length (grep ("grandpa_mom", Focus ))/ length (Focus)
ToDadMom <- length (grep ("grandma_da", Focus))/ length (Focus)
ToDadDad <- length (grep ("grandpa_da", Focus))/length (Focus)
#Focus is not equally related to her paternal grandparents or her maternal granparents.  The average relatedness is still 0.25
Sibling_01 <- makeBaby (Brenda, Alan)
#I expect roughly 50% of DNA to be shared between the siblings. They share 44.625%.
ToSib <- length (intersect (Focus, Sibling_01))/length (Focus)
ManySiblings <- replicate (1e3, length(intersect(Focus, makeBaby( Brenda, Alan)))/length (Focus))
quantile (ManySiblings)
mean(ManySiblings)
plot (density(ManySiblings), main="", xlab= "proportion shared genes")
#There is a range of values above because even though each offspring gets 50 percent DNA from both dad and mom, what makes up that 50% from each parent can vary considerably because there are 10,000 loci being observed.  Two siblings can receive a gene for a certain loci from different parents. When comparing 10,000 loci, siblings can be shown to be very genetically different.
HWE <- function(p) {
  aa <- p^2
  ab <- 2*p*(1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}

# make HWE plot
HWE(0.5)
plot (1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines (p, GenoFreq [, "aa"], lwd=2, col="red")
lines (p, GenoFreq[, "ab"], lwd=2, col="purple")
lines (p, GenoFreq [, "bb"], lwd=2, col="blue")
legend ("top", legend=c ("aa", "ab", "bb"), col=c ("red", "purple", "blue"), lty=1, lwd=2, bty="n")

TestP <- 0.5
abline(v=TestP)
points(TestP, TestP^2, pch=16, col="red", cex=2)
points(TestP, (1-TestP)^2, pch=16, col="blue", cex=2)
points(TestP, 2*TestP*(1-TestP), pch=16, col="purple", cex=1.5)


Pop <- simPop (500)
points (Pop[, "freqa"], Pop[, "Genotypes.aa"]/1000, pch=21, bg="red")

#Yes the frequency does match the expectation.
Pop2 <- simPop(50)
points (Pop2[,"freqa"], Pop2[,"Genotypes.aa"]/50, pch=22, bg="green")

#The second population does not fit the HWE expectation because drift occurs faster in smaller populations causing the frequency of allele a to change from the fixed 0.5
install.packages("learnPopGen", dep=T)
library (learnPopGen)
x <- genetic.drift (Ne=200, nrep=5, pause=0.01)
x <- genetic.drift (Ne=300, nrep=5, pause=0.01)
x <- genetic.drift (Ne=50, nrep=5, pause=0.01)
x <- genetic.drift (Ne=10, nrep=5, pause=0.01)
x <- genetic.drift (Ne=1000, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)

# Run analysis
tExt <- sapply (Samples, function (x) nrow (simPop (x, 500)))


par(mfrow=c(1,2))
# First plot
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline (Line)

Line2 <- lm(tExt ~ Samples + 0)
summary(Line2)
Line2$coef
plot(Samples, tExt)
abline(Line2)
#It shows that it takes genetic drift longer to cause an allele to go extinct as the population size increases.
