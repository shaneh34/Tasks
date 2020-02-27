setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_07")
source("http://jonsmitchell.com/code/reformatData07.R")
source("http://jonsmitchell.com/code/simFxn.R")

plot(1,1, type="n", xlim=c(1998, 2013), ylim=c(0,1))
s <- apply(overallFreq, 2, function(x) lines(overallFreq[,1],x, col=rgb(0,0,0,0.1)))


rescaleFreq <- apply(overallFreq[,3:ncol(overallFreq)],2,function(x)x-x[1])
plot(1,1, type="n", xlim=c(1998, 2013), ylim=c(-0.25,0.25))
s <- apply(rescaleFreq, 2, function(x) lines(overallFreq[,1],x, col=rgb(0,0,0,0.01)))

smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin=100)

smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp= Pal, nbin=100, xlab="year", ylab="change in allele freq. since 1998")
addFit(nruns=5, h=1, s=0, n=100, ngens=18, startT=1997, simCol="gray40", rescale=TRUE)

plot(alleleFreqs$d_freq, alleleFreqs$d_im, xlim=c(-0.15, 0.15), xlab="overall freq. change", ylab="freq. change in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col="blue")
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')