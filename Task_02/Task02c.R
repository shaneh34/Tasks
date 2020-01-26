setwd("C:\\Users\\harle\\Evolution\\Tasks")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv" , stringsAsFactors = F)
Data
write.csv(Data, "raw_data.csv", quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1 ,]
Data[2 ,]
Data[1:3 ,]
Data[1:3 , 4]
Data[1:5 , 1:3]
beren <- Data
Feeds <- which(beren[,9] == "bottle")
berenMilk <- beren[Feeds,]
head(berenMilk)
Feeds <- which(beren[,"event"] == "bottle")
Feeds <- which(beren$event == "bottle")
dayID <- apply(beren, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin =" 2019-04-18")
beren$age <- dateID - dateID[which(beren$event == "birth")]
head(beren)
beren2 <- beren
beren3 <- beren2[order(beren2$age),]

#Hypothesis is that as Beren ages his growth rate will decrease
Weigh <- which (beren3[,9] == "trait_mass")


Hypothesis <- beren3[Weigh,]
Regression <- nls(value ~ p1 / (1 + exp(-(p2 + p3 * age))), start=list(p1=100 ,p2=-1.096, p3=.002), data=Hypothesis, trace=TRUE)

plot(,Weigh)# use hypothesis
lines() # get x data from hypothesis, use predict(Regression) for y data

