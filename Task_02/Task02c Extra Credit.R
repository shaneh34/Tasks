setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_02")
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

write.csv(x = beren3, file = "beren_new.csv", quote=F, row.names=F)

#Hypothesis is that as Beren ages his growth rate will decrease

setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_02")

beren3 <- read.csv("beren_new.csv", stringsAsFactors = F)

shanesberendata <- read.csv("beren_new.csv", stringsAsFactors = F)

Weigh <- which (beren3[,"event"] == "trait_mass")

beren4 <- beren3[Weigh,]

Regression <- nls(value ~ p1 / (1 + exp(-(p2 + p3 * age))), start=list(p1=100 ,p2=-1.096, p3=.002), data=beren4, trace=TRUE)


plot(beren4$age, beren4$value, type="p", xlab="age (days)")
abline(lm(beren4$value~beren4$age))

Fake <- data.frame(age=seq(from=1,to=300,by=1), value=rep(NA, 300))
Predict <- predict(Regression, newdata=Fake)
lines(Fake$age, Predict, type="l", col="red", lty=2)

#Hypothesis is accepted
Predict
Regression
Predict [284]
#My prediction is that Beren will weigh 8.831052 kg on his check up on Monday.
