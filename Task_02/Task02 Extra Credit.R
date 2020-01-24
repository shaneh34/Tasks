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
write.csv(beren3, "beren_new.csv", quote=F, row.names = FALSE)

#bonus 
Naps <- which(beren3$event == "nap")
beren4 <- beren3[Naps,]
head(beren4)
beren4[5:6]
beren4[7:8]

#naming
beren4$end_minute <- beren4$end_minute / 60
beren4$start_minute <- beren4$start_minute / 60
beren4$napstart <- beren4$start_hour + beren4$start_minute
beren4$napend <- beren4$end_hour + beren4$end_minute
beren4$naplength <- beren4$napend - beren4$napstart
naptime <- tapply(beren4$day[Naps], beren4$age [Naps], sum)

#graphing 
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5 ,0), tck=-.01) 
plot(as.numeric(names(naptime)), naptime, type="b", pch=16, xlab="day", ylab="naptime")
cor.test(beren4$age, beren4$naplength)
#there is little correlation between the length of his naps and his age