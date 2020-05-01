Input <- "C:\\Users\\harle\\Evolution\\Tasks\\Project\\Data"
setwd(Input)

Graphdata <-read.csv(file.choose(), header=TRUE)
Graphdata
PercentWolfDNA <- Graphdata$WOLF.DNA
Heterozygosity <- Graphdata$RELATIVE.FST
plot (PercentWolfDNA, Heterozygosity, xlab="Amount of Wolf DNA (%)", ylab="Coyote Heterozygosity", main= "X-Y Scatter Plot")
abline(lm(Heterozygosity~PercentWolfDNA), col="red")
Regression <- lm(Heterozygosity~PercentWolfDNA)
Regression
Correlationtest <- cor.test(PercentWolfDNA, Heterozygosity, 
                method = "pearson")
Correlationtest
install.packages("ggpubr")
library("ggpubr")
ggscatter(Graphdata, x = "WOLF.DNA", y = "RELATIVE.FST", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Amount of Wolf DNA (%)", ylab = "Relative Heterozygosity", main= "Pearson Correlation Test")
MultipleRegression <-read.csv(file.choose(), header=TRUE)
MultipleRegression
PercentWolfDNA <- MultipleRegression$WOLF.DNA
Heterozygosity <- MultipleRegression$RELATIVE.FST
MultipleRegression$State.Province <- as.factor(MultipleRegression$State.Province)
Location <- MultipleRegression$State.Province
MRegression <- lm(Heterozygosity~PercentWolfDNA+Location)
MRegression
cor(PercentWolfDNA, Location, method= "pearson")
plot(MRegression)
summary(MRegression)
