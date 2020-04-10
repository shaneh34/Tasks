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
Correlationtest <- cor.test(PercentWolfDNA, Heterozygosity, 
                method = "pearson")
Correlationtest
install.packages("ggpubr")
library("ggpubr")
ggscatter(Graphdata, x = "WOLF.DNA", y = "RELATIVE.FST", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent Wolf DNA", ylab = "Relative Heterozygosity")
#To do list
#1 Run a regression analysis to look for correlation.
#2 Create a phylogeny tree of the coyotes with the data.