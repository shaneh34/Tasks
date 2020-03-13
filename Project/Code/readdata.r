Input <- "C:\\Users\\harle\\Evolution\\Tasks\\Project\\Data"
setwd(Input)


library(snpStats)

test <- read.pedfile("Heppenheimer_et_al_Ecol_Evol_2018.ped", snps = "Heppenheimer_et_al_Ecol_Evol_2018.map")


CoyoteGenotypes <- test$genotype@.Data