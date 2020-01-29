install.packages ("paleobioDB", dep = T)
setwd("C:\\Users\\harle\\Evolution\\Tasks\\Task_02")
library(paleobioDB)
Taxon <- "Dinosauria"
MinMA <- 66
MaxMA <- 252
fossils <- pbdb_occurrences (base_name = Taxon, show = c ("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
#How many species are known from each time period?
Res <- 5
nspeciesOverTime <- pbdb_richness (fossils, rank = "genus", temporal_extent =c (MaxMA, MinMA), res=Res)
par (mar = c (4, 5, 2, 1), las = 1, tck = -.01, mgp = c (2.5, 0.5, 0))
plot(seq ( to = MaxMA, from = MinMA, length.out = nrow (nspeciesOverTime)), nspeciesOverTime [,2], xlim = c (MaxMA, MinMA), type = "l", xlab = "age (millions of years ago)", ylab = "num. of species", main = Taxon)
newspeciesOverTime <- pbdb_orig_ext (fossils, res=5, rank= "species", temporal_exten = c (MinMA, MaxMA))
par (mar = c (4,5,2,1), las = 1, tck = -0.01, mgp = c (2.5,0.5,0))
plot (seq (to = MaxMA, from = MinMA, length.out = nrow (newspeciesOverTime)), newspeciesOverTime [,1], xlim = c (MaxMA, MinMA), type = "l", xlab = "age (millions of years ago)", ylab = "num. of species", main = Taxon)
lines (seq (to = MaxMA, from = MinMA, length.out = nrow (newspeciesOverTime)), newspeciesOverTime [,2], col = "red")
legend ("topleft", legend = c ("first appear", "go extinct"), col = c ("black", "red"), lty = 1, bty = "n")
#The lines are roughly the same until the time period approaches roughly 80 million years ago, where first appearances dramatically increase.  Dinosaurs were evolving the fastest during the same time period according to the graph.
OceanCol <- "light blue"
LandCol <- "black"
Cols <- c ("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")
par (las = 0)
pbdb_map_richness (fossils, col.ocean = OceanCol, col.int = LandCol, col.rich = Cols)
MinMA <- 201
MaxMA <- 252
triassic_fossils <- pbdb_occurrences (base_name = Taxon, show = c ("phylo", "coords", "ident"), min_ma = MinMA, max_ma = MaxMA)
MinMA <- 145
MaxMA <- 201
jurassic_fossils <- pbdb_occurrences (base_name = Taxon, show = c ("phylo", "coords", "ident"), min_ma = MinMA, max_ma = MaxMA)
MinMA <- 66
MaxMA <- 145
cretaceous_fossils <- pbdb_occurrences (base_name = Taxon, show = c ("phylo", "coords", "ident"), min_ma = MinMA, max_ma = MaxMA)
dev.new (height = 7.8, width = 13)
pbdb_map_richness (jurassic_fossils, col.ocean = OceanCol, col.int = LandCol, col.rich = Cols)
mtext (side = 3, "Cretaceous (145 - 66Ma)", cex = 3, line = -2)
Taxon2 <- "Mammalia"
MinMA <- 66
MaxMA <- 252
fossils2 <- pbdb_occurrences (base_name = Taxon2, show = c ("phylo", "coords", "ident"), min_ma = MinMA, max_ma = MaxMA)
nspeciesOverTime2 <- pbdb_richness (fossils2, rank = "genus", temporal_extent = c (MaxMA, MinMA), res = Res)
par (mar = c (4,5,2,1), las = 1, tck = -0.01, mgp = c (2.5,0.5,0))
Col_dino <- Cols [length (Cols)]
Col_mammal <- Cols[1]
LineWidth <- 2
plot (seq(to = MaxMA, from = MinMA, length.out = nrow (nspeciesOverTime)), nspeciesOverTime[,2], xlim = c (MaxMA, MinMA), type = "l", xlab = "age (millions of years ago)", ylab = "num. of species", col = Col_dino, lwd = LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out = nrow (nspeciesOverTime2)), nspeciesOverTime2[,2], col = Col_mammal, lwd = LineWidth)
legend("topleft", legend = c (Taxon, Taxon2), col = c (Col_dino, Col_mammal), bty = "n", lwd = LineWidth)
Taxon3 <- "Echinodermata"
MinMA <- 100
MaxMA <- 290
fossils2 <- pbdb_occurrences (base_name = Taxon3, show = c ("phylo", "coords", "ident"), min_ma = MinMA, max_ma = MaxMA)
nspeciesOverTime2 <- pbdb_richness (fossils2, rank = "genus", temporal_extent = c (MaxMA, MinMA), res = Res)
Taxon4 <- "Chordata"
MinMA <- 100
MaxMA <- 290
fossils2 <- pbdb_occurrences (base_name = Taxon4, show = c ("phylo", "coords", "ident"), min_ma = MinMA, max_ma = MaxMA)
nspeciesOverTime2 <- pbdb_richness (fossils2, rank = "genus", temporal_extent = c (MaxMA, MinMA), res = Res)
par (mar = c (4,5,2,1), las = 1, tck = -0.01, mgp = c (2.5,0.5,0))
Col_echino <- Cols [length (Cols)]
Col_chorda <- Cols[1]
LineWidth <- 2
plot (seq(to = MaxMA, from = MinMA, length.out = nrow (nspeciesOverTime)), nspeciesOverTime[,2], xlim = c (MaxMA, MinMA), type = "l", xlab = "age (millions of years ago)", ylab = "num. of species", col = Col_dino, lwd = LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out = nrow (nspeciesOverTime2)), nspeciesOverTime2[,2], col = Col_mammal, lwd = LineWidth)
legend("topleft", legend = c (Taxon3, Taxon4), col = c (Col_echino, Col_chorda), bty = "n", lwd = LineWidth)
#My hypothesis is that the echinodermata and chordata growth rates are postively correlated from 200 million years ago to 100 million years ago.