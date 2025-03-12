# Load Packages
library(picante)
library(caper)
library(ape)

# Load 100 phylogenies
tree <- read.nexus("data/100_uncertainty_trees_nexus")

# Calculate ED for each tree
# Adapted from Emily K. Brantner(2015) 

temp.list <- NULL
temp.ed <- NULL
spp.ed <- NULL
output.ed <- NULL

temp.list <- ed.calc(tree[[1]])
names <- temp.list$spp
names <- names$species

for (i in 1:100) {
   temp.list <- ed.calc(tree[[i]])
   spp.ed <- temp.list$spp
   temp.ed <- spp.ed$ED
   output.ed <- cbind(output.ed, temp.ed)
}

# Save Results 

write.csv(output.ed, file = "Results/ED_uncertainty.csv")

# Get average ED for each species
meanED <- rowMeans(output.ed)

# Save average ED for each species 
write.csv(meanED, file = "Results/Mean_ED.csv")
