# Load packages
library(picante)

# read tree
tree <- read.tree("Data/Final_tree.txt")

# plot tree
plot(tree, cex = 0.5)
axisPhylo()

# Evolutionary distinctiveness
ED <- evol.distinct(tree,
                    type = "fair.proportion",
                    scale = FALSE,
                    use.branch.lengths = TRUE)
# Save results
write.csv(ED, file = "Results/Evol_distinctiveness.csv", row.names = FALSE)
