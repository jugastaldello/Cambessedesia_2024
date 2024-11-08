
library(picante)

tree<-read.tree("Resultados/Final_tree.txt")
plot(tree, cex=0.5)
axisPhylo()

ED<- evol.distinct(tree, type = "fair.proportion", scale = FALSE, use.branch.lengths = TRUE)

write.csv(ED, file ="Evol_distinctiveness.csv", row.names =FALSE)
