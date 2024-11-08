
rm(list=ls())
library(picante)
library(caper)
library(ape)

list.files("data")
#Lendo a lista de 100 filogenias
tree<-read.nexus("data/100_uncertainty_trees_nexus")


# loop da dissertação de Emily K. Brantner(2015) adaptado para 100 filogenias de Cambessedesia

temp.list<-NULL
temp.ed<-NULL
spp.ed<-NULL
output.ed<-NULL

 temp.list<-ed.calc(tree[[1]])
 names<-temp.list$spp
 names<-names$species

 for (i in 1:100){
   temp.list<-ed.calc(tree[[i]])
   spp.ed<-temp.list$spp
   temp.ed<-spp.ed$ED
 output.ed<-cbind(output.ed,temp.ed) 
 }

#salvando os ED 
write.csv(output.ed,file="Results/ED_uncertainty.csv")

#Calculando a média dos valores
meanED<-rowMeans(output.ed)
meanED

#Salvando 
write.csv(meanED,file="results/Mean_ED.csv")
