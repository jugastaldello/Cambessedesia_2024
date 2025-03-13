
#########
wd <- "C:/Users/naja-/Desktop/Mestrado/Cambessedesia/Cambessedesia R/Distribution_models"
setwd(wd)
dir()

rm(list=ls())

library(sdm)
library(dismo)
library(raster)
library(shiny)
library(CoordinateCleaner)
library(maptools)
library(mapview)
library(magrittr)
library(dplyr)
library(usdm)
library(OpenImageR)
library(rJava)


data(wrld_simpl)


#Getting the distribution points

cambess<- read.csv("Todas as spp. - Página2.csv")

cambess<- cambess[ ,3:5] #Apenas as colunas de 3 a 5
head(cambess)

#Conferindo a distribuição
plot(wrld_simpl, xlim=c(-60,-30), ylim=c(-30,5), axes=TRUE, col="light yellow", las=1, main="Distribuição de Cambessedesia")
box()
points(cambess$longitude, cambess$latitude, col="orange", pch=20, cex=0.75)


####### Modelagem ###
#Chamando as variáveis
bio <- raster::getData('worldclim', var='bio', res=2.5)

#Cortando os rasters
biocr <- crop(bio, extent(-60, -30, -30, 5))

plot(biocr, xlim=c(-60,-30), ylim=c(-30,5))

#Teste de colinearidade

v1<-vifstep(biocr)
v2<-vifcor(biocr,th=0.7) #threshold 0.7
biom <- exclude(biocr,v2) # exluindo variáveis
cambesvar <-stack(biom) #stack de variáveis preditoras para Cambessedesia

plot(biom, xlim=c(-60,-30), ylim=c(-30,5)) 


#Criando os modelos
#na etapa abaixo "geo<-....", C. striatela ficará de fora da modelagem, por só ter apenas 1 registro, e aqui contamos > 3 
geo <- cambess[cambess$scientificname %in% names(which(table(cambess$scientificname) > 3)), ]
species <- names(which(table(geo$scientificname)!=0))

Sys.time() -> start_time_total

for(i in 1:length(species)){
  Sys.time() -> start_time
  sp0 = species[i]
  sp1 <- geo[geo$scientificname==sp0,]
  sp1$scientificname <- 1
  coordinates(sp1) <- ~ longitude + latitude
  

  d <- sdmData(scientificname~., sp1, predictors = cambesvar, bg = list(n=1000)) # 1000 background points 
  m <- sdm(scientificname~., d, methods =c("maxent"),        # escolhendo o método: Maxent
           replication=c('boot'), n=10)  # 10 replications 
  
  Stat <- getEvaluation(m,stat=c('AUC'),opt=1, file=paste(sp0, "Cambessev.csv", sp="")) # getting the evaluation AUC for each model of each species
  write.csv(Stat, file=paste(sp0, "Cambessev.csv", sp=""))
  
  x <- getVarImp(m, id=1, wtest="test.dep")  # getting the relative importance of each variable per species
  write.table(data.frame(), file=paste0(sp0, "_varimp.txt"))
  sink(paste0(sp0, "_varimp.txt"))
  print(x)
  sink()
  plot(x,'auc')
  
  en <- ensemble(m, cambesvar, paste(sp0, "_ensemble.tif", sep="") ,
                 setting=list(method='weighted',stat=c("AUC")))      # ensemble ("fusion model")
  
 
  #ploting pdfs:  roc curve (AUC), ensemble models and variable importance (graph) 
  pdf(file=paste(sp0, "_roc", ".pdf", sep=""), width = 8, height = 5)
  roc(m, 1)
  title("maxent model", adj=0, add=T)
  dev.off()
  
  pdf(file=paste(sp0, "_sdm", ".pdf", sep=""), width = 10, height = 5)
  plot(en, zlim=c(0,1))
  plot(sp1, pch=19, cex=0.1, col="tomato1", add=T)
  title(main=paste(sp0, "current distribution", sep=" "))
  dev.off()
  
  pdf(file=paste0(sp0, "_getvarimp", ".pdf"), width = 3, height = 3)
  plot(x, 'cor')
  title("cor")
  plot(x, 'auc')
  title("auc")
  dev.off()
  
  Sys.time() -> end_time
  print(c(species[i], "done!"))
  print(end_time-start_time)
}
Sys.time() -> end_time_total
print(end_time_total-start_time_total)
