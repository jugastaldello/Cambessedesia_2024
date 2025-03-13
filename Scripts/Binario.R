
########################################

library(sdm)
library(dismo)
library(raster)
library(maptools)
library(mapview)
library(magrittr)
library(dplyr)
library(usdm)
library(OpenImageR)
library(rJava)


data(wrld_simpl)

cambess<- read.csv("Dados/Todas as spp.csv")
cambess<- cambess[ ,3:5]


geo <- cambess[cambess$scientificname %in% names(which(table(cambess$scientificname) > 3)), ]
species <- names(which(table(geo$scientificname)!=0))


# Loading rasters #
wd <- "C:/Cambessedesia R/Resultados/Distribution models/Raster"
setwd(paste(wd))

list.files(pattern="tif$") -> files
sapply(files, raster, simplify = F) -> current
sub("_current.tif", "", files) -> labs

setwd(paste(wd))


# establishing thresholds
thresholdC = 0.5


sp0 = species[]
sp1 <- geo[geo$scientificname==sp0,]

Sys.time() -> start_time_total
for(u in 1:length(current)){
  Sys.time() -> start_time
  current[[u]]-> curr.bin
  
  curr.bin[current[[u]][] < thresholdC] <- 0
  curr.bin[current[[u]][] >= thresholdC] <- 1
  curr.bin[curr.bin[] == 0] <- NA
  
  if (is.na(which.min(curr.bin))==T) {
    print(c(labs[u], "sorry, no range...")) 
  }
  else {
    
    
    pdf(file=paste(labs[u],".tif", sep="_"), width = 15, height=10)
    plot(curr.bin, col="green", legend=F, xlim=c(-60,-30), ylim=c(-30,5))
    plot(wrld_simpl, add=T, xlim=c(-60,-30), ylim=c(-30,5))
    plot(sp1, pch=19, cex=0.1, col="tomato1", add=T, xlim=c(-60,-30), ylim=c(-30,5))
    title(main=paste(labs[u], "current distribution", sep=" "))
    
    dev.off() 
    # plot polygon
    
    
    rasterToPolygons(curr.bin, dissolve=T) -> rangeC 
    
    if (is.null(rangeC)==T) {
      print(c(labs[u], "sorry, no range..."))
    }
    else {
      
      # shape
      writeSpatialShape(rangeC, paste(labs[u], "_current", sep=""))
      
      # total area in each time slice
      
      print(c(labs[u], "done!"))
      Sys.time() -> end_time
      print(end_time-start_time)
      
    }
  }
}  
#########################################################

