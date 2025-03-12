# Script to map edges and richness
# Packages
library(letsR)
library(terra)
library(maps)
library(sf)
library(rnaturalearth)
library(ggmap)

# load shapefiles
files_list <- list.files("Data/Binary_maps/", full.names = TRUE)

shps <- list()

for (i in 1:length(files_list)) {
  temp <- as.polygons(rast(files_list[i]), dissolve = TRUE, values = TRUE)
  shps[[i]] <- temp[2, ]
}


shape_all <- terra::vect(shps, type = "polygons")
shps_names <- paste0("Cambessedesia_", gsub("_bi\\.tif", "", list.files("Data/Binary_maps/")))
shape_all$binomial <- shps_names

# Plot shapefiles
colors <- rainbow(length(unique(shape_all$binomial)), alpha = 0.5)
position <- match(shape_all$binomial, shape_all$binomial)
colors <- colors[position]

plot(shape_all,
     col = colors,
     lty = 0,
     main = "Spatial polygons of Phyllomedusa")
map(add = TRUE)

# Make PAM
PAM <- lets.presab(
  shape_all,
  xmn = -61,
  xmx = -36,
  ymn = -25,
  ymx = 6,
  res = .1
)
plot(PAM)


# Map edge values
edge_data <- read.csv("Data/edge2_mean_values.csv")
brazil <- st_read("Data/UFEBRASIL.shp")

map_edge <- lets.maplizer(PAM,
                          edge_data$EDGE2,
                          edge_data$Species,
                          ras = TRUE,
                          func = sum)

map_edge1 <- lets.maplizer(PAM,
                           edge_data$EDGE,
                           edge_data$Species,
                           ras = TRUE,
                           func = sum)
plot(map_edge$Raster)

# Map edge and richness in a ggplot 
map.edge <- cbind(crds(map_edge$Raster), na.omit(values(map_edge$Raster)))
map.edge1 <- cbind(crds(map_edge1$Raster), na.omit(values(map_edge1$Raster)))

map.rich <- cbind(crds(PAM$R), na.omit(values(PAM$R)))
df2 <- data.frame(map.edge)
df3 <- data.frame(map.rich)
df4 <- data.frame(map.edge1)

df3$lyr.1[df3$lyr.1 == 0] <- NA
# df4$lyr.1[df4$lyr.1 == 0] <- NA

#Make appropriate column headings
colnames(df2) <- c("Longitude" , "Latitude" , "MAP")
colnames(df3) <- c("Longitude" , "Latitude" , "MAP")
colnames(df4) <- c("Longitude" , "Latitude" , "MAP")

# df4 <- rbind(df2, df3)
# df4$variable <- c(rep("EDGE2", nrow(df2)),
#                   rep("Richness", nrow(df3)))

EDGE1 <- ggplot(data = df4) +
  geom_sf(
    data = brazil,
    color = 'gray',
    fill = 'gray',
    size = 0.5
  ) +
  geom_tile(aes(fill = MAP, y = Latitude, x = Longitude)) +
  geom_sf(
    data = brazil,
    color = 'white',
    fill = NA,
    size = 0.5
  ) +
  
  theme_bw() +
  coord_sf(ylim = c(-25, -2), xlim = c(-57, -35)) +
  scale_fill_viridis_c(name = "EDGE", na.value = "transparent")

EDGE2 <- ggplot(data = df2) +
  geom_sf(
    data = brazil,
    color = 'gray',
    fill = 'gray',
    size = 0.5
  ) +
  geom_tile(aes(fill = MAP, y = Latitude, x = Longitude)) +
  geom_sf(
    data = brazil,
    color = 'white',
    fill = NA,
    size = 0.5
  ) +
  
  theme_bw() +
  coord_sf(ylim = c(-25, -2), xlim = c(-57, -35)) +
  scale_fill_viridis_c(name = "EDGE2", na.value = "transparent")
Richness <- ggplot(data = df3) +
  geom_sf(
    data = brazil,
    color = 'gray',
    fill = 'gray',
    size = 0.5
  ) +
  geom_tile(aes(fill = MAP, y = Latitude, x = Longitude)) +
  geom_sf(
    data = brazil,
    color = 'white',
    fill = NA,
    size = 0.5
  ) +
  
  theme_bw() +
  coord_sf(ylim = c(-25, -2), xlim = c(-57, -35)) +
  scale_fill_viridis_c(name = "Richness", na.value = "transparent")


all <- gridExtra::grid.arrange(EDGE1, EDGE2, Richness)
ggsave(
  "Figures/all_edge_maps.tiff",
  all,
  height = 20,
  width = 10,
  units = "cm"
)
