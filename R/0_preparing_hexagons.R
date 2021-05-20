

# Credits -----------------------------------------------------------------

# Created by Jayme A. Prevedello and Luara Tourinho 
# (http://lattes.cnpq.br/7678496109580941)
# (https://github.com/luaratourinho)

# Last update: March 2021


#Loading library

library(sp)
library(rgdal)
library(raster)
library(reshape)
library(gdistance)
library(rgeos)
library(maptools)
library(parallel)
library(foreach)
library(doParallel)


# Projecting SOS MA 

# atlantic_forest <- readOGR("./SOS_MA", "MA_original")
# crs.GRS80 <- CRS("+proj=longlat +ellps=GRS80 +no_defs ")
# proj4string(atlantic_forest) <- crs.GRS80
 crs.albers <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")
# atlantic_forest2 <- spTransform(atlantic_forest, crs.albers)
# writeOGR(atlantic_forest2, dsn = "./SOS_MA" , layer = "SOS_MA_albers", driver = "ESRI Shapefile")


# Importing species names 

sp.d <- read.csv("./dispersal_ability.csv")
sp.names <- read.csv("./dispersal_ability.csv")
sp.names <- sp.names$spp

# Intersection between hexagons and SOS_MA

atlantic_forest2 <- readOGR("./SOS_MA", layer = "SOS_MA_albers")

hex_inside <- readOGR("./paisagem_dentro", sp.names[i])
hex_inside <- spTransform(hex_inside, crs.albers)
# plot(hex_inside)

atlantic_forest2 <- crop(atlantic_forest2, extent(hex_inside))
# plot(atlantic_forest2)
# plot(hex_inside, add=T)

clip <- gIntersection(hex_inside, atlantic_forest2, byid = T, drop_lower_td = F)
plot(clip)
summary(clip)

test <- intersect(clip, hex_inside)
test

unique <- unique(test@data$id)

for (i in 1:length(unique)) {
  tmp <- test[test$id == unique[i], ]
  dir.create(paste0("./hex_SOSMA_per_species/", sp.names[i]))
  writeOGR(tmp, dsn= paste0("./hex_SOSMA_per_species/", sp.names[i]), unique[i], driver="ESRI Shapefile",
           overwrite_layer=TRUE)
}

