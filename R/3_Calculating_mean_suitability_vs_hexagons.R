
# Credits -----------------------------------------------------------------

# Created by Jayme A. Prevedello and Luara Tourinho 
# (http://lattes.cnpq.br/7678496109580941)
# (https://github.com/luaratourinho)

# Last update: March 2021


#Loading library

library(sp)
library(rgdal)
library(raster)

sp.names <- read.csv("./dispersal_ability.csv")
sp.names <- sp.names$spp
sp.n <- sp.names[1]


# hexagonos dentro - 47
hex_dentro <- readOGR("./paisagem_dentro", sp.names[7])
plot(hex_dentro)



# hexagonos com floresta - 45
# dsn_shapelist <- './hexagonos_amazona'
# # Creating list with all landscape shapefiles
# shapelist <- as.list(ogrListLayers(dsn_shapelist))#lista dos shapes existentes na pasta dsn
dsn_shapelist <- paste0("./hex_SOSMA_per_species/", sp.names[7])
shapelist <- as.list(ogrListLayers(dsn_shapelist))#lista dos shapes existentes na pasta dsn


nomes_hexagonos_com_floresta <- as.numeric(unlist(shapelist)) # ids dos hexagonos

hexagonos_com_floresta <- which(hex_dentro$id%in%nomes_hexagonos_com_floresta)

hex_dentro_com_floresta <- hex_dentro[hexagonos_com_floresta,3]
plot(hex_dentro_com_floresta)


# presente
suitability_current <- raster(paste0("./ensemble_proj/", sp.n,"_current_cont", ".tif"))


t <- extract(suitability_current, hex_dentro_com_floresta) # extrai a adeq de cada pixel de cada hexagono
resultado <- unlist(lapply(t, function(x) mean(x, na.rm=T))) # adeq media em cada hexagono

write.table(resultado, paste0("~/mestrado/Nodes_dists_PC/", sp.names[7], "/Adeq_media_por_hexagono_current.txt"))



# futuro
suitability_future <- raster(paste0("./ensemble_proj/", sp.n,"_future_cont", ".tif"))

t <- extract(suitability_future, hex_dentro_com_floresta) # extrai a adeq de cada pixel de cada hexagono
resultado <- unlist(lapply(t, function(x) mean(x, na.rm=T))) # adeq media em cada hexagono

write.table(resultado, paste0("~/mestrado/Nodes_dists_PC/", sp.names[7], "/Adeq_media_por_hexagono_future.txt"))

