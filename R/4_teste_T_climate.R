
# Credits -----------------------------------------------------------------

# Created by Luara Tourinho 
# (https://github.com/luaratourinho)

# Last update: March 2021


#Loading library

library(tidyverse)
library(raster)
library(dplyr)
library(rgdal)

table <- read.csv("./sp_names_2018.csv",head=T)
sp.names <- as.character(unique(table$sp))
n <-length(sp.names)



# Building table to HM ----------------------------------------------------

Answer2_HM <- matrix(nrow = n, ncol = 4)
colnames(Answer2_HM) <- c("Species", "mean_diff_fut_cur_porcent","t", "p")

for(i in 2:n) {

CUR_cont <- raster(paste0("D:/OneDrive/Documentos/Mestrado/Results_para_LE_2018/Cut_clima_only/"
                          , sp.names[i], "/presente_cont_mask",'.tif'))

MPI8570_cont <- raster(paste0("D:/OneDrive/Documentos/Mestrado/Results_para_LE_2018/Cut_clima_only/"
                              , sp.names[i], "/futuro_cont_mask",'.tif'))

#only for first species
# CUR_bin <- raster(paste0("D:/OneDrive/Documentos/Mestrado/Results_para_LE_2018/Cut_clima_only/"
#                           , sp.names[i], "/pres_bin_mask",'.tif'))
# 
# MPI8570_bin <- raster(paste0("D:/OneDrive/Documentos/Mestrado/Results_para_LE_2018/Cut_clima_only/"
#                               , sp.names[i], "/fut_bin_mask",'.tif'))
# 
# mask <- MPI8570_bin + CUR_bin

# mask[mask != 0] <- 1
# mask[mask == 0] <- NA
# mask[mask >= 0] <- 1
#poly <- rasterToPolygons(mask, dissolve = T)
# mask_2 <- crop(mask, poly)
# mask_2 <- mask(mask_2, poly)
# 
# writeRaster(mask_2, filename = paste0("D:/OneDrive/Documentos/Mestrado/Results_para_LE_2018/mask/",
#                                       sp.names[i], "_mask",'.tif'),
#             format="GTiff", overwrite=TRUE)

# error:
# mask_2 <- readOGR(paste0("D:/OneDrive/Documentos/Mestrado/Results_para_LE_2018/pol_sum/",
#                          sp.names[i], ".shp"))

CUR_cont2 <- crop(CUR_cont, mask_2)
CUR_cont <- mask(CUR_cont2, mask_2)
MPI8570_cont2 <- crop(MPI8570_cont, mask_2)
MPI8570_cont <- mask(MPI8570_cont2, mask_2)

testt <- t.test(MPI8570_cont[], CUR_cont[], paired=T)
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_porcent <- mean_diff_fut_cur*100
p <- testt$p.value
t <- testt$statistic

Answer2_HM[i, ] <- c(sp.names[i], mean_diff_fut_cur_porcent, t, p)

}

table <- rbind(Answer2_HM, Answer2_HM2, Answer2_HM3)
