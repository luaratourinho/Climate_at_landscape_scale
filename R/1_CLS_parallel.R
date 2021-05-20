

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


# To paralell
detectCores()
getDoParWorkers()
cl <- parallel::makeCluster(9, outfile="./outfile.log")
registerDoParallel(cl)
getDoParWorkers()


# Importing species names 

sp.d <- read.csv("./dispersal_ability.csv")
sp.names <- read.csv("./dispersal_ability.csv")
sp.names <- sp.names$spp

# Defining path to all landscape shapefiles (after split)

dsn_shapelist <- paste0("./hex_SOSMA_per_species/", sp.names[1:71])
# Creating list with all landscape shapefiles
shapelist <- as.list(ogrListLayers(dsn_shapelist))



# Calculating nodes and distances

ini = Sys.time()
for(sp.n in sp.names[1:71]){
  
  # first run all for present, after run all script for future
  # Climate suitability layer have to be in Albers projection
  suitability <- raster(paste0("./ensemble_proj/", sp.n,"_current_cont", ".tif"))
  # suitability <- raster(paste0("./ensemble_proj/", sp.n,"_future_cont", ".tif"))
  
  ################################################################
  ##Script for creating distances and nodes data for one or several landscape shapefiles
  
  
  #Creating some objects to be used during the loop
  shapes <-vector(mode="list", length= length(shapelist))
  nodes <- NULL
  dists <- NULL
  dists_melt <- NULL
  nlinhaso <- NULL
  dimpesos <- NULL
  posicoes <- NULL
  pesos <- NULL
  nlinhaso <- NULL
  dimpesos <- NULL
  pesos2 <- NULL
  distancia <- NULL
  
  
  
  #####
  #loop to import shapefiles
  #####
  system.time( #Monitoring the time of the loop
    foreach(i = 1:length(shapelist), .packages = c("rgdal", "raster", "sp", 
                                                   "rgeos", "reshape")) %dopar% {
                                                     #for( i in 1:length(shapelist)){
                                                     #Importing shapefiles to a list
                                                     shapes[[i]] <-  readOGR(dsn=dsn_shapelist, layer=shapelist[[i]])
                                                     #Adding a new column and calculating the area of each polygon
                                                     shapes[[i]]$area <- round(gArea(shapes[[i]], byid=TRUE),4)
                                                     #creating node matrix to have values inserted
                                                     
                                                     nodes[[i]]<-as.data.frame(matrix(c(1:length(shapes[[i]]), 
                                                                                        shapes[[i]]$area), nrow=length(shapes[[i]])))
                                                     # Calculating mean suitability of each fragment
                                                     cents <- coordinates(shapes[[i]]) 
                                                     # Calculating the centroid of each fragment - faster
                                                     suit <- extract(suitability, cents) 
                                                     # Calculating the mean suitability of the fragment - too slow
                                                     #suit <- extract(suitability, shapes[[i]], fun=mean, na.rm=TRUE, method="simple", weights=F) 
                                                     
                                                     nodes[[i]][,3] <- as.vector(suit)
                                                     nodes[[i]][,4] <- nodes[[i]][,2]*nodes[[i]][,3]
                                                     nodes[[i]] <- nodes[[i]][,-2:-3]
                                                     
                                                     
                                                     # writing tables with the IDHEX in the name of the text file
                                                     # write.table(nodes[[i]], file=paste(
                                                     #   "./output_land/", sp.n,"/", nome[[1]][1], "/", 
                                                     #   "nodes_",i, "_cu.txt", sep=""), 
                                                     #   row.names = F, col.names = F,    sep="\t", quote=F)
                                                     write.table(nodes[[i]], file=paste("nodes_",i, "_cu.txt", sep=""), 
                                                                 row.names = F, col.names = F,    sep="\t", quote=F)
                                                     
                                                     
                                                     
        
                                                     
                                                     if (length(shapes[[i]]$area) > 1) {
                                                       #Calculating distances between each polygon
                                                       dists[[i]] <-
                                                         gDistance(shapes[[i]], shapes[[i]], byid = T)
                                                       
                                                       dists_melt[[i]] <-
                                                         melt(dists[[i]])[melt(upper.tri(dists[[i]]))$value,]
                                                       dists_melt[[i]][, 3] <-
                                                         round(dists_melt[[i]][, 3], 10)
                                                       #organizing distance data to be exported as text file
                                                       posicoes[[i]] <-
                                                         data.frame(c(1:length(shapes[[i]])))
                                                       pesos[[i]] <-
                                                         data.frame(matrix(NA, nrow = 1, ncol = 2))
                                                       
                                                       for (o in 1:(length(shapes[[i]]) -
                                                                    1))
                                                       {
                                                         nlinhaso[[i]] <- (dim(posicoes[[i]])[1]) - o
                                                         dimpesos[[i]] <-
                                                           dim(pesos[[i]])[1]
                                                         pesos[[i]][(1 + dimpesos[[i]]):(dimpesos[[i]] +
                                                                                           nlinhaso[[i]]), 1] <-
                                                           o
                                                         pesos[[i]][(1 + dimpesos[[i]]):(dimpesos[[i]] +
                                                                                           nlinhaso[[i]]), 2] <-
                                                           ((o + 1):dim(posicoes[[i]])[1])
                                                       }
                                                       
                                                       pesos2[[i]] <-
                                                         (pesos[[i]][2:dim(pesos[[i]])[1],])
                                                       distancia[[i]] <-
                                                         cbind(pesos2[[i]], dists_melt[[i]][, 3])
                                                       
                                                     } else
                                                       (distancia[[i]] <-
                                                          data.frame(x = 1, y = 1, z = 0))
                                                     
                                                     #writting distance results in text files
                                                     write.table(
                                                       distancia[[i]],
                                                       file = paste("dists_", i, "_cu.txt", sep = ""),
                                                       row.names = F,
                                                       col.names = F,
                                                       sep = "\t",
                                                       quote = F
                                                     )
                                                     print(i)
                                                     
                                                     
                                                     
                                                   }, gcFirst = TRUE)
  
  
  # Calculating CLS
  
 shell(
    paste0(
      "coneforWin64.exe ",
      "-nodeFile nodes_ ",
      "-conFile dists_ ",
      "-* ",
      "-t dist notall ",
      "-confProb ",
      sp.d[grep(sp.n, sp.d$spp),3],
      " 0.5 ",
      "-PC onlyoverall"
    )
  )

  # Saving in the right directory
  
  dir.create(paste0("/./Nodes_dists_cu/", sp.n))
  current_folder <- "."
  target_dir <- paste0("/./Nodes_dists_cu/", sp.n)
  
  
  # Clean the files from getwd() directory
  
  file <- list.files(current_folder, ".txt")
  for(i in 1:length(file)){
    file=dir(path= ".", paste0( "nodes_", i, "_cu.txt"))
    file.copy(file.path(current_folder, file), target_dir)
  }
  
  file <- list.files(current_folder, ".txt")
  for(i in 1:length(file)){
    file=dir(path= ".", paste0( "dists_", i, "_cu.txt"))
    file.copy(file.path(current_folder, file), target_dir)
  }
  
  file <- list.files(current_folder, ".txt")
  for(i in 1:length(file)){
    file2=dir(path= ".", paste0( "nodes_",i, "_cu.txt"))
    file.remove(file2)
  }
  
}

