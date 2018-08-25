# Given a list of coordinates set up polygons for claimable areas, POIs, and out of play zones
# Nathan Pratt 
# 8/22/18
# This one shouldn't be run as is, I mostly used this for testing.
# This work should be done from the console using the functions from "conanFx.r" as needed. Examples below.

requiredPkgs = c("stringr", "raster", "grid", "png")
missingPkgs = requiredPkgs[!(requiredPkgs %in% installed.packages()[,"Package"])]
if(length(missingPkgs)) install.packages(missingPkgs)
library(stringr)
library(raster)
library(grid)
library(png)
source("conanFx.r")
rowNames = LETTERS[1:20]
colNames = seq(from = 0, to = 24)
imgDark = readPNG("Images/T_FullscreenMap_NO-CLAIM.png")

# Replace coords with points of polygons
rast = brick(imgDark)
extent(rast) = extent(x = -0.5, xmax = 24.5, ymin = 0.5, ymax = 20.5)


plotImg(rast)


coord = paste0("E", 4)
plotZoomedImage(rast, coord)
e = GetRoundedClickPoints(80)
e # validate coords


# Modify polygons
# claim and outof play only
claimCorners = c(3)
OutOfPlayCorners = c(2,1,4)
claimableCoords[[coord]]$Polygons[[1]] = rbind(claimableCoords[[coord]]$Polygons[[1]][claimCorners,], e)
OutOfPlayCoords[[coord]]$Polygons[[1]] = rbind(OutOfPlayCoords[[coord]]$Polygons[[1]][OutOfPlayCorners,], e)


#poiCorners = c()
#poiCoords[[coord]]$Polygons[[1]] = e2

#verify
sapply(coord, function(x) polygon(claimableCoords[[x]]$Polygons[[1]], col = adjustcolor("blue", alpha.f = 0.3), border = NA))
sapply(coord, function(x) polygon(OutOfPlayCoords[[x]]$Polygons[[1]], col = adjustcolor("red", alpha.f = 0.3), border = NA))

sapply(coord, function(x) polygon(poiCoords[[x]]$Polygons[[1]], col = adjustcolor("white", alpha.f = 0.3), border = NA))


e1 = GetRoundedClickPoints()
e2 = GetRoundedClickPoints()
e3 = GetRoundedClickPoints()


claimableCoords[[coord]]$Polygons[[1]] = rbind(claimableCoords[[coord]]$Polygons[[1]][2,], e1, e2)
OutOfPlayCoords[[coord]]$Polygons[[1]] = rbind(OutOfPlayCoords[[coord]]$Polygons[[1]][c(4,1),], e1)
poiCoords[[coord]]$Polygons[[1]] = rbind(poiCoords[[coord]]$Polygons[[1]][3,], e2)


#Save obj
#saveRDS(list(Claimable = claimableCoords, OutofPlay = OutOfPlayCoords, POI = poiCoords), "ConanMap/cmMapCoordsFinal.rds")
#tmpList = readRDS("MetaData/cmMapCoordsFinal.rds")
#claimableCoords = tmpList$Claimable
#OutOfPlayCoords = tmpList$OutofPlay
#poiCoords = tmpList$POI