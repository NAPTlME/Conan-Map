# Supporting functions for use in the Conan Map project for Solitude's Server
# Nathan Pratt
# 8/17/18

library(stringr)
#library(abind)
library(reshape2)
#library(grid)
#library(raster)
# Set variable for array file
claimMatrixFile = "MetaData/mapMatrixBaseInfo.rds"

GetDfFromArray = function(mapArray){
  # Get first owner found in matrix ### should only ever be one, but there's no reason to trust
  tmpArray = array(apply(mapArray, c(1, 2), function(x) 
    if(sum(as.numeric(x)) > 0){ 
      names(x)[which(as.numeric(x) == 1)[1]] 
    } else { 
      "None" 
    }), 
    dim = dim(mapArray), dimnames = list(dimnames(mapArray)[[1]], dimnames(mapArray)[[2]]))
  return(melt(tmpArray))
}
CreateBasePolygon = function(coords){
  startX = numeric(0)
  startY = numeric(0)
  endX = numeric(0)
  endY = numeric(0)
  for(coord in coords){
    coordY = which(LETTERS == str_match(toupper(coord), "[A-Z]")[1,1])
    coordX = as.numeric(str_match(coord, "[\\d\\-]+")[1,1])
    startX = c(startX, coordX - 0.5)
    endX = c(endX, coordX + 0.5)
    startY = c(startY, coordY - 0.5)
    endY = c(endY, coordY + 0.5)
  }
  returnVal = as.matrix(expand.grid(c(min(startY), max(endY)), c(min(startX), max(endX))))
  dimnames(returnVal)[[2]] = c("Y", "X")
  returnVal = returnVal[c(1,2,4,3), 2:1] # makes coords start in bottom left and go clockwise
  return(returnVal)
}