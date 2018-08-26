# Script to modify Claim matrix for Conan Map claiming system. 
# Might fail if you give it incorrect commands. I need to take the time to put in better error handling
# Nathan Pratt
# 8/23/18

library(stringr)
source("conanFx.r")

ar = readRDS(claimMatrixFile)
tmpList = readRDS("MetaData/cmMapCoordsFinal.rds")
claimableCoords = tmpList$Claimable
OutOfPlayCoords = tmpList$OutofPlay
poiCoords = tmpList$POI
options(warn = 1)
mkChanges = function(ar, claimableCoords){
  cont = T
  while(cont){
    input = readline("What would you like to do?\na = Add claim\nr = Remove claim\nra = Remove all claims for a clan\nf = Finished\n")
    if (tolower(input) == "a"){
      input = readline("Enter the Clan receiving this claim: ")
      clan = input
      cancel = F
      if (!(clan  %in% dimnames(ar)[[3]])){
        loop = T
        while(loop){
          currClans = dimnames(ar)[[3]][-1]
          print(paste(0:length(currClans), c(clan, currClans), collapse = ", ", sep = ": "))
          input = readline(paste0(clan, " not found in current clans.\nWould you prefer to select a clan that already exists?\nEnter index or '0' to use ", clan, ".\n"))
          numInput = as.numeric(input)
          if (input == "0"){
            loop = F
            ar = AddClanToMapArray(ar, clan)
          } else if (numInput %in% 1:length(currClans)){
            loop = F
            clan = currClans[numInput]
          } else if (input == "c"){
            cancel = T
          } else {
            warning("Input not recognized as valid index.")
          }
        }
      }
      input = readline("Enter coordinates (separate with space or ','): ")
      coords = str_match_all(input, "\\w+")[[1]][,1]
      ar = ConvertArrayValuesToT(ar, coords, clan, claimableCoords)
    } else if (tolower(input) == "r"){
      input = readline("Enter the coords to neutralize (Separate with space or ','): ")
      coords = str_match_all(input, "\\w+")[[1]][,1]
      ar = ConvertArrayValuesToT(ar, coords, "None", claimableCoords, overwrite = T)
    } else if (tolower(input) == "ra"){
      input = readline(paste0(paste0(dimnames(ar)[[3]][-1], collapse = ", "), "\n", "Enter clan name to remove: "))
      if (!(input %in% dimnames(ar)[[3]])){
        warning(paste0(input, " not found in array."))
      } else {
        ar = ar[,,dimnames(ar)[[3]] != input]
      }
    } else if (tolower(input == "f")){
      cont = F
    }
    print("Success")
  }
  return(ar)
}
promptSave = function(ar){
  input = readline("Would you like to save your changes? (y/Y): ")
  if (tolower(input) == "y"){
    saveRDS(ar, claimMatrixFile)
    print("Save complete.")
  } else {
    print("Discarding changes...")
  }
}
ar = mkChanges(ar, claimableCoords)
promptSave(ar)
options(warn = 0)