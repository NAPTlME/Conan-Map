# Setup for empty Conan Claim Matrix
# Warning, will overwrite current claim array, use with caution.
# Nathan Pratt
# 8/18/18
requiredPkgs = c("dplyr", "reshape2")
missingPkgs = requiredPkgs[!(requiredPkgs %in% installed.packages()[,"Package"])]
if(length(missingPkgs)) install.packages(missingPkgs)
library(dplyr)
library(reshape2)

source("conanFx.r")

rowNames = LETTERS[1:20]
colNames = seq(from = 0, to = 24)


if (file.exists(claimMatrixFile)){
  file.remove(claimMatrixFile)
}
ar = CreateEmptyMapArray(rowNames, colNames, "None")
ar[ar == 0] = 1
# save map
saveRDS(ar, claimMatrixFile)
