# Author: Ryan Yee
# Date: July 10, 2024
# Purpose: get metadata / diagnostics on benchmarking data
# Details: 
# Dependencies:

dir = "data/"

# datasets
dsets = c("abalone", "ais", "amenity", "attend", "baseball", "basketball", "boston",
      "budget", "cane", "cpu", "diabetes", "diamonds", "edu", "labor", "mpg",
      "rice", "servo", "strikes")

# # loads .RData object
# loadRData <- function(filename) {
#   load(filename)
#   get(ls()[ls() != "filename"])
# }

# get n
n_obs = rep(NA, times = length(dsets))
for (d in 1:length(dsets)){
  load(paste0(dir, dsets[d], "_data.RData"))
  n_obs[d] = length(Y)
}
max(n_obs)
min(n_obs)

# get p
preds = rep(NA, times = length(dsets))
for (d in 1:length(dsets)){
  load(paste0(dir, dsets[d], "_data.RData"))
  preds[d] = ncol(X_cont) + ncol(X_cat)
}
max(preds)
min(preds)

