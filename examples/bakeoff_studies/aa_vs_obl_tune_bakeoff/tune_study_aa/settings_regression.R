# Author: Paul Nguyen
# Date: July 30, 2024
# Purpose: create file to set simulation setting
# Details: 
# Dependencies:

# model index
m = c(
  "xgboost",
  "extra_trees",
  "rand_forest"
)

# dataset
d = c("abalone", "ais", "amenity", "attend", "baseball", "basketball", "boston",
      "budget", "cane", "cpu", "diabetes", "diamonds", "edu", "labor", "mpg",
      "rice", "servo", "strikes")

# test set
s = 1:20

# save settings
settings = expand.grid(model = m, data = d, split = s)
