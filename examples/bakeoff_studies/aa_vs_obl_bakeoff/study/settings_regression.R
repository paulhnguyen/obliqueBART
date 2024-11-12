# Author: Paul Nguyen
# Date: July 30, 2024
# Purpose: create file to set simulation setting
# Details: 
# Dependencies:

# model index
m = c(
  "wbart",
  "axis-aligned",
  "obart1.5a", "obart1.5a_oh", 
  "xgboost",
  "extra_trees",
  "rand_forest",
  "flexBART"
)

# dataset
d = c("abalone", "ais", "amenity", "attend", "baseball", "basketball", "boston",
      "budget", "cane", "cpu", "diabetes", "diamonds", "edu", "labor", "mpg",
      "rice", "servo", "strikes")

# test set
s = 1:20

# save settings
settings = expand.grid(model = m, data = d, split = s)
