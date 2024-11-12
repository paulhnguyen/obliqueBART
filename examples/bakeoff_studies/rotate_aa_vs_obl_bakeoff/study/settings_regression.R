# Author: Paul Nguyen
# Date: July 30, 2024
# Purpose: create file to set simulation setting
# Details: 
# Dependencies:

# model index
m = c(
  "obart1.5a", 
  "rot_extra_trees",
  "rot_rand_forest",
  "rot_wbart",
  "rot_xgboost"
)

# dataset
# d = c("abalone", "ais", "amenity", "attend", "baseball", "basketball", "boston",
#       "budget", "cane", "cpu", "diabetes", "diamonds", "edu", "labor", "mpg",
#       "rice", "servo", "strikes")
d = "labor"

# test set
s = 1:20


num_rota <- c(1, 4, 16, 50, 100, 200)


# base_settings = expand.grid(model = m[1], data = d, split= s, num_rota = 200)
expanded_settings = expand.grid(model = m[2:5], data = d, split= s, num_rota = num_rota)
# save settings
settings = expanded_settings
