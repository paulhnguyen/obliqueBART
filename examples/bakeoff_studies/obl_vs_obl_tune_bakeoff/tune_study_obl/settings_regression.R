m = c(
  "ODRF",
  "aorsf"
)

# dataset
d = c("abalone", "ais", "amenity", "attend", "baseball", "basketball", "boston",
      "budget", "cane", "cpu", "diabetes", "diamonds", "edu", "labor", "mpg",
      "rice", "servo", "strikes")

# test set
s = 1:20

# save settings
settings = expand.grid(model = m, data = d, split = s)
