# Author: Ryan Yee
# Date: July 17, 2024
# Purpose: prepare benchmark data for bake-off (classification)
# Details: haberman-survival dataset (https://archive.ics.uci.edu/dataset/43/haberman+s+survival)
# Dependencies: 

data_dir = "../source/"
save_dir = "../data/"
name = "haberman-survival"

# load data
data = read.table(paste0(data_dir, "haberman.data"), header = FALSE, sep = ",")

# remove rows with missing data
data = na.omit(data)

# identify the response
Y = as.integer(data[,4])

# split data into continuous and categorical data types
X_cont = data[, c(1:3)]
X_cat = matrix(0)

# scale continuous variables
my_scale <- function(x) return( scales::rescale(x, to = c(-.99, .99), from = range(x)) )
if (!is.null(names(X_cont))) {
  for (col in names(X_cont)) { X_cont[[col]] = my_scale(X_cont[[col]]) }
} else {
  X_cont = my_scale(X_cont) # handles case when there is only one continuous variable
}

# create list of category levels
cat_level_list = list()
if (!is.null(names(X_cat))) {
  for (col in 1:length(names(X_cat))) { cat_level_list[[col]] = seq(0, length(unique(X_cat[, col])) - 1) }
} else {
  cat_level_list[[1]] = seq(0, length(unique(X_cat)) - 1) # handles case when there is only one categorical variable
}

# create 20 random test splits with 25% of the observations, rounded up
test_split_list = replicate(20, sample(seq(1, nrow(data)), size = ceiling(nrow(data) / 4), replace = FALSE), simplify = FALSE)

# convert to matrix
X_cont = data.matrix(X_cont)
X_cat = data.matrix(X_cat)

save(Y, X_cont, X_cat, cat_level_list, test_split_list, file = paste0(save_dir, name, "_data.RData"))

