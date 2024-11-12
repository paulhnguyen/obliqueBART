# Author: Paul Nguyen
# Date: July 16, 2024
# Purpose: prepare benchmark data for bake-off (classification)
# Details: banknote authentification dataset (https://archive.ics.uci.edu/dataset/267/banknote+authentication)
# Dependencies: 
set.seed(2)
data_dir = "../source/data_banknote_authentication.txt"
save_dir = "../data/"
name = "banknote_authentication"

# setup -------------------------------------------------------------------
data = read.table(data_dir, sep=",")

# process data ------------------------------------------------------------

# remove rows with missing data
data = na.omit(data)

# split data into continuous and categorical data types
Y = ifelse(data[,5] == 1, 1, 0)
X_cont = data[, c(1:4)] 
X_cont <- sapply( X_cont, as.numeric )
X_cat =  matrix(0) # no categorical variables
Y <- as.integer(Y)

# scale continuous variables
my_scale <- function(x) return( scales::rescale(x, to = c(-.99, .99), from = range(x)) )
if (!is.null(names(X_cont))) {
  for (col in names(X_cont)) { X_cont[[col]] = my_scale(X_cont[[col]]) }
} else {
  X_cont = my_scale(X_cont) # handles case when there is only one continuous variable
}

# factor categorical variables
if (!is.null(names(X_cat))) {
  for (col in names(X_cat)) { X_cat[[col]] = as.numeric(factor(X_cat[[col]])) - 1 }
} else {
  X_cat = as.numeric(factor(X_cat)) - 1 # handles case when there is only one categorical variable
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

