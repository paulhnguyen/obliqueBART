# Author: Paul Nguyen
# Date: July 16, 2024
# Purpose: prepare benchmark data for bake-off (classification)
# Details: breast cancer prognostic dataset (https://archive.ics.uci.edu/dataset/16/breast+cancer+wisconsin+prognostic)
# Dependencies: 
set.seed(2)
data_dir = "../source/wpbc.data"
save_dir = "../data/"
name = "breast_cancer_prognostic"

# setup -------------------------------------------------------------------
data = read.table("wpbc.data", sep=",")

# process data ------------------------------------------------------------

# remove rows with missing data
data = na.omit(data)
# dealing with na
data <- data[data[,35] != "?",]
data[,35] <- as.numeric(data[,35])

# split data into continuous and categorical data types
Y = as.integer(ifelse(data[,2] == "R", 1, 0))
X_cont = data[, c(4:35)] 
X_cont <- sapply( X_cont, as.numeric )
X_cat =  matrix(0) # no categorical variables

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

