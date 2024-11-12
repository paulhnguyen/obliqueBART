# Author: Paul Nguyen
# Date: May 22, 2024
# Purpose: prepare benchmark data for bake-off
# Details: budget dataset (http://qed.econ.queensu.ca/jae/datasets/bollino001/)
# Dependencies: 

data_dir = "../source/bpr_data.csv"
save_dir = "../data/"
name = "budget"

# setup -------------------------------------------------------------------
set.seed(2)
# load data
data = read.csv(data_dir, sep = ",", header = TRUE)

# process data ------------------------------------------------------------

# remove rows with missing data
data = na.omit(data)

# split data into continuous and categorical data types
Y = data[,7]
X_cont = data[c(1:6, 8:11)]
X_cat = matrix(0) # no categorical variables

# scale continuous variables
my_scale <- function(x) return( scales::rescale(x, to = c(-.99, .99), from = range(x)) )
if (!is.null(names(X_cont))) {
  for (col in names(X_cont)) { X_cont[[col]] = my_scale(X_cont[[col]]) }
} else {
  X_cont = my_scale(X_cont) # handles case when there is only one continuous variable
}

# create list of category levels
cat_level_list = NULL

# create 20 random test splits with 25% of the observations, rounded up
test_split_list = replicate(20, sample(seq(1, nrow(data)), size = ceiling(nrow(data) / 4), replace = FALSE), simplify = FALSE)

# convert to matrix
X_cont = data.matrix(X_cont)
X_cat = data.matrix(X_cat)

save(Y, X_cont, X_cat, cat_level_list, test_split_list, file = paste0(save_dir, name, "_data.RData"))

