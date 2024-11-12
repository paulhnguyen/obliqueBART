# Author: Ryan Yee
# Date: May 13, 2024
# Purpose: prepare benchmark data for bake-off
# Details: tecator dataset (http://lib.stat.cmu.edu/datasets/tecator)
# Dependencies: 

data_dir = "../source/"
save_dir = "../data/"
name = "tecator"

# load data
tecator = read.table(paste0(data_dir, "tecator.data"), header = FALSE, fill = TRUE)
tmp = as.matrix(tecator)
data = matrix(rep(NA, times = nrow(tmp) * ncol(tmp)), nrow = 240)
for (i in 1:nrow(tmp)){
  row = (i - 1) %/% 25 + 1
  start = (i - 1) %% 25 * 5 + 1
  end = start + 4
  data[row, start:end] = unname(tmp[i, ])
}

# the last 25 samples are simulated
data = data[1:215, ]

### process data ###

# remove rows with missing data
data = na.omit(data)

# identify the response
Y = data[, 124]

# split data into continuous and categorical data types
X_cont = data[, -c(124)]
X_cat = matrix(0, nrow = 1, ncol = 1)

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
