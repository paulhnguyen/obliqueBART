# Author: Ryan Yee
# Date: July 17, 2024
# Purpose: prepare benchmark data for bake-off (classification)
# Details: echocardiogram dataset (https://archive.ics.uci.edu/dataset/38/echocardiogram)
# Dependencies: 

data_dir = "../source/"
save_dir = "../data/"
name = "echocardiogram"

# load data
data = read.table(paste0(data_dir, "echocardiogram.data"), sep = ",", header = FALSE, fill = TRUE)

### process data ###

# remove rows with missing data
data = na.omit(data)

# identify the response
Y = as.integer(data[, 2])

