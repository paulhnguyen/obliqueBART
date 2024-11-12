library(tidyverse)
data_dir <- "../data/"
save_dir <- "../one_hot_data/"

loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(data_dir, list.files(data_dir), sep = "")
files = files[!files %in% c("../data/cylinder_bands_data.RData",
                    "../data/thoraric_surgery_data.RData"
                    )]


for (i in 1:length(files)) {
  print(files[i])
  load(files[i])
  X_cat = as.data.frame(X_cat)
  for (col in 1:ncol(X_cat)){
    X_cat[, col] = factor(X_cat[, col],
                          levels = cat_level_list[[col]])
  }
  if (nrow(X_cat) != 1){
    X_cat[, sapply(X_cat, function(col) length(unique(col))) > 1]
    X_df = dplyr::bind_cols(X_cont, X_cat)
  } else{
    X_df = as.data.frame(X_cont)
  }
  design_matrix = model.matrix( ~  ., data= X_df )[,-1]
  test_split_list = replicate(20, 
                              sample(seq(1, nrow(X_df)),
                                         size = ceiling(nrow(X_df) / 4),
                                         replace = FALSE), simplify = FALSE)
  name = str_sub(files[i], 9)
  save(Y, design_matrix,
       cat_level_list, 
       test_split_list,
       file = paste0(save_dir, name))
}





