# Author: Paul Nguyen
# Date: February 17, 2024
# Purpose: run oblique ensembles on real data
# Details: 
# Dependencies: aorsf, dplyr, ODRF 
library(dplyr)

study = "bakeoff"
data_dir = "data/"
script_dir = "tune_study_obl/"

source(paste0(script_dir, "odrf_wrapper_reg.R"))
source(paste0(script_dir, "aorsf_wrapper_reg.R"))


### simulation settings ###
source(paste0(script_dir, "settings_regression.R"))
args = commandArgs(TRUE)
job_id = as.numeric(args[1]) + 1
model = as.character(settings$model[job_id])
data = as.character(settings$data[job_id])
split = as.integer(settings$split[job_id])

### load data ###
load(paste0(data_dir, data, "_data.RData"))
Y_train = Y[-test_split_list[[split]]]
X_cont_train = X_cont[-test_split_list[[split]], ]
# test split
Y_test = Y[test_split_list[[split]]]
X_cont_test = X_cont[test_split_list[[split]], ]
if (!is.null(cat_level_list)){
  # some data sets only have one categorical variable
  # but rffbart and flexbart require a matrix input
  if ((ncol(X_cat) > 1)){
    X_cat_train = X_cat[-test_split_list[[split]], ]
    X_cat_test = X_cat[test_split_list[[split]], ]
  } else if (nrow(X_cat) > 1) {
    X_cat_train = matrix(X_cat[-test_split_list[[split]], ], ncol = 1)
    X_cat_test = matrix(X_cat[test_split_list[[split]], ], ncol = 1)
  } else if (nrow(X_cat) == 1){
    X_cat_train = matrix(0L, nrow = 1, ncol = 1)
    X_cat_test = matrix(0L, nrow = 1, ncol = 1)
  }
  
} else {
  X_cat_train = matrix(0L, nrow = 1, ncol = 1)
  X_cat_test = matrix(0L, nrow = 1, ncol = 1)
}

if ((is.null(cat_level_list)) |  (nrow(X_cat) == 1)) {
  X_train_df = data.frame(X_cont_train)
  X_test_df = data.frame(X_cont_test)
} else {
  X_train_df = data.frame(dplyr::bind_cols(X_cont_train, X_cat_train))
  X_test_df = data.frame(dplyr::bind_cols(X_cont_test, X_cat_test))
}


N_train = nrow(X_train_df)
N_test = nrow(X_test_df)



### fit model ###
if (model == "ODRF") {
  fit = odrf_wrapper_reg(Y_train,
                     X_train_df, 
                     X_test_df)
} else if (model == "aorsf"){
  fit = aorsf_wrapper_reg(Y_train,
                      X_train_df, 
                      X_test_df)
}




rmse_train = MLmetrics::RMSE(Y_train, fit$train)
rmse_test = MLmetrics::RMSE(Y_test, fit$test)
mean_train_time = mean(fit$timing)

smse_train = mean((Y_train - fit$train)^2) / mean((Y_train - mean(Y_train))^2)
smse_test = mean((Y_test - fit$test)^2) / mean((Y_test - mean(Y_train))^2)

results = list(job_id = job_id,
               model = model,
               data = data,
               split = split,
               rmse_train = rmse_train,
               rmse_test = rmse_test,
               smse_train = smse_train,
               smse_test = smse_test
)


name = paste0(study, "_", model, "_", data, "_", split)
assign(name, results)
save(list = name, file = paste0(name, ".RData"))
