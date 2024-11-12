# Author: Paul Nguyen
# Date: August 14, 2024
# Purpose: run real dataset bake-off to determine gains from obliqueBART vs other rotated axis aligned ensembles
# Details: 
# Dependencies: obliqueBART, flexBART, BART, randomForest, dplyr
library(dplyr)

study = "bakeoff"
data_dir = "data/"
# data_dir = "../../benchmark_datasets/regression/data/"
script_dir = "study/"

source(paste0(script_dir, "rotate_data.R"))
source(paste0(script_dir, "obliqueBART_wrapper.R"))
source(paste0(script_dir, "rot_extra_trees_reg_wrapper.R"))
source(paste0(script_dir, "rot_randForest_reg_wrapper.R"))
source(paste0(script_dir, "rot_wbart_wrapper.R"))
source(paste0(script_dir, "rot_xgboost_reg_wrapper.R"))


### simulation settings ###
args = commandArgs(TRUE)
job_id = as.numeric(args[1]) + 1
source(paste0(script_dir, "settings_regression.R"))
model = as.character(settings$model[job_id])
data = as.character(settings$data[job_id])
split = as.integer(settings$split[job_id])
num_rota = as.integer(settings$num_rota[job_id])
### load data ###
load(paste0(data_dir, data, "_data.RData"))


# train split
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
  } else if (nrow(X_cat == 1)){
    X_cat_train = matrix(0L, nrow = 1, ncol = 1)
    X_cat_test = matrix(0L, nrow = 1, ncol = 1)
  }
} else {
  X_cat_train = matrix(0L, nrow = 1, ncol = 1)
  X_cat_test = matrix(0L, nrow = 1, ncol = 1)
}

# one-hot encode categorical variables
if (sum(dim(X_cat)) > 2) {
  tmp_df = as.data.frame(X_cat) %>% mutate(across(everything(), as.factor))
  one_hot_cat = dummy::dummy(tmp_df) %>% mutate(across(everything(), as.integer))
  X_cat_train = one_hot_cat[-test_split_list[[split]], ]
  X_cat_test = one_hot_cat[test_split_list[[split]], ]
  
  X_train = dplyr::bind_cols(X_cont_train, X_cat_train) %>% as.matrix()
  X_test = dplyr::bind_cols(X_cont_test, X_cat_test) %>% as.matrix()
} else{
  X_train = X_cont_train
  X_test = X_cont_test
}


#change this to pbart for classification
# if (model %in% c("rot_rand_forest", "extra_trees")){
#   # df
#   X_train_df = data.frame(X_train)
#   X_test_df = data.frame(X_test)
# } else{
  X_train_df = as.matrix(X_train)
  X_test_df = as.matrix(X_test)
# }


### fit model ###
n_chains = 1
if (model == "obart1.5a"){
  fit = obliqueBART_wrapper(
    Y_train = Y_train,
    X_cont_train = X_train_df,
    X_cont_test = X_test_df,
    prob_aa = 0.5,
    adaptive_prob_aa_option = T,
    phi_option = 1,
    n_chains = n_chains
  )
} else if (model == "rot_wbart"){
  fit = wbart_wrapper(
    Y_train, 
    X_train_df, 
    X_test_df,
    num_rota = num_rota
  )
} else if (model == "rot_extra_trees"){
  fit = rot_extra_trees_reg_wrapper (Y_train,
                                     Y_test,
                                     X_train_df,
                                     X_test_df,
                                     num_rota = num_rota
  )
}  else if (model == "rot_rand_forest"){
  fit = rot_randForest_reg_wrapper(Y_train = Y_train,
                               X_train_df = X_train_df,
                               X_test_df = X_test_df,
                               num_rota = num_rota)
} else if (model == "rot_xgboost"){
  fit = xgb_reg_wrapper(
    Y_train, 
    Y_test,
    X_train_df,
    X_test_df,
    n_chains = n_chains,
    num_rota = num_rota
  )
}

rmse_train = MLmetrics::RMSE(Y_train, fit$train$fit[,"MEAN"])
rmse_test = MLmetrics::RMSE(Y_test, fit$test$fit[,"MEAN"])

# randomForest does not do uncertainty quantification
if (!(model %in% c("wbart", "obart1.5a")  )){
  ystar_rmse_train = NA
  ystar_rmse_test = NA
  ystar_cov_train = NA
  ystar_cov_test = NA
  mean_int_len_train = NA
  mean_int_len_test = NA
} else {
  ystar_rmse_train = MLmetrics::RMSE(Y_train, fit$train$ystar[,"MEAN"])
  ystar_rmse_test = MLmetrics::RMSE(Y_test, fit$test$ystar[,"MEAN"])
  ystar_cov_train = mean( (Y_train >= fit$train$ystar[,"L95"] & Y_train <= fit$train$ystar[,"U95"]) )
  ystar_cov_test = mean( (Y_test >= fit$test$ystar[,"L95"] & Y_test <= fit$test$ystar[,"U95"]) )
  mean_int_len_train = mean(fit$train$ystar[,"U95"] - fit$train$ystar[,"L95"])
  mean_int_len_test = mean(fit$test$ystar[,"U95"] - fit$test$ystar[,"L95"])
}

mean_train_time = mean(fit$timing)

# mean_accept_rate = NA
# mean_tree_depth = NA
# if (model != "wbart" ) {
#   mean_accept_rate = fit$mean_accept_rate
#   mean_tree_depth = fit$mean_tree_depth
# }

results = list(job_id = job_id,
               model = model,
               data = data,
               split = split,
               rmse_train = rmse_train,
               rmse_test = rmse_test,
               ystar_rmse_train = ystar_rmse_train,
               ystar_rmse_test = ystar_rmse_test,
               ystar_cov_train = ystar_cov_train,
               ystar_cov_test = ystar_cov_test,
               # mean_int_len_train = mean_int_len_train,
               # mean_int_len_test = mean_int_len_test,
               mean_train_time = mean_train_time,
               # mean_accept_rate = mean_accept_rate,
               # mean_tree_depth = mean_tree_depth,
               num_rota = num_rota
)
results

name = paste0(study, "_", model, "_", data, "_", split, "_", num_rota)
assign(name, results)
save(list = name, file = paste0(name, ".RData"))
