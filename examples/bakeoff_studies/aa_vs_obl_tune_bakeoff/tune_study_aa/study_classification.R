# Author: Paul Nguyen
# Date: August 20, 2024
# Purpose: run real dataset bake-off to determine gains from obliqueBART vs other axis aligned ensembles
# Details: 
# Dependencies: obliqueBART, flexBART, BART, randomForest, dplyr
library(dplyr)

study = "bakeoff"
data_dir = "data/"
# data_dir = "../../benchmark_datasets/classification/data/"
script_dir = "tune_study_aa/"


source(paste0(script_dir, "extra_trees_class_wrapper.R"))
source(paste0(script_dir, "xgboost_class_wrapper.R"))
source(paste0(script_dir, "randForest_class_wrapper.R"))


### simulation settings ###
args = commandArgs(TRUE)
job_id = as.numeric(args[1]) + 1
source(paste0(script_dir, "settings_classification.R"))
model = as.character(settings$model[job_id])
data = as.character(settings$data[job_id])
split = as.integer(settings$split[job_id])

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
  } else if (nrow(X_cat) == 1){
    X_cat_train = matrix(0L, nrow = 1, ncol = 1)
    X_cat_test = matrix(0L, nrow = 1, ncol = 1)
  }
} else {
  X_cat_train = matrix(0L, nrow = 1, ncol = 1)
  X_cat_test = matrix(0L, nrow = 1, ncol = 1)
}

# combine continuous predictors and categorical predictors into either a 
#   matrix or a df depending on the model
if (model %in% c("pbart", "rand_forest", "extra_trees")){
  # df
  if(sum(dim(X_cat_train)) == 2){
    X_train_df = data.frame(X_cont_train)
    X_test_df = data.frame(X_cont_test)
  } else{
    X_train_df = data.frame(dplyr::bind_cols(X_cont_train, X_cat_train))
    X_test_df = data.frame(dplyr::bind_cols(X_cont_test, X_cat_test))
  }
} 


### fit model ###
n_chains = 1
if (model == "xgboost"){
  fit = xgb_class_wrapper( Y_train = Y_train, 
                         X_cont_train = X_cont_train, 
                         X_cont_test = X_cont_test,
                         X_cat = X_cat,
                         test_split_list = test_split_list,
                         split = split,
                         n_chains = 1)
} else if (model == "extra_trees"){
  fit = extra_trees_class_wrapper(Y_train = Y_train,
                                X_train_df = X_train_df,
                                X_test_df = X_test_df,
                                n_chains = 1)
} else if (model == "rand_forest"){
  fit = randForest_class_wrapper(Y_train = Y_train,
                               X_train_df = X_train_df,
                               X_test_df = X_test_df,
                               n_chains = 1)
}


if (model %in% c("rand_forest", "xgboost", "extra_trees")) {
  accuracy_train = MLmetrics::Accuracy(fit$train >= .5, Y_train)
  accuracy_test = MLmetrics::Accuracy(fit$test >= .5, Y_test)
  auc_train = MLmetrics::AUC(fit$train >= .5, Y_train)
  auc_test = MLmetrics::AUC(fit$test >= .5, Y_test)
  recall_train = MLmetrics::Recall(Y_train, as.numeric(fit$train >= .5))
  recall_test = MLmetrics::Recall(Y_test, as.numeric(fit$test >= .5))
  precision_train = MLmetrics::Precision(Y_train, (as.numeric(fit$train >= .5)))
  precision_test = MLmetrics::Precision(Y_test, as.numeric(fit$test >= .5))
} else{
  accuracy_train = MLmetrics::Accuracy(fit$train[,"MEAN"] >= .5, Y_train)
  accuracy_test = MLmetrics::Accuracy(fit$test[,"MEAN"] >= .5, Y_test)
  auc_train = MLmetrics::AUC(fit$train[,"MEAN"] >= .5, Y_train)
  auc_test = MLmetrics::AUC(fit$test[,"MEAN"] >= .5, Y_test)
  recall_train = MLmetrics::Recall(Y_train, as.numeric(fit$train[,"MEAN"] >= .5))
  recall_test = MLmetrics::Recall(Y_test, as.numeric(fit$test[,"MEAN"] >= .5))
  precision_train = MLmetrics::Precision(Y_train, as.numeric(fit$train[,"MEAN"] >= .5))
  precision_test = MLmetrics::Precision(Y_test, as.numeric(fit$test[,"MEAN"] >= .5))
}

if (model %in% c("pbart", "rotationForest", "xgboost",
                 "rand_forest", "extra_trees")){
  mean_int_len_train = NA
  mean_int_len_test = NA
  log_loss_test = MLmetrics::LogLoss(fit$test, Y_test)
  log_loss_train = MLmetrics::LogLoss(fit$train, Y_train)
} else {
  mean_int_len_train = mean(fit$train[,"U95"] - fit$train[,"L95"])
  mean_int_len_test = mean(fit$test[,"U95"] - fit$test[,"L95"])
  log_loss_test = MLmetrics::LogLoss(fit$test[,"MEAN"], Y_test)
  log_loss_train = MLmetrics::LogLoss(fit$train[,"MEAN"], Y_train)
}



mean_train_time = mean(fit$timing)


results = list(job_id = job_id,
               model = model,
               data = data,
               split = split,
               accuracy_train = accuracy_train,
               accuracy_test = accuracy_test,
               recall_train = recall_train,
               recall_test = recall_test,
               precision_train = precision_train,
               precision_test = precision_test,
               auc_train = auc_train,
               auc_test = auc_test,
               log_loss_test = log_loss_test,
               log_loss_train = log_loss_train,
               mean_int_len_train = mean_int_len_train,
               mean_int_len_test = mean_int_len_test,
               mean_train_time = mean_train_time
)

print(results)


name = paste0(study, "_", model, "_", data, "_", split)
assign(name, results)
save(list = name, file = paste0(name, ".RData"))
