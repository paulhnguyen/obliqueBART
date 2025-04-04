# Author: Paul Nguyen
# Date: February 17, 2024
# Purpose: run real dataset on various hyperparameters
# Details: 
# Dependencies: aorsf, ODRF, rerf, RPEnsemble, rotationForest
library(dplyr)

study = "bakeoff"
data_dir = "data/"
# data_dir = "../../benchmark_datasets/classification/data/"
script_dir = "tune_study_obl/"

source(paste0(script_dir, "odrf_wrapper_class.R"))
source(paste0(script_dir, "aorsf_wrapper_class.R"))
source(paste0(script_dir, "rpensemble_wrapper_class.R"))
source(paste0(script_dir, "rotationforest_wrapper_class.R"))


### simulation settings ###
source(paste0(script_dir, "settings_classification.R"))
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
if (model == "RPEnsemble") {
  fit = rpensemble_wrapper_class(Y_train,
                     X_train_df, 
                     X_test_df)
} else if (model == "rotationForest"){
  fit = rotation_forest_wrapper_class(Y_train,
                      X_train_df, 
                      X_test_df)
} else if (model == "ODRF"){
  fit = odrf_wrapper_class(Y_train,
                      X_train_df, 
                      X_test_df)
} else if (model == "aorsf"){
  fit = aorsf_wrapper_class(Y_train,
                      X_train_df, 
                      X_test_df)
} 




accuracy_train = MLmetrics::Accuracy(fit$train, Y_train)
accuracy_test = MLmetrics::Accuracy(fit$test, Y_test)
auc_train = MLmetrics::AUC(fit$train, Y_train)
auc_test = MLmetrics::AUC(fit$test, Y_test)
recall_train = MLmetrics::Recall(Y_train, as.numeric(fit$train))
recall_test = MLmetrics::Recall(Y_test, as.numeric(fit$test))
precision_train = MLmetrics::Precision(Y_train, (as.numeric(fit$train)))
precision_test = MLmetrics::Precision(Y_test, as.numeric(fit$test))
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
               auc_test = auc_test
)


name = paste0(study, "_", model, "_", data, "_", split)
assign(name, results)
save(list = name, file = paste0(name, ".RData"))
