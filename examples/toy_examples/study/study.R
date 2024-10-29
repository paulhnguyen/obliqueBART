# Author: Ryan Yee
# Date: August 13, 2024
# Purpose: toy examples for Oblique BART paper
# Details: 
# Dependencies: obliqueBART

study = "toy"
script_dir = "study/"

source(paste0(script_dir, "obliqueBART_wrapper.R"))
source(paste0(script_dir, "wbart_wrapper.R"))

### simulation settings ###
args = commandArgs(TRUE)
job_id = as.numeric(args[1]) + 1
source(paste0(script_dir, "settings.R"))
model = as.character(settings$model[job_id])
experiment = settings$exp[job_id]
replicate = settings$rep[job_id]
n_train = settings$n_train[job_id]
n_test = settings$n_test[job_id]
n_trees = settings$m[job_id]
n_rand_rot = settings$p[job_id]
n_sigma = settings$sigma[job_id]
delta = settings$delta[job_id]
theta = settings$theta[job_id]

### generate data ###
set.seed(101 * replicate)
source(paste0(script_dir, "generate_data.R"))

### fit model ###
n_chains = 1
if (model == "obart"){
  fit = obliqueBART_wrapper(
    Y_train = Y_train,
    X_cont_train = X_cont_train,
    X_cont_test = X_cont_test,
    M = n_trees,
    prob_aa = 0.5,
    adaptive_prob_aa_option = TRUE,
    phi_option = 1,
    n_chains = n_chains
  )
} else if (model == "aabart"){
  fit = obliqueBART_wrapper(
    Y_train = Y_train,
    X_cont_train = X_cont_train,
    X_cont_test = X_cont_test,
    prob_aa = 1,
    n_chains = n_chains
  )
} else if (model == "rrbart"){
  fit = obliqueBART_wrapper(
    Y_train = Y_train,
    X_cont_train = X_cont_train,
    X_cont_test = X_cont_test,
    prob_aa = 1,
    n_chains = n_chains
  )
} else if (model == "awbart"){
  fit = wbart_wrapper(
    Y_train = Y_train,
    X_train_df = X_cont_train,
    X_test_df = X_cont_test,
    n_chains = n_chains
  )
} else if (model == "rwbart"){
  fit = wbart_wrapper(
    Y_train = Y_train,
    X_train_df = X_cont_train,
    X_test_df = X_cont_test,
    n_chains = n_chains
  )
}

### results ###
rmse_train = MLmetrics::RMSE(Y_train, fit$train$fit[,"MEAN"])
rmse_test = MLmetrics::RMSE(Y_test, fit$test$fit[,"MEAN"])
# acceptance_rate = mean(fit$mixing_rates)
# mean_tree_depth = mean(fit$tree_depths)
train_time = mean(fit$train_time)
acceptance_rate = NA
mean_tree_depth = NA

results = list(
  job_id = job_id,
  model = model,
  experiment = experiment,
  replicate = replicate,
  n_train = n_train,
  n_test = n_test,
  n_trees = n_trees,
  n_rand_rot = n_rand_rot,
  sigma = sigma,
  delta = delta,
  theta = theta,
  rmse_train = rmse_train,
  rmse_test = rmse_test,
  acceptance_rate = acceptance_rate,
  mean_tree_depth = mean_tree_depth,
  train_time = train_time
)

name = paste0(study, "_", job_id, "_results")
assign(name, results)
save(list = name, file = paste0(name, ".RData"))

