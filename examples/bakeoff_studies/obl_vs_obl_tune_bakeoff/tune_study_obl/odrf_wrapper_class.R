# Author: Paul Nguyen
# Date: May 24, 2024
# Purpose: wrapper function for fitting ODRF in simulation study
# Details: 
# Dependencies: ODRF
library(ODRF)
odrf_wrapper_class = function(
    Y_train,
    X_train_df, 
    X_test_df
){
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  timing = NA
  #hyperparameter tuning
  validation_sets <- list()
  p <- dim(X_train_df)[2]
  cat("Tuning Parameters at ", format(Sys.time(), "%b %d %Y %X"), "\n")
  start_tune_time <- Sys.time()
  for (i in 1:5) {
    set.seed(i)
    hyper_train_df_index <- sample(1:nrow(X_train_df), .8*nrow(X_train_df))
    hyper_train_df <- X_train_df[hyper_train_df_index,]
    hyper_validation_df <- X_train_df[-hyper_train_df_index,]
    hyper_Y_train <- Y_train[hyper_train_df_index]
    hyper_Y_validate <- Y_train[-hyper_train_df_index]
    validation_sets[[paste0("train_set_", i)]] = hyper_train_df
    validation_sets[[paste0("validation_set_", i)]] = hyper_validation_df
    validation_sets[[paste0("hyper_Y_train", i)]] = hyper_Y_train
    validation_sets[[paste0("hyper_Y_validate", i)]] = hyper_Y_validate
  }
  ## change this for different models
  param_set <- expand.grid(ntrees = c(10, 50, 100, 200),
                           it = 1:5) %>%
    mutate(validation_accuracy = NA)
  for (i in 1:nrow(param_set)) {
    it <- param_set$it[i]
    hyper_train_df <- (validation_sets[[paste0("train_set_", it)]])
    hyper_validation_df <- validation_sets[[paste0("validation_set_", it)]]
    hyper_Y_train <- validation_sets[[paste0("hyper_Y_train", it)]]
    hyper_Y_validate <- validation_sets[[paste0("hyper_Y_validate", it)]]
    
    fit <-  ODRF::ODRF(as.matrix(hyper_train_df),
                             as.factor(hyper_Y_train),
                             ntrees = param_set$ntrees[i],
                       parallel = F)
    #change this for class
    validate_pred <- as.integer(predict(fit, Xnew = hyper_validation_df))
    validation_accuracy <- mean(validate_pred == hyper_Y_validate)
    param_set$validation_accuracy[i] <- validation_accuracy
  }
  #change this for class
  best_param <- param_set %>%
    group_by(ntrees) %>%
    summarize(mean_accuracy = mean(validation_accuracy)) %>%
    arrange(desc(mean_accuracy)) %>%
    ungroup() %>%
    mutate(rank = row_number()) %>%
    filter(rank == 1)
  # change this depending on model
  best_ntrees <- best_param$ntrees[1]
  end_tune_time <- Sys.time()
  
  
  cat("Starting at ", format(Sys.time(), "%b %d %Y %X"), "\n")
  train_time = system.time(
    fit <- ODRF::ODRF(as.matrix(X_train_df),
                      as.factor(Y_train),
                      ntrees = best_ntrees,
                      parallel = F)
  )
  
  
  y_hat_train = as.integer(predict(fit, Xnew = X_train_df))
  y_hat_test = as.integer(predict(fit, Xnew = X_test_df))
  timing = train_time["elapsed"] + end_tune_time - start_tune_time
  
  return(
    list(timing = timing,
         train = y_hat_train,
         test = y_hat_test
    ))
}
