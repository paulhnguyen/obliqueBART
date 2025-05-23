# Author: Paul Nguyen
# Date: May 24, 2024
# Purpose: wrapper function for fitting aorsf in simulation study
# Details: 
# Dependencies: ODRF

aorsf_wrapper_reg = function(
    Y_train,
    X_train_df, 
    X_test_df
){
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  timing = NA
  
  train_big_df <- cbind(X_train_df, Y_train) %>%
    rename(Y = 'Y_train')
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
  param_set <- expand.grid(mtry = c(ceiling(p/10), 
                                    ceiling(p/3),
                                    ceiling(sqrt(p)), 
                                    ceiling(p/2)),
                           n_tree = c(10, 50, 100, 200),
                           n_split = c(1,2,5),
                           it = 1:5) %>%
    mutate(validation_mse = NA)
  for (i in 1:nrow(param_set)) {
    print(i)
    it <- param_set$it[i]
    hyper_train_df <- (validation_sets[[paste0("train_set_", it)]])
    hyper_validation_df <- validation_sets[[paste0("validation_set_", it)]]
    hyper_Y_train <- validation_sets[[paste0("hyper_Y_train", it)]]
    hyper_Y_validate <- validation_sets[[paste0("hyper_Y_validate", it)]]
    hyper_train_big_df <- cbind(hyper_train_df, hyper_Y_train) %>%
      rename(Y = 'hyper_Y_train')
    
    
    fit <- aorsf::orsf(data = hyper_train_big_df,
                       n_tree = param_set$n_tree[i],
                       mtry = param_set$mtry[i],
                       n_split = param_set$n_split[i],
                       formula = Y ~ .)
    #change this for class
    validate_pred <- predict(fit, new_data = hyper_validation_df)
    validation_mse <- mean((validate_pred - hyper_Y_validate)^2)
    param_set$validation_mse[i] <- validation_mse
  }
  #change this for class
  best_param <- param_set %>%
    group_by(n_tree, mtry, n_split) %>%
    summarize(mean_mse = mean(validation_mse)) %>%
    arrange(mean_mse) %>%
    ungroup() %>%
    mutate(rank = row_number()) %>%
    filter(rank == 1)
  # change this depending on model
  best_mtry <- best_param$mtry
  best_n_tree <- best_param$n_tree
  best_n_split <- best_param$n_split
  end_tune_time <- Sys.time()
  
  
  cat("Starting at ", format(Sys.time(), "%b %d %Y %X"), "\n")
  train_time = system.time(
    fit <- aorsf::orsf(data = train_big_df,
                n_tree = best_n_tree,
                mtry = best_mtry,
                n_split = best_n_split,
                formula = Y ~ .)
  )

  
  
  y_hat_train = predict(fit, new_data = X_train_df)
  y_hat_test = predict(fit, new_data = X_test_df)
  timing = train_time["elapsed"] + end_tune_time - start_tune_time
  
  return(
    list(timing = timing,
         train = y_hat_train,
         test = y_hat_test
    ))
}
