# Author: Paul Nguyen
# Date: July 30, 2024
# Purpose: wrapper function for fitting extremely randomized trees in simulation study
# Details: 
# Dependencies: ranger

extra_trees_class_wrapper = function(
    Y_train,
    X_train_df,
    X_test_df,
    n_chains = 1
){
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  
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
                                    sqrt(p), 
                                    ceiling(p/2)),
                           num.trees = c(10, 50, 100, 200),
                           num.random.splits = c(1,2),
                           it = 1:5) %>%
    mutate(validation_accuracy = NA)
  for (i in 1:nrow(param_set)) {
    it <- param_set$it[i]
    hyper_train_df <- (validation_sets[[paste0("train_set_", it)]])
    hyper_validation_df <- validation_sets[[paste0("validation_set_", it)]]
    hyper_Y_train <- validation_sets[[paste0("hyper_Y_train", it)]]
    hyper_Y_validate <- validation_sets[[paste0("hyper_Y_validate", it)]]
    
    fit <- ranger::ranger(
      x = hyper_train_df,
      y = as.factor(hyper_Y_train),
      splitrule = "extratrees",
      replace = FALSE, #disables bagging.
      sample.fraction = 1,
      mtry = param_set$mtry[i],
      num.trees = param_set$num.trees[i],
      num.random.splits = param_set$num.random.splits[i]
    )
    #change this for class
    validate_pred <- as.integer(predict(fit, hyper_validation_df)$predictions)-1
    validation_accuracy <- mean(validate_pred == hyper_Y_validate)
    param_set$validation_accuracy[i] <- validation_accuracy
  }
  #change this for class
  best_param <- param_set %>%
    group_by(mtry, num.trees, num.random.splits) %>%
    summarize(mean_accuracy = mean(validation_accuracy)) %>%
    arrange(desc(mean_accuracy)) %>%
    ungroup() %>%
    mutate(rank = row_number()) %>%
    filter(rank == 1)
  # change this depending on model
  best_mtry <- best_param$mtry
  best_num.trees <- best_param$num.trees
  best_num.random.splits <- best_param$num.random.splits
  end_tune_time <- Sys.time()
  
  # Create output containers 
  yhat_train_samples = array(NA, dim = c(n_chains, N_train))
  yhat_test_samples = array(NA, dim = c(n_chains, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    train_time = system.time(
      fit <- ranger::ranger(
        x = X_train_df,
        y = as.factor(Y_train),
        splitrule = "extratrees",
        replace = FALSE, #disables bagging.
        sample.fraction = 1,
        mtry = best_mtry,
        num.trees = best_num.trees,
        num.random.splits = best_num.random.splits
      )
    )
    
    yhat_train_samples[chain, ] = as.integer(predict(fit, X_train_df)$predictions)-1
    yhat_test_samples[chain, ] = as.integer(predict(fit, X_test_df)$predictions)-1 
    timing[chain] = train_time["elapsed"] + end_tune_time - start_tune_time
  }
  
  # Containers to summarize all the posterior samples of the regression function
  fit_summary_train = array(dim = c(N_train, 1), dimnames = list(c(), c("MEAN")))
  fit_summary_test = array(dim = c(N_test, 1), dimnames = list(c(), c("MEAN")))
  
  fit_summary_train[,"MEAN"] = apply(yhat_train_samples, MARGIN = 2, FUN = mean)
  fit_summary_test[,"MEAN"] = apply(yhat_test_samples, MARGIN = 2, FUN = mean)
  final_list <- list(timing = timing,
                     train = fit_summary_train,
                     test = fit_summary_test
                     )
  return(final_list)
}
