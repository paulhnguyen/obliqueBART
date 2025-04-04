# Author: Paul Nguyen
# Date: July 31, 2024
# Purpose: wrapper function for fitting gbm in simulation classification study
# Details: 
# Dependencies: gbm

xgb_reg_wrapper = function(
    Y_train, 
    X_cont_train, 
    X_cont_test,
    X_cat,
    test_split_list,
    split,
    n_chains = 1
)
{
  N_train = nrow(X_cont_train)
  N_test = nrow(X_cont_test)
  
  # one-hot encode categorical variables
  tmp_df = as.data.frame(X_cat) %>% dplyr::mutate(across(everything(), as.factor))
  one_hot_cat = dummy::dummy(tmp_df) %>% dplyr::mutate(across(everything(), as.integer))
  X_cat_train = one_hot_cat[-test_split_list[[split]], ]
  X_cat_test = one_hot_cat[test_split_list[[split]], ]
  # combine into matrix
  if (sum(dim(X_cat_train > 2))) {
    X_train = dplyr::bind_cols(X_cont_train, X_cat_train) %>% as.matrix()
    X_test = dplyr::bind_cols(X_cont_test, X_cat_test) %>% as.matrix()
  } else{
    X_train = X_cont_train %>% as.matrix()
    X_test = X_cont_test %>% as.matrix()
  }
  
  
  #hyperparameter tuning
  validation_sets <- list()
  cat("Tuning Parameters at ", format(Sys.time(), "%b %d %Y %X"), "\n")
  start_tune_time <- Sys.time()
  for (i in 1:5) {
    set.seed(i)
    hyper_train_df_index <- sample(1:nrow(X_train), .8*nrow(X_train))
    hyper_train_df <- X_train[hyper_train_df_index,]
    hyper_validation_df <- X_train[-hyper_train_df_index,]
    hyper_Y_train <- Y_train[hyper_train_df_index]
    hyper_Y_validate <- Y_train[-hyper_train_df_index]
    validation_sets[[paste0("train_set_", i)]] = hyper_train_df
    validation_sets[[paste0("validation_set_", i)]] = hyper_validation_df
    validation_sets[[paste0("hyper_Y_train", i)]] = hyper_Y_train
    validation_sets[[paste0("hyper_Y_validate", i)]] = hyper_Y_validate
  }
  param_set <- expand.grid(eta = c(.01, .05, .1, .25), #shrinkage
                           max_depth = c(1,2,3,4),
                           nrounds = c(50, 100, 200),
                           it = 1:5) %>%
    mutate(validation_mse = NA)
  for (i in 1:nrow(param_set)) {
    it <- param_set$it[i]
    hyper_train_df <- (validation_sets[[paste0("train_set_", it)]])
    hyper_validation_df <- validation_sets[[paste0("validation_set_", it)]]
    hyper_Y_train <- validation_sets[[paste0("hyper_Y_train", it)]]
    hyper_Y_validate <- validation_sets[[paste0("hyper_Y_validate", it)]]
    hyper_dtrain <- xgboost::xgb.DMatrix(data = hyper_train_df, label = hyper_Y_train)
    hyper_dvalidate = xgboost::xgb.DMatrix(data = hyper_validation_df, label = hyper_Y_validate)
    
    fit <- xgboost::xgboost(
      data = hyper_dtrain,
      max.depth = param_set$max_depth[i],
      eta = param_set$eta[i],
      nrounds = param_set$nrounds[i]
    )
    validate_pred <- predict(fit, hyper_dvalidate)
    validate_mse <- mean((validate_pred - hyper_Y_validate)^2)
    param_set$validation_mse[i] <- validate_mse
    }
  best_param <- param_set %>%
    group_by(eta, max_depth, nrounds) %>%
    summarize(mean_mse = mean(validation_mse)) %>%
    arrange((mean_mse)) %>%
    ungroup() %>%
    mutate(rank = row_number()) %>%
    filter(rank == 1)
  best_max_depth <- best_param$max_depth
  best_eta <- best_param$eta
  best_nrounds <- best_param$nrounds
  
  end_tune_time <- Sys.time()
  
  # Create output containers 
  yhat_train_samples = array(NA, dim = c(n_chains, N_train))
  yhat_test_samples = array(NA, dim = c(n_chains, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    dtrain <- xgboost::xgb.DMatrix(data = X_train, label = Y_train)
    dtest = xgboost::xgb.DMatrix(data = X_test, label = Y_test)
    train_time = system.time(
      fit <- xgboost::xgboost(
        data = dtrain,
        max.depth = best_max_depth,
        eta = best_eta,
        nrounds = best_nrounds
      )
    )
    
    yhat_train_samples[chain, ] = predict(fit, X_train)
    yhat_test_samples[chain, ] = predict(fit, X_test)
    timing[chain] = train_time["elapsed"] + end_tune_time - start_tune_time
  }
  
  # Containers to summarize all the posterior samples of the regression function
  fit_summary_train = array(dim = c(N_train, 1), dimnames = list(c(), c("MEAN")))
  fit_summary_test = array(dim = c(N_test, 1), dimnames = list(c(), c("MEAN")))

  fit_summary_train[,"MEAN"] = apply(yhat_train_samples, MARGIN = 2, FUN = mean)
  fit_summary_test[,"MEAN"] = apply(yhat_test_samples, MARGIN = 2, FUN = mean)
  
  return(
    list(timing = timing,
         train = list(fit = fit_summary_train),
         test = list(fit = fit_summary_test)
    ))
}
