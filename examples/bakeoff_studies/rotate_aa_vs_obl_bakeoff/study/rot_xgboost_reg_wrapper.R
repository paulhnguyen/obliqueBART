# Author: Paul Nguyen
# Date: July 31, 2024
# Purpose: wrapper function for fitting gbm in simulation classification study
# Details: 
# Dependencies: gbm

xgb_reg_wrapper = function(
    Y_train, 
    Y_test,
    X_train_df,
    X_test_df,
    n_chains = 1,
    num_rota = num_rota
)
{
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  rot_X <- rotate_data(num_rota = num_rota,
                       X_train_df,
                       X_test_df,
                       Y_train,
                       Y_test)
  rotated_X_train <- as.matrix(rot_X$rotated_X_train)
  rotated_X_test <- as.matrix(rot_X$rotated_X_test)
  
  # Create output containers 
  yhat_train_samples = array(NA, dim = c(n_chains, N_train))
  yhat_test_samples = array(NA, dim = c(n_chains, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    dtrain <- xgboost::xgb.DMatrix(data = rotated_X_train, label = Y_train)
    dtest = xgboost::xgb.DMatrix(data = rotated_X_test, label = Y_test)
    train_time = system.time(
      fit <- xgboost::xgboost(
        data = dtrain,
        max.depth = 3, nrounds = 100
      )
    )
    
    yhat_train_samples[chain, ] = predict(fit, rotated_X_train)
    yhat_test_samples[chain, ] = predict(fit, rotated_X_test)
    timing[chain] = train_time["elapsed"]
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
