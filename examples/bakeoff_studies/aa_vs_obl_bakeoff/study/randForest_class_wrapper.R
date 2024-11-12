# Author: Paul Nguyen
# Date: Aug 20, 2024
# Purpose: wrapper function for fitting random forest (classification) in simulation study
# Details: 
# Dependencies: randomForest

randForest_class_wrapper = function(
    Y_train,
    X_train_df,
    X_test_df,
    n_chains = 4
){
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  
  # Create output containers 
  yhat_train_samples = array(NA, dim = c(n_chains, N_train))
  yhat_test_samples = array(NA, dim = c(n_chains, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    train_time = system.time(
      fit <- randomForest::randomForest(
        x = X_train_df,
        y = as.factor(Y_train),
      )
    )
    
    yhat_train_samples[chain, ] = as.integer(fit$predicted) - 1
    yhat_test_samples[chain, ] = as.integer(predict(fit, X_test_df)) - 1
    timing[chain] = train_time["elapsed"]
  }
  
  # Containers to summarize all the posterior samples of the regression function
  fit_summary_train = array(dim = c(N_train, 1), dimnames = list(c(), c("MEAN")))
  fit_summary_test = array(dim = c(N_test, 1), dimnames = list(c(), c("MEAN")))
  
  fit_summary_train[,"MEAN"] = apply(yhat_train_samples, MARGIN = 2, FUN = mean)
  fit_summary_test[,"MEAN"] = apply(yhat_test_samples, MARGIN = 2, FUN = mean)
  final_list <- list(timing = timing,
                     train = fit_summary_train,
                     test = fit_summary_test)
  return(final_list)
}
