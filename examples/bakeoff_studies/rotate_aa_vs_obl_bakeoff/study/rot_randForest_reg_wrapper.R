# Author: Paul Nguyen 
# Date: Aug 20, 2024
# Purpose: wrapper function for fitting random forest (regression) in simulation study
# Details: 
# Dependencies: randomForest

rot_randForest_reg_wrapper = function(
    Y_train,
    X_train_df,
    X_test_df,
    num_rota = 200
){
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  rot_X <- rotate_data(num_rota = num_rota,
                       X_train_df,
                       X_test_df,
                       Y_train,
                       Y_test)
  
  # Create output containers 
  yhat_train_samples = array(NA, dim = c(1, N_train))
  yhat_test_samples = array(NA, dim = c(1, N_test))
  timing = rep(NA, times = 1)
  
  rotated_X_train <- rot_X$rotated_X_train
  rotated_X_test <- rot_X$rotated_X_test
  train_time = system.time(
      fit <- randomForest::randomForest(
        x = rotated_X_train,
        y = Y_train
      )
    )
    
    yhat_train_samples[1, ] = predict(fit, rotated_X_train)
    yhat_test_samples[1, ] = predict(fit, rotated_X_test)
    timing = train_time["elapsed"]
  
  
  # Containers to summarize all the posterior samples of the regression function
  fit_summary_train = array(dim = c(N_train, 1), dimnames = list(c(), c("MEAN")))
  fit_summary_test = array(dim = c(N_test, 1), dimnames = list(c(), c("MEAN")))
  
  fit_summary_train[,"MEAN"] = apply(yhat_train_samples, MARGIN = 2, FUN = mean)
  fit_summary_test[,"MEAN"] = apply(yhat_test_samples, MARGIN = 2, FUN = mean)
  final_list <- list(timing = sum(timing),
                     train = list(fit = fit_summary_train),
                     test = list(fit = fit_summary_test))
  return(final_list)
}
