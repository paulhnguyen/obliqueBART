# Author: Paul Nguyen
# Date: May 24, 2024
# Purpose: wrapper function for fitting rpensemble in simulation study
# Details: 
# Dependencies: ODRF

rpensemble_wrapper_class = function(
    Y_train,
    X_train_df, 
    X_test_df
){
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  timing = NA
  
  Y_train = Y_train + 1
  Y_test = Y_test + 1
  
  
  
  Out_train <- RPEnsemble::RPParallel(XTrain = as.matrix(X_train_df),
                                      YTrain = Y_train,
                                      d = 2,
                                      XTest = as.matrix(X_train_df),
                                      YTest = Y_train)
  cat("Starting at ", format(Sys.time(), "%b %d %Y %X"), "\n")
  train_time = system.time(
    Out_test <- RPEnsemble::RPParallel(XTrain = as.matrix(X_train_df), 
                                  YTrain = Y_train,
                                  d = 2,
                                  XTest = as.matrix(X_test_df),
                                  YTest = Y_test)
  )
  
  
  
  y_hat_train = RPEnsemble::RPEnsembleClass(Out_train,         
                                            n = N_train,
                                            n.test = N_train,
                                            p1 = sum(Y_train == 1) / length(Y_train),
                                            alpha = RPEnsemble::RPalpha(Out_train, Y = Y_train, p1 = sum(Y_train == 1) / length(Y_train)))
  y_hat_test = RPEnsemble::RPEnsembleClass(Out_test,         
                                           n = N_train,
                                           n.test = N_test,
                                           p1 = sum(Y_train == 1) / length(Y_train),
                                           alpha = RPEnsemble::RPalpha(Out_test, Y = Y_train, p1 = sum(Y_train == 1) / length(Y_train)))
  
  # return y to proper 0-1
  Y_train = Y_train - 1
  Y_test = Y_test - 1
  y_hat_train = as.integer(y_hat_train - 1)
  y_hat_test = as.integer(y_hat_test - 1)
  timing = train_time["elapsed"]
  
  return(
    list(timing = timing,
         train = y_hat_train,
         test = y_hat_test
    ))
}
