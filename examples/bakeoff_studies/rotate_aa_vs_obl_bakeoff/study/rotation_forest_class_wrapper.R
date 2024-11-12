# Author: Paul Nguyen
# Date: July 18, 2024
# Purpose: wrapper function for fitting rotation forest in simulation study
# Details: 
# Dependencies: rotationForest

rotation_forest_wrapper = function(
    Y_train,
    X_train_df, 
    X_test_df,
    n_chains = 4
){
  X_train_df <- model.matrix( ~  ., data=X_train_df )[,-1]
  X_test_df <- model.matrix( ~  ., data=X_test_df )[,-1]
  colnames(X_train_df) <- paste("v", 1:dim(X_train_df)[2], sep = "")
  colnames(X_test_df) <- paste("v", 1:dim(X_train_df)[2], sep = "")
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  
  # Create output containers 
  yhat_train_samples = array(NA, dim = c(n_chains, N_train))
  yhat_test_samples = array(NA, dim = c(n_chains, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    train_time = system.time(
      fit <- rotationForest::rotationForest(data.frame(X_train_df),
                            as.factor(Y_train))
    )
    
  
    yhat_train_samples[chain, ] = predict(object=fit,
                                          newdata=(data.frame(X_train_df)))
    yhat_test_samples[chain, ] = predict(object=fit,
                                         newdata=(data.frame(X_test_df)))
    timing[chain] = train_time["elapsed"]
  }
  
  # Containers to summarize all the posterior samples of the regression function
  fit_summary_train = array(dim = c(N_train, 1), dimnames = list(c(), c("MEAN")))
  fit_summary_test = array(dim = c(N_test, 1), dimnames = list(c(), c("MEAN")))
  
  fit_summary_train[,"MEAN"] = apply(yhat_train_samples, MARGIN = 2, FUN = mean)
  fit_summary_test[,"MEAN"] = apply(yhat_test_samples, MARGIN = 2, FUN = mean)
  
  return(
    list(timing = timing,
         train = fit_summary_train,
         test =  fit_summary_test
    ))
}

