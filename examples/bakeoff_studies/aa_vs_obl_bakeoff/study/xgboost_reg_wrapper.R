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
  X_train = dplyr::bind_cols(X_cont_train, X_cat_train) %>% as.matrix()
  X_test = dplyr::bind_cols(X_cont_test, X_cat_test) %>% as.matrix()
  
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
        max.depth = 3, nrounds = 100
      )
    )
    
    yhat_train_samples[chain, ] = predict(fit, X_train)
    yhat_test_samples[chain, ] = predict(fit, X_test)
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
