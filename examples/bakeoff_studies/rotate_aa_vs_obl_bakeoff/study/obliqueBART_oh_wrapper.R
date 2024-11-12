# Author: Paul Nguyen
# Date: August 15
# Purpose: wrapper function for fitting obliqueBART (one hot version) in simulation study
# Details: 
# Dependencies: obliqueBART

obliqueBART_oh_wrapper = function(
    Y_train,
    X_cont_train = matrix(0, nrow = 1, ncol = 1),
    X_cont_test = matrix(0, nrow = 1, ncol = 1),
    X_cat = matrix(0, nrow = 1, ncol = 1),
    cat_levels_list = NULL,
    prob_aa = 0,
    adaptive_prob_aa_option = FALSE,
    phi_option = 1,
    nd = 1000, burn = 1000, thin = 1,
    save_samples = TRUE, save_trees = FALSE,
    verbose = TRUE, print_every = floor((nd * thin + burn)/10),
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
  if (sum(dim(X_cat)) == 2) {
    X_train = X_cont_train %>% as.matrix()
    X_test = X_cont_test %>% as.matrix()
  } else {
    X_train = dplyr::bind_cols(X_cont_train, X_cat_train) %>% as.matrix()
    X_test = dplyr::bind_cols(X_cont_test, X_cat_test) %>% as.matrix()
  }

  
  # Create output containers 
  sigma_samples = rep(NA, times = n_chains * nd)
  yhat_train_samples = array(NA, dim = c(n_chains * nd, N_train))
  yhat_test_samples = array(NA, dim = c(n_chains * nd, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    train_time = system.time(
      fit <- obliqueBART::obliqueBART_lp(
        Y_train = Y_train,
        X_cont_train = X_train,
        X_cont_test = X_test,
        prob_aa = prob_aa,
        adaptive_prob_aa_option = adaptive_prob_aa_option,
        phi_option = phi_option,
        nd = nd, burn = burn, thin = thin,
        save_samples = save_samples,
        verbose = verbose, print_every = print_every
      )
    )
    start_index = (chain - 1) * nd + 1
    end_index = chain * nd
    
    sigma_samples[start_index:end_index] = fit$sigma[-(1:burn)]
    yhat_train_samples[start_index:end_index, ] = fit$yhat.train
    yhat_test_samples[start_index:end_index, ] = fit$yhat.test
    timing[chain] = train_time["elapsed"]
  }
  
  # Containers to summarize all the posterior samples of the regression function
  fit_summary_train = array(dim = c(N_train, 3), dimnames = list(c(), c("MEAN", "L95", "U95")))
  fit_summary_test = array(dim = c(N_test, 3), dimnames = list(c(), c("MEAN", "L95", "U95")))
  
  fit_summary_train[,"MEAN"] = apply(yhat_train_samples, MARGIN = 2, FUN = mean)
  fit_summary_train[,"L95"] = apply(yhat_train_samples, MARGIN = 2, FUN = quantile, probs = 0.025)
  fit_summary_train[,"U95"] = apply(yhat_train_samples, MARGIN = 2, FUN = quantile, probs = 0.975)
  
  fit_summary_test[,"MEAN"] = apply(yhat_test_samples, MARGIN = 2, FUN = mean)
  fit_summary_test[,"L95"] = apply(yhat_test_samples, MARGIN = 2, FUN = quantile, probs = 0.025)
  fit_summary_test[,"U95"] = apply(yhat_test_samples, MARGIN = 2, FUN = quantile, probs = 0.975)
  
  # Container for samples from posterior predictive
  ystar_summary_train = array(dim = c(N_train, 3), dimnames = list(c(), c("MEAN", "L95", "U95")))
  ystar_summary_test = array(dim = c(N_test, 3), dimnames = list(c(), c("MEAN", "L95", "U95")))
  
  mean_accept_rate = mean((1 - ((fit$obl_rejected + fit$aa_rejected)) / (fit$obl_proposed + fit$aa_proposed))[-(1:burn)])
  mean_tree_depth = mean(fit$tree_depths)
  
  for(i in 1:N_train){
    tmp_ystar = yhat_train_samples[,i] + sigma_samples * rnorm(n = length(sigma_samples), mean = 0, sd = 1)
    ystar_summary_train[i,"MEAN"] = mean(tmp_ystar)
    ystar_summary_train[i,"L95"] = quantile(tmp_ystar, probs = 0.025)
    ystar_summary_train[i,"U95"] = quantile(tmp_ystar, probs = 0.975)
  }
  for(i in 1:N_test){
    tmp_ystar = yhat_test_samples[,i] + sigma_samples * rnorm(n = length(sigma_samples), mean = 0, sd = 1)
    ystar_summary_test[i,"MEAN"] = mean(tmp_ystar)
    ystar_summary_test[i,"L95"] = quantile(tmp_ystar, probs = 0.025)
    ystar_summary_test[i,"U95"] = quantile(tmp_ystar, probs = 0.975)
  }
  
  return(
    list(timing = timing,
         train = list(fit = fit_summary_train, ystar = ystar_summary_train),
         test = list(fit = fit_summary_test, ystar = ystar_summary_test),
         mean_accept_rate = mean_accept_rate,
         mean_tree_depth = mean_tree_depth
    ))
}
