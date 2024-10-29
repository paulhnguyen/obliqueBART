# Author: Ryan Yee
# Date: May 24, 2024
# Purpose: wrapper function for fitting obliqueBART in simulation study
# Details: 
# Dependencies: obliqueBART

obliqueBART_wrapper = function(
    Y_train,
    X_cont_train = matrix(0, nrow = 1, ncol = 1),
    X_cat_train = matrix(0, nrow = 1, ncol = 1),
    X_cont_test = matrix(0, nrow = 1, ncol = 1),
    X_cat_test = matrix(0, nrow = 1, ncol = 1),
    cat_levels_list = NULL,
    M = 200,
    prob_aa = 0,
    adaptive_prob_aa_option = FALSE,
    phi_option = 1,
    nd = 1000, burn = 1000, thin = 1,
    save_samples = TRUE, save_trees = FALSE,
    verbose = TRUE, print_every = floor((nd * thin + burn)/10),
    n_chains = 4
)
{
  N_train = nrow(X_cont_train)
  N_test = nrow(X_cont_test)
  
  # Create output containers 
  sigma_samples = rep(NA, times = n_chains * nd)
  mixing_rates = rep(NA, times = n_chains * nd)
  tree_depths = rep(NA, times = n_chains)
  yhat_train_samples = array(NA, dim = c(n_chains * nd, N_train))
  yhat_test_samples = array(NA, dim = c(n_chains * nd, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    train_time = system.time(
      fit <- obliqueBART::obliqueBART_lp(
        Y_train = Y_train,
        X_cont_train = X_cont_train,
        X_cat_train = X_cat_train,
        cat_levels_list = cat_levels_list,
        X_cont_test = X_cont_test,
        X_cat_test = X_cat_test,
        prob_aa = prob_aa,
        M = M,
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
    mixing_rates[start_index:end_index] = (1 - ((fit$obl_rejected + fit$aa_rejected)) / (fit$obl_proposed + fit$aa_proposed))[-(1:burn)]
    tree_depths[chain] = mean(fit$tree_depths)
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
         mixing_rates = mixing_rates,
         tree_depths = tree_depths,
         train_time = timing
    ))
}
