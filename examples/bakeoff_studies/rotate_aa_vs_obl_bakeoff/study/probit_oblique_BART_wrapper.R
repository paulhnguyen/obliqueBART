# Author: Paul Nguyen
# Date: July 18, 2024
# Purpose: wrapper function for fitting probit oblique BART in simulation study
# Details: 
# Dependencies: obliqueBART

probit_oblique_BART_wrapper = function(
    Y_train,
    X_cont_train = matrix(0, nrow = 1, ncol = 1),
    X_cat_train = matrix(0, nrow = 1, ncol = 1),
    X_cont_test = matrix(0, nrow = 1, ncol = 1),
    X_cat_test = matrix(0, nrow = 1, ncol = 1),
    cat_levels_list = NULL,
    prob_aa = 0,
    adaptive_prob_aa_option = FALSE,
    centering = TRUE,
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
  phat_train_samples = array(NA, dim = c(n_chains * nd, N_train))
  phat_test_samples = array(NA, dim = c(n_chains * nd, N_test))
  timing = rep(NA, times = n_chains)
  
  for (chain in 1:n_chains){
    cat("Starting chain ", chain, " at ", format(Sys.time(), "%b %d %Y %X"), "\n")
    train_time = system.time(
      fit <- obliqueBART::probit_obliqueBART_lp(
        Y_train = as.integer(Y_train),
        X_cont_train = X_cont_train,
        X_cat_train = X_cat_train,
        cat_levels_list = cat_levels_list,
        X_cont_test = X_cont_test,
        X_cat_test = X_cat_test,
        prob_aa = prob_aa,
        adaptive_prob_aa_option = adaptive_prob_aa_option,
        phi_option = phi_option,
        centering = centering,
        nd = nd, burn = burn, thin = thin,
        save_samples = save_samples,
        verbose = verbose, print_every = print_every
      )
    )
    start_index = (chain - 1) * nd + 1
    end_index = chain * nd
    
    phat_train_samples[start_index:end_index, ] = fit$prob.train
    phat_test_samples[start_index:end_index, ] = fit$prob.test
    timing[chain] = train_time["elapsed"]
  }
  
  # Containers to summarize all the posterior samples of the regression function
  fit_summary_train = array(dim = c(N_train, 3), dimnames = list(c(), c("MEAN", "L95", "U95")))
  fit_summary_test = array(dim = c(N_test, 3), dimnames = list(c(), c("MEAN", "L95", "U95")))
  
  fit_summary_train[,"MEAN"] = apply(phat_train_samples, MARGIN = 2, FUN = mean)
  fit_summary_train[,"L95"] = apply(phat_train_samples, MARGIN = 2, FUN = quantile, probs = 0.025)
  fit_summary_train[,"U95"] = apply(phat_train_samples, MARGIN = 2, FUN = quantile, probs = 0.975)
  
  fit_summary_test[,"MEAN"] = apply(phat_test_samples, MARGIN = 2, FUN = mean)
  fit_summary_test[,"L95"] = apply(phat_test_samples, MARGIN = 2, FUN = quantile, probs = 0.025)
  fit_summary_test[,"U95"] = apply(phat_test_samples, MARGIN = 2, FUN = quantile, probs = 0.975)

  
  mean_accept_rate = mean((1 - ((fit$obl_rejected + fit$aa_rejected)) / (fit$obl_proposed + fit$aa_proposed))[-(1:burn)])
  mean_tree_depth = mean(fit$tree_depths)
  
  return(
    list(timing = timing,
         train = fit_summary_train,
         test = fit_summary_test,
         mean_accept_rate = mean_accept_rate,
         mean_tree_depth = mean_tree_depth
    ))
}
