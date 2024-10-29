probit_obliqueBART_lp <- function(Y_train,
                        X_cont_train = matrix(0, nrow = 1, ncol = 1),
                        X_cat_train = matrix(0, nrow = 1, ncol = 1),
                        X_cont_test = matrix(0, nrow = 1, ncol = 1),
                        X_cat_test = matrix(0L, nrow = 1, ncol = 1),
                        unif_cuts = rep(TRUE, times = ncol(as.matrix(X_cont_train))),
                        cutpoints_list = NULL,
                        cat_levels_list = NULL,
                        prob_aa = 0.5, 
                        adaptive_prob_aa_option = FALSE,
                        alpha_dp = 5,
                        phi_option = 1,
                        a_theta = 1,
                        b_theta = 1,
                        centering = FALSE,
                        M = 200,
                        nd = 1000, burn = 1000, thin = 1,
                        save_samples = TRUE,
                        verbose = TRUE, print_every = floor( (nd*thin + burn))/10)
{
  if(!is.integer(Y_train)) stop("Y_train must be an integer vector")
  if(!all(Y_train %in% c(0,1))) stop("All elements of Y_train must be 0 or 1")
  
  p_cont <- 0
  p_cat <- 0
  cont_names <- c()
  cat_names <- c()
  
  if(length(X_cont_train) > 1){
    p_cont <- ncol(as.matrix(X_cont_train))
    if(is.null(colnames(X_cont_train))){
      print("help")
      print(p_cont)
      cont_names <- paste0("X", 1:p_cont)
    } else{
      cont_names <- colnames(X_cont_train)
    }
  } else{
    cont_names <- c()
  }
  
  if(length(X_cat_train) > 1){
    p_cat <- ncol(as.matrix(X_cat_train))
    if(is.null(colnames(X_cat_train))){
      cat_names <- paste0("X", (p_cont + 1):(p_cont + p_cat))
    } else{
      cat_names <- colnames(X_cat_train)
    }
  } else{
    cat_names <- c()
  }
  
  pred_names <- c(cont_names, cat_names)
  tau = 1 / sqrt(M)
  mu0 = 0
  if (centering) {
    mu0 = stats::qnorm(mean(Y_train)) / M
  }
  
  ####################
  # Check options
  ####################
  
  
  fit <- .probit_oblique_BARTfit_lp(Y_train = Y_train,
                          tX_cont_train = t(X_cont_train),
                          tX_cat_train = t(X_cat_train),
                          tX_cont_test = t(X_cont_test),
                          tX_cat_test = t(X_cat_test),
                          unif_cuts = unif_cuts,
                          cutpoints_list = cutpoints_list,
                          cat_levels_list = cat_levels_list,
                          edge_mat_list = NULL,
                          graph_split = rep(FALSE, times = ncol(as.matrix(X_cat_train))),
                          graph_cut_type = 0,
                          aa_prob = prob_aa,
                          adaptive_aa_prob_option = adaptive_prob_aa_option,
                          alpha_dp = alpha_dp,
                          phi_option = phi_option,
                          a_theta = a_theta, b_theta = b_theta,
                          mu0 = mu0, tau = tau,
                          M = M, nd = nd, burn = burn, thin = thin,
                          save_samples = save_samples,
                          verbose = verbose, print_every = print_every)

  results <- list()
  results[["prob.train.mean"]] <- fit$fit_train_mean
  if(save_samples) results[["prob.train"]] <- fit$fit_train
  if(!is.null(fit$fit_test_mean)){
    results[["prob.test.mean"]] <- fit$fit_test_mean
    if(save_samples) results[["prob.test"]] <- fit$fit_test
  }
  # varcounts <- fit$var_count
  # if(length(pred_names) != ncol(varcounts)){
  #   warning("There was an issue tracking variable names. Not naming columns of varcounts object")
  # } else{
  #   colnames(varcounts) <- pred_names
  # }
  # results[["varcounts"]] <- varcounts
  # if(save_trees) results[["trees"]] <- fit$trees
  results[["is.probit"]] <- TRUE

  results[["prob_aa"]] <- fit$prob_aa
  results[["aa_count"]] <- fit$aa_count
  results[["obl_count"]] <- fit$obl_count
  results[["total_accept"]] <- fit$total_accept
  results[["aa_proposed"]] <- fit$aa_proposed
  results[["aa_rejected"]] <- fit$aa_rejected
  results[["cat_proposed"]] <- fit$cat_proposed
  results[["cat_rejected"]] <- fit$cat_rejected
  results[["obl_proposed"]] <- fit$obl_proposed
  results[["obl_rejected"]] <- fit$obl_rejected
  results[["tree_depths"]] <- fit$tree_depths
  
  return(results)
}