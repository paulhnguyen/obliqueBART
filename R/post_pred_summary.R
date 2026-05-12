post_pred_summary <- function(mu_samples, sigma_samples, alpha = c(0.5, 0.8, 0.9, 0.95)){
  N <- ncol(mu_samples)
  nd <- length(sigma_samples)
  if(nrow(mu_samples) != nd){
    print(paste("mu_samples has", nrow(mu_samples), "and", ncol(mu_samples)))
    print(paste("sigma_samples is of length", length(sigma_samples)))
    stop("nrow(mu_samples) and length(sigma_samples) must be the same")
  }
  
  probs <- sort(c((1-alpha)/2, (1+alpha)/2))
  

  ystar_sum <- array(dim = c(N, 1+length(probs)))
  dimnames(ystar_sum) <- list(c(), c("MEAN", paste0("L", 100*rev(alpha)), paste0("U", 100*alpha)))
  
  for(i in 1:N){
    tmp_ystar <- mu_samples[,i] + sigma_samples * rnorm(n = nd, mean = 0, sd = 1)
    ystar_quants <- quantile(tmp_ystar, probs = probs)
    ystar_sum[i,] <- c(mean(tmp_ystar), ystar_quants)
  }
  
  return(ystar_sum)
  
}
