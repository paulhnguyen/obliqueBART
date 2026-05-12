assess_performance <- function(y, 
                               ystar_summary, 
                               baseline_mean, 
                               alpha = c(0.5, 0.8, 0.9, 0.95))
{
  
  int_names <- c("MEAN", paste0("L", 100*rev(alpha)), paste0("U", 100*alpha))
  if(!identical(int_names, colnames(ystar_summary))){
    print("ystar_summary column names:")
    print(colnames(ystar_summary))
    print("Anticipated column names:")
    print(int_names)
    stop("ystar_summary has wrong column names. Was it built using post_pred_summary?")
  }
  
  
  results <- list()
  
  # RMSE and SMSE
  results[["rmse"]] <- sqrt(mean( (y - ystar_summary[,"MEAN"])^2 ))
  results[["smse"]] <- mean( (y - ystar_summary[,"MEAN"])^2 ) / mean( (y - baseline_mean)^2 )
  
  # Coverage for each interval
  coverage <- rep(NA, times = length(alpha))
  int_score <- rep(NA, times = length(alpha))
  names(coverage) <- paste0("COV", 100*alpha)
  names(int_score) <- paste0("INTSCORE", 100*alpha)
  for(a in alpha){
    
    tmp <- ystar_summary[,paste0(c("L", "U"),100*a)]
    
    coverage[paste0("COV", 100*a)] <- mean( (tmp[,1] <= y) & (tmp[,2] >= y))
    int_score[paste0("INTSCORE", 100*a)] <- 
      mean((tmp[,2]-tmp[,1]) + 
             2/a * (tmp[,1] - y) * (y <= tmp[,1]) + 
             2/a * (y - tmp[,2]) * (tmp[,2] < y))
  }
  
  results[["coverage"]] <- coverage
  results[["int_score"]] <- int_score
  
  return(results)
}





