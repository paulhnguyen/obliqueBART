# Author: Paul Nguyen
# Date: July 18, 2024
# Purpose: reusing wrapper function for fitting rotation forest in simulation study
# Details: 
# Dependencies: rotationForest

rotation_forest_wrapper_class = function(
    Y_train,
    X_train_df, 
    X_test_df
){
  X_train_df <- model.matrix( ~  ., data=X_train_df )[,-1]
  X_test_df <- model.matrix( ~  ., data=X_test_df )[,-1]
  colnames(X_train_df) <- paste("v", 1:dim(X_train_df)[2], sep = "")
  colnames(X_test_df) <- paste("v", 1:dim(X_train_df)[2], sep = "")
  N_train = nrow(X_train_df)
  N_test = nrow(X_test_df)
  
  #hyperparameter tuning
  validation_sets <- list()
  p <- dim(X_train_df)[2]
  cat("Tuning Parameters at ", format(Sys.time(), "%b %d %Y %X"), "\n")
  start_tune_time <- Sys.time()
  for (i in 1:5) {
    set.seed(i)
    hyper_train_df_index <- sample(1:nrow(X_train_df), .8*nrow(X_train_df))
    hyper_train_df <- X_train_df[hyper_train_df_index,]
    hyper_validation_df <- X_train_df[-hyper_train_df_index,]
    hyper_Y_train <- Y_train[hyper_train_df_index]
    hyper_Y_validate <- Y_train[-hyper_train_df_index]
    validation_sets[[paste0("train_set_", i)]] = hyper_train_df
    validation_sets[[paste0("validation_set_", i)]] = hyper_validation_df
    validation_sets[[paste0("hyper_Y_train", i)]] = hyper_Y_train
    validation_sets[[paste0("hyper_Y_validate", i)]] = hyper_Y_validate
  }
  ## change this for different models
  param_set <- expand.grid(L = c(10, 50, 100, 200),
                           K = c(ceiling(p/10), 
                                  ceiling(p/3),
                                  sqrt(p)),
                           it = 1:5) %>%
    mutate(validation_accuracy = NA)
  for (i in 1:nrow(param_set)) {
    it <- param_set$it[i]
    hyper_train_df <- (validation_sets[[paste0("train_set_", it)]])
    hyper_validation_df <- validation_sets[[paste0("validation_set_", it)]]
    hyper_Y_train <- validation_sets[[paste0("hyper_Y_train", it)]]
    hyper_Y_validate <- validation_sets[[paste0("hyper_Y_validate", it)]]
    
    
    
    fit <- rotationForest::rotationForest(data.frame(hyper_train_df),
                                                 as.factor(hyper_Y_train))
    
    #change this for class
    validate_pred <- as.integer(predict(object=fit,
                                        newdata=(data.frame(hyper_validation_df))) >= .5)
    validation_accuracy <- mean(validate_pred == hyper_Y_validate)
    param_set$validation_accuracy[i] <- validation_accuracy
  }
  #change this for class
  best_param <- param_set %>%
    group_by(L,K) %>%
    summarize(mean_accuracy = mean(validation_accuracy)) %>%
    arrange(desc(mean_accuracy)) %>%
    ungroup() %>%
    mutate(rank = row_number()) %>%
    filter(rank == 1)
  # change this depending on model
  best_L <- best_param$L
  best_K <- best_param$K
  end_tune_time <- Sys.time()
  
  
  
  cat("Starting at ", format(Sys.time(), "%b %d %Y %X"), "\n")
  train_time = system.time(
    fit <- rotationForest::rotationForest(data.frame(X_train_df),
                                          as.factor(Y_train),
                                          L = best_L,
                                          K = best_K)
  )
  
  y_hat_train = as.integer(predict(object=fit,
                                   newdata=(data.frame(X_train_df))) >= .5)
  y_hat_test = as.integer(predict(object=fit,
                                  newdata=(data.frame(X_test_df))) >= .5)
     
    
  timing = train_time["elapsed"]
  
  

  
  return(
    list(timing = timing,
         train = y_hat_train,
         test =  y_hat_test
    ))
}

