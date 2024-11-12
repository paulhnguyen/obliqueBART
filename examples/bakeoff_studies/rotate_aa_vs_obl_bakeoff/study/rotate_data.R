# Author: Paul Nguyen
# Date: May 24, 2024
# Purpose: wrapper function for fitting random rotation ensembles  in simulation study 
# Details: 
# Dependencies: random.rotation

rotate_data = function(num_rota = 1,
                       X_train,
                       X_test,
                       Y_train,
                       Y_test) {
  N_train = nrow(X_train)
  N_test = nrow(X_test)
  
  NUM_ROTA <- num_rota + 1   # number of rotations to create (1 for each tree)
  #preprocess data
  train_df <- cbind(Y_train, X_train)
  colnames(train_df)[1] <- "Y"
  test_df <- cbind(Y_test, X_test)
  colnames(test_df)[1] <- "Y"
  total_data <- rbind(train_df, test_df)
  pre <- random.rotation::pre_sample_preprocessing(total_data, target = "Y")
  X_pre <- data.frame(pre[[1]])
  Y_pre <- pre[[2]]
  # create rotation matrices
  mArray <- random.rotation::create_random_rotation_matrix_array(NUM_ROTA-1,
                                                                 length(random.rotation::numeric_cols(X_pre)))
  # post processing
  r_train = 1:nrow(X_train)
  r_test = (nrow(X_train) + 1):nrow(total_data)
  X <- random.rotation::post_sample_preprocessing(X_pre, Y_pre, 
                                                  r_train, r_test)
  Y <- Y_pre
  # apply rotations
  tArray <- random.rotation::df_apply_random_rotations(X, mArray)
  rotated_X_train <- as.data.frame(tArray[[2]][r_train,] %>%
                                     mutate_all(function(x) as.numeric(as.character(x))))
  rotated_X_test <-  as.data.frame(tArray[[2]][r_test,] %>%
                                          mutate_all(function(x) as.numeric(as.character(x))))
  if (NUM_ROTA > 2){
    for (i in 3:NUM_ROTA) {
      rotated_X_train <- cbind(rotated_X_train, tArray[[i]][r_train,])
      rotated_X_test <- cbind(rotated_X_test, tArray[[i]][r_test,])
    }
  }
  
  final_list <- list(rotated_X_train = rotated_X_train,
                     rotated_X_test = rotated_X_test)
  
  
  
  return(final_list)
}

