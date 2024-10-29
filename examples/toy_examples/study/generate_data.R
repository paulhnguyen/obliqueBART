# Author: Ryan Yee
# Date: August 13, 2024
# Purpose: generate data for rotated axis experiment
# Details: 
# Dependencies:

# rotated-axes data
if (experiment == "rot_axes"){
  x1 = runif(n_train + n_test,  min = -1, max = 1)
  x2 = runif(n_train + n_test,  min = -1, max = 1)
  x_mat = cbind(x1, x2)
  
  a = x1 * cos(theta) - x2 * sin(theta)
  b = x1 * sin(theta) + x2 * cos(theta)
  y = ifelse(a * b < 0, delta, -delta) + rnorm(n_train + n_test, sd = sigma)
  
  # train / test splits
  X_cont_train = x_mat[1:n_train,]
  Y_train = y[1:n_train]
  X_cont_test = x_mat[(n_train + 1):(n_test + n_train),]
  Y_test = y[(n_train + 1):(n_test + n_train)]
}

# sinusoidal data
if (experiment == "sin"){
  x1 = runif(n_train + n_test,  min = -1, max = 1)
  x2 = runif(n_train + n_test,  min = -1, max = 1)
  y = ifelse(x2 > theta * sin(x1 * 10), delta, -delta) + rnorm(n_train + n_test, sd = sigma)
  
  # train / test splits
  x_mat = cbind(x1, x2)
  X_cont_train = x_mat[1:n_train,]
  Y_train = y[1:n_train]
  X_cont_test = x_mat[(n_train + 1):(n_test + n_train),]
  Y_test = y[(n_train + 1):(n_test + n_train)]
}

# randomly rotated features
if (model %in% c("rrbart", "rwbart")){
  rot_angles = runif(n = n_rand_rot, min = 0, max = 2 * pi)
  rot_mat = matrix(c(cos(rot_angles), sin(rot_angles)), ncol = 2)
  
  X_cont_train = X_cont_train %*% t(rot_mat)
  X_cont_test = X_cont_test %*% t(rot_mat)
}
