# Author: Ryan Yee
# Date: August 13, 2024
# Purpose: toy example settings
# Details: 
# Dependencies: 

# general factors
n_train = 1000 #c(100, 1000, 10000)
n_test = 1000
sigma = 1
delta = 4 # c(0.5, 1, 2, 4)

# rotation factors
theta = seq(0, pi / 4, length.out = 10)

# sinusoidal factors
amp = seq(0, 1, length.out = 10)

# tree size (obart only)
m = 200 # c(25, 50, 100, 200)

# number of features (random rotations only)
p = c(1, 4, 16, 50, 100, 200)

# replicates
r = 1:20

# oblique experiments
o_rot = expand.grid(
  model = "obart",
  exp = "rot_axes",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = 0,
  sigma = sigma,
  delta = delta,
  theta = theta
)

o_sin = expand.grid(
  model = "obart",
  exp = "sin",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = 0,
  sigma = sigma,
  delta = delta,
  theta = amp
)

# axis-aligned experiments
a_rot = expand.grid(
  model = "aabart",
  exp = "rot_axes",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = 0,
  sigma = sigma,
  delta = delta,
  theta = theta
)

a_sin = expand.grid(
  model = "aabart",
  exp = "sin",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = 0,
  sigma = sigma,
  delta = delta,
  theta = amp
)

# random rotation experiments
r_rot = expand.grid(
  model = "rrbart",
  exp = "rot_axes",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = p,
  sigma = sigma,
  delta = delta,
  theta = theta
)

r_sin = expand.grid(
  model = "rrbart",
  exp = "sin",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = p,
  sigma = sigma,
  delta = delta,
  theta = amp
)

# wbart experiments
aw_rot = expand.grid(
  model = "awbart",
  exp = "rot_axes",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = 0,
  sigma = sigma,
  delta = delta,
  theta = theta
)

aw_sin = expand.grid(
  model = "awbart",
  exp = "sin",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = 0,
  sigma = sigma,
  delta = delta,
  theta = amp
)

rw_rot = expand.grid(
  model = "rwbart",
  exp = "rot_axes",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = p,
  sigma = sigma,
  delta = delta,
  theta = theta
)

rw_sin = expand.grid(
  model = "rwbart",
  exp = "sin",
  rep = r,
  n_train = n_train,
  n_test = n_test,
  m = m,
  p = p,
  sigma = sigma,
  delta = delta,
  theta = amp
)

# settings
settings = rbind(o_rot, o_sin, a_rot, a_sin, r_rot, r_sin, aw_rot, aw_sin, rw_rot, rw_sin)
