# Author: Ryan Yee
# Date: August 16, 2024
# Purpose: toy example figures for manuscript
# Details: 
# Dependencies: dplyr, ryplot

base_dir = "../results4/"
comp_dir = "../results3/"
# dir = "../results4/"
save_dir = "../figures/"

library(tidyverse)
library(ggplot2)

### load results ###

# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
# files = stringr::str_c(dir, list.files(dir), sep = "")
base_files = stringr::str_c(base_dir, list.files(base_dir), sep = "")
comp_files = stringr::str_c(comp_dir, list.files(comp_dir), sep = "")

# loads results
# results = lapply(files, loadRData) %>% bind_rows()

base_results = lapply(base_files, loadRData) %>%
  bind_rows() %>%
  filter(model == "obart7")

comp_results = lapply(comp_files, loadRData) %>% bind_rows()

# base_results = subset(results, model == "obart")
# comp_results = subset(results, model != "obart")

### graph data ###
base = base_results %>%
  filter(n_trees == 200) %>%
  rename(
    base_rmse_train = rmse_train,
    base_rmse_test = rmse_test,
    base_train_time = train_time
  ) %>%
  select(experiment, replicate, n_train, n_test, sigma, delta, theta, base_rmse_train, base_rmse_test, base_train_time)

comps = comp_results %>%
  filter(n_trees == 200) %>%
  select(-c(acceptance_rate, mean_tree_depth))

summary = left_join(comps, base, by = join_by(experiment, replicate, n_train, n_test, sigma, delta, theta)) %>%
  drop_na() %>%
  mutate(
    rmse_train_ratio = rmse_train / base_rmse_train,
    rmse_test_ratio = rmse_test / base_rmse_test,
    train_time_ratio = train_time / base_train_time
  )


### visualizations ###

# color palette
col_pal = c("#DDAA33", "#BB5566", "#004488")
col_pal = rev(c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB"))

# RMSE line plot (rotated axes)
rot_rmse_dat = summary %>%
  filter(
    experiment == "rot_axes",
    model %in% c("rwbart", "awbart")
  ) %>%
  # filter(
  #   experiment == "rot_axes",
  #   n_train == 1000,
  #   delta == 4,
  #   n_trees == 200,
  #   (n_rand_rot > 10 | n_rand_rot == 0)
  # ) %>%
  group_by(model, theta, n_rand_rot) %>%
  summarize(
    mean = mean(rmse_test_ratio),
    se = sd(rmse_test_ratio) / sqrt(n())
  ) %>%
  mutate(
    U95 = mean + 2 * se,
    L95 = mean - 2 * se,
    label = paste0(model, n_rand_rot)
  )

t = seq(0, pi / 4, length.out = 10)
m = split(rot_rmse_dat$mean, rot_rmse_dat$n_rand_rot)
u = split(rot_rmse_dat$U95, rot_rmse_dat$n_rand_rot)
l = split(rot_rmse_dat$L95, rot_rmse_dat$n_rand_rot)

png(file = paste0(save_dir, "rot_rmse_fig.png"), width = 6.5, height = 5, units = "in", res = 500)
par(mar = c(c(3, 3, 2, 4)), mgp = c(1.8, 0.5, 0))
ryplot::init_scale(leg_pos = "none", main = "Rotated axes partition", xlab = expression(paste(theta)), ylab = "RMSE ratio (test)", xlim = c(0, pi / 4), ylim = c(0.95, 3), auto_mar = FALSE)
# polygon(x = c(t, rev(t)), y = c(l$aabart, rev(u$aabart)), border = NA, col = scales::alpha(col_pal[1], 0.25))
# polygon(x = c(t, rev(t)), y = c(l$rrbart, rev(u$rrbart)), border = NA, col = scales::alpha(col_pal[2], 0.25))
# polygon(x = c(t, rev(t)), y = c(l$obart, rev(u$obart)), border = NA, col = scales::alpha(col_pal[3], 0.25))
lines(x = t, y = rep(1, times = length(t)), col = "black", lty = 2)
for (i in 1:length(m)){
  polygon(x = c(t, rev(t)), y = c(l[[i]], rev(u[[i]])), border = NA, col = scales::alpha(col_pal[i], 0.25))
  lines(x = t, y = m[[i]], col = col_pal[i], lwd = 1.5)
  points(x = t, y = m[[i]], col = col_pal[i], pch = i)
}
# legend("bottom", legend = c("AA", "R160", "R20", "R40", "R80"), col = col_pal, lty = 1, lwd = 1.5, bty = "n", xpd = TRUE, horiz = TRUE, inset = c(0, -.45))
legend("right", legend = c("AA", "R1", "R4", "R16", "R50", "R100", "R200"), col = col_pal, lty = 1, lwd = 1.6, bty = "n", pch = 1:7, xpd = TRUE, inset = c(-0.16, 0), y.intersp = 2, cex = 0.8)
dev.off()

# RMSE line plot (sin)
sin_rmse_dat = summary %>%
  filter(
    experiment == "sin",
    model %in% c("rwbart", "awbart")
  ) %>%
  # filter(
  #   experiment == "sin",
  #   n_train == 1000,
  #   delta == 4,
  #   n_trees == 200,
  #   (n_rand_rot > 10 | n_rand_rot == 0)
  # ) %>%
  group_by(model, theta, n_rand_rot) %>%
  summarize(
    mean = mean(rmse_test_ratio),
    se = sd(rmse_test_ratio) / sqrt(n())
  ) %>%
  mutate(
    U95 = mean + 2 * se,
    L95 = mean - 2 * se,
    label = paste0(model, n_rand_rot)
  )

t = seq(0, 1, length.out = 10)
m = split(sin_rmse_dat$mean, sin_rmse_dat$n_rand_rot)
u = split(sin_rmse_dat$U95, sin_rmse_dat$n_rand_rot)
l = split(sin_rmse_dat$L95, sin_rmse_dat$n_rand_rot)

# png(file = paste0(save_dir, "sin_rmse_fig.png"), width = 6.5, height = 5, units = "in", res = 500)
par(mar = c(c(3, 3, 2, 4)), mgp = c(1.8, 0.5, 0))
ryplot::init_scale(leg_pos = "none", main = "Sinusoidal partition", xlab = expression(paste(alpha)), ylab = "RMSE ratio (test)", xlim = c(0, 1), ylim = c(0.95, 2.8), auto_mar = FALSE)
lines(x = t, y = rep(1, times = length(t)), col = "black", lty = 2)
for (i in 1:length(m)){
  polygon(x = c(t, rev(t)), y = c(l[[i]], rev(u[[i]])), border = NA, col = scales::alpha(col_pal[i], 0.25))
  lines(x = t, y = m[[i]], col = col_pal[i], lwd = 1.5)
  points(x = t, y = m[[i]], col = col_pal[i], pch = i)
}
# legend("right", legend = c("AA", "R20", "R40", "R80", "R160"), col = col_pal, lty = 1, lwd = 1.5, bty = "n", pch = 1:5, xpd = TRUE, inset = c(-.2, 0), y.intersp = 2)
legend("right", legend = c("AA", "R1", "R4", "R16", "R50", "R100", "R200"), col = col_pal, lty = 1, lwd = 1.6, bty = "n", pch = 1:7, xpd = TRUE, inset = c(-0.16, 0), y.intersp = 2, cex = 0.8)
# dev.off()




# old --------------------------------------------------------------------------

# training time line plot(rotated axes)
rot_time_dat = summary %>%
  filter(
    experiment == "rot_axes",
    n_train == 1000,
    delta == 4,
    n_trees == 200,
    (n_rand_rot > 10 | n_rand_rot == 0)
  ) %>%
  group_by(model, theta, n_rand_rot) %>%
  summarize(
    mean = mean(train_time_ratio),
    se = sd(train_time_ratio) / sqrt(n())
  ) %>%
  mutate(
    U95 = mean + 2 * se,
    L95 = mean - 2 * se,
    label = paste0(model, n_rand_rot)
  )

t = seq(0, pi / 4, length.out = 10)
m = split(rot_time_dat$mean, rot_time_dat$n_rand_rot)
u = split(rot_time_dat$U95, rot_time_dat$n_rand_rot)
l = split(rot_time_dat$L95, rot_time_dat$n_rand_rot)

# png(file = paste0(save_dir, "rot_time_fig.png"), width = 7, height = 5, units = "in", res = 500)
par(mar = c(c(4.1, 4.1, 2.1, 4.5)))
ryplot::init_scale(leg_pos = "none", xlab = expression(paste(theta)), ylab = "Training time (s)", xlim = c(0, pi / 4), ylim = c(0, 2), auto_mar = FALSE)
lines(x = t, y = rep(1, times = length(t)), col = "black", lty = 2)
for (i in 1:length(m)){
  lines(x = t, y = m[[i]], col = col_pal[i], lwd = 1.5)
  points(x = t, y = m[[i]], col = col_pal[i], pch = i)
}
legend("right", legend = c("AA", "R20", "R40", "R80", "R160"), col = col_pal, lty = 1, lwd = 1.5, bty = "n", pch = 1:5, xpd = TRUE, inset = c(-.2, 0), y.intersp = 2)
# dev.off()



# polygon(x = c(t, rev(t)), y = c(l$aabart, rev(u$aabart)), border = NA, col = scales::alpha(col_pal[1], 0.25))
# polygon(x = c(t, rev(t)), y = c(l$rrbart, rev(u$rrbart)), border = NA, col = scales::alpha(col_pal[2], 0.25))
# polygon(x = c(t, rev(t)), y = c(l$obart, rev(u$obart)), border = NA, col = scales::alpha(col_pal[3], 0.25))
lines(x = t, y = m$aabart0, col = col_pal[1], lwd = 1.5)
lines(x = t, y = m$rrbart20, col = col_pal[2], lwd = 1.5)
lines(x = t, y = m$rrbart40, col = col_pal[3], lwd = 1.5)
lines(x = t, y = m$rrbart80, col = col_pal[4], lwd = 1.5)
lines(x = t, y = m$rrbart160, col = col_pal[5], lwd = 1.5)
lines(x = t, y = m$obart0, col = col_pal[6], lwd = 1.5)
legend(0, 1250, legend = c("Axis-aligned BART", "Axis-Aligned BART + 160 Random Rotations", "Oblique BART (adaptive)"), col = col_pal, lty = 1, lwd = 1.5, bty = "n")
# dev.off()


# training time line plot(sin)
sin_time_dat = results %>%
  filter(
    experiment == "sin",
    n_train == 1000,
    delta == 4,
    n_trees == 200,
    (n_rand_rot == 160 | n_rand_rot == 0)
  ) %>%
  group_by(model, theta) %>%
  summarize(
    mean = mean(train_time),
    se = sd(train_time) / sqrt(n())
  ) %>%
  mutate(
    U95 = mean + 2 * se,
    L95 = mean - 2 * se
  )

t = seq(0, pi / 4, length.out = 10)
m = split(sin_time_dat$mean, sin_time_dat$model)
u = split(sin_time_dat$U95, sin_time_dat$model)
l = split(sin_time_dat$L95, sin_time_dat$model)

# png(file = paste0(save_dir, "sin_time_fig.png"), width = 7, height = 5, units = "in", res = 500)
par(mar = c(c(4.1, 4.1, 2.1, 2.1)))
ryplot::init_scale(leg_pos = "none", xlab = expression(paste(alpha)), ylab = "Training time (s)", xlim = c(0, pi / 4), ylim = c(0, 1250), auto_mar = FALSE)
polygon(x = c(t, rev(t)), y = c(l$aabart, rev(u$aabart)), border = NA, col = scales::alpha(col_pal[1], 0.25))
polygon(x = c(t, rev(t)), y = c(l$rrbart, rev(u$rrbart)), border = NA, col = scales::alpha(col_pal[2], 0.25))
polygon(x = c(t, rev(t)), y = c(l$obart, rev(u$obart)), border = NA, col = scales::alpha(col_pal[3], 0.25))
lines(x = t, y = m$aabart, col = col_pal[1], lwd = 1.5)
lines(x = t, y = m$rrbart, col = col_pal[2], lwd = 1.5)
lines(x = t, y = m$obart, col = col_pal[3], lwd = 1.5)
legend(0, 1250, legend = c("Axis-aligned BART", "Axis-Aligned BART + 160 Random Rotations", "Oblique BART (adaptive)"), col = col_pal, lty = 1, lwd = 1.5, bty = "n")
# dev.off()




####################
### other plots ###
####################

# acceptance rates
rot_acc_dat = results %>%
  filter(
    experiment == "rot_axes",
    n_train == 1000,
    n_trees == 200,
    (n_rand_rot == 160 | n_rand_rot == 0)
  ) %>%
  group_by(model, theta) %>%
  summarize(
    mean = mean(acceptance_rate),
    se = sd(acceptance_rate) / sqrt(n())
  ) %>%
  mutate(
    U95 = mean + 2 * se,
    L95 = mean - 2 * se
  )

t = seq(0, pi / 4, length.out = 10)
m = split(rot_acc_dat$mean, rot_acc_dat$model)
u = split(rot_acc_dat$U95, rot_acc_dat$model)
l = split(rot_acc_dat$L95, rot_acc_dat$model)

# png(file = paste0(save_dir, "rot_time_fig.png"), width = 7, height = 5, units = "in", res = 500)
par(mar = c(c(4.1, 4.1, 2.1, 2.1)))
ryplot::init_scale(leg_pos = "none", xlab = expression(paste(theta)), ylab = "Acceptance rate", xlim = c(0, pi / 4), ylim = c(.35, .45), auto_mar = FALSE)
polygon(x = c(t, rev(t)), y = c(l$aabart, rev(u$aabart)), border = NA, col = scales::alpha(col_pal[1], 0.25))
polygon(x = c(t, rev(t)), y = c(l$rrbart, rev(u$rrbart)), border = NA, col = scales::alpha(col_pal[2], 0.25))
polygon(x = c(t, rev(t)), y = c(l$obart, rev(u$obart)), border = NA, col = scales::alpha(col_pal[3], 0.25))
lines(x = t, y = m$aabart, col = col_pal[1], lwd = 1.5)
lines(x = t, y = m$rrbart, col = col_pal[2], lwd = 1.5)
lines(x = t, y = m$obart, col = col_pal[3], lwd = 1.5)
legend(0, .45, legend = c("Axis-aligned BART", "Axis-Aligned BART + 160 Random Rotations", "Oblique BART (adaptive)"), col = col_pal, lty = 1, lwd = 1.5, bty = "n")
# dev.off()


# tree depths
rot_depth_dat = results %>%
  filter(
    experiment == "rot_axes",
    n_train == 1000,
    n_trees == 200,
    (n_rand_rot == 160 | n_rand_rot == 0)
  ) %>%
  group_by(model, theta) %>%
  summarize(
    mean = mean(mean_tree_depth),
    se = sd(mean_tree_depth) / sqrt(n())
  ) %>%
  mutate(
    U95 = mean + 2 * se,
    L95 = mean - 2 * se
  )

t = seq(0, pi / 4, length.out = 10)
m = split(rot_depth_dat$mean, rot_depth_dat$model)
u = split(rot_depth_dat$U95, rot_depth_dat$model)
l = split(rot_depth_dat$L95, rot_depth_dat$model)

# png(file = paste0(save_dir, "rot_time_fig.png"), width = 7, height = 5, units = "in", res = 500)
par(mar = c(c(4.1, 4.1, 2.1, 2.1)))
ryplot::init_scale(leg_pos = "none", xlab = expression(paste(theta)), ylab = "Mean tree depth", xlim = c(0, pi / 4), ylim = c(1.45, 1.6), auto_mar = FALSE)
polygon(x = c(t, rev(t)), y = c(l$aabart, rev(u$aabart)), border = NA, col = scales::alpha(col_pal[1], 0.25))
polygon(x = c(t, rev(t)), y = c(l$rrbart, rev(u$rrbart)), border = NA, col = scales::alpha(col_pal[2], 0.25))
polygon(x = c(t, rev(t)), y = c(l$obart, rev(u$obart)), border = NA, col = scales::alpha(col_pal[3], 0.25))
lines(x = t, y = m$aabart, col = col_pal[1], lwd = 1.5)
lines(x = t, y = m$rrbart, col = col_pal[2], lwd = 1.5)
lines(x = t, y = m$obart, col = col_pal[3], lwd = 1.5)
legend(0, 1.6, legend = c("Axis-aligned BART", "Axis-Aligned BART + 160 Random Rotations", "Oblique BART (adaptive)"), col = col_pal, lty = 1, lwd = 1.5, bty = "n")
# dev.off()


