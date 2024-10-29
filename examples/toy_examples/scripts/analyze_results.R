# Author: Ryan Yee
# Date: August 13, 2024
# Purpose: analyze results of toy examples
# Details: 
# Dependencies: dplyr, ggplot2

dir = "../results2/"

library(tidyverse)
library(ggplot2)

### load results ###

# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(dir, list.files(dir), sep = "")

# loads results
results = lapply(files, loadRData) %>% bind_rows()

# check that all experiments ran successfully
n_check = results %>%
  group_by(model, experiment, n_train, sigma, n_trees) %>%
  summarize(n = n()) %>%
  pivot_wider(
    names_from = experiment,
    values_from = n
  )


### results ###

# quick ggplot stuff (we'll make nicer figures for the manuscript)

# 1. compare aa and obart with different numbers of trees
results %>%
  # filter(model %in% c("obart", "aabart")) %>%
  mutate(
    fn_trees = as.factor(n_trees),
    label = paste0(model, n_trees)
    ) %>%
  group_by(experiment, label, n_train, fn_trees, theta) %>%
  summarize(
    mean_rmse_train = mean(rmse_train),
    mean_rmse_test = mean(rmse_test),
    mean_acceptance = mean(acceptance_rate),
    mean_depth = mean(mean_tree_depth),
    mean_train_time = mean(train_time)
  ) %>%
  ggplot(aes(x = theta, y = mean_rmse_test, col = label)) +
  geom_line() +
  facet_grid(rows = vars(n_train), cols = vars(experiment), scales = "free")

# 2. see how random rotations did
results %>%
  filter((model == "rrbart" | (model == "obart" & n_trees == 200))) %>%
  mutate(fp = as.factor(n_rand_rot)) %>%
  group_by(experiment, model, n_train, fp, theta) %>%
  summarize(
    mean_rmse_train = mean(rmse_train),
    mean_rmse_test = mean(rmse_test),
    mean_acceptance = mean(acceptance_rate),
    mean_depth = mean(mean_tree_depth),
    mean_train_time = mean(train_time)
  ) %>%
  ggplot(aes(x = theta, y = mean_rmse_test, col = fp)) +
  geom_line() +
  facet_grid(rows = vars(n_train), cols = vars(experiment), scales = "free")

# 3. compare test RMSEs
results %>%
  filter(
    n_trees == 200,
    (n_rand_rot == 0 | n_rand_rot == 80)
  ) %>%
  group_by(experiment, model, n_train, theta) %>%
  summarize(
    mean_rmse_train = mean(rmse_train),
    mean_rmse_test = mean(rmse_test),
    mean_acceptance = mean(acceptance_rate),
    mean_depth = mean(mean_tree_depth),
    mean_train_time = mean(train_time)
  ) %>%
  ggplot(aes(x = theta, y = mean_rmse_test, col = model)) +
  geom_line() +
  facet_grid(rows = vars(n_train), cols = vars(experiment), scales = "free")

# 4. compare acceptance ratios
results %>%
  filter(
    n_trees == 200,
    (n_rand_rot == 0 | n_rand_rot == 80)
  ) %>%
  group_by(experiment, model, n_train, theta) %>%
  summarize(
    mean_rmse_train = mean(rmse_train),
    mean_rmse_test = mean(rmse_test),
    mean_acceptance = mean(acceptance_rate),
    mean_depth = mean(mean_tree_depth),
    mean_train_time = mean(train_time)
  ) %>%
  ggplot(aes(x = theta, y = mean_acceptance, col = model)) +
  geom_line() +
  facet_grid(rows = vars(n_train), cols = vars(experiment), scales = "free")

# 5. compare tree depths
results %>%
  filter(
    n_trees == 200,
    (n_rand_rot == 0 | n_rand_rot == 80)
  ) %>%
  group_by(experiment, model, n_train, theta) %>%
  summarize(
    mean_rmse_train = mean(rmse_train),
    mean_rmse_test = mean(rmse_test),
    mean_acceptance = mean(acceptance_rate),
    mean_depth = mean(mean_tree_depth),
    mean_train_time = mean(train_time)
  ) %>%
  ggplot(aes(x = theta, y = mean_depth, col = model)) +
  geom_line() +
  facet_grid(rows = vars(n_train), cols = vars(experiment), scales = "free")

# 6. compare train times
results %>%
  filter(
    n_trees == 200,
    (n_rand_rot == 0 | n_rand_rot == 80)
  ) %>%
  group_by(experiment, model, n_train, theta) %>%
  summarize(
    mean_rmse_train = mean(rmse_train),
    mean_rmse_test = mean(rmse_test),
    mean_acceptance = mean(acceptance_rate),
    mean_depth = mean(mean_tree_depth),
    mean_train_time = mean(train_time)
  ) %>%
  ggplot(aes(x = theta, y = mean_train_time, col = model)) +
  geom_line() +
  facet_grid(rows = vars(n_train), cols = vars(experiment), scales = "free")

