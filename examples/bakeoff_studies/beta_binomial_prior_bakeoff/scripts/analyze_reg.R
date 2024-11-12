dir = "reg_results/" # set this to be the folder with results
data_dir <- "data/"


library(tidyverse)
library(ggplot2)



### load results ###


# obliqueBART results -----------------------------------------------------


# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(dir, list.files(dir), sep = "")

# loads results
results = lapply(files, loadRData) %>% bind_rows() %>%
  select(!contains("ystar"),
         -rmse_train)

# want 
# RAW MSE / mean( (Y_test - mean(Y_train))^2 )


#lets make a dataframe that has all the denominator for the above equation
# finding smse ------------------------------------------------------------
denom_df <- data.frame(data = rep(unique(results$data), 20))
for (i in 1:nrow(denom_df)) {
  denom_df$split[i] <- ((i-1) %/% 18) + 1
}

# load data
for (i in 1:nrow(denom_df)) {
  data_name <- stringr::str_c(stringr::str_c(data_dir,
                                             denom_df$data[i], 
                                             sep = ""),
                              "_data.RData", sep = "")
  load(data_name)
  split <- denom_df$split[i]
  Y_test <- Y[test_split_list[[split]]]
  Y_train <- Y[-test_split_list[[split]]]
  denom <- mean( (Y_test - mean(Y_train))^2 )
  denom_df$denom[i] <- denom
}
# model index
m = c(
  "obart7", "obart7oh"
)
# check how many of each model we have
n_check = results %>%
  distinct() %>%
  filter(model %in% m) %>%
  group_by(model, data) %>%
  summarize(n = n()) %>%
  pivot_wider(
    names_from = data,
    values_from = n
  )


# now lets look at other priors
prior_dir = "../oblique_priors_bakeoff/regression_prior_results/"
# gets names of all the files in dir
prior_files = stringr::str_c(prior_dir, list.files(prior_dir), sep = "")

# loads results
prior_results = lapply(prior_files, loadRData) %>% bind_rows() 

m = c(
  "axis_aligned",
  "axis_aligned-oh",
  "obart1.0", "obart1.5a", 
  "obart2.0", "obart2.5a", 
  "obart3.0", "obart3.5a", 
  "obart4.0", "obart4.5a", 
  "obart1.0-oh", "obart1.5a-oh", 
  "obart2.0-oh", "obart2.5a-oh", 
  "obart3.0-oh", "obart3.5a-oh", 
  "obart4.0-oh", "obart4.5a-oh",
  "wbart"
)

# check how many of each model we have
n_check = prior_results %>%
  distinct() %>%
  filter(model %in% m) %>%
  group_by(model, data) %>%
  summarize(n = n()) %>%
  pivot_wider(
    names_from = data,
    values_from = n
  )

prior_results <- prior_results %>%
  dplyr::select(job_id, model, data, split, rmse_test, mean_train_time)
prior_results <- prior_results %>%
  filter(model != "wbart") %>%
  dplyr::select(job_id, model, data, split, rmse_test, mean_train_time)

prior_total_results <- rbind(results, prior_results) %>%
  mutate(mse_test = rmse_test^2) %>%
  select(-rmse_test)





# find standardized test mse
for (i in 1:nrow(prior_total_results)) {
  split_id <- prior_total_results$split[i]
  data_id <- prior_total_results$data[i]
  denom <- denom_df %>%
    filter(split == split_id,
           data == data_id) %>%
    select(denom) %>%
    pull()
  prior_total_results$denom[i] <- denom
}
prior_total_results <- prior_total_results %>%
  mutate(standardized_mse_test = mse_test / denom)

# write_csv(prior_total_results, "../results/prior_total_results.csv")


prior_table <- prior_total_results %>% 
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model),
         !grepl("5a", model)) %>%
  group_by(data, model) %>%  
  summarize(
    mean_standardized_mse_test = mean(standardized_mse_test),
    mean_train_time = mean(mean_train_time)
  ) 

prior_table %>%
  ggplot(mapping = aes(x = model, 
                       y = mean_standardized_mse_test,
                       fill = model)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

prior_table_wide <- prior_table %>%
  select(-mean_train_time) %>%
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model)) %>% 
  group_by(data) %>%
  mutate(min_mse = min(mean_standardized_mse_test),
         mse_round = signif(mean_standardized_mse_test, 3)) %>%
  mutate(mse_round = cell_spec(mse_round,
                               format = "latex",
                               bold = mean_standardized_mse_test == min_mse)) %>%
  select(-min_mse, -mean_standardized_mse_test) %>%
  pivot_wider(names_from = model,
              values_from = mse_round)

prior_table_wide %>%
  kable(caption = "Mean standardized MSE across 20 cross validations. The best performing model is bolded. The Servo dataset did not have enough continuous predictors to form three-predictor combinations.",
        escape = F,
        label = "prior_table_reg",
        format = "latex") # Output format = latex 



#comparing to axis aligned
# now lets look at other priors
aa_vs_obl_dir = "../aa_vs_obl_bakeoff/results_aa_vs_obl_reg/"
# gets names of all the files in dir
aa_vs_obl_files = stringr::str_c(aa_vs_obl_dir, list.files(aa_vs_obl_dir), sep = "")

# loads results
aa_vs_obl_results = lapply(aa_vs_obl_files, loadRData) %>% bind_rows() 



aa_vs_obl_results <- aa_vs_obl_results %>%
  dplyr::select(job_id, model, data, split, rmse_test, mean_train_time)

aa_vs_obl_total_results <- rbind(results, aa_vs_obl_results) %>%
  mutate(mse_test = rmse_test^2) %>%
  select(-rmse_test)

# find standardized test mse
for (i in 1:nrow(aa_vs_obl_total_results)) {
  split_id <- aa_vs_obl_total_results$split[i]
  data_id <- aa_vs_obl_total_results$data[i]
  denom <- denom_df %>%
    filter(split == split_id,
           data == data_id) %>%
    select(denom) %>%
    pull()
  aa_vs_obl_total_results$denom[i] <- denom
}
aa_vs_obl_total_results <- aa_vs_obl_total_results %>%
  mutate(standardized_mse_test = mse_test / denom)

# write_csv(aa_vs_obl_total_results, "../results/aa_vs_obl_total_results_reg.csv")

aa_vs_obl_table <- aa_vs_obl_total_results %>% 
  group_by(data, model) %>%  
  filter(!(model %in% c("axis-aligned",
                        "obart1.5a",
                        "obart1.5a_oh",
                        "obart7oh"))) %>%
  summarize(
    mean_standardized_mse_test = mean(standardized_mse_test),
    mean_train_time = mean(mean_train_time)
  ) 

aa_vs_obl_table %>%
  ggplot(mapping = aes(x = model, 
                       y = mean_standardized_mse_test,
                       fill = model)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

aa_vs_obl_table_wide <- aa_vs_obl_table %>%
  select(-mean_train_time) %>%
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model)) %>% 
  group_by(data) %>%
  mutate(min_mse = min(mean_standardized_mse_test),
         mse_round = signif(mean_standardized_mse_test, 3)) %>%
  mutate(mse_round = cell_spec(mse_round,
                               format = "latex",
                               bold = mean_standardized_mse_test == min_mse)) %>%
  select(-min_mse, -mean_standardized_mse_test) %>%
  pivot_wider(names_from = model,
              values_from = mse_round)

aa_vs_obl_table_wide2 <- aa_vs_obl_table %>%
  select(-mean_train_time) %>%
  pivot_wider(names_from = model,
              values_from = mean_standardized_mse_test)

aa_vs_obl_table_wide %>%
  kable(caption = "Mean standardized MSE across 20 cross validations. The best performing model is bolded",
        escape = F,
        label = "aa_vs_obl_table_reg",
        format = "latex",
        booktabs = T) # Output format = latex 


############################
############################

#comparing to rot aa
# 
# rot_aa_vs_obl_dir = "../rotate_aa_vs_obl_bakeoff/rot_aa_vs_obl_reg_results/"
# # gets names of all the files in dir
# rot_aa_vs_obl_files = stringr::str_c(rot_aa_vs_obl_dir, list.files(rot_aa_vs_obl_dir), sep = "")
# 
# # loads results
# rot_aa_vs_obl_results = lapply(rot_aa_vs_obl_files, loadRData) %>% bind_rows() 
# 
# 
# 
# rot_aa_vs_obl_results <- rot_aa_vs_obl_results %>%
#   dplyr::select(job_id, num_rota, model, data, split, rmse_test, mean_train_time)
# 
# results$num_rota <- 1
# 
# rot_aa_vs_obl_total_results <- rbind(results %>%
#                                        select(job_id, num_rota, model, data, split, rmse_test, mean_train_time), rot_aa_vs_obl_results) %>%
#   mutate(mse_test = rmse_test^2) %>%
#   select(-rmse_test)
# 
# # find standardized test mse
# for (i in 1:nrow(rot_aa_vs_obl_total_results)) {
#   split_id <- rot_aa_vs_obl_total_results$split[i]
#   data_id <- rot_aa_vs_obl_total_results$data[i]
#   denom <- denom_df %>%
#     filter(split == split_id,
#            data == data_id) %>%
#     select(denom) %>%
#     pull()
#   rot_aa_vs_obl_total_results$denom[i] <- denom
# }
# rot_aa_vs_obl_total_results <- rot_aa_vs_obl_total_results %>%
#   mutate(standardized_mse_test = mse_test / denom)
# 
# 
# base_df <- rot_aa_vs_obl_total_results %>%
#   filter(model == "obart7") %>%
#   group_by(data) %>%
#   summarize(mean_mse_standard = mean(standardized_mse_test))
# 
# rot_aa_vs_obl_df <- rot_aa_vs_obl_total_results %>% 
#   filter(!(model %in% c("axis-aligned",
#                         "obart1.5a",
#                         "obart1.5a_oh",
#                         "obart7oh"))) %>%
#   group_by(data, num_rota, model) %>%
#   summarize(mean_mse_standard = mean(standardized_mse_test)) 
# 
# for (i in 1:nrow(rot_aa_vs_obl_df)) {
#   dataset <- rot_aa_vs_obl_df$data[i]
#   obl_result <- base_df %>%
#     filter(data == dataset) %>%
#     select(mean_mse_standard) %>%
#     pull()
#     
#   rot_aa_vs_obl_df$obl_result[i] <- obl_result
# }
# rot_aa_vs_obl_table <- rot_aa_vs_obl_df %>%
#   filter(mean_mse_standard <= obl_result) %>%
#   group_by(data, model) %>%
#   summarize(min_num_rota = min(num_rota)) %>%
#   pivot_wider(names_from = model,
#               values_from = min_num_rota)
# 
# 
# 
# rot_aa_vs_obl_table <- rot_aa_vs_obl_total_results %>% 
#   filter(!(model %in% c("axis-aligned",
#                         "obart1.5a",
#                         "obart1.5a_oh",
#                         "obart7oh"))) %>%
#   group_by(data, model, num_rota) %>%  
#   summarize(
#     mean_standardized_mse_test = mean(standardized_mse_test),
#     mean_train_time = mean(mean_train_time)
#   )  %>%
#   group_by(model, data) %>%
#   summarize(min_mse_test = min(mean_standardized_mse_test))
# 
# rot_aa_vs_obl_table %>%
#   ggplot(mapping = aes(x = model, 
#                        y = min_mse_test,
#                        fill = model)) + 
#   geom_boxplot() +
#   theme(legend.position="none") +
#   theme(axis.text.x=element_text(angle = -45, hjust = 0))
# 
# rot_aa_vs_obl_table_wide <- rot_aa_vs_obl_table %>%
#   group_by(data) %>%
#   mutate(min_mse = min(min_mse_test, na.rm = T),
#          mse_round = signif(min_mse_test, 3)) %>%
#   mutate(mse_round = cell_spec(mse_round,
#                                format = "latex",
#                                bold = min_mse_test == min_mse)) %>%
#   select(-min_mse, -min_mse_test) %>%
#   pivot_wider(names_from = model,
#               values_from = mse_round)
# rot_aa_vs_obl_table_wide <- rot_aa_vs_obl_table_wide[, c("data",
#                                                          "obart7",
#                                                          "rot_extra_trees",
#                                                          "rot_wbart",
#                                                          "rot_rand_forest",
#                                                          "rot_xgboost"
#                                                          )]
# 
# rot_aa_vs_obl_table_wide %>%
#   kable(caption = "Mean standardized MSE across 20 cross validations. The best performing model is bolded",
#         escape = F,
#         label = "rot_aa_vs_obl_table_reg",
#         format = "latex",
#         booktabs = T) # Output format = latex 



# t tests -----------------------------------------------------------------


######### code for t tests.
#give it results and base results

base_results <- prior_total_results %>%
  filter(model == "obart7")

get_t_results <- function(base_results, total_results, model_name, dataset){
  model_results_vec <- total_results %>%
    filter(model == model_name,
           data == dataset) %>%
    select(standardized_mse_test) %>%
    pull()
  if (length(model_results_vec) == 0) {
    return(NA)
  }
  else{
    base_results_vec <- base_results %>%
      filter(model == "obart7",
             data == dataset) %>%
      select(standardized_mse_test) %>%
      pull()
    if (length(model_results_vec) == length(base_results_vec)) {
      test_results <- t.test(model_results_vec, base_results_vec,
                             alternative = "greater",
                             paired = T)
    } else{
      test_results <- t.test(model_results_vec, base_results_vec,
                             alternative = "greater",
                             paired = F) # some missing results for pbart 200 num rota
    }
    pval <- test_results$p.value
    return((!is.na(pval)) & (pval < .05))
  }
  }
 

get_t_results(base_results,
              total_results = prior_total_results,
              model_name = "obart3.0",
              dataset = "budget")


t_test_df_prior <- data.frame(matrix(nrow = 18,
                          ncol = 5))
colnames(t_test_df_prior) <- c("data",
                               "obart1.0",
                               "obart2.0", 
                               "obart3.0",
                               "obart4.0")
t_test_df_prior$data <- unique(prior_total_results$data)
for (i in 1:nrow(t_test_df_prior)) {
  for (j in 2:length(colnames(t_test_df_prior))) {
    dataset <-  t_test_df_prior$data[i]
    model_name <- colnames(t_test_df_prior)[j]
    t_test_df_prior[i,j] <- get_t_results(base_results,
                                          total_results = prior_total_results,
                                          model_name = model_name,
                                          dataset = dataset)
    
  }
}

t_test_df_prior$obart7 <- rep(F, nrow(t_test_df_prior))

t_test_df_prior <- t_test_df_prior %>%
  pivot_longer(cols = 2:6,
               names_to = "model",
               values_to = "ttest")


prior_table_t_test <- prior_table %>%
  select(-mean_train_time) %>%
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model)) %>% 
  left_join(t_test_df_prior) %>%
  group_by(data) %>%
  mutate(min_mse = min(mean_standardized_mse_test),
         mse_round = signif(mean_standardized_mse_test, 3)) %>%
  mutate(mse_round = cell_spec(mse_round,
                               format = "latex",
                               bold = mean_standardized_mse_test == min_mse,
                               underline = ttest),
         mse_round = gsub("\\\\underline", "*", mse_round)) %>%
  select(model, mse_round) %>%
  pivot_wider(names_from = model,
              values_from = mse_round) 
colnames(prior_table_t_test) <- c("data", "2-sparse", "3-sparse", "$p$-sparse", "spike-slab")
prior_table_t_test %>%
  kable(caption = "Mean standardized MSE across 20 cross validations. The best performing model is bolded. Data and prior combinations that differed significantly from `Spike-Slab' results are marked with an asterisk",
        escape = F,
        label = "prior_table_reg",
        format = "latex") # Output format = latex 




# now, to do aa vs obl

base_results <- prior_total_results %>%
  filter(model == "obart7")

get_t_results(base_results,
              total_results = aa_vs_obl_total_results,
              model_name = "xgboost",
              dataset = "abalone")


t_test_df_aa_vs_obl <- data.frame(matrix(nrow = 18,
                                     ncol = 6))
colnames(t_test_df_aa_vs_obl) <- c("data",
                               "extra_trees", 
                               "flexBART",
                               "rand_forest",
                               "wbart",
                               "xgboost")
t_test_df_aa_vs_obl$data <- unique(aa_vs_obl_total_results$data)
for (i in 1:nrow(t_test_df_aa_vs_obl)) {
  for (j in 2:length(colnames(t_test_df_aa_vs_obl))) {
    dataset <-  t_test_df_aa_vs_obl$data[i]
    model_name <- colnames(t_test_df_aa_vs_obl)[j]
    t_test_df_aa_vs_obl[i,j] <- get_t_results(base_results,
                                          total_results = aa_vs_obl_total_results,
                                          model_name = model_name,
                                          dataset = dataset)
    
  }
}

#just adding an extra row here so that i can put in paper easier
t_test_df_aa_vs_obl$obart7 <- rep(F, nrow(t_test_df_aa_vs_obl))

t_test_df_aa_vs_obl <- t_test_df_aa_vs_obl %>%
  pivot_longer(cols = 2:7,
               names_to = "model",
               values_to = "ttest")


aa_va_obl_table_t_test <- aa_vs_obl_table %>%
  select(-mean_train_time) %>%
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model),
         model!= "flexBART") %>% 
  left_join(t_test_df_aa_vs_obl) %>%
  group_by(data) %>%
  mutate(min_mse = min(mean_standardized_mse_test),
         mse_round = signif(mean_standardized_mse_test, 3)) %>%
  mutate(mse_round = cell_spec(mse_round,
                               format = "latex",
                               bold = mean_standardized_mse_test == min_mse,
                               underline = ttest),
         mse_round = gsub("\\\\underline", "*", mse_round)) %>%
  select(model, mse_round) %>%
  pivot_wider(names_from = model,
              values_from = mse_round) 
colnames(aa_va_obl_table_t_test) <- c("data", "extraTrees", "obliqueBART", "randomForest", "wbart", "xgboost")
aa_va_obl_table_t_test %>%
  kable(caption = "Mean standardized MSE across 20 cross validations. The best performing model is bolded. Data and model combinations that differed significantly from ObliqueBART results are marked with an asterisk",
        escape = F,
        label = "aa_vs_obl_table_reg",
        format = "latex") # Output format = latex 

aa_vs_obl_table %>% group_by(data) %>%
  summarize(min = min(mean_standardized_mse_test))

wide2_aa_vs_obl_table <- aa_vs_obl_table %>%
  select(-mean_train_time) %>%
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model),
         model!= "flexBART") %>%
  pivot_wider(names_from = model,
              values_from = mean_standardized_mse_test)

sum(wide2_aa_vs_obl_table$obart7 <= wide2_aa_vs_obl_table$wbart)


# rotated ensembles -------------------------------------------------------


rot_aa_vs_obl_dir = "../rotate_aa_vs_obl_bakeoff/rot_aa_vs_obl_reg_results/"
# gets names of all the files in dir
rot_aa_vs_obl_files = stringr::str_c(rot_aa_vs_obl_dir, list.files(rot_aa_vs_obl_dir), sep = "")

# loads results
rot_aa_vs_obl_results = lapply(rot_aa_vs_obl_files, loadRData) %>% bind_rows() 



rot_aa_vs_obl_results <- rot_aa_vs_obl_results %>%
  dplyr::select(job_id, num_rota, model, data, split, rmse_test, mean_train_time) 

results$num_rota <- 1

rot_aa_vs_obl_total_results <- rbind(results %>%
                                       select(job_id, num_rota, model, data, split, rmse_test, mean_train_time), rot_aa_vs_obl_results) %>%
  mutate(mse_test = rmse_test^2) %>%
  select(-rmse_test)

# find standardized test mse
for (i in 1:nrow(rot_aa_vs_obl_total_results)) {
  split_id <- rot_aa_vs_obl_total_results$split[i]
  data_id <- rot_aa_vs_obl_total_results$data[i]
  denom <- denom_df %>%
    filter(split == split_id,
           data == data_id) %>%
    select(denom) %>%
    pull()
  rot_aa_vs_obl_total_results$denom[i] <- denom
}
rot_aa_vs_obl_total_results <- rot_aa_vs_obl_total_results %>%
  mutate(standardized_mse_test = mse_test / denom) %>%
  filter(!(model %in% c("obart7oh",
                        "obart1.5a")))

# write_csv(rot_aa_vs_obl_total_results, "../results/rot_aa_vs_obl_total_results_reg.csv")
get_t_results_rot <- function(base_results, total_results, num_rotation, model_name, dataset){
  model_results_vec <- total_results %>%
    filter(model == model_name,
           data == dataset,
           num_rota == num_rotation) %>%
    select(standardized_mse_test) %>%
    pull()
  if (length(model_results_vec) == 0) {
    return(NA)
  }
  else{
    base_results_vec <- base_results %>%
      filter(model == "obart7",
             data == dataset) %>%
      select(standardized_mse_test) %>%
      pull()
    if (length(model_results_vec) == length(base_results_vec)) {
      test_results <- t.test(model_results_vec, base_results_vec,
                             alternative = "two.sided",
                             paired = T)
    } else{
      test_results <- t.test(model_results_vec, base_results_vec,
                             alternative = "two.sided",
                             paired = F) # some missing results for pbart 200 num rota
    }
    
    pval <- test_results$p.value
    return((!is.na(pval)) & (pval < .05))
  }
}

t_test_obl_vs_obl <- expand.grid(num_rota = unique(rot_aa_vs_obl_total_results$num_rota),
                                 data = unique(rot_aa_vs_obl_total_results$data),
                                 model = unique(rot_aa_vs_obl_total_results$model)) %>%
  filter(!((model == "obart7") & (num_rota!=1))) 
t_test_obl_vs_obl$ttest = NA
for (i in 1:nrow(t_test_obl_vs_obl)) {
  t_test_obl_vs_obl$ttest[i] <- get_t_results_rot(base_results,
                                                    rot_aa_vs_obl_total_results,
                                                    num_rotation = t_test_obl_vs_obl$num_rota[i],
                                                    model_name = t_test_obl_vs_obl$model[i],
                                                    dataset = t_test_obl_vs_obl$data[i])
}

get_t_results_rot(base_results, rot_aa_vs_obl_total_results,
                  num_rotation = 1,
                  model_name = "rot_extra_trees",
                  data = "abalone")

# this table has the minimum number of rotations for the difference between obliqueBART and rotated ensemble is *not* statistically significant. ie minimum number of rotations for the results to be similar
t_test_obl_vs_obl_min <- t_test_obl_vs_obl %>%
  filter(!ttest) %>%
  group_by(data, model) %>%
  summarize(min_num_rota = min(num_rota)) %>%
  pivot_wider(names_from = model,
              values_from = min_num_rota) 
t_test_obl_vs_obl_min_table <- t_test_obl_vs_obl_min %>%
  select(-obart7) 
colnames(t_test_obl_vs_obl_min_table) <- c("data", "wbart.r", "xgboost.r", "extraTrees.r", "randomForest.r")
t_test_obl_vs_obl_min_table %>%
  kable(caption = "Number of random rotations of the data for difference between obliqueBART and rotated ensemble MSE to be statistically insignificant. Model and data combinations with NA had MSE's that were very different than obliqueBART, even with up to 200 random data rotations.",
        escape = F,
        label = "rot_aa_vs_obl_table_reg",
        format = "latex") # Output format = latex 



# old table
rot_aa_vs_obl_dir = "../rotate_aa_vs_obl_bakeoff/rot_aa_vs_obl_reg_results/"
# gets names of all the files in dir
rot_aa_vs_obl_files = stringr::str_c(rot_aa_vs_obl_dir, list.files(rot_aa_vs_obl_dir), sep = "")

# loads results
rot_aa_vs_obl_results = lapply(rot_aa_vs_obl_files, loadRData) %>% bind_rows()



rot_aa_vs_obl_results <- rot_aa_vs_obl_results %>%
  dplyr::select(job_id, num_rota, model, data, split, rmse_test, mean_train_time)

results$num_rota <- 1

rot_aa_vs_obl_total_results <- rbind(results %>%
                                       select(job_id, num_rota, model, data, split, rmse_test, mean_train_time), rot_aa_vs_obl_results) %>%
  mutate(mse_test = rmse_test^2) %>%
  select(-rmse_test)

# find standardized test mse
for (i in 1:nrow(rot_aa_vs_obl_total_results)) {
  split_id <- rot_aa_vs_obl_total_results$split[i]
  data_id <- rot_aa_vs_obl_total_results$data[i]
  denom <- denom_df %>%
    filter(split == split_id,
           data == data_id) %>%
    select(denom) %>%
    pull()
  rot_aa_vs_obl_total_results$denom[i] <- denom
}
rot_aa_vs_obl_total_results <- rot_aa_vs_obl_total_results %>%
  mutate(standardized_mse_test = mse_test / denom)


base_df <- rot_aa_vs_obl_total_results %>%
  filter(model == "obart7") %>%
  group_by(data) %>%
  summarize(mean_mse_standard = mean(standardized_mse_test))

rot_aa_vs_obl_df <- rot_aa_vs_obl_total_results %>%
  filter(!(model %in% c("axis-aligned",
                        "obart1.5a",
                        "obart1.5a_oh",
                        "obart7oh"))) %>%
  group_by(data, num_rota, model) %>%
  summarize(mean_mse_standard = mean(standardized_mse_test))

for (i in 1:nrow(rot_aa_vs_obl_df)) {
  dataset <- rot_aa_vs_obl_df$data[i]
  obl_result <- base_df %>%
    filter(data == dataset) %>%
    select(mean_mse_standard) %>%
    pull()

  rot_aa_vs_obl_df$obl_result[i] <- obl_result
}
rot_aa_vs_obl_table <- rot_aa_vs_obl_df %>%
  group_by(data, model) %>%
  summarize(min_mean_mse_standard = min(mean_mse_standard)) %>%
  pivot_wider(names_from = model,
              values_from = min_mean_mse_standard)
rot_aa_vs_obl_table$obart7 < rot_aa_vs_obl_table[,4:6]



# investigating smse issues -----------------------------------------------


# rot_aa_vs_obl_table <- rot_aa_vs_obl_total_results %>%
#   filter(!(model %in% c("axis-aligned",
#                         "obart1.5a",
#                         "obart1.5a_oh",
#                         "obart7oh"))) %>%
#   group_by(data, model, num_rota) %>%
#   summarize(
#     mean_standardized_mse_test = mean(standardized_mse_test),
#     mean_train_time = mean(mean_train_time)
#   )  %>%
#   group_by(model, data) %>%
#   summarize(min_mse_test = min(mean_standardized_mse_test))
# 
# rot_aa_vs_obl_table %>%
#   ggplot(mapping = aes(x = model,
#                        y = min_mse_test,
#                        fill = model)) +
#   geom_boxplot() +
#   theme(legend.position="none") +
#   theme(axis.text.x=element_text(angle = -45, hjust = 0))
# 
# rot_aa_vs_obl_table_wide <- rot_aa_vs_obl_table %>%
#   group_by(data) %>%
#   mutate(min_mse = min(min_mse_test, na.rm = T),
#          mse_round = signif(min_mse_test, 3)) %>%
#   mutate(mse_round = cell_spec(mse_round,
#                                format = "latex",
#                                bold = min_mse_test == min_mse)) %>%
#   select(-min_mse, -min_mse_test) %>%
#   pivot_wider(names_from = model,
#               values_from = mse_round)
# rot_aa_vs_obl_table_wide <- rot_aa_vs_obl_table_wide[, c("data",
#                                                          "obart7",
#                                                          "rot_extra_trees",
#                                                          "rot_wbart",
#                                                          "rot_rand_forest",
#                                                          "rot_xgboost"
#                                                          )]
# 
# rot_aa_vs_obl_table_wide %>%
#   kable(caption = "Mean standardized MSE across 20 cross validations. The best performing model is bolded",
#         escape = F,
#         label = "rot_aa_vs_obl_table_reg",
#         format = "latex",
#         booktabs = T) # Output format = latex

