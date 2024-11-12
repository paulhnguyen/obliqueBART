dir = "class_results/"  # set this to be the folder with results

library(tidyverse)
library(ggplot2)
library(kableExtra)
library(stringr)

#want to report raw accuracies, not the ratios

### load results ###

# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(dir, list.files(dir), sep = "")

# loads results
results = lapply(files, loadRData) %>% bind_rows() %>% filter(model == "obart7")

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
prior_dir = "../oblique_priors_bakeoff/class_prior_results/"
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
  "obart4.0-oh", "obart4.5a-oh"
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

total_results <- rbind(results, 
                       prior_results  %>% 
                        dplyr::select(1:17)) %>%
  filter(model != "pbart")


prior_table <- total_results  %>% 
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model),
         !grepl("5a", model)) %>% 
  group_by(data, model) %>%  
  summarize(
    mean_accuracy_test = mean(accuracy_test),
    mean_train_time = mean(mean_train_time),
  ) 

prior_table %>%
  ggplot(mapping = aes(x = model, 
                       y = mean_accuracy_test,
                       fill = model)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))


prior_table_wide <- prior_table %>%
  select(-mean_train_time) %>%
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model),
         !grepl("5a", model)) %>% 
  group_by(data) %>%
  mutate(max_accuracy = max(mean_accuracy_test),
         mean_accuracy_test_round = signif(mean_accuracy_test, 3)) %>%
  mutate(mean_accuracy_test_round = cell_spec(mean_accuracy_test_round,
                                        format = "latex",
                                        bold = mean_accuracy_test == max_accuracy)) %>%
  select(-max_accuracy, -mean_accuracy_test) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test_round)

prior_table_wide$data <- c("ILPD",
                           "banknote",
                           "blood transfusion",
                           "breast cancer diag.",
                           "breast cancer",
                           "breast cancer prog.",
                           "climate crashes",
                           "connectionist sonar",
                           "credit approval",
                           "echocardiogram",
                           "fertility",
                           "hepatitis",
                           "ionosphere",
                           "ozone 1",
                           "ozone 8",
                           "parkinsons",
                           "planning relax",
                           "qsar biodegradable",
                           "seismic bumps",
                           "spambase",
                           "spectf heart",
                           "statlog german cred.")

prior_table_wide %>%
  kable(caption = "Mean accuracy across 20 cross validations. The best performing model is bolded",
        escape = F,
        digits = 3,
        label = "prior_table_class",
        format = "latex") # Output format = latex 


# now lets look at other aa vs obl
aa_vs_obl_dir = "../aa_vs_obl_bakeoff/results_aa_vs_obl_class/"
# gets names of all the files in dir
aa_vs_obl_files = stringr::str_c(aa_vs_obl_dir, list.files(aa_vs_obl_dir), sep = "")

# loads results
aa_vs_obl_results = lapply(aa_vs_obl_files, loadRData) %>% bind_rows() 

m = c(
  "pbart",
  "axis-aligned",
  "obart1.5a", "obart1.5a_oh", 
  "xgboost",
  "extra_trees",
  "rand_forest",
  "flexBART"
)

# check how many of each model we have
n_check = aa_vs_obl_results %>%
  distinct() %>%
  filter(model %in% m) %>%
  group_by(model, data) %>%
  summarize(n = n()) %>%
  pivot_wider(
    names_from = data,
    values_from = n
  )

aa_vs_obl_total_results <- rbind(results, 
                       aa_vs_obl_results  %>% 
                         dplyr::select(1:17))

aa_vs_obl_table <- aa_vs_obl_total_results %>%
  filter(!(model %in% c("obart1.5a", "obart1.5a_oh"))) %>%
  group_by(data, model) %>%  
  summarize(
    mean_accuracy_test = mean(accuracy_test),
    mean_train_time = mean(mean_train_time),
  ) 

aa_vs_obl_table %>%
  ggplot(mapping = aes(x = model, 
                       y = mean_accuracy_test,
                       fill = model)) + 
  geom_boxplot()  +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

aa_vs_obl_table_wide <- aa_vs_obl_table %>%
  filter(model != "axis-aligned",
         data != "thoraric_surgery") %>%
  select(-mean_train_time) %>%
  group_by(data) %>%
  mutate(max_accuracy = max(mean_accuracy_test),
         mean_accuracy_test_round = signif(mean_accuracy_test, 3)) %>%
  mutate(mean_accuracy_test_round = cell_spec(mean_accuracy_test_round,
                                              format = "latex",
                                              bold = mean_accuracy_test == max_accuracy)) %>%
  select(-max_accuracy, -mean_accuracy_test) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test_round)


aa_vs_obl_table_wide2 <- aa_vs_obl_table %>%
  filter(model != "axis-aligned",
         data != "thoraric_surgery") %>%
  select(-mean_train_time) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test)

mean(aa_vs_obl_table_wide2$extra_trees) - mean(aa_vs_obl_table_wide2$obart7)

dif <- (aa_vs_obl_table_wide2[,"extra_trees"] - aa_vs_obl_table_wide2[,"obart7"])

sum(aa_vs_obl_table_wide2$pbart > aa_vs_obl_table_wide2$obart7)
dif2 <- (aa_vs_obl_table_wide2[,"pbart"] - aa_vs_obl_table_wide2[,"obart7"])
aa_vs_obl_table_wide$data <- c("ILPD",
                           "banknote",
                           "blood transfusion",
                           "breast cancer diag.",
                           "breast cancer",
                           "breast cancer prog.",
                           "climate crashes",
                           "connectionist sonar",
                           "credit approval",
                           "echocardiogram",
                           "fertility",
                           "hepatitis",
                           "ionosphere",
                           "ozone 1",
                           "ozone 8",
                           "parkinsons",
                           "planning relax",
                           "qsar biodegradable",
                           "seismic bumps",
                           "spambase",
                           "spectf heart",
                           "statlog german cred.")

aa_vs_obl_table_wide %>%
  kable(caption = "Mean accuracy across 20 cross validations. The best performing model is bolded",
        escape = F,
        digits = 3,
        label = "aa_vs_obl_table_class",
        format = "latex") # Output format = latex 

# now lets look at obl vs obl

# now lets look at other aa vs obl
rot_aa_vs_obl_dir = "../rotate_aa_vs_obl_bakeoff/rot_aa_vs_obl_class_results/"
# gets names of all the files in dir
rot_aa_vs_obl_files = stringr::str_c(rot_aa_vs_obl_dir, list.files(rot_aa_vs_obl_dir), sep = "")

# loads results
rot_aa_vs_obl_results = lapply(rot_aa_vs_obl_files, loadRData) %>% bind_rows() 

m = c(
  "rot_extra_trees",
  "rot_rand_forest",
  "rot_pbart",
  "rot_xgboost",
  "obart1.5a",
  "rotation_forest"
)

# check how many of each model we have
n_check = rot_aa_vs_obl_results %>%
  distinct() %>%
  filter(model %in% m) %>%
  group_by(model, data) %>%
  summarize(n = n()) %>%
  pivot_wider(
    names_from = data,
    values_from = n
  )

results$num_rota <- 1
total_results <- rbind(results, 
                       rot_aa_vs_obl_results  %>% 
                         dplyr::select(1:18)) %>%
  filter(!(model %in% c("obart1.5a", "obart1.5a_oh"))) 


rot_aa_vs_obl_table <- total_results %>% 
  group_by(data, model, num_rota) %>%  
  summarize(
    mean_accuracy_test = mean(accuracy_test),
    mean_train_time = mean(mean_train_time),
  )  %>%
  group_by(model, data) %>%
  summarize(max_mean_accuracy_test = max(mean_accuracy_test))


rot_aa_vs_obl_table %>%
  ggplot(mapping = aes(x = model, 
                       y = max_mean_accuracy_test,
                       fill = model)) + 
  geom_boxplot() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))


rot_aa_vs_obl_table_wide <- rot_aa_vs_obl_table %>%
  filter(data != "thoraric_surgery") %>%
  group_by(data) %>%
  mutate(max_accuracy = max(max_mean_accuracy_test),
         mean_accuracy_test_round = signif(max_mean_accuracy_test, 3)) %>%
  mutate(mean_accuracy_test_round = cell_spec(mean_accuracy_test_round,
                                              format = "latex",
                                              bold = max_mean_accuracy_test == max_accuracy)) %>%
  select(-max_accuracy, -max_mean_accuracy_test) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test_round)

rot_aa_vs_obl_table_wide$data <- c("ILPD",
                               "banknote",
                               "blood transfusion",
                               "breast cancer diag.",
                               "breast cancer",
                               "breast cancer prog.",
                               "climate crashes",
                               "connectionist sonar",
                               "credit approval",
                               "echocardiogram",
                               "fertility",
                               "hepatitis",
                               "ionosphere",
                               "ozone 1",
                               "ozone 8",
                               "parkinsons",
                               "planning relax",
                               "qsar biodegradable",
                               "seismic bumps",
                               "spambase",
                               "spectf heart",
                               "statlog german cred.")

rot_aa_vs_obl_table_wide %>%
  kable(caption = "Mean accuracy across 20 cross validations. The best performing model is bolded",
        escape = F,
        digits = 3,
        label = "rot_aa_vs_obl_table_class",
        format = "latex") # Output format = latex 

# tables for the paper


# t tests -----------------------------------------------------------------


######### code for t tests.
#give it results and base results
# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(dir, list.files(dir), sep = "")

# loads results
results = lapply(files, loadRData) %>% bind_rows() %>% filter(model == "obart7")

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
prior_dir = "../oblique_priors_bakeoff/class_prior_results/"
# gets names of all the files in dir
prior_files = stringr::str_c(prior_dir, list.files(prior_dir), sep = "")

# loads results
prior_results = lapply(prior_files, loadRData) %>% bind_rows() 

prior_total_results <- rbind(results, prior_results[,1:ncol(results)]) %>%
  filter(data != "thoraric_surgery")

# write_csv(prior_total_results, "../results/prior_total_results_class.csv")

base_results <- prior_total_results %>%
  filter(model == "obart7")

get_t_results <- function(base_results, total_results, model_name, dataset){
  model_results_vec <- total_results %>%
    filter(model == model_name,
           data == dataset) %>%
    select(accuracy_test) %>%
    pull()
  if (length(model_results_vec) == 0) {
    return(NA)
  }
  else{
    base_results_vec <- base_results %>%
      filter(model == "obart7",
             data == dataset) %>%
      select(accuracy_test) %>%
      pull()
    test_results <- t.test(model_results_vec, base_results_vec,
                           alternative = "less",
                           paired = T)
    pval <- test_results$p.value
    return((!is.na(pval)) & (pval < .05))
  }
}


get_t_results(base_results,
              total_results = prior_total_results,
              model_name = "obart4.0",
              dataset = "qsar_biodegradable")


t_test_df_prior <- data.frame(matrix(nrow = length(unique(total_results$data) ) - 1,
                                     ncol = 4))
colnames(t_test_df_prior) <- c("data",
                               "obart1.0",
                               "obart3.0",
                               "obart4.0")
t_test_df_prior$data <- unique(prior_total_results$data)[1:22]
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
  pivot_longer(cols = 2:5,
               names_to = "model",
               values_to = "ttest")


prior_table_t_test <- prior_table %>%
  select(-mean_train_time) %>%
  filter(!grepl("oh", model),
         model != "axis_aligned",
         !grepl("2", model)) %>% 
  left_join(t_test_df_prior) %>%
  group_by(data) %>%
  mutate(max_acc = max(mean_accuracy_test),
         accuracy_round = signif(mean_accuracy_test, 3)) %>%
  mutate(accuracy_round = cell_spec(accuracy_round,
                               format = "latex",
                               bold = (mean_accuracy_test == max_acc),
                               underline = ttest),
         accuracy_round = gsub("\\\\underline", "*", accuracy_round)) %>%
  select(model, accuracy_round) %>%
  pivot_wider(names_from = model,
              values_from = accuracy_round) 
colnames(prior_table_t_test) <- c("data", "2-sparse", "3-sparse", "$p$-sparse", "spike-slab")
prior_table_t_test$data <- c("ILPD",
                                   "banknote",
                                   "blood transfusion",
                                   "breast cancer diag.",
                                   "breast cancer",
                                   "breast cancer prog.",
                                   "climate crashes",
                                   "connectionist sonar",
                                   "credit approval",
                                   "echocardiogram",
                                   "fertility",
                                   "hepatitis",
                                   "ionosphere",
                                   "ozone 1",
                                   "ozone 8",
                                   "parkinsons",
                                   "planning relax",
                                   "qsar biodegradable",
                                   "seismic bumps",
                                   "spambase",
                                   "spectf heart",
                                   "statlog german cred.")
prior_table_t_test %>%
  kable(caption = "Mean test accuracy across 20 cross validations. The best performing model is bolded. Data and prior combinations that differed significantly from `Spike-Slab' results are marked with an asterisk",
        escape = F,
        label = "prior_table_class",
        format = "latex") # Output format = latex 




# now, to do aa vs obl
aa_vs_obl_dir = "../aa_vs_obl_bakeoff/results_aa_vs_obl_class/"
# gets names of all the files in dir
aa_vs_obl_files = stringr::str_c(aa_vs_obl_dir, list.files(aa_vs_obl_dir), sep = "")

# loads results
aa_vs_obl_results = lapply(aa_vs_obl_files, loadRData) %>% bind_rows() 



aa_vs_obl_results <- aa_vs_obl_results %>%
  dplyr::select(job_id, model, data, split, accuracy_test, mean_train_time)

base_results <- prior_total_results %>%
  filter(model == "obart7")


t_test_df_aa_vs_obl <- data.frame(matrix(nrow = 22,
                                         ncol = 6))
colnames(t_test_df_aa_vs_obl) <- c("data",
                                   "extra_trees", 
                                   "flexBART",
                                   "rand_forest",
                                   "pbart",
                                   "xgboost")
aa_vs_obl_total_results <- rbind(results %>%
                                   select(names(aa_vs_obl_results)), aa_vs_obl_results)  %>%
  filter(data != "thoraric_surgery")

# write_csv(aa_vs_obl_total_results, "../results/aa_vs_obl_total_results_class.csv")

t_test_df_aa_vs_obl$data <- unique(aa_vs_obl_total_results$data)[1:22]
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
         model != "axis-aligned",
         !grepl("2", model),
         data != "thoraric_surgery",
         model != "flexBART") %>% 
  left_join(t_test_df_aa_vs_obl) %>%
  group_by(data) %>%
  mutate(max_acc = max(mean_accuracy_test),
         accuracy_round = signif(mean_accuracy_test, 3)) %>%
  mutate(accuracy_round = cell_spec(accuracy_round,
                                    format = "latex",
                                    bold = (mean_accuracy_test == max_acc),
                                    underline = ttest),
         accuracy_round = gsub("\\\\underline", "*", accuracy_round)) %>%
  select(model, accuracy_round) %>%
  pivot_wider(names_from = model,
              values_from = accuracy_round) 
aa_va_obl_table_t_test$data <- c("ILPD",
                             "banknote",
                             "blood transfusion",
                             "breast cancer diag.",
                             "breast cancer",
                             "breast cancer prog.",
                             "climate crashes",
                             "connectionist sonar",
                             "credit approval",
                             "echocardiogram",
                             "fertility",
                             "hepatitis",
                             "ionosphere",
                             "ozone 1",
                             "ozone 8",
                             "parkinsons",
                             "planning relax",
                             "qsar biodegradable",
                             "seismic bumps",
                             "spambase",
                             "spectf heart",
                             "statlog german cred.")
colnames(aa_va_obl_table_t_test) <- c("data", "extraTrees", "obliqueBART", "pbart", "randomForest", "xgboost")
aa_va_obl_table_t_test %>%
  kable(caption = "Mean standardized MSE across 20 cross validations. The best performing model is bolded. Data and model combinations that differed significantly from ObliqueBART results are marked with an asterisk",
        escape = F,
        label = "aa_vs_obl_table_class",
        format = "latex") # Output format = latex 



# rotated ensembles -------------------------------------------------------


rot_aa_vs_obl_dir = "../rotate_aa_vs_obl_bakeoff/rot_aa_vs_obl_class_results/"
# gets names of all the files in dir
rot_aa_vs_obl_files = stringr::str_c(rot_aa_vs_obl_dir, list.files(rot_aa_vs_obl_dir), sep = "")

# loads results
rot_aa_vs_obl_results = lapply(rot_aa_vs_obl_files, loadRData) %>% bind_rows() 



rot_aa_vs_obl_results <- rot_aa_vs_obl_results %>%
  dplyr::select(job_id, num_rota, model, data, split, accuracy_test, mean_train_time)

results$num_rota <- 1

rot_aa_vs_obl_total_results <- rbind(results %>%
                                       select(job_id, num_rota, model, data, split, accuracy_test, mean_train_time), rot_aa_vs_obl_results) 


rot_aa_vs_obl_total_results <- rot_aa_vs_obl_total_results  %>%
  filter(!(model %in% c("obart7oh",
                        "obart1.5a")),
         data != "thoraric_surgery")

# write_csv(rot_aa_vs_obl_total_results, "../results/rot_aa_vs_obl_total_results_class.csv")

get_t_results_rot <- function(base_results, total_results, num_rotation, model_name, dataset){
  model_results_vec <- total_results %>%
    filter(model == model_name,
           data == dataset,
           num_rota == num_rotation) %>%
    select(accuracy_test) %>%
    pull()
  if (length(model_results_vec) == 0) {
    return(NA)
  }
  else{
    base_results_vec <- base_results %>%
      filter(model == "obart7",
             data == dataset) %>%
      select(accuracy_test) %>%
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
count_df <- rot_aa_vs_obl_total_results %>%
  count(model, data, num_rota)

t_test_obl_vs_obl <- expand.grid(num_rota = unique(rot_aa_vs_obl_total_results$num_rota),
                                 data = unique(rot_aa_vs_obl_total_results$data),
                                 model = unique(rot_aa_vs_obl_total_results$model)) %>%
  filter(!((model == "obart7") & (num_rota!=1)),
         data != "thoraric_surgery") 
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
                  data = "banknote_authentication")

# this table has the minimum number of rotations for the difference between obliqueBART and rotated ensemble is *not* statistically significant. ie minimum number of rotations for the results to be similar
t_test_obl_vs_obl_min <- t_test_obl_vs_obl %>%
  filter(!ttest,
         model!= "rotation_forest") %>%
  group_by(data, model) %>%
  summarize(min_num_rota = min(num_rota)) %>%
  pivot_wider(names_from = model,
              values_from = min_num_rota) 
t_test_obl_vs_obl_min_table <- t_test_obl_vs_obl_min %>%
  select(-obart7) 
t_test_obl_vs_obl_min_table$data <- c(
                                 "banknote",
                                 "blood transfusion",
                                 "breast cancer diag.",
                                 "breast cancer",
                                 "breast cancer prog.",
                                 "climate crashes",
                                 "connectionist sonar",
                                 "credit approval",
                                 "echocardiogram",
                                 "fertility",
                                 "hepatitis",
                                 "ILPD",
                                 "ionosphere",
                                 "ozone 1",
                                 "ozone 8",
                                 "parkinsons",
                                 "planning relax",
                                 "qsar biodegradable",
                                 "seismic bumps",
                                 "spambase",
                                 "spectf heart",
                                 "statlog german cred.")
colnames(t_test_obl_vs_obl_min_table) <- c("data", "extraTrees.r", "pbart.r", "xgboost.r", "randomForest.r")
t_test_obl_vs_obl_min_table[c(12,1),] <- t_test_obl_vs_obl_min_table[c(1,12),]
t_test_obl_vs_obl_min_table %>%
  kable(caption = "Number of random rotations of the data for difference between obliqueBART and rotated ensemble accuracies to be statistically insignificant. Model and data combinations with NA had MSE's that were very different than obliqueBART, up to 200 random data rotations.",
        escape = F,
        label = "rot_aa_vs_obl_table_class",
        format = "latex") # Output format = latex 



# old table
rot_aa_vs_obl_dir = "../rotate_aa_vs_obl_bakeoff/rot_aa_vs_obl_class_results/"
# gets names of all the files in dir
rot_aa_vs_obl_files = stringr::str_c(rot_aa_vs_obl_dir, list.files(rot_aa_vs_obl_dir), sep = "")

# loads results
rot_aa_vs_obl_results = lapply(rot_aa_vs_obl_files, loadRData) %>% bind_rows()



rot_aa_vs_obl_results <- rot_aa_vs_obl_results %>%
  dplyr::select(job_id, num_rota, model, data, split, accuracy_test, mean_train_time)

results$num_rota <- 1

rot_aa_vs_obl_total_results <- rbind(results %>%
                                       select(job_id, num_rota, model, data, split, accuracy_test, mean_train_time), rot_aa_vs_obl_results)




base_df <- rot_aa_vs_obl_total_results %>%
  filter(model == "obart7") %>%
  group_by(data) %>%
  summarize(mean_accuracy_test = mean(accuracy_test))

rot_aa_vs_obl_df <- rot_aa_vs_obl_total_results %>%
  filter(!(model %in% c("axis-aligned",
                        "obart1.5a",
                        "obart1.5a_oh",
                        "obart7oh")),
         data != "thoraric_surgery") %>%
  group_by(data, num_rota, model) %>%
  summarize(mean_accuracy_test = mean(accuracy_test))
rot_aa_vs_obl_df$obl_result <- NA
for (i in 1:nrow(rot_aa_vs_obl_df)) {
  dataset <- rot_aa_vs_obl_df$data[i]
  obl_result <- base_df %>%
    filter(data == dataset) %>%
    select(mean_accuracy_test) %>%
    pull()
  
  rot_aa_vs_obl_df$obl_result[i] <- obl_result
}
rot_aa_vs_obl_table <- rot_aa_vs_obl_df %>%
  group_by(data, model) %>%
  summarize(max_accuracy = max(mean_accuracy_test)) %>%
  pivot_wider(names_from = model,
              values_from = max_accuracy)
rot_aa_vs_obl_table$obart7 < rot_aa_vs_obl_table[,4:6]

