dir = "results_aa_vs_obl_class/"  # set this to be the folder with results

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

# model index
m = c(
  "wbart",
  "axis-aligned",
  "obart1.5a", "obart1.5a_oh", 
  "xgboost",
  "extra_trees",
  "rand_forest",
  "flexBART"
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

### visualize results ###
mean_results <- results %>%
  group_by(data, model) %>%
  summarize(mean_accuracy_test = mean(accuracy_test),
            mean_recall_test = mean(recall_test),
            mean_precision_test = mean(precision_test),
            mean_auc_test = mean(auc_test),
            mean_log_loss_test = mean(log_loss_test),
            mean_train_time = mean(mean_train_time))



mean_results %>%
  ggplot(aes(x = model, y = mean_accuracy_test, fill = model)) + 
  geom_boxplot(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 


### tabular results ###
# say the baseline is wbart
# finding baseline test metrics
base_df <- results %>%
  filter(model == "obart1.5a") %>%
  select(accuracy_test, mean_train_time, data, split) %>%
  rename(base_accuracy_test = accuracy_test,
         base_time = mean_train_time)

base_df <- results %>%
  filter(model == "obart1.5a") %>%
  select(accuracy_train, accuracy_test, 
         auc_train, auc_test, 
         precision_train, precision_test,
         recall_train, recall_test,
         log_loss_train, log_loss_test,
         mean_train_time, data, split) %>%
  rename(base_accuracy_test = accuracy_test, 
         base_accuracy_train = accuracy_train,
         base_auc_test = auc_test, base_auc_train = auc_train,
         base_precision_train = precision_train, 
         base_precision_test = precision_test,
         base_recall_train = recall_train,
         base_recall_test = recall_test,
         base_log_loss_train = log_loss_train,
         base_log_loss_test = log_loss_test,
         base_time = mean_train_time)

table <- results %>% 
  left_join(base_df,
            by = join_by("data","split")) %>%
  mutate(
    accuracy_test_ratio = accuracy_test / base_accuracy_test,
    auc_test_ratio = auc_test / base_auc_test,
    precision_ratio = precision_test / base_precision_test,
    recall_ratio = recall_test / base_recall_test,
    log_loss_ratio = log_loss_test / base_log_loss_test,
    train_time_ratio = mean_train_time / base_time
  ) %>%
  group_by(data, model) %>%  
  summarize(
    mean_accuracy_test_ratio = mean(accuracy_test_ratio),
    mean_auc_test_ratio = mean(auc_test_ratio),
    mean_precision_ratio = mean(precision_ratio),
    mean_recall_ratio = mean(recall_ratio),
    mean_log_loss_ratio = mean(log_loss_ratio),
    mean_train_time_ratio = mean(train_time_ratio),
  ) %>%
  filter(model != "obart1.5a")

table_wide <- table %>%
  select(data, model, mean_accuracy_test_ratio) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test_ratio)

table_wide <- table %>%
  filter(!(str_detect(model, "oh")),
         !(model  %in% c( "obart2.0", "obart2.5a"))) %>%
  select(data, model, mean_accuracy_test_ratio) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test_ratio) %>%
  select(pbart, rand_forest, extra_trees, xgboost)

table_wide$data <- c("ILPD", "banknote authentication", "blood transfusion", "breast cancer diag.", "breast cancer", "breast cancer prog.", "climate crashes", "connectionist bench sonar", "credit approval", "echocardiogram", "fertility", "hepatitis", "ionosphere", "ozone1", "ozone8", "parkinsons", "planning relax", "qsar biodegradable", "seismic bumps", "spambase", "spectf heart", "statlog german credit", "thoraric surgery")

# formatting to latex
for (i in 1:nrow(table_wide)){
  for (j in 1:ncol(table_wide)){
    if (j == 1){
      name = unlist(table_wide[i, j])
      cat(name, "& ")
    }
    else{ 
      value = unname(unlist(table_wide[i, j]))
      n = round(unname(unlist(table_wide[i, j])), 3)
      if (!is.na(n)){
        if (value < 1) {n = paste0("\\textbf{", format(n, nsmall = 2), "}")}
        else {n = format(n, nsmall = 2)}
      }
      if (j == 5) cat(n, " \\\\")
      else cat(n, " & ")
    }
  }
  cat("\n")
}


test <- table  %>%
  filter(data != "thoraric_surgery") %>%
  group_by(model) %>%
  summarize(mean = round(mean(mean_accuracy_test_ratio), 5))


table_log_loss <- table %>%
  select(data, model, mean_log_loss_ratio) %>%
  pivot_wider(names_from = model,
              values_from = mean_log_loss_ratio)

table_log_loss <- table %>%
  filter(!(str_detect(model, "oh")),
         !(model  %in% c("axis-aligned", "wbart", 
                         "obart2.0", "obart2.5a", "flexBART"))) %>%
  select(data, model, mean_accuracy_test_ratio) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test_ratio) %>%
  select(pbart, rand_forest, extra_trees, xgboost)

table_log_loss$data <- c("ILPD", "banknote authentication", "blood transfusion", "breast cancer diag.", "breast cancer", "breast cancer prog.", "climate crashes", "connectionist bench sonar", "credit approval", "echocardiogram", "fertility", "hepatitis", "ionosphere", "ozone1", "ozone8", "parkinsons", "planning relax", "qsar biodegradable", "seismic bumps", "spambase", "spectf heart", "statlog_german credit", "thoraric surgery")

# formatting to latex
for (i in 1:nrow(table_log_loss)){
  for (j in 1:ncol(table_log_loss)){
    if (j == 1){
      name = unlist(table_log_loss[i, j])
      cat(name, "& ")
    }
    else{ 
      value = unname(unlist(table_log_loss[i, j]))
      n = round(unname(unlist(table_log_loss[i, j])), 3)
      if (!is.na(n)){
        if (value > 1) {n = paste0("\\textbf{", format(n, nsmall = 2), "}")}
        else {n = format(n, nsmall = 2)}
      }
      if (j == 5) cat(n, " \\\\")
      else cat(n, " & ")
    }
  }
  cat("\n")
}


model_ranking <- mean_results %>%
  group_by(data) %>%
  mutate(model_rank_accuracy = 9 - rank(mean_accuracy_test, na.last = TRUE),
         model_rank_log_loss = rank(mean_log_loss_test, na.last = TRUE),
         model_rank_recall = 9-rank(mean_recall_test, na.last = TRUE),
         model_rank_precision = 9-rank(mean_precision_test, na.last = TRUE),
         model_rank_auc = 9-rank(mean_auc_test, na.last = TRUE),
         model_rank_train_time = rank(mean_train_time,na.last = TRUE)) %>%
  group_by(model) %>%
  summarize(average_rank_accuracy = mean(model_rank_accuracy),
            average_rank_train_time = mean(model_rank_train_time),
            average_rank_log_loss = mean(model_rank_log_loss),
            average_rank_recall = mean(model_rank_recall),
            average_rank_precision = mean(model_rank_precision),
            average_rank_auc = mean(model_rank_auc)
            )


model_ranking %>%
  select(model, average_rank_accuracy) %>%
  arrange(average_rank_accuracy)

model_ranking %>%
  select(model, average_rank_log_loss) %>%
  arrange(average_rank_log_loss)

model_ranking %>%
  select(model, average_rank_recall) %>%
  arrange(average_rank_recall)

model_ranking %>%
  select(model, average_rank_precision) %>%
  arrange(average_rank_precision)

model_ranking %>%
  select(model, average_rank_auc) %>%
  arrange(average_rank_auc)

model_ranking %>%
  select(model, average_rank_train_time) %>%
  arrange(average_rank_train_time)


ggplot(table, mapping = aes(x = model, y = mean_accuracy_test_ratio)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 

ggplot(table, mapping = aes(x = model, y = mean_train_time_ratio)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 



mean_results %>%
  ggplot(aes(x = model, y = mean_accuracy_test, fill = model)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 


ggplot(mean_results, mapping = aes(x = model, y = mean_train_time)) + 
  geom_boxplot() + 
  ylim(c(0,750)) + 
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 
