dir = "rot_aa_vs_obl_class_results/" # set this to be folder with results

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
  "obart1.5a", 
  "rot_extra_trees",
  "rot_rand_forest",
  "rotation_forest"
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
  group_by(data, model, num_rota) %>%
  summarize(mean_accuracy_test = mean(accuracy_test),
            mean_train_time = mean(mean_train_time)) %>%
  filter(data != "thoraric_surgery")

ggplot(data = mean_results %>% filter(model == "rot_rand_forest"),
       mapping = aes(x = num_rota, y = mean_accuracy_test)) + 
  geom_line() +
  facet_wrap(~data)

ggplot(data = mean_results %>% filter(model == "rot_pbart"),
       mapping = aes(x = num_rota, y = mean_accuracy_test)) + 
  geom_line() +
  facet_wrap(~data)

ggplot(data = mean_results,
       mapping = aes(x = num_rota, y = mean_accuracy_test, color = model)) + 
  geom_line() +
  facet_wrap(~data)

ggplot(data = mean_results %>% filter(model == "rot_xgboost",
                                      data == "hepatitis"),
       mapping = aes(x = num_rota, y = mean_accuracy_test)) + 
  geom_line()


mean_results %>%
  ggplot(aes(x = model, y = mean_accuracy_test, fill = model)) + 
  geom_boxplot(show.legend = FALSE) 


### tabular results ###
# say the baseline is wbart
# finding baseline test metrics
base_df <- results %>%
  filter(model == "obart1.5a") %>%
  select(accuracy_test, mean_train_time, data, split) %>%
  rename(base_accuracy_test = accuracy_test,
         base_time = mean_train_time)

table <- results %>% 
  left_join(base_df,
            by = join_by("data","split")) %>%
  mutate(
    mse_accuracy_ratio = accuracy_test / base_accuracy_test,
    train_time_ratio = mean_train_time / base_time
  ) %>%
  group_by(data, model) %>%  
  summarize(
    mean_accuracy_test_ratio = mean(mse_accuracy_ratio),
    mean_train_time_ratio = mean(train_time_ratio)
  ) %>%
  filter(model != "obart1.5a")

table_wide <- table %>%
  select(data, model, mean_accuracy_test_ratio) %>%
  pivot_wider(names_from = model,
              values_from = mean_accuracy_test_ratio) %>%
  filter(data != "thoraric_surgery") %>%
  select(rot_pbart, rot_rand_forest, rot_extra_trees,
         rot_xgboost, rotation_forest )
table_wide$data <- c("ILPD", "banknote authentication", "blood transfusion", "breast cancer diag.", "breast cancer", "breast cancer prog.", "climate crashes", "connectionist bench sonar", "credit approval", "echocardiogram", "fertility", "hepatitis", "ionosphere", "ozone1", "ozone8", "parkinsons", "planning relax", "qsar biodegradable", "seismic bumps", "spambase", "spectf heart", "statlog german credit")


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
      if (j == 6) cat(n, " \\\\")
      else cat(n, " & ")
    }
  }
  cat("\n")
}



model_ranking <- mean_results %>%
  group_by(data) %>%
  mutate(model_rank_accuracy = rank(mean_accuracy_test),
         model_rank_train_time = rank(mean_train_time)) %>%
  group_by(model) %>%
  summarize(average_rank_accuracy = mean(model_rank_accuracy),
            average_rank_train_time = mean(model_rank_train_time))


model_ranking %>%
  select(model, average_rank_accuracy) %>%
  arrange(average_rank_accuracy)


model_ranking %>%
  select(model, average_rank_train_time) %>%
  arrange(average_rank_train_time)

