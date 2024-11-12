dir = "results_aa_vs_obl_reg/"  # set this to be the folder with results

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
  "rand_forest"
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
  summarize(mean_rmse_test = mean(rmse_test),
            mean_train_time = mean(mean_train_time))



mean_results %>%
  ggplot(aes(x = model, y = mean_rmse_test, fill = model)) + 
  geom_boxplot(show.legend = FALSE) + 
  ylim(c(0, 4)) +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 


### tabular results ###
# say the baseline is wbart
# finding baseline test metrics
base_df <- results %>%
  filter(model == "obart1.5a") %>%
  select(rmse_test, mean_train_time, data, split) %>%
  rename(base_rmse_test = rmse_test,
         base_time = mean_train_time)

table <- results %>% 
  left_join(base_df,
            by = join_by("data","split")) %>%
  mutate(
    mse_test_ratio = rmse_test^2 / base_rmse_test^2,
    train_time_ratio = mean_train_time / base_time
  ) %>%
  group_by(data, model) %>%  
  summarize(
    mean_mse_test_ratio = mean(mse_test_ratio),
    mean_train_time_ratio = mean(train_time_ratio)
  ) %>%
  filter(model != "obart1.5a")

table_wide <- table %>%
  filter(!(str_detect(model, "oh")),
         !(model  %in% c( "obart2.0",
                         "obart2.5a"))) %>%
  select(data, model, mean_mse_test_ratio) %>%
  pivot_wider(names_from = model,
              values_from = mean_mse_test_ratio) %>%
  select(wbart, rand_forest, extra_trees, xgboost )


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
        if (value > 1) {n = paste0("\\textbf{", format(n, nsmall = 2), "}")}
        else {n = format(n, nsmall = 2)}
      }
      if (j == 5) cat(n, " \\\\")
      else cat(n, " & ")
    }
  }
  cat("\n")
}

test <- table %>% filter(data != "labor",
                 data != "rice") %>%
  group_by(model) %>%
  summarize(mean = round(mean(mean_mse_test_ratio), 5))


model_ranking <- mean_results %>%
  group_by(data) %>%
  mutate(model_rank_rmse = rank(mean_rmse_test),
         model_rank_train_time = rank(mean_train_time)) %>%
  group_by(model) %>%
  summarize(average_rank_rmse = mean(model_rank_rmse),
            average_rank_train_time = mean(model_rank_train_time))


model_ranking %>%
  select(model, average_rank_rmse) %>%
  arrange(average_rank_rmse)


model_ranking %>%
  select(model, average_rank_train_time) %>%
  arrange(average_rank_train_time)


ggplot(table, mapping = aes(x = model, y = mean_mse_test_ratio)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 

ggplot(table, mapping = aes(x = model, y = mean_train_time_ratio)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 



mean_results %>%
  ggplot(aes(x = model, y = mean_rmse_test, fill = model)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 


ggplot(mean_results, mapping = aes(x = model, y = mean_train_time)) + 
  geom_boxplot() + 
  ylim(c(0,750)) + 
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5)) 
