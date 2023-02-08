#Set up a testing environment
#set the seed and split the data

library(tidymodels)

reg_dataset_rf <- 
  reg_dataset_testing %>% 
  st_drop_geometry()

set.seed(20211101)
reg_dataset_testing_split <- initial_split(
  reg_dataset_rf, 
  prop = .8,
  strata = year)
reg_dataset_train <- training(reg_dataset_testing_split)
reg_dataset_test <- testing(reg_dataset_testing_split)

#here's the dataframe we'll use for the model
cleaned_training_set <- 
  reg_dataset_train %>%
  select(median_hh_inc:prcnt_ba_or_hghr_deg, num_complaints)

cleaned_reg_dataset_rf <- reg_dataset_rf %>%
  select(median_hh_inc:prcnt_ba_or_hghr_deg, num_complaints)

#resampling with 10 folds
folds <- vfold_cv(data = states_2017_train, v = 10)

#crafting recipe
states_2017_rec <- recipe(med_income ~ ., data = states_2017_train) %>%
  #removes rows with nans
  step_naomit(all_predictors()) %>%
  #removes variables that are sparce and unbalanced
  step_nzv(all_predictors()) %>%
  step_filter_missing(all_predictors()) %>%
  #logs ged since it's skewed
  step_log(ged_count_prop) %>%
  #centers our variables but not the z-scored ones
  step_center(all_predictors(), -starts_with("z_")) 


treeNumb <- 1000
gridVal <- 10
#since random forests can be computationally heavier, we use parallel processing
cores <- parallel::detectCores()

#random forest with 1000 iterations while utilizing the parallel processing (num.threads = cores)
rf_mod1 <- rand_forest(mtry = tune(), min_n = tune(), trees = treeNumb) %>% 
  set_engine(engine = "ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("regression")

#initiating the workflow for random forests
rf_workflow1 <- workflow() %>% 
  add_recipe(recipe = states_2017_rec) %>%
  add_model(spec = rf_mod1)

#tune with the given number of folds and grid-value
rf_res <- rf_workflow1 %>% 
  tune_grid(resamples = folds, grid = gridVal,
            control = control_grid(save_pred = TRUE))

#these are the best hyperparameters (mtry = , min_n = )
rf_best <- rf_res %>% 
  select_best(metric = "rmse")

#applying the best hyperparameters to our original workflow (since we used tune() as "place holders" for mtry and min_n)
rf_final <- rf_workflow1 %>%
  finalize_workflow(parameters = rf_best)

#apply the worlflow when fitting the data
rf_last_fit <- rf_final %>%
  last_fit(states_2017_split)

#fitting the workflow with the original data
rf_final_fit <- fit(rf_final, only_numb)

#applying the updated workflow to the fit
rf_fit_rs <- rf_final %>%
  fit_resamples(resamples = folds)

#mean RMSE across the 10 samples
collect_metrics(rf_fit_rs) %>%
  filter(.metric == "rmse") %>%
  select(mean) %>%
  pull(mean)
