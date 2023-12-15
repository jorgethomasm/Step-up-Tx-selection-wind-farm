# Elastic Net Recipes:
glm_rec_60 <-
  recipe(ws_60_bc ~ ., data = ws_train_bc) |>
  update_role(datetime, new_role = "timestamp") |>  
  step_normalize(all_numeric_predictors())

glm_rec_100 <-
  recipe(ws_100_bc ~ ., data = ws_train_bc) |>
  update_role(datetime, new_role = "timestamp") |>  
  step_normalize(all_numeric_predictors())

glm_rec_120 <-
  recipe(ws_120_bc ~ ., data = ws_train_bc) |>
  update_role(datetime, new_role = "timestamp") |>  
  step_normalize(all_numeric_predictors())

# Model: Elastic Net -------------
glm_spec <-
  linear_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |>
  set_mode("regression")  

glm_wf_60 <- workflow() |> add_recipe(glm_rec_60) |> add_model(glm_spec)
glm_wf_100 <- workflow() |> add_recipe(glm_rec_100) |> add_model(glm_spec)
glm_wf_120 <- workflow() |> add_recipe(glm_rec_120) |> add_model(glm_spec)


# ------ WS 60 Metres ------
tic()
set.seed(1982)
doParallel::registerDoParallel()
glm_tune_60 <- tune_grid(glm_wf_60, resamples = ws_train_boots, grid = 20)
toc()

show_best(glm_tune_60, metric = "rmse")

# Set best parameters
final_glm_60 <- glm_wf_60 |> finalize_workflow(select_best(glm_tune_60, metric = "rmse"))
final_glm_60


# ------ WS 100 Metres ------
tic()
set.seed(1982)
doParallel::registerDoParallel()
glm_tune_100 <- tune_grid(glm_wf_100, resamples = ws_train_boots, grid = 20)
toc()

# Explore Results 
show_best(glm_tune_100, metric = "rmse")

# Set best parameters
final_glm_100 <- glm_wf_100 |> finalize_workflow(select_best(glm_tune_100, metric = "rmse"))
final_glm_100


# ------ WS 120 Metres ------
tic()
set.seed(1982)
doParallel::registerDoParallel()
glm_tune_120 <- tune_grid(glm_wf_120, resamples = ws_train_boots, grid = 20)
toc()

# Explore Results 
show_best(glm_tune_120, metric = "rmse")

# Set best parameters
final_glm_120 <- glm_wf_120 |> finalize_workflow(select_best(glm_tune_120, metric = "rmse"))
final_glm_120