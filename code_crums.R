#| eval: false


# Fill ws_100 in function of ws_120 
lm_fit_ws_100_120 <- lm(ws_100_bc ~ ws_120_bc, data = ws_bc, na.action = na.exclude)
# which(is.na(ws$ws_100) & !is.na(ws$ws_120))

# Fill ws_120 in function of ws_100 
lm_fit_ws_120_100 <- lm(ws_120_bc ~ ws_100_bc, data = ws_bc, na.action = na.exclude)
# which(is.na(ws$ws_120) & !is.na(ws$ws_100))

# Fill ws_100 in function of ws_060 
lm_fit_ws_100_060 <- lm(ws_100_bc ~ ws_060_bc, data = ws_bc, na.action = na.exclude)
# which(is.na(ws$ws_100) & !is.na(ws$ws_060))

ws_bc <- 
    ws_bc |>
    mutate(ws_100_bc_pred = predict(lm_fit_ws_100_120, ws_bc)) |>
    mutate(ws_100_bc = coalesce(ws_100_bc, ws_100_bc_pred), .keep = "unused") |>

    mutate(ws_120_bc_pred = predict(lm_fit_ws_120_100, ws_bc)) |>
    mutate(ws_120_bc = coalesce(ws_120_bc, ws_120_bc_pred), .keep = "unused") |>

    mutate(ws_100_bc_pred_2 = predict(lm_fit_ws_100_060, ws_bc)) |>
    mutate(ws_100_bc = coalesce(ws_100_bc, ws_100_bc_pred_2), .keep = "unused")

# Fill AGAIN ws_120 in function of ws_100 (already filled with ws_060)
lm_fit_ws_120_100_part_2 <- lm(ws_120_bc ~ ws_100_bc, data = ws_bc, na.action = na.exclude)

ws_bc <- 
    ws_bc |>
    mutate(ws_120_bc_pred_2 = predict(lm_fit_ws_120_100_part_2, ws_bc)) |>
    mutate(ws_120_bc = coalesce(ws_120_bc, ws_120_bc_pred_2), .keep = "unused")

# Instpect ws_060
# which(is.na(ws_bc$ws_060_bc) & !is.na(ws_bc$ws_100_bc))

# Fill ws_060 in function of ws_100 
lm_fit_ws_060_100 <- lm(ws_060_bc ~ ws_100_bc, data = ws_bc, na.action = na.exclude)

ws_bc <- 
    ws_bc |>
    mutate(ws_060_bc_pred = predict(lm_fit_ws_060_100, ws_bc)) |>
    mutate(ws_060_bc = coalesce(ws_060_bc, ws_060_bc_pred), .keep = "unused")

length(which((is.na(ws_bc$ws_100_bc))))
752
# Inverse Box-Cox

ws <- 
    ws_bc |>
    mutate(ws_060 = inv_boxcox(ws_060_bc, ws_060_bc_results$lambda), .keep = "unused") |>
    mutate(ws_100 = inv_boxcox(ws_100_bc, ws_100_bc_results$lambda), .keep = "unused") |>
    mutate(ws_120 = inv_boxcox(ws_120_bc, ws_120_bc_results$lambda), .keep = "unused") |>
    mutate(hour = hour(datetime)) |>
    mutate(day = day(datetime)) |>
    mutate(week = week(datetime)) |>
    mutate(month = month(datetime)) 

idx_NA <- which(is.na(ws$ws_060))
unique(date(ws$datetime[idx_NA]))

rdxklywind <-
    rdxklywind |>
    select(-datetime & !contains("ws")) |>
    bind_cols(ws)


calc_mi_score(rdxklywind, "ws_100")


ws_060_rec <-
    recipe(tem_007 ~ ., data = rdxklywind) |>
    step_impute_bag(ws_060, 
                    impute_with = imp_vars(pr, tem_115, tem_007, rh_007),
                    seed_val = 1982)

ws_060_imputed <- ws_060_rec |> prep() |> bake(new_data = NULL)

ws_060_imputed$ws_060[idx_NA]

# test <- ws_060_imputed[idx_NA, ]
# View(test)

# test |>
    
#      # fill(A7_RH_7_0_K846TH)
#     pivot_longer(cols = where(is.numeric), names_to = "sensor", values_to = "value") |>
#     mutate(sensor = as.factor(sensor)) |>
#     ggplot(aes(x = datetime)) +
#     geom_point(aes(y = value)) +     
#     scale_x_datetime(date_labels = "%b-%Y") + 
#     labs(y = "", x = "Time" ) +
#     facet_wrap(~sensor, ncol = 1, scales = "free_y") 


# Corrplot

rdxklywind |>
    select(-datetime)|>
    remove_missing()|>
    cor(method = "pearson") |>
    corrplot(type = "lower", method = "circle", insig = 'blank', order = "hclust", diag = FALSE,  tl.col="black", tl.cex = 0.8, addCoef.col = 'black', number.cex = 0.6)







ws_060_test_1 <- ws[which(is.na(ws$ws_060) & !is.na(ws$ws_100) & !is.na(ws$ws_120)), ]

# TRAIN / TEST (PRED) SPLIT
ws_train <- ws |> remove_missing()

# Histograms
ws_train |>   
    select(-datetime) |>
    pivot_longer(cols = everything(), names_to = "WS_sensor", values_to = "WindSpeed") |>
    mutate(WS_sensor = factor(WS_sensor, levels = c("ws_060", "ws_100", "ws_120", "ws_060_bc", "ws_100_bc", "ws_120_bc"))) |>
    ggplot(aes(x = WindSpeed)) +
    geom_histogram(bins = 50, color = "grey", fill = "lightgrey", alpha = 0.5) + 
    labs(x = "Wind Speed [m/s]") +
    scale_x_continuous(breaks = seq(0, 35, 5)) +  
    facet_wrap(~WS_sensor, nrow = 2) 

# The Bootstrap: for assessment of test error ----
set.seed(1982)
ws_train_boots_060 <- bootstraps(ws_train, times = 1000, strata = ws_060_bc)

set.seed(1982)
ws_train_boots_100 <- bootstraps(ws_train, times = 1000, strata = ws_100_bc)

set.seed(1982)
ws_train_boots_120 <- bootstraps(ws_train, times = 1000, strata = ws_120_bc)

# lm Recipes:
lm_rec_ws060 <-
  recipe(ws_060_bc ~ ., data = ws_train) |>
  update_role(datetime, new_role = "timestamp") |>
  update_role(ws_060, new_role = "orignal") |>
  update_role(ws_100, new_role = "orignal") |>
  update_role(ws_120, new_role = "orignal")
  
lm_rec_ws100 <-
  recipe(ws_100_bc ~ ., data = ws_train) |>
  update_role(datetime, new_role = "timestamp") |>
  update_role(ws_060, new_role = "orignal") |>
  update_role(ws_100, new_role = "orignal") |>
  update_role(ws_120, new_role = "orignal")

lm_rec_ws120 <-
  recipe(ws_120_bc ~ ., data = ws_train) |>
  update_role(datetime, new_role = "timestamp") |>
  update_role(ws_060, new_role = "orignal") |>
  update_role(ws_100, new_role = "orignal") |>
  update_role(ws_120, new_role = "orignal")

# Model Spec: LM -------------
lm_spec <-
  linear_reg() |> 
  set_engine("lm") |>
  set_mode("regression")   

lm_wf_ws060 <- workflow() |> add_recipe(lm_rec_ws060) |> add_model(lm_spec)
lm_wf_ws100 <- workflow() |> add_recipe(lm_rec_ws100) |> add_model(lm_spec)
lm_wf_ws120 <- workflow() |> add_recipe(lm_rec_ws120) |> add_model(lm_spec)

# === Test Error Estimations ===

# ------ WS 60 Metres ------
tic()
set.seed(1982)
doParallel::registerDoParallel()
lm_testing_fit_ws060 <- fit_resamples(lm_wf_ws060, resamples = ws_train_boots)
toc()
lm_testing_fit_ws060 |> collect_metrics(summarize = TRUE)

# ------ WS 100 Metres ------
tic()
set.seed(1982)
doParallel::registerDoParallel()
lm_testing_fit_ws100 <- fit_resamples(lm_wf_ws100, resamples = ws_train_boots)
toc()
lm_testing_fit_ws100 |> collect_metrics(summarize = TRUE)

# ------ WS 120 Metres ------
tic()
set.seed(1982)
doParallel::registerDoParallel()
lm_testing_fit_ws120 <- fit_resamples(lm_wf_ws120, resamples = ws_train_boots)
toc()
lm_testing_fit_ws120 |> collect_metrics(summarize = TRUE)

# === FINAL FITS ===

lm_fit_ws060 <- lm_wf_ws060 |> fit(ws_train)
lm_fit_ws100 <- lm_wf_ws100 |> fit(ws_train)
lm_fit_ws120 <- lm_wf_ws120 |> fit(ws_train)

# Add predictions


ws_060_test_2 <- ws[which(is.na(ws$ws_060) & !is.na(ws$ws_120)), ]

ws_100_test <- ws[which(is.na(ws$ws_100)), ]
ws_120_test <- ws[which(is.na(ws$ws_120)), ]

ws_060_pred <- inv_boxcox(predict(lm_fit_ws060, ws_train), ws_060_bc_results$lambda)

ws <-
  ws |>
  predict(lm_fit_ws100, ws_train_bc) |>
  mutate(ws_100_pred = inv_boxcox(.pred, ws_100_bc_results$lambda), .keep = "all")

ws <-
  ws |>
  predict(lm_fit_ws120, ws_train_bc) |>
  mutate(ws_120_pred = inv_boxcox(.pred, ws_120_bc_results$lambda), .keep = "all")

glimpse(ws)




# klywind_ws |>
#     select(-datetime)|>
#     cor(method = "pearson") |>
#     corrplot(type = "lower", method = "circle", insig = 'blank', order = "hclust", diag = FALSE,  tl.col="black", tl.cex = 0.8, addCoef.col = 'black', number.cex = 0.6)


# klywind_ws |>
#     select(-datetime) |>
#     pivot_longer(cols = everything(), names_to = "WS_sensor", values_to = "WindSpeed") |>
#     mutate(WS_sensor = as.factor(WS_sensor)) |>
#     ggplot(aes(x = ws)) +
#     geom_histogram(bins = 50, color = "lightgrey", fill = "lightgrey") + 
#     labs(x = "Wind Speed [m/s]") +
#     scale_x_continuous(breaks = seq(0, 35, 5)) +  
#     facet_wrap(~WS_sensor)  

    # mutate(ws_120 = (F1_WS_120_A_330_TFCA + F2_WS_120_B_150_TFCA)/2, .keep = "unused") |> 
    # mutate(ws_100 = (F3_WS_100_A_330_TFCA + F4_WS_100_B_150_TFCA)/2, .keep = "unused") |> 
    # mutate(ws_60 = (F5_WS_60_A_330_TFCA + F6_WS_60_B_150_TFCA)/2, .keep = "unused") 


