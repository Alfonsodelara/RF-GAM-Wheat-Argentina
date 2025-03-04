
# eonr_data <- rf_eonr_shap$eonr_data[[1]]
# eonr_rf <- rf_eonr_shap$eonr_rf[[1]]
# data <- eonr_data
# x_vars <- rf_eonr_shap$x_vars[[1]]
# mean_y <- eonr_data[, mean(nrate)]

get_shap_values <- function(eonr_data, x_vars, eonr_rf) {
  explainer <-
    shapr(
      eonr_data[, ..x_vars],
      eonr_rf,
      n_combinations = 3000
    )

  #* scale variables for shap value visualization
  scaled_data <-
    copy(eonr_data)[,
      (x_vars) :=
        lapply(
          .SD,
          function(x) {
            min_x <- min(x)
            max_x <- max(x)
            scaled_x <- (x - min_x) / (max_x - min_x)
          }
        ),
      .SDcols = x_vars
    ] %>%
    .[, c("obs_id", x_vars), with = FALSE]

  #* set up a dataset
  temp_data <-
    copy(eonr_data) %>%
    #* randomly sample 200 observations
    .[sample(.N, 200)] %>%
    #* run 10 at a time due to high computational burden
    .[, num_it := ceiling(1:.N / 10)]

  shap_data <-
    lapply(
      unique(temp_data[, num_it]),
      function(x) {
        print(x)
        explain(
          temp_data[num_it == x, ..x_vars],
          approach = "empirical",
          explainer = explainer,
          prediction_zero = eonr_data[, mean(nrate)]
        ) %>%
          .$dt
      }
    ) %>%
    rbindlist() %>%
    setnames(names(.), paste0("shap_", names(.))) %>%
    cbind(temp_data[, .(obs_id)], .) %>%
    scaled_data[., on = "obs_id"] %>%
    melt(id.vars = "obs_id") %>%
    .[, type := fifelse(
      str_detect(variable, "shap"),
      "shap",
      "variable"
    )] %>%
    .[, variable := gsub("shap_", "", variable)] %>%
    .[variable != "none", ] %>%
    .[order(obs_id), ]

  return(shap_data)
}