
# 02_model_regression_ml.R
# MSc thesis empirical analysis script


# 0. Setup and imports


options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

install_missing_packages <- FALSE
analysis_seed <- 2026
set.seed(analysis_seed)

core_packages <- c(
  "readr", "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "broom", "sandwich", "lmtest"
)

optional_packages <- c(
  "car", "MASS", "clubSandwich", "robustbase",
  "rsample", "recipes", "parsnip", "workflows", "yardstick",
  "tune", "dials", "glmnet", "ranger", "vip"
)

missing_core <- core_packages[!vapply(core_packages, requireNamespace, logical(1), quietly = TRUE)]
missing_optional <- optional_packages[!vapply(optional_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_core) > 0) {
  if (isTRUE(install_missing_packages)) {
    install.packages(missing_core)
  } else {
    stop(
      "Missing required core packages: ", paste(missing_core, collapse = ", "),
      ". Install them or set install_missing_packages <- TRUE."
    )
  }
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(broom)
  library(sandwich)
  library(lmtest)
})

has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

if (length(missing_optional) > 0) {
  message("Optional packages not available and related sections may be skipped: ",
          paste(missing_optional, collapse = ", "))
}

output_dir <- "outputs"
model_dir <- file.path(output_dir, "models")
table_dir <- file.path(output_dir, "tables")
diagnostic_dir <- file.path(output_dir, "diagnostics")
ml_dir <- file.path(output_dir, "ml")
robustness_dir <- file.path(output_dir, "robustness")

for (dir_path in c(output_dir, model_dir, table_dir, diagnostic_dir, ml_dir, robustness_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

model_warnings <- character()
skipped_models <- character()
created_files <- character()

safe_write_csv <- function(data, path) {
  readr::write_csv(data, path)
  created_files <<- unique(c(created_files, path))
  invisible(data)
}

record_warning <- function(msg) {
  warning(msg, call. = FALSE)
  model_warnings <<- unique(c(model_warnings, msg))
}

record_skip <- function(msg) {
  message("SKIP: ", msg)
  skipped_models <<- unique(c(skipped_models, msg))
}

find_existing_file <- function(primary, fallbacks = character()) {
  candidates <- unique(c(primary, fallbacks))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) return(existing[1])

  # Also allow local browser/download suffixes such as file(6).csv.
  base <- tools::file_path_sans_ext(primary)
  ext <- tools::file_ext(primary)
  pattern <- paste0("^", gsub("([.()])", "\\\\\\1", basename(base)), "(\\\\([0-9]+\\\\))?\\.", ext, "$")
  matches <- list.files(".", pattern = pattern, full.names = TRUE)
  if (length(matches) > 0) return(matches[1])

  NA_character_
}

make_formula <- function(outcome, predictors) {
  if (length(predictors) == 0) {
    stats::as.formula(paste(outcome, "~ 1"))
  } else {
    stats::reformulate(termlabels = predictors, response = outcome)
  }
}

rmse_vec <- function(truth, pred) sqrt(mean((truth - pred)^2, na.rm = TRUE))
mae_vec <- function(truth, pred) mean(abs(truth - pred), na.rm = TRUE)

significance_marker <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.10 ~ ".",
    TRUE ~ ""
  )
}


# 1. Load final datasets


data_path <- find_existing_file("final_sample_cleaned.csv")

if (is.na(data_path)) {
  stop("Could not find final_sample_cleaned.csv. Run 01_clean_final_sample.R first.")
}

final_sample_cleaned <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  mutate(.row_id_global = row_number())

audit_path <- find_existing_file("final_sample_cleaned_audit.csv")

audit_data <- NULL
if (!is.na(audit_path)) {
  audit_data <- readr::read_csv(audit_path, show_col_types = FALSE)
} else {
  record_skip("Audit dataset not found; continuing with slim final dataset only.")
}

# Coerce variables that should be categorical/character.
if (!"entry_year_factor" %in% names(final_sample_cleaned)) {
  final_sample_cleaned$entry_year_factor <- as.factor(final_sample_cleaned$entry_year)
} else {
  final_sample_cleaned$entry_year_factor <- as.factor(final_sample_cleaned$entry_year_factor)
}

if (!"entry_era" %in% names(final_sample_cleaned)) {
  final_sample_cleaned$entry_era <- factor(NA_character_, levels = c("2000_2007", "2008_2014", "2015_2021"))
} else {
  final_sample_cleaned$entry_era <- as.factor(final_sample_cleaned$entry_era)
}

final_sample_cleaned <- final_sample_cleaned %>%
  mutate(
    artist_cluster_id = as.character(artist_cluster_id),
    featured_artist_flag = as.integer(featured_artist_flag),
    log1p_artist_prior_top10_count_z = as.numeric(log1p_artist_prior_top10_count_z)
  )


# 2. Define outcomes, predictors, and validation checks


main_outcomes <- c("time_to_peak", "weeks_on_chart", "rise_speed", "decay_rate")

main_audio_predictors <- c(
  "danceability_z", "energy_z", "loudness_z", "speechiness_z",
  "acousticness_z", "liveness_z", "valence_z", "tempo_z"
)

main_lyric_predictors <- c(
  "log_num_words_z", "uniq_ratio_z", "avg_word_length_z", "top_5_word_share_z",
  "sentiment_balance_z", "emotionality_share_z", "self_reference_share_z",
  "second_person_share_z"
)

main_controls <- c("entry_year_c_z", "log1p_artist_prior_top10_count_z", "featured_artist_flag")
cluster_var <- "artist_cluster_id"

main_combined_predictors <- c(main_controls, main_audio_predictors, main_lyric_predictors)
main_continuous_predictors <- c(
  "entry_year_c_z", "log1p_artist_prior_top10_count_z",
  main_audio_predictors, main_lyric_predictors
)

required_columns <- unique(c(
  "weekly_song_id", "song", "artist", "entry_year", cluster_var,
  main_outcomes, main_combined_predictors,
  "peaked_on_entry_flag", "long_gap_flag", "lyric_fuzzy_or_supp_flag", "spotify_fuzzy_flag",
  "sample_2000_2020_flag", "right_censored_flag",
  "weeks_to_peak_0", "log1p_weeks_to_peak_0", "log_time_to_peak", "log_weeks_on_chart",
  "log1p_rise_speed", "log1p_decay_rate", "first_run_weeks_on_chart", "first_run_decay_rate"
))

missing_required <- setdiff(required_columns, names(final_sample_cleaned))
if (length(missing_required) > 0) {
  stop("The final dataset is missing required analysis columns: ", paste(missing_required, collapse = ", "))
}

missing_main_predictor_cells <- final_sample_cleaned %>%
  summarise(across(all_of(main_combined_predictors), ~ sum(is.na(.x)))) %>%
  as.matrix() %>%
  sum()

missing_added_control_cells <- final_sample_cleaned %>%
  summarise(across(all_of(main_controls), ~ sum(is.na(.x)))) %>%
  as.matrix() %>%
  sum()

duplicate_weekly_song_ids <- final_sample_cleaned %>%
  count(weekly_song_id, name = "n") %>%
  filter(n > 1) %>%
  nrow()

peaked_on_entry_n <- sum(final_sample_cleaned$peaked_on_entry_flag == 1, na.rm = TRUE)
missing_rise_speed_n <- sum(is.na(final_sample_cleaned$rise_speed))
missing_rise_equals_peaked <- peaked_on_entry_n == missing_rise_speed_n

sample_dataset_validation <- tibble(
  check = c(
    "dataset_path", "n_rows", "n_columns", "duplicate_weekly_song_id_count",
    paste0("missing_", main_outcomes),
    "missing_main_predictor_cells", "missing_added_control_cells",
    "peaked_on_entry_flag_count", "missing_rise_speed_count",
    "missing_rise_equals_peaked_on_entry", "long_gap_flag_count",
    "lyric_fuzzy_or_supp_flag_count", "spotify_fuzzy_flag_count",
    "sample_2000_2020_count", "sample_2021_count", "right_censored_flag_count"
  ),
  value = as.character(c(
    data_path, nrow(final_sample_cleaned), ncol(final_sample_cleaned), duplicate_weekly_song_ids,
    purrr::map_int(main_outcomes, ~ sum(is.na(final_sample_cleaned[[.x]]))),
    missing_main_predictor_cells, missing_added_control_cells,
    peaked_on_entry_n, missing_rise_speed_n, missing_rise_equals_peaked,
    sum(final_sample_cleaned$long_gap_flag == 1, na.rm = TRUE),
    sum(final_sample_cleaned$lyric_fuzzy_or_supp_flag == 1, na.rm = TRUE),
    sum(final_sample_cleaned$spotify_fuzzy_flag == 1, na.rm = TRUE),
    sum(final_sample_cleaned$sample_2000_2020_flag == 1, na.rm = TRUE),
    sum(final_sample_cleaned$sample_2000_2020_flag == 0, na.rm = TRUE),
    sum(final_sample_cleaned$right_censored_flag == 1, na.rm = TRUE)
  ))
)

safe_write_csv(sample_dataset_validation, file.path(diagnostic_dir, "sample_dataset_validation.csv"))
print(sample_dataset_validation)

if (duplicate_weekly_song_ids > 0) stop("Duplicate weekly_song_id values found; stop before modeling.")
if (missing_main_predictor_cells > 0) stop("Missing values found in main predictor cells; stop before modeling.")
if (!missing_rise_equals_peaked) record_warning("Missing rise_speed count does not equal peaked_on_entry_flag count.")
if (nrow(final_sample_cleaned) != 894) record_warning("Dataset row count is not 894. Check whether the final sample changed.")

make_outcome_dataset <- function(data, outcome) {
  data %>% filter(!is.na(.data[[outcome]]))
}

outcome_datasets <- setNames(
  purrr::map(main_outcomes, ~ make_outcome_dataset(final_sample_cleaned, .x)),
  main_outcomes
)

outcome_model_sample_sizes <- tibble(
  outcome = main_outcomes,
  n = purrr::map_int(outcome_datasets, nrow),
  expected_n = c(894L, 894L, 808L, 893L),
  matches_expected = n == expected_n
)

safe_write_csv(outcome_model_sample_sizes, file.path(table_dir, "outcome_model_sample_sizes.csv"))
print(outcome_model_sample_sizes)
if (any(!outcome_model_sample_sizes$matches_expected)) {
  record_warning("At least one outcome-specific sample size differs from the expected value.")
}


# 3. Descriptive statistics, correlations, and VIF
# Descriptive diagnostics summarize the modeling variables and check whether predictors are too strongly collinear.


descriptive_variables <- unique(c(main_outcomes, main_audio_predictors, main_lyric_predictors, main_controls))

descriptive_statistics <- final_sample_cleaned %>%
  summarise(
    across(
      all_of(descriptive_variables),
      list(
        n = ~ sum(!is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        p25 = ~ as.numeric(quantile(.x, 0.25, na.rm = TRUE)),
        median = ~ median(.x, na.rm = TRUE),
        p75 = ~ as.numeric(quantile(.x, 0.75, na.rm = TRUE)),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "value") %>%
  separate(measure, into = c("variable", "statistic"), sep = "__") %>%
  pivot_wider(names_from = statistic, values_from = value)

safe_write_csv(descriptive_statistics, file.path(table_dir, "descriptive_statistics.csv"))

find_high_correlations <- function(data, vars, cutoff = 0.70) {
  cor_data <- data %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric))
  cor_matrix <- stats::cor(cor_data, use = "pairwise.complete.obs")
  idx <- which(abs(cor_matrix) >= cutoff & upper.tri(cor_matrix), arr.ind = TRUE)
  if (nrow(idx) == 0) {
    return(tibble(var1 = character(), var2 = character(), correlation = numeric(), abs_correlation = numeric()))
  }
  tibble(
    var1 = rownames(cor_matrix)[idx[, "row"]],
    var2 = colnames(cor_matrix)[idx[, "col"]],
    correlation = as.numeric(cor_matrix[idx]),
    abs_correlation = abs(correlation)
  ) %>% arrange(desc(abs_correlation))
}

high_correlations_main_predictors <- find_high_correlations(
  final_sample_cleaned, main_continuous_predictors, cutoff = 0.70
)

safe_write_csv(high_correlations_main_predictors, file.path(diagnostic_dir, "high_correlations_main_predictors.csv"))

compute_vif_from_data <- function(data, vars) {
  vif_data <- data %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    drop_na()
  purrr::map_dfr(vars, function(v) {
    others <- setdiff(vars, v)
    if (length(others) == 0 || sd(vif_data[[v]], na.rm = TRUE) == 0) {
      return(tibble(term = v, vif = NA_real_))
    }
    fit <- stats::lm(stats::reformulate(others, response = v), data = vif_data)
    r2 <- summary(fit)$r.squared
    tibble(term = v, vif = 1 / (1 - r2))
  }) %>%
    mutate(vif_gt_5 = vif > 5, vif_gt_10 = vif > 10) %>%
    arrange(desc(vif))
}

vif_main_predictors <- compute_vif_from_data(final_sample_cleaned, main_combined_predictors)
safe_write_csv(vif_main_predictors, file.path(diagnostic_dir, "vif_main_predictors.csv"))

if (any(vif_main_predictors$vif > 10, na.rm = TRUE)) {
  record_warning("At least one main predictor VIF is above 10. Inspect VIF table before interpreting coefficients.")
}


# 4. Main OLS feature-block models

feature_blocks <- list(
  null = character(),
  controls_only = main_controls,
  audio = c(main_controls, main_audio_predictors),
  lyrics = c(main_controls, main_lyric_predictors),
  combined = main_combined_predictors
)

fit_lm_model <- function(data, outcome, predictors, cluster_var = "artist_cluster_id") {
  keep_vars <- unique(c("weekly_song_id", "song", "artist", "entry_year", cluster_var, outcome, predictors))
  model_input <- data %>%
    select(any_of(keep_vars)) %>%
    drop_na(all_of(c(outcome, predictors))) %>%
    mutate(.row_id_model = row_number())

  if (nrow(model_input) < max(10L, length(predictors) + 3L)) {
    stop("Too few observations to fit model for outcome ", outcome, ".")
  }

  fit <- stats::lm(make_formula(outcome, predictors), data = model_input)
  attr(fit, "final_sample_cleaned") <- model_input
  attr(fit, "outcome") <- outcome
  attr(fit, "predictors") <- predictors
  attr(fit, "cluster_var") <- cluster_var
  fit
}

model_fit_stats <- function(model, outcome, model_block, notes = "") {
  y <- stats::model.response(stats::model.frame(model))
  pred <- stats::fitted(model)
  glance_stats <- broom::glance(model)
  tibble(
    outcome = outcome,
    model_block = model_block,
    n = stats::nobs(model),
    r_squared = glance_stats$r.squared,
    adjusted_r_squared = glance_stats$adj.r.squared,
    RMSE = rmse_vec(y, pred),
    MAE = mae_vec(y, pred),
    AIC = stats::AIC(model),
    BIC = stats::BIC(model),
    residual_standard_error = glance_stats$sigma,
    preferred_inference_method = "artist_clustered_if_available_else_HC3",
    notes = notes
  )
}

coeftest_to_tibble <- function(ct, se_col, p_col) {
  ct_mat <- as.matrix(unclass(ct))
  out <- as.data.frame(ct_mat[, 1:4, drop = FALSE])
  names(out) <- c("estimate", se_col, "statistic", p_col)
  tibble::as_tibble(out, rownames = "term") %>%
    select(term, estimate, all_of(se_col), all_of(p_col))
}

safe_hc3_tidy <- function(model) {
  tryCatch({
    ct <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC3"))
    coeftest_to_tibble(ct, "std_error_HC3", "p_value_HC3")
  }, error = function(e) {
    record_warning(paste("HC3 SE failed:", conditionMessage(e)))
    broom::tidy(model) %>% transmute(term, std_error_HC3 = NA_real_, p_value_HC3 = NA_real_)
  })
}

safe_cluster_tidy <- function(model) {
  model_data <- attr(model, "final_sample_cleaned")
  cluster_var <- attr(model, "cluster_var")

  if (is.null(cluster_var) || !cluster_var %in% names(model_data)) {
    return(broom::tidy(model) %>% transmute(term, std_error_clustered = NA_real_, p_value_clustered = NA_real_))
  }

  cluster <- model_data[[cluster_var]]
  n_clusters <- dplyr::n_distinct(cluster[!is.na(cluster)])
  if (n_clusters < 20) {
    record_warning(paste("Clustered SE skipped for", attr(model, "outcome"), "because clusters < 20."))
    return(broom::tidy(model) %>% transmute(term, std_error_clustered = NA_real_, p_value_clustered = NA_real_))
  }

  tryCatch({
    vc <- sandwich::vcovCL(model, cluster = cluster, type = "HC1")
    ct <- lmtest::coeftest(model, vcov. = vc)
    coeftest_to_tibble(ct, "std_error_clustered", "p_value_clustered")
  }, error = function(e) {
    record_warning(paste("Artist-clustered SE failed:", conditionMessage(e)))
    broom::tidy(model) %>% transmute(term, std_error_clustered = NA_real_, p_value_clustered = NA_real_)
  })
}

all_se_tidy <- function(model, outcome, model_block) {
  conventional <- broom::tidy(model) %>%
    transmute(
      term,
      estimate,
      std_error_conventional = std.error,
      p_value_conventional = p.value
    )

  hc3 <- safe_hc3_tidy(model)
  clustered <- safe_cluster_tidy(model)
  model_data <- attr(model, "final_sample_cleaned")
  n_clusters <- if (cluster_var %in% names(model_data)) dplyr::n_distinct(model_data[[cluster_var]]) else NA_integer_

  conventional %>%
    left_join(hc3, by = c("term", "estimate")) %>%
    left_join(clustered, by = c("term", "estimate")) %>%
    mutate(
      outcome = outcome,
      model_block = model_block,
      n = stats::nobs(model),
      n_artist_clusters = n_clusters,
      preferred_p_value = if_else(!is.na(p_value_clustered), p_value_clustered, p_value_HC3),
      preferred_std_error = if_else(!is.na(std_error_clustered), std_error_clustered, std_error_HC3),
      preferred_inference_method = if_else(!is.na(p_value_clustered), "artist_clustered", "HC3"),
      conf_low_preferred = estimate - stats::qt(0.975, df = stats::df.residual(model)) * preferred_std_error,
      conf_high_preferred = estimate + stats::qt(0.975, df = stats::df.residual(model)) * preferred_std_error,
      significance_preferred = significance_marker(preferred_p_value),
      .before = 1
    )
}

main_ols_models <- purrr::imap(outcome_datasets, function(outcome_data, outcome_name) {
  purrr::imap(feature_blocks, function(predictors, block_name) {
    fit_lm_model(outcome_data, outcome_name, predictors, cluster_var = cluster_var)
  })
})

main_ols_model_fit_comparison <- purrr::imap_dfr(main_ols_models, function(models_by_block, outcome_name) {
  purrr::imap_dfr(models_by_block, function(model, block_name) {
    model_fit_stats(model, outcome_name, block_name)
  })
})

safe_write_csv(main_ols_model_fit_comparison, file.path(table_dir, "main_ols_model_fit_comparison.csv"))

main_ols_coefficients_all_se_types <- purrr::imap_dfr(main_ols_models, function(models_by_block, outcome_name) {
  purrr::imap_dfr(models_by_block, function(model, block_name) {
    all_se_tidy(model, outcome_name, block_name)
  })
})

safe_write_csv(main_ols_coefficients_all_se_types, file.path(table_dir, "main_ols_coefficients_all_se_types.csv"))

main_combined_model_coefficients_clustered <- main_ols_coefficients_all_se_types %>%
  filter(model_block == "combined")

safe_write_csv(main_combined_model_coefficients_clustered, file.path(table_dir, "main_combined_model_coefficients_clustered.csv"))


# 5. Multiple-testing correction


main_combined_model_coefficients_bh_corrected <- main_combined_model_coefficients_clustered %>%
  filter(term != "(Intercept)") %>%
  group_by(outcome) %>%
  mutate(
    p_BH_within_outcome = p.adjust(preferred_p_value, method = "BH"),
    raw_sig_0_05 = preferred_p_value < 0.05,
    raw_sig_0_10 = preferred_p_value < 0.10,
    BH_within_sig_0_05 = p_BH_within_outcome < 0.05,
    BH_within_sig_0_10 = p_BH_within_outcome < 0.10
  ) %>%
  ungroup() %>%
  mutate(
    p_BH_across_all_combined = p.adjust(preferred_p_value, method = "BH"),
    BH_across_sig_0_05 = p_BH_across_all_combined < 0.05,
    BH_across_sig_0_10 = p_BH_across_all_combined < 0.10,
    interpretation_label = case_when(
      BH_across_sig_0_05 ~ "survives BH across all combined tests",
      BH_within_sig_0_05 ~ "survives BH within outcome only",
      raw_sig_0_05 ~ "nominal p < .05 only; exploratory",
      raw_sig_0_10 ~ "nominal p < .10 only; weak exploratory",
      TRUE ~ "not statistically significant"
    )
  ) %>%
  arrange(outcome, preferred_p_value)

safe_write_csv(main_combined_model_coefficients_bh_corrected, file.path(table_dir, "main_combined_model_coefficients_bh_corrected.csv"))

thesis_main_model_summary <- main_ols_model_fit_comparison %>%
  select(outcome, model_block, n, adjusted_r_squared, RMSE, MAE, AIC, BIC, preferred_inference_method, notes)

safe_write_csv(thesis_main_model_summary, file.path(table_dir, "thesis_main_model_summary.csv"))


# 6. Thesis explanatory conclusion table


main_model_wide <- main_ols_model_fit_comparison %>%
  select(outcome, model_block, adjusted_r_squared, RMSE, MAE, AIC, BIC) %>%
  pivot_wider(names_from = model_block, values_from = c(adjusted_r_squared, RMSE, MAE, AIC, BIC))

main_robust_terms <- main_combined_model_coefficients_bh_corrected %>%
  filter(BH_across_sig_0_05 | BH_within_sig_0_05) %>%
  group_by(outcome) %>%
  summarise(
    robust_predictors_after_BH = ifelse(n() == 0, "None", paste(term, collapse = "; ")),
    .groups = "drop"
  )

thesis_explanatory_conclusion_table <- main_ols_model_fit_comparison %>%
  group_by(outcome) %>%
  slice_max(adjusted_r_squared, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    outcome,
    best_fitting_OLS_block_by_adjusted_R2 = model_block,
    best_adjusted_R2 = adjusted_r_squared
  ) %>%
  left_join(main_model_wide, by = "outcome") %>%
  left_join(main_robust_terms, by = "outcome") %>%
  mutate(
    robust_predictors_after_BH = replace_na(robust_predictors_after_BH, "None"),
    combined_minus_audio_adjusted_R2 = adjusted_r_squared_combined - adjusted_r_squared_audio,
    combined_minus_lyrics_adjusted_R2 = adjusted_r_squared_combined - adjusted_r_squared_lyrics,
    combined_improves_over_audio_and_lyrics = adjusted_r_squared_combined >= pmax(adjusted_r_squared_audio, adjusted_r_squared_lyrics, na.rm = TRUE),
    interpretation_note = case_when(
      combined_improves_over_audio_and_lyrics ~ "Combined feature block has the highest or tied adjusted R2; interpret size of gain cautiously.",
      TRUE ~ "Combined block does not dominate simpler feature blocks by adjusted R2; emphasize parsimony."
    )
  )

safe_write_csv(thesis_explanatory_conclusion_table, file.path(table_dir, "thesis_explanatory_conclusion_table.csv"))


# 7. Robustness checks
# Robustness checks test whether the combined-model conclusions depend on controls, sample restrictions, or match quality.

fit_robustness_model <- function(data, outcome, predictors, robustness_type, sample_filter = "none", baseline_model = NULL) {
  model <- fit_lm_model(data, outcome, predictors, cluster_var = cluster_var)
  fit <- model_fit_stats(model, outcome, robustness_type, notes = paste("sample_filter:", sample_filter))
  coef <- all_se_tidy(model, outcome, robustness_type) %>%
    mutate(sample_filter = sample_filter)
  list(model = model, fit = fit, coefficients = coef)
}

robustness_results <- list()
add_robustness_result <- function(name, result) {
  robustness_results[[length(robustness_results) + 1]] <<- list(name = name, result = result)
}

# A. Era controls: replace entry_year_c_z with entry_era.
era_controls <- c("entry_era", "log1p_artist_prior_top10_count_z", "featured_artist_flag")
era_blocks <- list(
  controls_only = era_controls,
  audio = c(era_controls, main_audio_predictors),
  lyrics = c(era_controls, main_lyric_predictors),
  combined = c(era_controls, main_audio_predictors, main_lyric_predictors)
)

if ("entry_era" %in% names(final_sample_cleaned) && !all(is.na(final_sample_cleaned$entry_era))) {
  for (outcome in main_outcomes) {
    for (block in names(era_blocks)) {
      res <- fit_robustness_model(outcome_datasets[[outcome]], outcome, era_blocks[[block]],
                                  paste0("era_controls_", block), "full_sample")
      add_robustness_result(paste(outcome, "era", block, sep = "__"), res)
    }
  }
} else {
  record_skip("Era controls robustness skipped because entry_era is unavailable.")
}

# B. Year fixed effects: combined model only.
year_fe_controls <- c("entry_year_factor", "log1p_artist_prior_top10_count_z", "featured_artist_flag")
if ("entry_year_factor" %in% names(final_sample_cleaned)) {
  for (outcome in main_outcomes) {
    res <- fit_robustness_model(outcome_datasets[[outcome]], outcome,
                                c(year_fe_controls, main_audio_predictors, main_lyric_predictors),
                                "year_fixed_effects_combined", "full_sample")
    add_robustness_result(paste(outcome, "year_FE", sep = "__"), res)
  }
}

# C-E. Sample exclusions, combined model only.
exclusion_checks <- list(
  exclude_2021 = list(var = "sample_2000_2020_flag", keep = 1L),
  exclude_right_censored = list(var = "right_censored_flag", keep = 0L),
  exclude_long_gap = list(var = "long_gap_flag", keep = 0L),
  exclude_fuzzy_or_supp_lyrics = list(var = "lyric_fuzzy_or_supp_flag", keep = 0L),
  exclude_spotify_fuzzy = list(var = "spotify_fuzzy_flag", keep = 0L)
)

for (check_name in names(exclusion_checks)) {
  check <- exclusion_checks[[check_name]]
  if (!check$var %in% names(final_sample_cleaned)) {
    record_skip(paste(check_name, "skipped because", check$var, "is unavailable."))
    next
  }
  for (outcome in main_outcomes) {
    filtered <- outcome_datasets[[outcome]] %>% filter(.data[[check$var]] == check$keep)
    if (nrow(filtered) < 50) {
      record_skip(paste(check_name, outcome, "skipped because filtered n < 50."))
      next
    }
    res <- fit_robustness_model(filtered, outcome, main_combined_predictors, check_name,
                                paste0(check$var, " == ", check$keep))
    add_robustness_result(paste(outcome, check_name, sep = "__"), res)
  }
}

# F. Alternative artist-history controls.
if ("artist_career_age_years_z" %in% names(final_sample_cleaned)) {
  alt_artist_predictors <- c("entry_year_c_z", "artist_career_age_years_z", "featured_artist_flag", main_audio_predictors, main_lyric_predictors)
  for (outcome in main_outcomes) {
    res <- fit_robustness_model(outcome_datasets[[outcome]], outcome, alt_artist_predictors,
                                "alternative_artist_history_tenure", "full_sample")
    add_robustness_result(paste(outcome, "alt_artist_tenure", sep = "__"), res)
  }
}

if ("artist_prior_hot100_count" %in% names(final_sample_cleaned)) {
  final_sample_cleaned <- final_sample_cleaned %>%
    mutate(log1p_artist_prior_hot100_count_z = as.numeric(scale(log1p(artist_prior_hot100_count))))
  outcome_datasets <- setNames(purrr::map(main_outcomes, ~ make_outcome_dataset(final_sample_cleaned, .x)), main_outcomes)
  alt_hot100_predictors <- c("entry_year_c_z", "log1p_artist_prior_hot100_count_z", "featured_artist_flag", main_audio_predictors, main_lyric_predictors)
  for (outcome in main_outcomes) {
    res <- fit_robustness_model(outcome_datasets[[outcome]], outcome, alt_hot100_predictors,
                                "alternative_artist_history_hot100_count", "full_sample")
    add_robustness_result(paste(outcome, "alt_artist_hot100", sep = "__"), res)
  }
}

# G. Alternative time-to-peak outcome.
if ("log1p_weeks_to_peak_0" %in% names(final_sample_cleaned)) {
  alt_ttp_data <- final_sample_cleaned %>% filter(!is.na(log1p_weeks_to_peak_0))
  res <- fit_robustness_model(alt_ttp_data, "log1p_weeks_to_peak_0", main_combined_predictors,
                              "alternative_time_to_peak_outcome_log1p_weeks_to_peak_0", "full_sample")
  add_robustness_result("time_to_peak__alternative_log1p_weeks_to_peak_0", res)
}

# H. Alternative first-run outcomes.
if ("first_run_weeks_on_chart" %in% names(final_sample_cleaned)) {
  first_run_data <- final_sample_cleaned %>% filter(!is.na(first_run_weeks_on_chart))
  res <- fit_robustness_model(first_run_data, "first_run_weeks_on_chart", main_combined_predictors,
                              "alternative_first_run_weeks_on_chart", "full_sample")
  add_robustness_result("weeks_on_chart__first_run", res)
}
if ("first_run_decay_rate" %in% names(final_sample_cleaned)) {
  first_decay_data <- final_sample_cleaned %>% filter(!is.na(first_run_decay_rate))
  if (nrow(first_decay_data) >= 50) {
    res <- fit_robustness_model(first_decay_data, "first_run_decay_rate", main_combined_predictors,
                                "alternative_first_run_decay_rate", "full_sample")
    add_robustness_result("decay_rate__first_run", res)
  } else {
    record_skip("first_run_decay_rate robustness skipped because n < 50.")
  }
}

robustness_model_fit_summary <- purrr::map_dfr(robustness_results, ~ .x$result$fit %>% mutate(robustness_name = .x$name, .before = 1))
robustness_coefficients <- purrr::map_dfr(robustness_results, ~ .x$result$coefficients %>% mutate(robustness_name = .x$name, .before = 1))
robustness_sample_sizes <- robustness_model_fit_summary %>% select(robustness_name, outcome, model_block, n, notes)

safe_write_csv(robustness_model_fit_summary, file.path(robustness_dir, "robustness_model_fit_summary.csv"))
safe_write_csv(robustness_coefficients, file.path(robustness_dir, "robustness_coefficients.csv"))
safe_write_csv(robustness_sample_sizes, file.path(robustness_dir, "robustness_sample_sizes.csv"))


# 8. Alternative distributional model checks

alternative_model_fits <- list()
alternative_model_coefs <- list()

fit_alternative_glm <- function(data, outcome, family, model_name) {
  model_input <- data %>% select(all_of(c(outcome, main_combined_predictors))) %>% drop_na()
  fit <- stats::glm(make_formula(outcome, main_combined_predictors), data = model_input, family = family)
  pred <- stats::predict(fit, type = "response")
  y <- model_input[[outcome]]
  list(
    fit = tibble(
      outcome = outcome, alternative_model = model_name, n = nrow(model_input),
      RMSE = rmse_vec(y, pred), MAE = mae_vec(y, pred), AIC = AIC(fit), BIC = BIC(fit),
      notes = "distributional robustness model"
    ),
    coef = broom::tidy(fit) %>% mutate(outcome = outcome, alternative_model = model_name, .before = 1)
  )
}

fit_alternative_lm <- function(data, outcome, predictors, model_name, base_outcome) {
  fit <- fit_lm_model(data, outcome, predictors, cluster_var = cluster_var)
  list(
    fit = model_fit_stats(fit, base_outcome, model_name,
                          notes = paste("transformed/alternative outcome:", outcome)),
    coef = all_se_tidy(fit, base_outcome, model_name)
  )
}

# Count-like checks for weeks_on_chart and time_to_peak.
if (has_pkg("MASS")) {
  for (outcome in c("weeks_on_chart", "time_to_peak")) {
    data_o <- outcome_datasets[[outcome]]
    poisson_res <- tryCatch(
      fit_alternative_glm(data_o, outcome, stats::poisson(), paste0(outcome, "_poisson")),
      error = function(e) { record_skip(paste(outcome, "Poisson skipped:", conditionMessage(e))); NULL }
    )
    if (!is.null(poisson_res)) {
      alternative_model_fits[[length(alternative_model_fits) + 1]] <- poisson_res$fit
      alternative_model_coefs[[length(alternative_model_coefs) + 1]] <- poisson_res$coef

      # Overdispersion check from Poisson fit; run NB if overdispersed.
      pfit <- stats::glm(make_formula(outcome, main_combined_predictors),
                         data = data_o %>% select(all_of(c(outcome, main_combined_predictors))) %>% drop_na(),
                         family = stats::poisson())
      dispersion <- sum(stats::residuals(pfit, type = "pearson")^2, na.rm = TRUE) / stats::df.residual(pfit)
      if (is.finite(dispersion) && dispersion > 1.5) {
        nb_res <- tryCatch({
          model_input <- data_o %>% select(all_of(c(outcome, main_combined_predictors))) %>% drop_na()
          nb_fit <- MASS::glm.nb(make_formula(outcome, main_combined_predictors), data = model_input)
          pred <- stats::predict(nb_fit, type = "response")
          y <- model_input[[outcome]]
          list(
            fit = tibble(outcome = outcome, alternative_model = paste0(outcome, "_negative_binomial"),
                         n = nrow(model_input), RMSE = rmse_vec(y, pred), MAE = mae_vec(y, pred),
                         AIC = AIC(nb_fit), BIC = BIC(nb_fit),
                         notes = paste("negative binomial; Poisson dispersion", round(dispersion, 3))),
            coef = broom::tidy(nb_fit) %>% mutate(outcome = outcome, alternative_model = paste0(outcome, "_negative_binomial"), .before = 1)
          )
        }, error = function(e) { record_skip(paste(outcome, "negative binomial skipped:", conditionMessage(e))); NULL })
        if (!is.null(nb_res)) {
          alternative_model_fits[[length(alternative_model_fits) + 1]] <- nb_res$fit
          alternative_model_coefs[[length(alternative_model_coefs) + 1]] <- nb_res$coef
        }
      }
    }
  }
} else {
  record_skip("Poisson/NB checks skipped because MASS is unavailable.")
}

# Transformed OLS checks.
transformed_checks <- list(
  time_to_peak = "log_time_to_peak",
  time_to_peak_alt = "log1p_weeks_to_peak_0",
  weeks_on_chart = "log_weeks_on_chart",
  rise_speed = "log1p_rise_speed",
  decay_rate = "log1p_decay_rate"
)

for (base in names(transformed_checks)) {
  transformed <- transformed_checks[[base]]
  if (!transformed %in% names(final_sample_cleaned)) next
  base_outcome <- ifelse(base == "time_to_peak_alt", "time_to_peak", base)
  data_t <- final_sample_cleaned %>% filter(!is.na(.data[[transformed]]))
  res <- tryCatch(
    fit_alternative_lm(data_t, transformed, main_combined_predictors, paste0("transformed_OLS_", transformed), base_outcome),
    error = function(e) { record_skip(paste("Transformed OLS skipped for", transformed, conditionMessage(e))); NULL }
  )
  if (!is.null(res)) {
    alternative_model_fits[[length(alternative_model_fits) + 1]] <- res$fit
    alternative_model_coefs[[length(alternative_model_coefs) + 1]] <- res$coef
  }
}

# Robust regression for rise_speed and decay_rate.
if (has_pkg("MASS")) {
  for (outcome in c("rise_speed", "decay_rate")) {
    data_r <- outcome_datasets[[outcome]] %>% select(all_of(c(outcome, main_combined_predictors))) %>% drop_na()
    rlm_res <- tryCatch({
      rfit <- MASS::rlm(make_formula(outcome, main_combined_predictors), data = data_r)
      pred <- stats::predict(rfit)
      y <- data_r[[outcome]]
      list(
        fit = tibble(outcome = outcome, alternative_model = paste0(outcome, "_robust_regression_rlm"),
                     n = nrow(data_r), RMSE = rmse_vec(y, pred), MAE = mae_vec(y, pred),
                     AIC = NA_real_, BIC = NA_real_, notes = "MASS::rlm; p-values not reported"),
        coef = tibble(term = names(stats::coef(rfit)), estimate = as.numeric(stats::coef(rfit))) %>%
          mutate(outcome = outcome, alternative_model = paste0(outcome, "_robust_regression_rlm"), .before = 1)
      )
    }, error = function(e) { record_skip(paste(outcome, "robust regression skipped:", conditionMessage(e))); NULL })
    if (!is.null(rlm_res)) {
      alternative_model_fits[[length(alternative_model_fits) + 1]] <- rlm_res$fit
      alternative_model_coefs[[length(alternative_model_coefs) + 1]] <- rlm_res$coef
    }
  }
}

alternative_model_fit_summary <- if (length(alternative_model_fits) > 0) bind_rows(alternative_model_fits) else tibble()
alternative_model_coefficients <- if (length(alternative_model_coefs) > 0) bind_rows(alternative_model_coefs) else tibble()

safe_write_csv(alternative_model_fit_summary, file.path(robustness_dir, "alternative_model_fit_summary.csv"))
safe_write_csv(alternative_model_coefficients, file.path(robustness_dir, "alternative_model_coefficients.csv"))


# 9. Machine-learning regression setup and execution


ml_required <- c("rsample", "recipes", "parsnip", "workflows", "yardstick", "tune", "dials")
ml_available <- all(vapply(ml_required, has_pkg, logical(1)))

ml_cv_results_long <- tibble()
ml_cv_summary <- tibble()
ml_best_tuning_parameters <- tibble()
ml_variable_importance <- tibble()
ml_vs_ols_comparison <- tibble()
thesis_predictive_conclusion_table <- tibble()

ml_predictors_raw <- c(
  "danceability", "energy", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo",
  "log_num_words", "uniq_ratio", "avg_word_length", "top_5_word_share", "sentiment_balance",
  "emotionality_share", "self_reference_share", "second_person_share",
  "entry_year", "log1p_artist_prior_top10_count", "featured_artist_flag"
)

if (!ml_available) {
  record_skip("ML skipped because one or more tidymodels packages are unavailable.")
} else {
  suppressPackageStartupMessages({
    library(rsample)
    library(recipes)
    library(parsnip)
    library(workflows)
    library(yardstick)
    library(tune)
    library(dials)
  })

  make_fold_id <- function(data) {
    if ("id2" %in% names(data)) paste(data$id, data$id2, sep = "_") else data$id
  }

  make_null_predictions <- function(folds, outcome) {
    purrr::pmap_dfr(folds, function(splits, id, id2 = NULL, ...) {
      train <- rsample::analysis(splits)
      assess <- rsample::assessment(splits)
      fold_id <- if (is.null(id2)) id else paste(id, id2, sep = "_")
      tibble(
        fold_id = fold_id,
        .row = assess$.ml_row_id,
        truth = assess[[outcome]],
        .pred = mean(train[[outcome]], na.rm = TRUE)
      )
    })
  }

  summarize_predictions <- function(pred, null_pred, outcome, model_name, n_obs, tuning = "none") {
    pred_std <- pred %>%
      mutate(
        fold_id = if ("id2" %in% names(.)) paste(id, id2, sep = "_") else id,
        .row = if (".row" %in% names(.)) .data$.row else row_number()
      ) %>%
      transmute(fold_id, .row, truth = .data[[outcome]], .pred)
    null_std <- null_pred %>% transmute(fold_id, .row, null_pred = .pred)
    joined <- pred_std %>% left_join(null_std, by = c("fold_id", ".row"))

    fold_metrics <- joined %>%
      group_by(fold_id) %>%
      summarise(
        n_fold = n(),
        SSE_model = sum((truth - .pred)^2, na.rm = TRUE),
        SSE_null = sum((truth - null_pred)^2, na.rm = TRUE),
        RMSE = sqrt(mean((truth - .pred)^2, na.rm = TRUE)),
        MAE = mean(abs(truth - .pred), na.rm = TRUE),
        oos_R2_vs_fold_mean_null = ifelse(SSE_null > 0, 1 - SSE_model / SSE_null, NA_real_),
        .groups = "drop"
      )

    total_SSE_model <- sum(fold_metrics$SSE_model, na.rm = TRUE)
    total_SSE_null <- sum(fold_metrics$SSE_null, na.rm = TRUE)
    tibble(
      outcome = outcome,
      model = model_name,
      n = n_obs,
      n_predictions = nrow(joined),
      n_folds = n_distinct(joined$fold_id),
      RMSE = sqrt(mean((joined$truth - joined$.pred)^2, na.rm = TRUE)),
      MAE = mean(abs(joined$truth - joined$.pred), na.rm = TRUE),
      out_of_sample_R2_vs_fold_mean_null = ifelse(total_SSE_null > 0, 1 - total_SSE_model / total_SSE_null, NA_real_),
      RMSE_sd_across_folds = sd(fold_metrics$RMSE, na.rm = TRUE),
      MAE_sd_across_folds = sd(fold_metrics$MAE, na.rm = TRUE),
      tuning_parameters = tuning,
      predictors_used = paste(ml_predictors_raw, collapse = "; ")
    )
  }

  collect_best_predictions <- function(result) {
    metrics <- tune::collect_metrics(result)
    preds <- tune::collect_predictions(result)
    if (".config" %in% names(metrics) && ".config" %in% names(preds)) {
      best_config <- metrics %>% filter(.metric == "rmse") %>% arrange(mean) %>% slice(1) %>% pull(.config)
      preds <- preds %>% filter(.config == best_config)
      tuning <- metrics %>% filter(.config == best_config, .metric == "rmse") %>% select(.config, mean, std_err)
    } else {
      tuning <- tibble(.config = "fixed", mean = NA_real_, std_err = NA_real_)
    }
    list(predictions = preds, tuning = tuning)
  }

  ml_summaries <- list()
  ml_tuning <- list()
  ml_prediction_rows <- list()

  for (outcome in main_outcomes) {
    ml_data <- final_sample_cleaned %>%
      select(all_of(c(outcome, ml_predictors_raw))) %>%
      drop_na() %>%
      mutate(.ml_row_id = row_number())

    set.seed(analysis_seed)
    folds <- rsample::vfold_cv(ml_data, v = 5, repeats = 5)
    null_pred <- make_null_predictions(folds, outcome)

    null_summary <- tibble(
      outcome = outcome,
      model = "null_mean_baseline",
      n = nrow(ml_data),
      n_predictions = nrow(null_pred),
      n_folds = n_distinct(null_pred$fold_id),
      RMSE = sqrt(mean((null_pred$truth - null_pred$.pred)^2, na.rm = TRUE)),
      MAE = mean(abs(null_pred$truth - null_pred$.pred), na.rm = TRUE),
      out_of_sample_R2_vs_fold_mean_null = 0,
      RMSE_sd_across_folds = NA_real_,
      MAE_sd_across_folds = NA_real_,
      tuning_parameters = "fold-specific training mean",
      predictors_used = "none"
    )
    ml_summaries[[length(ml_summaries) + 1]] <- null_summary

    rec <- recipes::recipe(make_formula(outcome, ml_predictors_raw), data = ml_data) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_normalize(recipes::all_numeric_predictors())

    control_resamples <- tune::control_resamples(save_pred = TRUE)
    control_grid <- tune::control_grid(save_pred = TRUE)
    metric_set <- yardstick::metric_set(yardstick::rmse, yardstick::mae)

    # CV OLS baseline.
    ols_spec <- parsnip::linear_reg() %>% parsnip::set_engine("lm") %>% parsnip::set_mode("regression")
    ols_wf <- workflows::workflow() %>% workflows::add_recipe(rec) %>% workflows::add_model(ols_spec)
    ols_res <- tune::fit_resamples(ols_wf, resamples = folds, metrics = metric_set, control = control_resamples)
    ols_best <- collect_best_predictions(ols_res)
    ml_summaries[[length(ml_summaries) + 1]] <- summarize_predictions(ols_best$predictions, null_pred, outcome, "cv_OLS_combined", nrow(ml_data), "fixed")

    # Elastic net.
    if (has_pkg("glmnet")) {
      en_spec <- parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
        parsnip::set_engine("glmnet") %>% parsnip::set_mode("regression")
      en_wf <- workflows::workflow() %>% workflows::add_recipe(rec) %>% workflows::add_model(en_spec)
      en_grid <- dials::grid_regular(dials::penalty(range = c(-4, 1)), dials::mixture(range = c(0, 1)), levels = c(penalty = 5, mixture = 5))
      en_res <- tune::tune_grid(en_wf, resamples = folds, grid = en_grid, metrics = metric_set, control = control_grid)
      en_best <- collect_best_predictions(en_res)
      ml_summaries[[length(ml_summaries) + 1]] <- summarize_predictions(en_best$predictions, null_pred, outcome, "elastic_net", nrow(ml_data), en_best$tuning$.config[1])
      ml_tuning[[length(ml_tuning) + 1]] <- en_best$tuning %>% mutate(outcome = outcome, model = "elastic_net", .before = 1)
    } else {
      record_skip(paste(outcome, "elastic net skipped because glmnet is unavailable."))
    }

    # Random forest.
    if (has_pkg("ranger")) {
      rf_spec <- parsnip::rand_forest(mtry = tune::tune(), trees = 500, min_n = tune::tune()) %>%
        parsnip::set_engine("ranger", importance = "permutation") %>% parsnip::set_mode("regression")
      rf_wf <- workflows::workflow() %>% workflows::add_recipe(rec) %>% workflows::add_model(rf_spec)
      p <- length(ml_predictors_raw)
      mtry_values <- unique(pmax(1L, pmin(p, round(c(1, sqrt(p), p / 2, p)))))
      rf_grid <- tidyr::crossing(mtry = as.integer(mtry_values), min_n = c(5L, 10L, 20L)) %>% slice_head(n = 8)
      rf_res <- tune::tune_grid(rf_wf, resamples = folds, grid = rf_grid, metrics = metric_set, control = control_grid)
      rf_best <- collect_best_predictions(rf_res)
      ml_summaries[[length(ml_summaries) + 1]] <- summarize_predictions(rf_best$predictions, null_pred, outcome, "random_forest", nrow(ml_data), rf_best$tuning$.config[1])
      ml_tuning[[length(ml_tuning) + 1]] <- rf_best$tuning %>% mutate(outcome = outcome, model = "random_forest", .before = 1)

      # Full-sample RF variable importance for supplementary interpretation only.
      rf_full <- tryCatch({
        ranger::ranger(
          formula = make_formula(outcome, ml_predictors_raw), data = ml_data,
          num.trees = 500, mtry = max(1, floor(sqrt(p))), min.node.size = 5,
          importance = "permutation", seed = analysis_seed
        )
      }, error = function(e) NULL)
      if (!is.null(rf_full)) {
        ml_variable_importance <- bind_rows(
          ml_variable_importance,
          tibble(outcome = outcome, model = "random_forest", term = names(rf_full$variable.importance),
                 importance = as.numeric(rf_full$variable.importance)) %>% arrange(outcome, desc(importance))
        )
      }
    } else {
      record_skip(paste(outcome, "random forest skipped because ranger is unavailable."))
    }
  }

  ml_cv_summary <- bind_rows(ml_summaries) %>% arrange(outcome, RMSE)
  ml_best_tuning_parameters <- if (length(ml_tuning) > 0) bind_rows(ml_tuning) else tibble()

  # Long-form output can be reconstructed from summary here; keep a clean long table.
  ml_cv_results_long <- ml_cv_summary %>%
    select(outcome, model, n, n_predictions, n_folds, RMSE, MAE, out_of_sample_R2_vs_fold_mean_null, tuning_parameters, predictors_used)

  ml_vs_ols_comparison <- ml_cv_summary %>%
    group_by(outcome) %>%
    mutate(
      null_RMSE = RMSE[model == "null_mean_baseline"][1],
      cv_OLS_RMSE = RMSE[model == "cv_OLS_combined"][1],
      RMSE_improvement_vs_null = ifelse(!is.na(null_RMSE), null_RMSE - RMSE, NA_real_),
      RMSE_improvement_vs_cv_OLS = ifelse(!is.na(cv_OLS_RMSE), cv_OLS_RMSE - RMSE, NA_real_),
      RMSE_improvement_pct_vs_null = ifelse(null_RMSE > 0, RMSE_improvement_vs_null / null_RMSE, NA_real_),
      best_model_for_outcome = RMSE == min(RMSE, na.rm = TRUE)
    ) %>%
    ungroup()

  thesis_predictive_conclusion_table <- ml_vs_ols_comparison %>%
    filter(model != "null_mean_baseline") %>%
    group_by(outcome) %>%
    slice_min(RMSE, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      outcome,
      best_ML_model = model,
      null_RMSE,
      best_ML_RMSE = RMSE,
      RMSE_improvement_percentage = RMSE_improvement_pct_vs_null,
      out_of_sample_R2 = out_of_sample_R2_vs_fold_mean_null,
      interpretation_note = case_when(
        out_of_sample_R2 > 0.10 ~ "meaningful but still limited predictive gain over null baseline",
        out_of_sample_R2 > 0 ~ "modest predictive gain over null baseline",
        TRUE ~ "does not outperform null baseline in out-of-sample R2"
      )
    )
}

safe_write_csv(ml_cv_results_long, file.path(ml_dir, "ml_cv_results_long.csv"))
safe_write_csv(ml_cv_summary, file.path(ml_dir, "ml_cv_summary.csv"))
safe_write_csv(ml_best_tuning_parameters, file.path(ml_dir, "ml_best_tuning_parameters.csv"))
safe_write_csv(ml_variable_importance, file.path(ml_dir, "ml_variable_importance.csv"))
safe_write_csv(ml_vs_ols_comparison, file.path(ml_dir, "ml_vs_ols_comparison.csv"))
safe_write_csv(thesis_predictive_conclusion_table, file.path(ml_dir, "thesis_predictive_conclusion_table.csv"))


# 10. Final validation and run summary
# The run summary records the files, skipped optional models, and warnings created during analysis.

model_run_summary <- tibble(
  item = c(
    "timestamp", "dataset_path", "n_rows", "n_columns", "n_outcomes_modeled",
    "n_main_OLS_models_run", "n_robustness_models_run", "n_alternative_models_run",
    "n_ML_models_reported", "n_output_files_created", "warnings", "skipped_models",
    "completed_successfully"
  ),
  value = as.character(c(
    as.character(Sys.time()), data_path, nrow(final_sample_cleaned), ncol(final_sample_cleaned), length(main_outcomes),
    length(main_outcomes) * length(feature_blocks), nrow(robustness_model_fit_summary), nrow(alternative_model_fit_summary),
    nrow(ml_cv_summary), length(created_files),
    ifelse(length(model_warnings) == 0, "None", paste(model_warnings, collapse = " | ")),
    ifelse(length(skipped_models) == 0, "None", paste(skipped_models, collapse = " | ")),
    TRUE
  ))
)

safe_write_csv(model_run_summary, file.path(output_dir, "model_run_summary.csv"))

cat("\n================ Analysis run summary ================\n")
print(model_run_summary)
cat("\nOutputs created in:", normalizePath(output_dir), "\n")
cat("\nScript completed successfully.\n")


# End of script

