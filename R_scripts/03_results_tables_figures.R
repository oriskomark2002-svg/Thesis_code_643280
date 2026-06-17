################################################################################
# 03_results_tables_figures.R
#
# MSc thesis Chapter 4 Results figures and tables
# What Drives Successful Song Trajectories?
# Comparative Analysis of Audio and Lyrical Features Using Spotify and Billboard
# Hot 100 Data
#
# Purpose:
#   Generate only the final retained Chapter 4 Results and appendix outputs:
#   - Body Tables 4.1, 4.2, 4.3, 4.4
#   - Body Figures 4.1, 4.2, 4.3, 4.4
#   - Appendix Figures A.1, A.2, A.3
#   - Appendix Tables A.4, A.5, A.6
#   - A manifest listing generated outputs
#
# This script is adapted to the v6 cleaning/modeling pipeline with right-censoring
# and Spotify-fuzzy robustness checks. It uses the newest output filenames from
# music_success_analysis_engineered_v6_regression_ml_rightcensor_spotifyrobust.R.
#
# It intentionally does NOT create older descriptive/sample/conceptual/diagnostic
# outputs that are no longer retained in the thesis.
################################################################################

# ------------------------------------------------------------------------------
# 0. Packages and setup
# ------------------------------------------------------------------------------

options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

# Set to TRUE only if you want this script to install missing packages.
auto_install_missing_packages <- FALSE

required_packages <- c(
  "readr", "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "ggplot2", "scales", "forcats", "flextable", "officer"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  if (isTRUE(auto_install_missing_packages)) {
    install.packages(missing_packages, repos = "https://cran.rstudio.com")
  } else {
    stop(
      "Install the following packages before running this script, or set ",
      "auto_install_missing_packages <- TRUE: ",
      paste(missing_packages, collapse = ", ")
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
  library(ggplot2)
  library(scales)
  library(forcats)
  library(flextable)
  library(officer)
})

set.seed(2026)

output_dir <- "outputs"
figure_dir <- file.path(output_dir, "results_figures_final")
table_dir <- file.path(output_dir, "results_tables_final")

dir.create(output_dir, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

created_outputs <- tibble(
  section = character(),
  output_type = character(),
  file = character(),
  source_files = character(),
  note = character()
)

record_output <- function(section, output_type, file, source_files = character(), note = "") {
  source_files <- unique(source_files[!is.na(source_files) & nzchar(source_files)])
  created_outputs <<- bind_rows(
    created_outputs,
    tibble(
      section = section,
      output_type = output_type,
      file = file,
      source_files = paste(source_files, collapse = "; "),
      note = note
    )
  )
  invisible(file)
}

# ------------------------------------------------------------------------------
# 1. Robust file-resolution and import helpers
# One resolver is enough: all result files are searched from the project root and the outputs folders.
# ------------------------------------------------------------------------------

escape_regex <- function(x) {
  stringr::str_replace_all(x, "([\\.\\+\\*\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1")
}

script_dir <- tryCatch({
  this_file <- normalizePath(sys.frame(1)$ofile, mustWork = FALSE)
  if (!is.na(this_file) && nzchar(this_file)) dirname(this_file) else getwd()
}, error = function(e) getwd())

input_search_dirs <- unique(c(
  ".",
  script_dir,
  output_dir,
  file.path(output_dir, "tables"),
  file.path(output_dir, "robustness"),
  file.path(output_dir, "ml"),
  file.path(output_dir, "diagnostics"),
  file.path(output_dir, "models"),
  table_dir,
  figure_dir,
  file.path(script_dir, "outputs"),
  file.path(script_dir, "outputs", "tables"),
  file.path(script_dir, "outputs", "robustness"),
  file.path(script_dir, "outputs", "ml"),
  file.path(script_dir, "outputs", "diagnostics"),
  file.path(script_dir, "outputs", "models"),
  file.path(script_dir, "outputs", "results_tables_final"),
  file.path(script_dir, "outputs", "results_figures_final")
))

thesis_resolve_file <- function(filename, dirs = NULL, required = TRUE) {
  if (is.null(dirs)) dirs <- input_search_dirs
  direct <- file.path(dirs, filename)
  direct <- direct[file.exists(direct)]
  if (length(direct) > 0) return(normalizePath(direct[[1]], mustWork = TRUE))

  stem <- tools::file_path_sans_ext(basename(filename))
  ext <- tools::file_ext(filename)
  pattern <- paste0("^", escape_regex(stem), "(\\([0-9]+\\))?\\.", escape_regex(ext), "$")

  matches <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(d)) return(character())
    file.path(d, list.files(d, pattern = pattern, full.names = FALSE))
  }))

  if (length(matches) > 0) {
    info <- file.info(matches)
    matches <- matches[order(info$mtime, decreasing = TRUE)]
    return(normalizePath(matches[[1]], mustWork = TRUE))
  }

  if (required) stop("Could not find required file: ", filename)
  NA_character_
}

thesis_resolve_first_available <- function(filenames, required = TRUE, dirs = NULL) {
  if (is.null(dirs)) dirs <- input_search_dirs
  for (f in filenames) {
    path <- thesis_resolve_file(f, dirs = dirs, required = FALSE)
    if (!is.na(path)) return(path)
  }
  if (required) stop("Could not find any of: ", paste(filenames, collapse = ", "))
  NA_character_
}


# Backward-compatible aliases for older section snippets.
# These avoid recursive default arguments in the file resolver.
resolve_file <- function(filename, dirs = NULL, required = TRUE) {
  thesis_resolve_file(filename, dirs = dirs, required = required)
}

resolve_first_available <- function(filenames, required = TRUE, dirs = NULL) {
  thesis_resolve_first_available(filenames, required = required, dirs = dirs)
}

read_csv_path <- function(path) {
  message("Reading: ", path)
  readr::read_csv(path, show_col_types = FALSE, guess_max = 50000)
}

thesis_read_csv_first_available <- function(filenames, required = TRUE) {
  path <- thesis_resolve_first_available(filenames, required = required)
  if (is.na(path)) return(NULL)
  read_csv_path(path)
}

source_name <- function(path) {
  out <- basename(path)
  out[is.na(out)] <- ""
  out
}

# ------------------------------------------------------------------------------
# 2. Labels, formatting, and themes
# Labels translate code variables into thesis-readable names used in the tables and figures.
# ------------------------------------------------------------------------------

outcome_levels <- c("time_to_peak", "weeks_on_chart", "rise_speed", "decay_rate")
outcome_labels <- c(
  time_to_peak = "Time to peak",
  weeks_on_chart = "Weeks on chart",
  rise_speed = "Rise speed",
  decay_rate = "Decay rate",
  log_time_to_peak = "Log time to peak",
  log_weeks_on_chart = "Log weeks on chart",
  log1p_weeks_to_peak_0 = "Log(1 + weeks to peak)",
  log1p_rise_speed = "Log(1 + rise speed)",
  log1p_decay_rate = "Log(1 + decay rate)",
  first_run_weeks_on_chart = "First-run weeks on chart",
  first_run_decay_rate = "First-run decay rate"
)

block_levels <- c("controls_only", "audio", "lyrics", "combined")
block_labels <- c(
  controls_only = "Controls-only",
  audio = "Audio",
  lyrics = "Lyrics",
  combined = "Combined",
  null = "Null"
)

model_labels <- c(
  null_mean_baseline = "Null mean baseline",
  cv_OLS_combined = "CV OLS combined",
  elastic_net = "Elastic net",
  random_forest = "Random forest",
  xgboost = "XGBoost"
)

term_labels <- c(
  entry_year_c_z = "Entry year",
  entry_year = "Entry year",
  log1p_artist_prior_top10_count_z = "Prior Top 10 history",
  log1p_artist_prior_top10_count = "Prior Top 10 history",
  log1p_artist_prior_hot100_count_z = "Prior Hot 100 history",
  artist_career_age_years_z = "Artist career age",
  featured_artist_flag = "Featured-artist credit",
  danceability_z = "Danceability",
  danceability = "Danceability",
  energy_z = "Energy",
  energy = "Energy",
  loudness_z = "Loudness",
  loudness = "Loudness",
  speechiness_z = "Speechiness",
  speechiness = "Speechiness",
  acousticness_z = "Acousticness",
  acousticness = "Acousticness",
  liveness_z = "Liveness",
  liveness = "Liveness",
  valence_z = "Valence",
  valence = "Valence",
  tempo_z = "Tempo",
  tempo = "Tempo",
  instrumentalness_z = "Instrumentalness",
  instrumentalness_any = "Any instrumentalness",
  log_num_words_z = "Lyric length",
  log_num_words = "Lyric length",
  uniq_ratio_z = "Lexical uniqueness",
  uniq_ratio = "Lexical uniqueness",
  avg_word_length_z = "Average word length",
  avg_word_length = "Average word length",
  top_5_word_share_z = "Top-five word share",
  top_5_word_share = "Top-five word share",
  sentiment_balance_z = "Sentiment balance",
  sentiment_balance = "Sentiment balance",
  emotionality_share_z = "Emotionality",
  emotionality_share = "Emotionality",
  self_reference_share_z = "Self-reference",
  self_reference_share = "Self-reference",
  second_person_share_z = "Second-person address",
  second_person_share = "Second-person address"
)

label_outcome <- function(x) {
  out <- outcome_labels[as.character(x)]
  out[is.na(out)] <- as.character(x)[is.na(out)]
  unname(out)
}

label_block <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "null"
  x <- str_replace_all(x, "-", "_")
  out <- block_labels[x]
  out[is.na(out)] <- str_to_title(str_replace_all(x[is.na(out)], "_", " "))
  unname(out)
}

label_model <- function(x) {
  out <- model_labels[as.character(x)]
  out[is.na(out)] <- str_to_title(str_replace_all(as.character(x)[is.na(out)], "_", " "))
  unname(out)
}

label_term <- function(x) {
  out <- term_labels[as.character(x)]
  out[is.na(out)] <- str_to_title(str_replace_all(as.character(x)[is.na(out)], "_", " "))
  unname(out)
}

format_decimal <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

format_p <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(x) ~ "",
    x < 0.001 ~ "< .001",
    TRUE ~ sub("^0", "", formatC(x, format = "f", digits = 3))
  )
}

format_percent <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

clean_model_block_values <- function(df) {
  if (!"model_block" %in% names(df)) return(df)
  df %>%
    mutate(
      model_block = as.character(model_block),
      model_block = if_else(is.na(model_block) | model_block == "" | model_block == "NA", "null", model_block),
      model_block = str_replace_all(model_block, "-", "_")
    )
}

coalesce_numeric_col <- function(df, candidates, default = NA_real_) {
  out <- rep(default, nrow(df))
  for (nm in candidates) {
    if (nm %in% names(df)) {
      val <- suppressWarnings(as.numeric(df[[nm]]))
      out <- ifelse(is.na(out), val, out)
    }
  }
  out
}

coalesce_character_col <- function(df, candidates, default = NA_character_) {
  out <- rep(default, nrow(df))
  for (nm in candidates) {
    if (nm %in% names(df)) {
      val <- as.character(df[[nm]])
      out <- ifelse(is.na(out) | out == "", val, out)
    }
  }
  out
}

palette_block <- c(
  "Controls-only" = "grey95",
  "Audio" = "grey65",
  "Lyrics" = "grey40",
  "Combined" = "grey20"
)

palette_p <- c(
  "Raw p-value" = "grey30",
  "BH within outcome" = "grey78",
  "BH across all combined tests" = "grey2"
)

palette_yes_no <- c("TRUE" = "grey88", "FALSE" = "#B0B0B0")

# Helpers for within-facet ordering.
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun, ...)
}

scale_y_reordered <- function(..., sep = "___") {
  scale_y_discrete(labels = function(x) gsub(paste0(sep, ".*$"), "", x), ...)
}

scale_x_reordered <- function(..., sep = "___") {
  scale_x_discrete(labels = function(x) gsub(paste0(sep, ".*$"), "", x), ...)
}

theme_thesis <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, margin = margin(b = 4)),
      plot.subtitle = element_text(size = base_size, color = "grey25", margin = margin(b = 8)),
      plot.caption = element_text(size = base_size - 2, color = "grey35", hjust = 0, margin = margin(t = 8)),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey20"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = NA),
      plot.margin = margin(12, 16, 12, 16)
    )
}

save_png <- function(plot, filename, width, height, section, source_files, note = "", dpi = 300) {
  path <- file.path(figure_dir, filename)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  record_output(section, "figure_png", path, source_files, note)
  invisible(path)
}

make_flextable_readable <- function(df, font_size = NULL) {
  df_print <- df %>% mutate(across(everything(), ~ as.character(.x)))
  if (is.null(font_size)) font_size <- ifelse(ncol(df_print) > 8, 7.2, 8.5)

  ft <- flextable::flextable(df_print)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, bg = "#EFEFEF", part = "header")
  ft <- flextable::color(ft, color = "#222222", part = "all")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding = 3, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::fontsize(ft, size = font_size + 0.5, part = "header")
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
  ft <- flextable::fix_border_issues(ft)
  ft
}

write_docx_table <- function(df, filename, title, note = NULL, section, source_files, landscape = TRUE, font_size = NULL) {
  path <- file.path(table_dir, filename)

  doc <- officer::read_docx()
  if (isTRUE(landscape)) {
    landscape_section <- officer::prop_section(
      page_size = officer::page_size(orient = "landscape"),
      page_margins = officer::page_mar(top = 0.45, bottom = 0.45, left = 0.45, right = 0.45)
    )
    doc <- officer::body_set_default_section(doc, value = landscape_section)
  }

  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.null(note) && nzchar(note)) {
    doc <- officer::body_add_par(doc, paste0("Note: ", note), style = "Normal")
  }
  doc <- flextable::body_add_flextable(doc, make_flextable_readable(df, font_size = font_size))
  print(doc, target = path)

  record_output(section, "table_docx", path, source_files, note %||% "")
  invisible(path)
}

`%||%` <- function(x, y) if (is.null(x)) y else x


# ------------------------------------------------------------------------------
# 3. Read final modeling outputs
# The results script reads the retained modeling outputs created by 02_model_regression_ml.R.
# ------------------------------------------------------------------------------

# The cleaned sample is read only when a figure needs the raw modeling rows.
analysis_path <- thesis_resolve_first_available(c("final_sample_cleaned.csv"), required = FALSE)
analysis_data <- if (!is.na(analysis_path)) read_csv_path(analysis_path) else NULL

main_fit_path <- thesis_resolve_first_available(c(
  "main_ols_model_fit_comparison.csv",
  "thesis_main_model_summary.csv"
))
main_fit <- read_csv_path(main_fit_path) %>% clean_model_block_values()

bh_coef_path <- thesis_resolve_first_available(c(
  "main_combined_model_coefficients_bh_corrected.csv",
  "main_combined_model_coefficients_bh_corrected(3).csv"
), required = FALSE)
clustered_coef_path <- thesis_resolve_first_available(c("main_combined_model_coefficients_clustered.csv"), required = FALSE)
all_se_path <- thesis_resolve_first_available(c("main_ols_coefficients_all_se_types.csv"))

if (!is.na(bh_coef_path)) {
  combined_bh <- read_csv_path(bh_coef_path) %>% clean_model_block_values()
  coefficient_source_path <- bh_coef_path
} else {
  if (is.na(clustered_coef_path)) {
    stop("Could not find main_combined_model_coefficients_bh_corrected.csv or main_combined_model_coefficients_clustered.csv.")
  }
  message("BH-corrected coefficient file not found; reconstructing BH corrections from clustered combined coefficients.")
  combined_bh <- read_csv_path(clustered_coef_path) %>%
    clean_model_block_values() %>%
    filter(model_block == "combined", term != "(Intercept)") %>%
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
        BH_across_sig_0_05 ~ "Survives BH across all combined tests",
        BH_within_sig_0_05 ~ "Survives BH within outcome only",
        raw_sig_0_05 ~ "Nominal p < .05 only; exploratory",
        raw_sig_0_10 ~ "Nominal p < .10 only; weak exploratory",
        TRUE ~ "Not statistically significant"
      )
    )
  coefficient_source_path <- clustered_coef_path
}

all_se <- read_csv_path(all_se_path) %>% clean_model_block_values()

vif_path <- thesis_resolve_first_available(c(
  "vif_main_predictors.csv",
  "vif_main_predictors(3).csv"
), required = FALSE)
vif_main <- if (!is.na(vif_path)) read_csv_path(vif_path) else NULL

robustness_sample_path <- thesis_resolve_first_available(c("robustness_sample_sizes.csv"), required = FALSE)
robustness_fit_path <- thesis_resolve_first_available(c("robustness_model_fit_summary.csv"), required = FALSE)
robustness_coef_path <- thesis_resolve_first_available(c("robustness_coefficients.csv"), required = FALSE)
alternative_fit_path <- thesis_resolve_first_available(c("alternative_model_fit_summary.csv"), required = FALSE)

robustness_sample <- if (!is.na(robustness_sample_path)) read_csv_path(robustness_sample_path) %>% clean_model_block_values() else NULL
robustness_fit <- if (!is.na(robustness_fit_path)) read_csv_path(robustness_fit_path) %>% clean_model_block_values() else NULL
robustness_coef <- if (!is.na(robustness_coef_path)) read_csv_path(robustness_coef_path) %>% clean_model_block_values() else NULL
alternative_fit <- if (!is.na(alternative_fit_path)) read_csv_path(alternative_fit_path) %>% clean_model_block_values() else NULL

ml_vs_ols_path <- thesis_resolve_first_available(c("ml_vs_ols_comparison.csv"), required = FALSE)
ml_cv_path <- thesis_resolve_first_available(c("ml_cv_summary.csv"), required = FALSE)
predictive_conclusion_path <- thesis_resolve_first_available(c("thesis_predictive_conclusion_table.csv"), required = FALSE)
ml_importance_path <- thesis_resolve_first_available(c("ml_variable_importance.csv"), required = FALSE)

ml_vs_ols <- if (!is.na(ml_vs_ols_path)) read_csv_path(ml_vs_ols_path) else NULL
ml_cv <- if (!is.na(ml_cv_path)) read_csv_path(ml_cv_path) else NULL
predictive_conclusion <- if (!is.na(predictive_conclusion_path)) read_csv_path(predictive_conclusion_path) else NULL
ml_importance <- if (!is.na(ml_importance_path)) read_csv_path(ml_importance_path) else NULL

# ------------------------------------------------------------------------------
# 4. Predictor definitions from the current modeling pipeline
# These definitions keep tables and figures aligned with the same feature blocks used in the models.
# ------------------------------------------------------------------------------

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
main_combined_predictors <- c(main_controls, main_audio_predictors, main_lyric_predictors)

# ------------------------------------------------------------------------------
# 4b. Appendix Table A.4: VIF values for main predictors
# ------------------------------------------------------------------------------

# This section is deliberately self-contained and placed before the main output
# sections. It first tries to read the modelling-script VIF CSV, including browser
# suffixes such as (3). If the CSV is not present in the working/project folders,
# it computes the same diagnostic from the final analysis dataset as a fallback.

compute_vif_from_data <- function(data, vars) {
  vif_data <- data %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    tidyr::drop_na()

  purrr::map_dfr(vars, function(v) {
    others <- setdiff(vars, v)
    if (length(others) == 0 || stats::sd(vif_data[[v]], na.rm = TRUE) == 0) {
      return(tibble(term = v, vif = NA_real_))
    }
    fit <- stats::lm(stats::reformulate(others, response = v), data = vif_data)
    r2 <- summary(fit)$r.squared
    tibble(term = v, vif = 1 / (1 - r2))
  }) %>%
    mutate(
      vif_gt_5 = vif > 5,
      vif_gt_10 = vif > 10
    ) %>%
    arrange(desc(vif))
}

if (is.null(vif_main) && !is.null(analysis_data)) {
  missing_vif_predictors <- setdiff(main_combined_predictors, names(analysis_data))
  if (length(missing_vif_predictors) == 0) {
    message("vif_main_predictors.csv not found; computing VIF values from analysis_data.")
    vif_main <- compute_vif_from_data(analysis_data, main_combined_predictors)
    vif_path <- analysis_path
  }
}

if (!is.null(vif_main)) {
  if (!"vif_gt_5" %in% names(vif_main)) vif_main$vif_gt_5 <- NA
  if (!"vif_gt_10" %in% names(vif_main)) vif_main$vif_gt_10 <- NA

  appendix_table_A4 <- vif_main %>%
    mutate(
      Predictor = label_term(term),
      vif = suppressWarnings(as.numeric(vif)),
      vif_gt_5 = if_else(is.na(as.logical(vif_gt_5)), vif > 5, as.logical(vif_gt_5)),
      vif_gt_10 = if_else(is.na(as.logical(vif_gt_10)), vif > 10, as.logical(vif_gt_10)),
      Interpretation = case_when(
        is.na(vif) ~ "VIF unavailable",
        vif_gt_10 ~ "High multicollinearity concern",
        vif_gt_5 ~ "Potential multicollinearity concern",
        TRUE ~ "No conventional VIF concern"
      )
    ) %>%
    arrange(desc(vif)) %>%
    transmute(
      Predictor,
      `Variable name` = term,
      VIF = format_decimal(vif, 3),
      `VIF > 5` = if_else(vif_gt_5, "Yes", "No"),
      `VIF > 10` = if_else(vif_gt_10, "Yes", "No"),
      Interpretation
    )

  write_docx_table(
    appendix_table_A4,
    "appendix_table_A4_vif_main_predictors.docx",
    "Appendix Table A.4. Variance inflation factors for main predictors",
    note = "VIF values are read from vif_main_predictors.csv where available; otherwise they are computed from the final analysis dataset. Conventional diagnostic cutoffs of 5 and 10 are shown as flags.",
    section = "Appendix A.4",
    source_files = source_name(vif_path),
    landscape = FALSE,
    font_size = 8.4
  )
} else {
  warning("Appendix Table A.4 was not created because vif_main_predictors.csv was not found and VIF values could not be computed from analysis_data.")
}

# ------------------------------------------------------------------------------
# 5. Table 4.1 and Figures 4.1-4.2: OLS feature-block model fit
# ------------------------------------------------------------------------------

main_fit_clean <- main_fit %>%
  filter(model_block %in% block_levels, outcome %in% outcome_levels) %>%
  mutate(
    outcome = factor(outcome, levels = outcome_levels),
    model_block = factor(model_block, levels = block_levels),
    Outcome = factor(label_outcome(outcome), levels = label_outcome(outcome_levels)),
    `Model block` = factor(label_block(model_block), levels = label_block(block_levels)),
    adjusted_r_squared = coalesce_numeric_col(cur_data_all(), c("adjusted_r_squared", "adj.r.squared"))
  ) %>%
  arrange(outcome, model_block)

table_4_1 <- main_fit_clean %>%
  transmute(
    Outcome = as.character(Outcome),
    `Model block` = as.character(`Model block`),
    n = as.integer(n),
    `Adjusted R-squared` = format_decimal(adjusted_r_squared, 3),
    RMSE = format_decimal(RMSE, 3),
    MAE = format_decimal(MAE, 3),
    AIC = format_decimal(AIC, 1),
    BIC = format_decimal(BIC, 1)
  )

write_docx_table(
  table_4_1,
  "table_4_1_ols_feature_block_model_fit_comparison.docx",
  "Table 4.1. OLS feature-block model fit comparison",
  note = "Null models are omitted. Lower RMSE/MAE/AIC/BIC indicates better fit; higher adjusted R-squared indicates better fit.",
  section = "4.1",
  source_files = source_name(main_fit_path),
  landscape = TRUE
)

figure_4_1 <- ggplot(main_fit_clean, aes(x = Outcome, y = adjusted_r_squared, fill = `Model block`)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_col(position = position_dodge(width = 0.76), width = 0.66) +
  geom_text(
    aes(label = format_decimal(adjusted_r_squared, 3)),
    position = position_dodge(width = 0.76),
    vjust = ifelse(main_fit_clean$adjusted_r_squared >= 0, -0.35, 1.15),
    size = 3.1
  ) +
  scale_fill_manual(values = palette_block, drop = FALSE) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), expand = expansion(mult = c(0.08, 0.18))) +
  labs(
    title = "Adjusted R-squared by outcome and feature block",
    subtitle = "",
    x = NULL,
    y = "Adjusted R-squared",
    fill = "Model block"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))

save_png(
  figure_4_1,
  "figure_4_1_adjusted_r_squared_by_outcome_feature_block.png",
  width = 9.2,
  height = 5.6,
  section = "4.1",
  source_files = source_name(main_fit_path)
)

selection_data <- bind_rows(
  main_fit_clean %>%
    group_by(outcome, Outcome) %>%
    slice_max(adjusted_r_squared, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Outcome, Criterion = "Highest adjusted R-squared", `Selected block` = as.character(`Model block`)),
  main_fit_clean %>%
    group_by(outcome, Outcome) %>%
    slice_min(AIC, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Outcome, Criterion = "Lowest AIC", `Selected block` = as.character(`Model block`)),
  main_fit_clean %>%
    group_by(outcome, Outcome) %>%
    slice_min(BIC, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Outcome, Criterion = "Lowest BIC", `Selected block` = as.character(`Model block`))
) %>%
  mutate(
    Outcome = factor(Outcome, levels = label_outcome(outcome_levels)),
    Criterion = factor(Criterion, levels = c("Highest adjusted R-squared", "Lowest AIC", "Lowest BIC")),
    `Selected block` = factor(`Selected block`, levels = label_block(block_levels))
  )

figure_4_2 <- ggplot(selection_data, aes(x = Criterion, y = Outcome, fill = `Selected block`)) +
  geom_tile(color = "white", linewidth = 0.75) +
  geom_text(aes(label = `Selected block`), fontface = "bold", size = 3.6) +
  scale_fill_manual(values = palette_block, drop = FALSE) +
  labs(
    title = "Model-selection criteria by outcome and feature block",
    subtitle = "",
    x = NULL,
    y = NULL,
    fill = "Favored block"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))

save_png(
  figure_4_2,
  "figure_4_2_model_selection_criteria_heatmap.png",
  width = 8.8,
  height = 4.8,
  section = "4.1",
  source_files = source_name(main_fit_path)
)

# ------------------------------------------------------------------------------
# 6. Table 4.2 and Figure 4.3: Combined coefficients and BH adjustment
# ------------------------------------------------------------------------------

manual_selected_path <- thesis_resolve_first_available(c(
  "selected_combined_model_coefficients.csv",
  "table_4_2_selected_combined_model_coefficients.csv",
  "selected_coefficient_table.csv",
  "selected_combined_coefficients.csv"
), required = FALSE)

selected_theory_terms <- c(
  "entry_year_c_z", "log1p_artist_prior_top10_count_z", "featured_artist_flag",
  "danceability_z", "speechiness_z", "valence_z", "loudness_z",
  "log_num_words_z", "uniq_ratio_z", "avg_word_length_z",
  "top_5_word_share_z", "second_person_share_z", "self_reference_share_z"
)

combined_bh_clean <- combined_bh %>%
  filter(term != "(Intercept)", outcome %in% outcome_levels) %>%
  mutate(
    raw_p = coalesce_numeric_col(cur_data_all(), c("preferred_p_value", "p.value", "p_value", "p_value_clustered", "p_value_HC3")),
    preferred_se = coalesce_numeric_col(cur_data_all(), c("preferred_std_error", "std_error_clustered", "std_error_HC3", "std.error")),
    p_BH_within_outcome = coalesce_numeric_col(cur_data_all(), c("p_BH_within_outcome", "p_bh_within_outcome")),
    p_BH_across_all_combined = coalesce_numeric_col(cur_data_all(), c("p_BH_across_all_combined", "p_bh_across_all_combined", "p_BH_across_all_combined_tests")),
    raw_sig_0_05 = raw_p < 0.05,
    BH_within_sig_0_05 = p_BH_within_outcome < 0.05,
    BH_across_sig_0_05 = p_BH_across_all_combined < 0.05,
    interpretation_label = coalesce_character_col(cur_data_all(), c("interpretation_label"))
  ) %>%
  mutate(
    interpretation_label = case_when(
      is.na(interpretation_label) & BH_across_sig_0_05 ~ "Survives BH across all combined tests",
      is.na(interpretation_label) & BH_within_sig_0_05 ~ "Survives BH within outcome only",
      is.na(interpretation_label) & raw_sig_0_05 ~ "Nominal p < .05 only; exploratory",
      is.na(interpretation_label) ~ "Not statistically significant",
      TRUE ~ str_to_sentence(interpretation_label)
    )
  )

# Selection rule for a compact body table:
# 1) all coefficients that are nominally significant;
# 2) all coefficients that survive either BH correction;
# 3) theoretically important coefficients if they are among the six smallest
#    p-values within their outcome.
selected_from_bh <- combined_bh_clean %>%
  group_by(outcome) %>%
  arrange(raw_p, .by_group = TRUE) %>%
  mutate(p_rank_within_outcome = row_number()) %>%
  ungroup() %>%
  filter(
    raw_sig_0_05 |
      BH_within_sig_0_05 |
      BH_across_sig_0_05 |
      (term %in% selected_theory_terms & p_rank_within_outcome <= 6)
  ) %>%
  mutate(
    Outcome = factor(label_outcome(outcome), levels = label_outcome(outcome_levels)),
    `Predictor label` = label_term(term)
  ) %>%
  arrange(Outcome, raw_p)

if (!is.na(manual_selected_path)) {
  table_4_2_raw <- read_csv_path(manual_selected_path)
  table_4_2 <- table_4_2_raw
  table_4_2_source <- manual_selected_path
} else {
  table_4_2 <- selected_from_bh %>%
    transmute(
      Outcome = as.character(Outcome),
      `Predictor label`,
      Estimate = format_decimal(estimate, 3),
      `Preferred SE` = format_decimal(preferred_se, 3),
      `Preferred p-value` = format_p(raw_p),
      `BH p-value within outcome` = format_p(p_BH_within_outcome),
      `BH p-value across all combined tests` = format_p(p_BH_across_all_combined),
      `Interpretation label` = interpretation_label
    )
  table_4_2_source <- coefficient_source_path
}

write_docx_table(
  table_4_2,
  "table_4_2_selected_combined_model_coefficients.docx",
  "Table 4.2. Selected combined-model coefficient results",
  note = "Selected terms include nominally significant coefficients, coefficients surviving Benjamini-Hochberg correction, and a small set of theoretically important terms needed for interpretation.",
  section = "4.2",
  source_files = source_name(table_4_2_source),
  landscape = TRUE,
  font_size = 7
)

selected_pairs_for_p_plot <- selected_from_bh %>% distinct(outcome, term)

if (!is.na(manual_selected_path) && exists("table_4_2_raw") && all(c("outcome", "term") %in% names(table_4_2_raw))) {
  selected_pairs_for_p_plot <- table_4_2_raw %>%
    transmute(outcome = as.character(outcome), term = as.character(term)) %>%
    filter(!is.na(outcome), !is.na(term)) %>%
    distinct()
}

p_plot_data <- combined_bh_clean %>%
  semi_join(selected_pairs_for_p_plot, by = c("outcome", "term")) %>%
  mutate(
    test_label = paste0(label_term(term), " (", label_outcome(outcome), ")"),
    test_label = forcats::fct_reorder(test_label, raw_p, .fun = min)
  ) %>%
  select(test_label, raw_p, p_BH_within_outcome, p_BH_across_all_combined) %>%
  pivot_longer(
    cols = c(raw_p, p_BH_within_outcome, p_BH_across_all_combined),
    names_to = "p_type",
    values_to = "p_value"
  ) %>%
  mutate(
    p_type = case_when(
      p_type == "raw_p" ~ "Raw p-value",
      p_type == "p_BH_within_outcome" ~ "BH within outcome",
      p_type == "p_BH_across_all_combined" ~ "BH across all combined tests",
      TRUE ~ p_type
    ),
    p_type = factor(p_type, levels = c("Raw p-value", "BH within outcome", "BH across all combined tests")),
    p_value = pmax(as.numeric(p_value), 1e-8)
  )

figure_4_3 <- ggplot(p_plot_data, aes(x = p_value, y = test_label, color = p_type)) +
  geom_vline(xintercept = 0.05, color = "grey35", linetype = "dashed", linewidth = 0.45) +
  geom_point(size = 2.2, alpha = 0.9) +
  scale_x_log10(labels = scales::label_number(accuracy = 0.001)) +
  scale_color_manual(values = palette_p, drop = FALSE) +
  labs(
    title = "Multiple-testing adjustment for selected combined-model coefficients",
    subtitle = "Raw p-values are compared with Benjamini-Hochberg corrections within outcome and across all combined-model tests.",
    x = "p-value, log scale",
    y = NULL,
    color = NULL,
    caption = "Dashed reference line marks p = .05. Movement to the right after adjustment indicates weakened evidence."
  ) +
  theme_thesis(base_size = 10)

save_png(
  figure_4_3,
  "figure_4_3_multiple_testing_adjustment.png",
  width = 10.5,
  height = max(5.8, 0.24 * length(unique(as.character(p_plot_data$test_label))) + 2.4),
  section = "4.2",
  source_files = source_name(coefficient_source_path)
)

# ------------------------------------------------------------------------------
# 7. Appendix A.1 and A.5: HC3 coefficient figure and full SE table
# ------------------------------------------------------------------------------

all_se_combined <- all_se %>%
  filter(model_block == "combined", outcome %in% outcome_levels) %>%
  mutate(
    Outcome = factor(label_outcome(outcome), levels = label_outcome(outcome_levels)),
    Predictor = label_term(term)
  )

appendix_selected_terms <- unique(c(
  if (exists("selected_from_bh")) selected_from_bh$term else character(),
  "entry_year_c_z", "log1p_artist_prior_top10_count_z", "speechiness_z",
  "uniq_ratio_z", "top_5_word_share_z", "second_person_share_z"
))
appendix_selected_terms <- setdiff(appendix_selected_terms, "(Intercept)")

hc3_plot_data <- all_se_combined %>%
  filter(term %in% appendix_selected_terms) %>%
  group_by(outcome) %>%
  mutate(df_resid_approx = max(n, na.rm = TRUE) - n_distinct(term) - 1L) %>%
  ungroup() %>%
  mutate(
    hc3_low = estimate - stats::qt(0.975, df = pmax(df_resid_approx, 30)) * std_error_HC3,
    hc3_high = estimate + stats::qt(0.975, df = pmax(df_resid_approx, 30)) * std_error_HC3,
    nominal_HC3 = p_value_HC3 < 0.05,
    Predictor = factor(Predictor, levels = rev(unique(label_term(appendix_selected_terms))))
  )

figure_A_1 <- ggplot(hc3_plot_data, aes(x = estimate, y = Predictor)) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(aes(xmin = hc3_low, xmax = hc3_high), orientation = "y", width = 0.14, color = "grey35", linewidth = 0.55) +
  geom_point(aes(shape = nominal_HC3), size = 2.2, color = "darkblue") +
  facet_wrap(~ Outcome, scales = "free_x", ncol = 2) +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16), labels = c("No", "Yes")) +
  labs(
    title = "Selected combined-model coefficients using HC3 confidence intervals",
    subtitle = "",
    x = "Coefficient estimate",
    y = NULL,
    shape = "HC3 p < .05",
    caption = "HC3 intervals are shown to visualize coefficient uncertainty under heteroskedasticity-robust inference."
  ) +
  theme_thesis(base_size = 10)

save_png(
  figure_A_1,
  "appendix_figure_A1_selected_coefficients_HC3_CI.png",
  width = 10,
  height = max(6, 0.22 * length(unique(hc3_plot_data$Predictor)) * 2),
  section = "Appendix A.1",
  source_files = source_name(all_se_path)
)

appendix_table_A5 <- all_se_combined %>%
  arrange(Outcome, match(term, c("(Intercept)", main_combined_predictors))) %>%
  transmute(
    Outcome = as.character(Outcome),
    Predictor,
    n = as.integer(n),
    `Artist clusters` = ifelse(is.na(n_artist_clusters), "", as.character(as.integer(n_artist_clusters))),
    Estimate = format_decimal(estimate, 3),
    `Conventional SE` = format_decimal(std_error_conventional, 3),
    `Conventional p-value` = format_p(p_value_conventional),
    `HC3 SE` = format_decimal(std_error_HC3, 3),
    `HC3 p-value` = format_p(p_value_HC3),
    `Clustered SE` = format_decimal(std_error_clustered, 3),
    `Clustered p-value` = format_p(p_value_clustered),
    `Preferred SE` = format_decimal(preferred_std_error, 3),
    `Preferred p-value` = format_p(preferred_p_value),
    `Preferred inference method` = preferred_inference_method,
    `Preferred CI lower` = format_decimal(conf_low_preferred, 3),
    `Preferred CI upper` = format_decimal(conf_high_preferred, 3)
  )

write_docx_table(
  appendix_table_A5,
  "appendix_table_A5_full_combined_model_coefficients_all_se_types.docx",
  "Appendix Table A.5. Full combined-model coefficients with all standard-error types",
  note = "Complete combined-model coefficient table for all outcomes. Preferred inference uses artist-clustered standard errors where available, otherwise HC3.",
  section = "Appendix A.5",
  source_files = source_name(all_se_path),
  landscape = TRUE,
  font_size = 6.3
)

# ------------------------------------------------------------------------------
# 8. Table 4.3: Robustness-check summary
# ------------------------------------------------------------------------------

main_combined_fit <- main_fit_clean %>%
  filter(model_block == "combined") %>%
  select(outcome, main_n = n, main_adj_r2 = adjusted_r_squared, main_RMSE = RMSE)

summarise_check <- function(check_name, label, purpose, affected_outcomes = NULL, fit_data = robustness_fit) {
  if (is.null(fit_data) || !"model_block" %in% names(fit_data)) {
    return(tibble(
      `Robustness check` = label,
      Purpose = purpose,
      `Affected outcomes` = "Not available",
      `Sample-size change or n range` = "Not available",
      `Main conclusion / stability note` = "Required robustness file not available."
    ))
  }

  d <- fit_data %>% filter(model_block == check_name)
  if (!is.null(affected_outcomes)) d <- d %>% filter(outcome %in% affected_outcomes)

  if (nrow(d) == 0) {
    return(tibble(
      `Robustness check` = label,
      Purpose = purpose,
      `Affected outcomes` = "Not available",
      `Sample-size change or n range` = "Not run or file not available",
      `Main conclusion / stability note` = "No rows found for this check in the robustness outputs."
    ))
  }

  d2 <- d %>%
    left_join(main_combined_fit, by = "outcome") %>%
    mutate(
      n_change = n - main_n,
      adj_r2_change = adjusted_r_squared - main_adj_r2
    )

  outcomes_txt <- paste(label_outcome(sort(unique(d2$outcome))), collapse = "; ")
  n_range <- if (all(d2$n_change == 0, na.rm = TRUE)) {
    paste0("n unchanged (", min(d2$n, na.rm = TRUE), "-", max(d2$n, na.rm = TRUE), ")")
  } else {
    paste0(
      "n = ", min(d2$n, na.rm = TRUE), "-", max(d2$n, na.rm = TRUE),
      "; change vs main = ", min(d2$n_change, na.rm = TRUE), " to ", max(d2$n_change, na.rm = TRUE)
    )
  }

  max_fit_shift <- max(abs(d2$adj_r2_change), na.rm = TRUE)
  note <- case_when(
    all(d2$n_change == 0, na.rm = TRUE) & check_name == "exclude_spotify_fuzzy" ~
      "Sample unchanged in the final dataset; Spotify fuzzy-match exclusion does not alter the analysed sample.",
    is.finite(max_fit_shift) & max_fit_shift <= 0.015 ~
      "Model-fit conclusions are stable; adjusted R-squared changes are very small.",
    is.finite(max_fit_shift) & max_fit_shift <= 0.050 ~
      "Model-fit conclusions are broadly stable, with modest changes in fit.",
    TRUE ~
      "This check shows larger fit changes; inspect robustness coefficients/fit outputs before wording final interpretation."
  )

  tibble(
    `Robustness check` = label,
    Purpose = purpose,
    `Affected outcomes` = outcomes_txt,
    `Sample-size change or n range` = n_range,
    `Main conclusion / stability note` = note
  )
}

robustness_table_parts <- list(
  summarise_check(
    "era_controls_combined",
    "Temporal controls / era controls",
    "Replace continuous entry-year control with broader era controls."
  ),
  summarise_check(
    "year_fixed_effects_combined",
    "Year fixed effects",
    "Absorb entry-year-specific chart conditions in the combined model."
  ),
  summarise_check(
    "exclude_2021",
    "Exclude 2021 entrants",
    "Check sensitivity to the last entry cohort and updated right-censoring treatment."
  ),
  summarise_check(
    "exclude_right_censored",
    "Exclude directly right-censored songs",
    "Check sensitivity to songs with trajectories ending at the observed chart boundary."
  ),
  summarise_check(
    "exclude_long_gap",
    "Exclude long-gap songs",
    "Check whether re-entry/gap-heavy chart trajectories drive results."
  ),
  summarise_check(
    "exclude_fuzzy_or_supp_lyrics",
    "Exclude fuzzy or supplemental lyric matches",
    "Check sensitivity to lower-certainty lyric matches."
  ),
  summarise_check(
    "exclude_spotify_fuzzy",
    "Exclude Spotify fuzzy matches",
    "Check sensitivity to lower-certainty Spotify audio-feature matches."
  ),
  summarise_check(
    "alternative_artist_history_tenure",
    "Alternative artist-history controls: career tenure",
    "Replace prior Top 10 history with artist career-age control where available."
  ),
  summarise_check(
    "alternative_artist_history_hot100_count",
    "Alternative artist-history controls: Hot 100 count",
    "Replace prior Top 10 history with prior Hot 100 count where available."
  ),
  summarise_check(
    "alternative_time_to_peak_outcome_log1p_weeks_to_peak_0",
    "Alternative outcome definition: time to peak",
    "Use log(1 + weeks to peak) with peak-on-entry songs retained at zero."
  ),
  summarise_check(
    "alternative_first_run_weeks_on_chart",
    "Alternative outcome definition: first-run weeks",
    "Use first continuous chart run rather than total observed weeks."
  ),
  summarise_check(
    "alternative_first_run_decay_rate",
    "Alternative outcome definition: first-run decay",
    "Use first-run decay rate to reduce sensitivity to re-entry gaps."
  )
)

alternative_note <- if (!is.null(alternative_fit) && nrow(alternative_fit) > 0) {
  alt_models <- alternative_fit %>%
    mutate(model_name = if_else(!is.na(alternative_model), alternative_model, model_block)) %>%
    filter(!is.na(model_name)) %>%
    pull(model_name) %>%
    unique()
  paste0("Alternative model families run: ", paste(str_replace_all(alt_models, "_", " "), collapse = "; "), ". See Appendix Table A.6.")
} else {
  "Alternative model-fit summary file not available."
}

robustness_table_parts[[length(robustness_table_parts) + 1]] <- tibble(
  `Robustness check` = "Alternative model families",
  Purpose = "Check distributional and estimator sensitivity beyond main OLS.",
  `Affected outcomes` = if (!is.null(alternative_fit)) paste(label_outcome(unique(alternative_fit$outcome)), collapse = "; ") else "Not available",
  `Sample-size change or n range` = if (!is.null(alternative_fit)) paste0("n = ", min(alternative_fit$n, na.rm = TRUE), "-", max(alternative_fit$n, na.rm = TRUE)) else "Not available",
  `Main conclusion / stability note` = alternative_note
)

table_4_3 <- bind_rows(robustness_table_parts)

write_docx_table(
  table_4_3,
  "table_4_3_robustness_check_summary.docx",
  "Table 4.3. Robustness-check summary",
  note = "This body table summarizes robustness purposes and stability at a high level; it intentionally avoids a full coefficient dump.",
  section = "4.3",
  source_files = c(source_name(robustness_sample_path), source_name(robustness_fit_path), source_name(alternative_fit_path), source_name(robustness_coef_path)),
  landscape = TRUE,
  font_size = 7
)

# ------------------------------------------------------------------------------
# 9. Appendix Figure A.2: Cook's distance
# ------------------------------------------------------------------------------

cook_path <- thesis_resolve_first_available(c(
  "top_influential_songs_cooks_distance.csv",
  "cooks_distance_top_observations.csv",
  "main_ols_cooks_distance.csv",
  "combined_model_cooks_distance.csv"
), required = FALSE)

if (!is.na(cook_path)) {
  cooks_top <- read_csv_path(cook_path)
  cooks_source <- cook_path

  cooks_top <- cooks_top %>%
    mutate(
      outcome = coalesce_character_col(cur_data_all(), c("outcome")),
      song = coalesce_character_col(cur_data_all(), c("song", "title", "track")),
      artist = coalesce_character_col(cur_data_all(), c("artist", "artist_name")),
      cooks_distance = coalesce_numeric_col(cur_data_all(), c("cooks_distance", "cook_distance", "cooks_d", "cooks.distance"))
    ) %>%
    filter(outcome %in% outcome_levels, !is.na(cooks_distance)) %>%
    group_by(outcome) %>%
    arrange(desc(cooks_distance), .by_group = TRUE) %>%
    slice_head(n = 5) %>%
    ungroup()
} else {
  if (is.null(analysis_data)) {
    stop("No Cook's distance file was found and final_sample_cleaned.csv is unavailable for refitting.")
  }

  missing_predictors <- setdiff(c(main_combined_predictors, main_outcomes, "song", "artist"), names(analysis_data))
  if (length(missing_predictors) > 0) {
    stop("Cannot compute Cook's distance because analysis data are missing: ", paste(missing_predictors, collapse = ", "))
  }

  make_formula <- function(outcome, predictors) stats::reformulate(termlabels = predictors, response = outcome)

  cooks_top <- purrr::map_dfr(main_outcomes, function(outcome_name) {
    model_input <- analysis_data %>%
      select(any_of(c("weekly_song_id", "song", "artist", outcome_name, main_combined_predictors))) %>%
      tidyr::drop_na(all_of(c(outcome_name, main_combined_predictors)))

    fit <- stats::lm(make_formula(outcome_name, main_combined_predictors), data = model_input)
    model_input %>%
      mutate(cooks_distance = as.numeric(stats::cooks.distance(fit)), outcome = outcome_name) %>%
      arrange(desc(cooks_distance)) %>%
      slice_head(n = 5) %>%
      select(outcome, song, artist, cooks_distance)
  })
  cooks_source <- analysis_path
}

cooks_plot_data <- cooks_top %>%
  mutate(
    Outcome = factor(label_outcome(outcome), levels = label_outcome(outcome_levels)),
    observation_label = if_else(!is.na(artist) & nzchar(artist), paste0(song, " - ", artist), song),
    observation_label = str_trunc(observation_label, 58),
    observation_ordered = reorder_within(observation_label, cooks_distance, Outcome)
  )

figure_A_2 <- ggplot(cooks_plot_data, aes(x = cooks_distance, y = observation_ordered)) +
  geom_col(fill = "#6F6F6F", width = 0.68) +
  geom_text(aes(label = format_decimal(cooks_distance, 3)), hjust = -0.08, size = 3.0) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 2) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "Most influential observations by Cook's distance",
    subtitle = "Top five observations per combined OLS outcome model.",
    x = "Cook's distance",
    y = NULL,
    caption = ""
  ) +
  theme_thesis(base_size = 10)

save_png(
  figure_A_2,
  "appendix_figure_A2_cooks_distance_top_observations.png",
  width = 11,
  height = 7.2,
  section = "Appendix A.2",
  source_files = source_name(cooks_source)
)

# ------------------------------------------------------------------------------
# 10. Appendix Table A.6: Alternative model fit summary
# ------------------------------------------------------------------------------

if (!is.null(alternative_fit)) {
  appendix_table_A6 <- alternative_fit %>%
    mutate(
      model_name = if_else(!is.na(alternative_model), alternative_model, model_block),
      model_name = str_replace_all(model_name, "_", " "),
      model_name = str_squish(str_to_sentence(model_name))
    ) %>%
    arrange(match(outcome, outcome_levels), model_name) %>%
    transmute(
      Outcome = label_outcome(outcome),
      `Alternative model/check` = model_name,
      n = as.integer(n),
      RMSE = format_decimal(RMSE, 3),
      MAE = format_decimal(MAE, 3),
      AIC = format_decimal(AIC, 1),
      BIC = format_decimal(BIC, 1),
      `Adjusted R-squared` = format_decimal(adjusted_r_squared, 3),
      Notes = notes
    )

  write_docx_table(
    appendix_table_A6,
    "appendix_table_A6_alternative_model_fit_summary.docx",
    "Appendix Table A.6. Alternative model fit summary",
    note = "Includes available Poisson/negative binomial checks, transformed OLS checks, and robust-regression checks.",
    section = "Appendix A.6",
    source_files = source_name(alternative_fit_path),
    landscape = TRUE,
    font_size = 7.2
  )
} else {
  warning("alternative_model_fit_summary.csv not found; Appendix Table A.6 was not created.")
}

# ------------------------------------------------------------------------------
# 11. Table 4.4 and Figure 4.4: ML prediction extension
# ------------------------------------------------------------------------------

if (!is.null(ml_vs_ols)) {
  ml_table_source <- ml_vs_ols_path
  ml_perf_source <- ml_vs_ols
} else if (!is.null(ml_cv)) {
  ml_table_source <- ml_cv_path
  ml_perf_source <- ml_cv %>%
    group_by(outcome) %>%
    mutate(
      null_RMSE = RMSE[model == "null_mean_baseline"][1],
      RMSE_improvement_vs_null = ifelse(!is.na(null_RMSE), null_RMSE - RMSE, NA_real_),
      RMSE_improvement_pct_vs_null = ifelse(null_RMSE > 0, RMSE_improvement_vs_null / null_RMSE, NA_real_),
      best_model_for_outcome = RMSE == min(RMSE, na.rm = TRUE)
    ) %>%
    ungroup()
} else if (!is.null(predictive_conclusion)) {
  ml_table_source <- predictive_conclusion_path
  ml_perf_source <- predictive_conclusion %>%
    transmute(
      outcome,
      model = best_ML_model,
      RMSE = best_ML_RMSE,
      MAE = NA_real_,
      out_of_sample_R2_vs_fold_mean_null = out_of_sample_R2,
      RMSE_improvement_pct_vs_null = RMSE_improvement_percentage,
      RMSE_improvement_vs_null = null_RMSE - best_ML_RMSE,
      best_model_for_outcome = TRUE
    )
} else {
  stop("No ML summary file found. Expected ml_vs_ols_comparison.csv, ml_cv_summary.csv, or thesis_predictive_conclusion_table.csv.")
}

ml_perf_clean <- ml_perf_source %>%
  mutate(
    Outcome = factor(label_outcome(outcome), levels = label_outcome(outcome_levels)),
    Model = label_model(model),
    beats_null = if_else(!is.na(RMSE_improvement_vs_null), RMSE_improvement_vs_null > 0, out_of_sample_R2_vs_fold_mean_null > 0),
    best_model_for_outcome = if_else(is.na(best_model_for_outcome), FALSE, as.logical(best_model_for_outcome))
  ) %>%
  arrange(Outcome, desc(best_model_for_outcome), RMSE)

table_4_4 <- ml_perf_clean %>%
  transmute(
    Outcome = as.character(Outcome),
    Model,
    RMSE = format_decimal(RMSE, 3),
    MAE = format_decimal(MAE, 3),
    `Out-of-sample R-squared vs null` = format_decimal(out_of_sample_R2_vs_fold_mean_null, 3),
    `Improvement over null baseline` = format_percent(RMSE_improvement_pct_vs_null, 1),
    `Beats null` = if_else(beats_null, "Yes", "No"),
    `Best model for outcome` = if_else(best_model_for_outcome, "Yes", "No")
  )

write_docx_table(
  table_4_4,
  "table_4_4_cross_validated_predictive_performance.docx",
  "Table 4.4. Cross-validated predictive performance",
  note = "",
  section = "4.4",
  source_files = c(source_name(ml_table_source), source_name(ml_cv_path), source_name(predictive_conclusion_path)),
  landscape = TRUE,
  font_size = 7.6
)

best_non_null <- ml_perf_clean %>%
  filter(model != "null_mean_baseline") %>%
  group_by(outcome, Outcome) %>%
  arrange(RMSE, desc(out_of_sample_R2_vs_fold_mean_null), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    improvement_pct = 100 * RMSE_improvement_pct_vs_null,
    beats_null = improvement_pct > 0
  )

figure_4_4 <- ggplot(best_non_null, aes(x = Outcome, y = improvement_pct, fill = as.character(beats_null))) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_col(width = 0.62) +
  geom_text(
    aes(label = paste0(format_decimal(improvement_pct, 1), "%\n", Model)),
    vjust = ifelse(best_non_null$improvement_pct >= 0, -0.25, 1.15),
    size = 3.2,
    lineheight = 0.92
  ) +
  scale_fill_manual(values = palette_yes_no, labels = c("FALSE" = "No", "TRUE" = "Yes"), drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.10, 0.22))) +
  labs(
    title = "Predictive gains over the null baseline",
    subtitle = "",
    x = NULL,
    y = "RMSE improvement over null baseline",
    fill = "Beats null"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))

save_png(
  figure_4_4,
  "figure_4_4_predictive_gains_over_null_baseline.png",
  width = 8.8,
  height = 5.2,
  section = "4.4",
  source_files = c(source_name(ml_table_source), source_name(predictive_conclusion_path))
)

# ------------------------------------------------------------------------------
# 12. Appendix Figure A.3: Random forest variable importance
# ------------------------------------------------------------------------------

if (!is.null(ml_importance)) {
  importance_plot_data <- ml_importance %>%
    filter(model == "random_forest" | is.na(model)) %>%
    mutate(
      Outcome = factor(label_outcome(outcome), levels = label_outcome(outcome_levels)),
      Predictor = label_term(term)
    ) %>%
    group_by(outcome, Outcome) %>%
    arrange(desc(importance), .by_group = TRUE) %>%
    slice_head(n = 8) %>%
    ungroup() %>%
    mutate(Predictor_ordered = reorder_within(Predictor, importance, Outcome))

  figure_A_3 <- ggplot(importance_plot_data, aes(x = importance, y = Predictor_ordered)) +
    geom_col(fill = "grey30", width = 0.68) +
    facet_wrap(~ Outcome, scales = "free", ncol = 2) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Random forest variable importance",
      subtitle = "",
      x = "Permutation importance",
      y = NULL,
      caption = "Variable importance does not estimate causal effects and can reflect correlation structure among predictors."
    ) +
    theme_thesis(base_size = 10)

  save_png(
    figure_A_3,
    "appendix_figure_A3_random_forest_variable_importance.png",
    width = 10.5,
    height = 7.2,
    section = "Appendix A.3",
    source_files = source_name(ml_importance_path)
  )
} else {
  warning("ml_variable_importance.csv not found; Appendix Figure A.3 was not created.")
}

# ------------------------------------------------------------------------------
# 13. Manifest
# ------------------------------------------------------------------------------

manifest_path <- file.path(table_dir, "generated_outputs_manifest.csv")
readr::write_csv(created_outputs, manifest_path)
record_output("Manifest", "manifest_csv", manifest_path, character(), "Generated-output manifest.")
readr::write_csv(created_outputs, manifest_path)

message("Done. Generated ", nrow(created_outputs), " recorded outputs.")
message("Figures: ", normalizePath(figure_dir, mustWork = FALSE))
message("Tables: ", normalizePath(table_dir, mustWork = FALSE))
message("Manifest: ", normalizePath(manifest_path, mustWork = FALSE))
