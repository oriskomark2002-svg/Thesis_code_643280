
# 01_clean_final_sample.R
#
#
# Purpose:
#   Build the song-level final sample used in the thesis models.
# 0. Packages
# ============================================================

options(stringsAsFactors = FALSE)

# Set to TRUE only if you want the script to install missing packages.
install_missing_packages <- FALSE

required_packages <- c(
  "dplyr", "tidyr", "stringr", "lubridate", "stringdist",
  "readr", "purrr", "tibble", "tidytext"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  if (isTRUE(install_missing_packages)) {
    install.packages(missing_packages)
  } else {
    stop(
      paste(
        "Missing required packages:",
        paste(missing_packages, collapse = ", "),
        "\nInstall them or set install_missing_packages <- TRUE."
      )
    )
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(stringdist)
  library(readr)
  library(purrr)
  library(tibble)
  library(tidytext)
})

output_dir <- "."
save_outputs <- TRUE
diagnostics_dir <- file.path(output_dir, "outputs", "diagnostics")

if (isTRUE(save_outputs)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
}

# supplemental matches.
# Spotify supplemental matches are not automatically accepted in the working pipeline.
# Lyric supplemental Top 10 matches are later accepted after review in the messy script.
accept_spotify_supplemental_top10 <- FALSE
accept_lyrics_supplemental_top10 <- TRUE

# Conservative fuzzy-match thresholds used for both Spotify and lyrics.
fuzzy_song_distance_cutoff <- 0.08
fuzzy_artist_distance_cutoff <- 0.12
fuzzy_year_distance_cutoff <- 3
fuzzy_margin_cutoff <- 0.02

# 1. Import raw datasets

data_dir <- "master_thesis_data"

Clean_Billboard <- readr::read_csv(
  file.path(data_dir, "Clean_Billboard.csv"),
  show_col_types = FALSE
)

SpotifyFeatures_All_1990_to_2021 <- readr::read_csv(
  file.path(data_dir, "SpotifyFeatures_All_1990_to_2021.csv"),
  show_col_types = FALSE
)

billboard_lyrics_spotify <- readr::read_csv(
  file.path(data_dir, "billboard-lyrics-spotify.csv"),
  show_col_types = FALSE
)

spotifyfeatures_all_1990_to_2021 <- SpotifyFeatures_All_1990_to_2021

raw_data_diagnostics <- tibble::tibble(
  dataset = c(
    "Clean_Billboard",
    "SpotifyFeatures_All_1990_to_2021",
    "billboard_lyrics_spotify"
  ),
  n_rows = c(
    nrow(Clean_Billboard),
    nrow(spotifyfeatures_all_1990_to_2021),
    nrow(billboard_lyrics_spotify)
  ),
  n_cols = c(
    ncol(Clean_Billboard),
    ncol(spotifyfeatures_all_1990_to_2021),
    ncol(billboard_lyrics_spotify)
  )
)

print(raw_data_diagnostics)

# 2. Helper functions
# These helpers keep title and artist normalization consistent across all three datasets.

clean_basic <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("[\u2018\u2019`]", "'") %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("\\+", " and ") %>%
    str_replace_all("[[:punct:]]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

clean_song_strict <- function(x) {
  clean_basic(x)
}

clean_song_relaxed <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("[\u2018\u2019`]", "'") %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("\\+", " and ") %>%
    str_remove_all("\\([^\\)]*\\)") %>%
    str_remove_all("\\[[^\\]]*\\]") %>%
    str_replace_all("\\bremaster(ed)?\\b", " ") %>%
    str_replace_all("\\bmono\\b|\\bstereo\\b", " ") %>%
    str_replace_all("\\bradio edit\\b|\\bedit\\b|\\bversion\\b", " ") %>%
    str_replace_all("\\bremix\\b|\\blive\\b", " ") %>%
    str_replace_all("[[:punct:]]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

clean_artist_full <- function(x) {
  clean_basic(x)
}

clean_artist_primary <- function(x) {
  x %>%
    clean_basic() %>%
    str_replace_all("\\bfeaturing\\b|\\bfeat\\b|\\bft\\b", " featuring ") %>%
    str_replace_all("\\bduet with\\b", " with ") %>%
    str_replace_all("\\bwith\\b", " with ") %>%
    str_split(" featuring | with ", simplify = TRUE) %>%
    .[, 1] %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

clean_artist_core <- function(x) {
  x %>%
    clean_artist_primary() %>%
    str_replace("^the\\s+", "") %>%
    str_replace_all("\\band his orchestra\\b.*$", "") %>%
    str_replace_all("\\band her orchestra\\b.*$", "") %>%
    str_replace_all("\\band their orchestra\\b.*$", "") %>%
    str_replace_all("\\bhis orchestra\\b.*$", "") %>%
    str_replace_all("\\bher orchestra\\b.*$", "") %>%
    str_replace_all("\\borchestra\\b.*$", "") %>%
    str_replace_all("\\sand the [a-z0-9 ]+$", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

first_non_missing <- function(x) {
  if (is.character(x)) {
    y <- x[!is.na(x) & str_trim(x) != ""]
  } else {
    y <- x[!is.na(x)]
  }

  if (length(y) == 0) {
    return(x[NA_integer_][1])
  }

  y[1]
}

safe_min <- function(x) {
  if (all(is.na(x))) x[NA_integer_][1] else min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) x[NA_integer_][1] else max(x, na.rm = TRUE)
}

z_score <- function(x) {
  x <- as.numeric(x)
  x_sd <- sd(x, na.rm = TRUE)

  if (is.na(x_sd) || x_sd == 0) {
    return(rep(NA_real_, length(x)))
  }

  (x - mean(x, na.rm = TRUE)) / x_sd
}


# Conservative collaboration detection for artist strings.
# The regex prioritizes explicit collaboration markers and common separators.
# It remains an approximate audit/control variable and should not be interpreted as a perfect artist-credit parser.
artist_collaboration_regex_current <- "\\b(featuring|feat\\.?|ft\\.?|with|duet with)\\b|\\s+(&|x|×|\\+)\\s+|,"

artist_separator_regex_current <- "\\bfeaturing\\b|\\bfeat\\.?\\b|\\bft\\.?\\b|\\bwith\\b|\\bduet with\\b|\\s+&\\s+|\\s+x\\s+|\\s+×\\s+|\\s+\\+\\s+|,"

detect_featured_artist_flag_current <- function(x) {
  x_clean <- str_to_lower(as.character(x))
  as.integer(str_detect(x_clean, artist_collaboration_regex_current))
}

count_credit_artists_current <- function(x) {
  x_clean <- str_squish(str_to_lower(as.character(x)))
  x_clean[is.na(x_clean) | x_clean == ""] <- NA_character_

  counts <- str_count(x_clean, artist_separator_regex_current) + 1L
  counts[is.na(x_clean)] <- NA_integer_
  pmax(counts, 1L, na.rm = TRUE)
}

safe_write_csv_current <- function(data, path) {
  if (isTRUE(save_outputs)) {
    readr::write_csv(data, path)
  }
  invisible(data)
}

stop_if_duplicates <- function(data, id_col, label) {
  dupes <- data %>%
    count(.data[[id_col]]) %>%
    filter(!is.na(.data[[id_col]]), n > 1)

  if (nrow(dupes) > 0) {
    print(dupes)
    stop(paste("Duplicate", label, "found in", id_col))
  }

  invisible(TRUE)
}

extract_years_from_weeks_string <- function(x) {
  dates <- str_extract_all(as.character(x), "\\d{4}-\\d{2}-\\d{2}")

  purrr::map_dfr(dates, function(date_values) {
    if (length(date_values) == 0) {
      return(tibble(min_year = NA_integer_, max_year = NA_integer_))
    }

    parsed_dates <- as.Date(date_values)

    tibble(
      min_year = year(min(parsed_dates, na.rm = TRUE)),
      max_year = year(max(parsed_dates, na.rm = TRUE))
    )
  })
}

custom_vif_legacy <- function(data, vars) {
  # VIF is intentionally deferred to the later modeling script.
  # This feature-engineering script should not call modeling functions such as lm().
  tibble(
    variable = vars,
    vif = NA_real_,
    note = "VIF deferred to modeling script; no lm() call made."
  )
}

find_high_correlations_legacy <- function(data, vars, cutoff = 0.70) {
  cor_data <- data %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    drop_na()

  if (nrow(cor_data) == 0) {
    return(tibble(var1 = character(), var2 = character(), correlation = numeric()))
  }

  cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

  as.data.frame(as.table(cor_matrix)) %>%
    as_tibble() %>%
    rename(var1 = Var1, var2 = Var2, correlation = Freq) %>%
    filter(var1 != var2) %>%
    mutate(
      pair = map2_chr(
        as.character(var1),
        as.character(var2),
        ~ paste(sort(c(.x, .y)), collapse = " --- ")
      )
    ) %>%
    distinct(pair, .keep_all = TRUE) %>%
    filter(abs(correlation) >= cutoff) %>%
    arrange(desc(abs(correlation))) %>%
    select(var1, var2, correlation)
}

make_model_data_legacy <- function(data, outcome) {
  data %>% filter(!is.na(.data[[outcome]]))
}

# 3. Billboard weekly preparation
# This block turns weekly chart rows into stable song identifiers for song-level trajectory analysis.

bb_weekly_legacy <- Clean_Billboard %>%
  rename_with(~ str_replace_all(.x, "-", "_")) %>%
  rename(
    weekly_date = date,
    weekly_rank = rank,
    weekly_song = song,
    weekly_artist = artist,
    weekly_last_week = last_week,
    weekly_peak_rank_reported = peak_rank,
    weekly_weeks_on_board_reported = weeks_on_board
  ) %>%
  mutate(
    weekly_row_id = row_number(),
    weekly_date = as.Date(weekly_date),
    weekly_year = year(weekly_date),
    weekly_rank = as.integer(weekly_rank),

    song_key_strict = clean_song_strict(weekly_song),
    song_key_relaxed = clean_song_relaxed(weekly_song),

    artist_full_key = clean_artist_full(weekly_artist),
    artist_primary_key = clean_artist_primary(weekly_artist),
    artist_core_key = clean_artist_core(weekly_artist)
  )

bb_weekly_diagnostics_legacy <- bb_weekly_legacy %>%
  summarise(
    weekly_rows = n(),
    unique_weekly_songs_by_clean_key = n_distinct(song_key_relaxed, artist_primary_key),
    min_date = min(weekly_date, na.rm = TRUE),
    max_date = max(weekly_date, na.rm = TRUE),
    min_year = min(weekly_year, na.rm = TRUE),
    max_year = max(weekly_year, na.rm = TRUE),
    missing_song = sum(is.na(weekly_song) | str_trim(weekly_song) == ""),
    missing_artist = sum(is.na(weekly_artist) | str_trim(weekly_artist) == ""),
    missing_rank = sum(is.na(weekly_rank))
  )

print(bb_weekly_diagnostics_legacy)

weekly_song_universe_legacy <- bb_weekly_legacy %>%
  group_by(song_key_relaxed, artist_primary_key) %>%
  arrange(weekly_date, .by_group = TRUE) %>%
  summarise(
    weekly_song_id = cur_group_id(),

    weekly_song_original = first(weekly_song),
    weekly_artist_original = first(weekly_artist),

    weekly_first_date = min(weekly_date, na.rm = TRUE),
    weekly_last_date = max(weekly_date, na.rm = TRUE),
    weekly_entry_year = year(weekly_first_date),
    weekly_last_year = year(weekly_last_date),

    weekly_peak_rank = min(weekly_rank, na.rm = TRUE),
    weekly_weeks_observed = n(),

    song_key_strict = first(song_key_strict),
    song_key_relaxed = first(song_key_relaxed),

    artist_full_key = first(artist_full_key),
    artist_primary_key = first(artist_primary_key),
    artist_core_key = first(artist_core_key),

    .groups = "drop"
  )

bb_weekly_legacy <- bb_weekly_legacy %>%
  left_join(
    weekly_song_universe_legacy %>%
      select(weekly_song_id, song_key_relaxed, artist_primary_key),
    by = c("song_key_relaxed", "artist_primary_key")
  )

stopifnot(nrow(bb_weekly_legacy) == nrow(Clean_Billboard))
stopifnot(!any(is.na(bb_weekly_legacy$weekly_song_id)))

weekly_song_universe_diagnostics_legacy <- weekly_song_universe_legacy %>%
  summarise(
    n_weekly_songs = n(),
    min_entry_year = min(weekly_entry_year, na.rm = TRUE),
    max_entry_year = max(weekly_entry_year, na.rm = TRUE),
    n_top10_songs = sum(weekly_peak_rank <= 10, na.rm = TRUE),
    n_top40_songs = sum(weekly_peak_rank <= 40, na.rm = TRUE),
    mean_weeks_observed = mean(weekly_weeks_observed, na.rm = TRUE),
    median_weeks_observed = median(weekly_weeks_observed, na.rm = TRUE)
  )

print(weekly_song_universe_diagnostics_legacy)


# 3b. Artist identity and internal Billboard-history preparation
# Artist-history controls are built only from earlier Billboard appearances, so same-date releases do not count as prior success.
# Same-date songs by the same artist are not counted as prior history for each other.

artist_history_song_universe_current <- weekly_song_universe_legacy %>%
  transmute(
    weekly_song_id,
    main_artist_key = artist_primary_key,
    artist_entry_date = weekly_first_date,
    artist_entry_year = weekly_entry_year,
    artist_peak_rank = weekly_peak_rank,
    artist_weeks_observed = weekly_weeks_observed
  )

artist_history_by_artist_date_current <- artist_history_song_universe_current %>%
  group_by(main_artist_key, artist_entry_date) %>%
  summarise(
    n_songs_this_date = n_distinct(weekly_song_id),
    n_top10_this_date = sum(artist_peak_rank <= 10, na.rm = TRUE),
    weeks_this_date = sum(artist_weeks_observed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(main_artist_key, artist_entry_date) %>%
  group_by(main_artist_key) %>%
  mutate(
    artist_prior_hot100_count = lag(cumsum(n_songs_this_date), default = 0L),
    artist_prior_top10_count = lag(cumsum(n_top10_this_date), default = 0L),
    artist_prior_weeks_on_chart = lag(cumsum(weeks_this_date), default = 0L),
    artist_first_hot100_year = year(first(artist_entry_date))
  ) %>%
  ungroup()

artist_history_lookup_current <- artist_history_song_universe_current %>%
  left_join(
    artist_history_by_artist_date_current %>%
      select(
        main_artist_key,
        artist_entry_date,
        artist_prior_hot100_count,
        artist_prior_top10_count,
        artist_prior_weeks_on_chart,
        artist_first_hot100_year
      ),
    by = c("main_artist_key", "artist_entry_date")
  ) %>%
  mutate(
    log1p_artist_prior_top10_count = log1p(artist_prior_top10_count),
    artist_career_age_years = artist_entry_year - artist_first_hot100_year
  ) %>%
  select(
    weekly_song_id,
    artist_prior_hot100_count,
    artist_prior_top10_count,
    log1p_artist_prior_top10_count,
    artist_prior_weeks_on_chart,
    artist_first_hot100_year,
    artist_career_age_years
  )

artist_history_lookup_diagnostics_current <- artist_history_lookup_current %>%
  summarise(
    n_rows = n(),
    missing_prior_hot100 = sum(is.na(artist_prior_hot100_count)),
    missing_prior_top10 = sum(is.na(artist_prior_top10_count)),
    min_prior_top10 = min(artist_prior_top10_count, na.rm = TRUE),
    max_prior_top10 = max(artist_prior_top10_count, na.rm = TRUE)
  )

print(artist_history_lookup_diagnostics_current)


# 4. Spotify feature preparation and deduplication
# Spotify rows are deduplicated before matching so that each cleaned song-artist pair contributes one audio-feature record.

spotify_audio_cols <- c(
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo"
)

spotify_feature_metadata_cols <- c(
  "spotify_id",
  "spotify_peak_rank_source",
  "spotify_total_weeks_source",
  "spotify_min_year",
  "spotify_max_year"
)

spotify_years_legacy <- extract_years_from_weeks_string(
  spotifyfeatures_all_1990_to_2021$weeks
)

spotify_features_legacy <- spotifyfeatures_all_1990_to_2021 %>%
  rename_with(~ ifelse(.x == "Unnamed: 0", "source_row_number", .x)) %>%
  bind_cols(spotify_years_legacy) %>%
  rename(
    spotify_song_original = song,
    spotify_artist_original = artist,
    spotify_peak_rank_source = peak_rank,
    spotify_total_weeks_source = total_weeks,
    spotify_weeks_source = weeks,
    spotify_min_year = min_year,
    spotify_max_year = max_year
  ) %>%
  mutate(
    spotify_feature_raw_id = row_number(),
    spotify_peak_rank_source = as.integer(spotify_peak_rank_source),
    spotify_total_weeks_source = as.integer(spotify_total_weeks_source),

    song_key_strict = clean_song_strict(spotify_song_original),
    song_key_relaxed = clean_song_relaxed(spotify_song_original),

    artist_full_key = clean_artist_full(spotify_artist_original),
    artist_primary_key = clean_artist_primary(spotify_artist_original),
    artist_core_key = clean_artist_core(spotify_artist_original)
  )

spotify_features_diagnostics_legacy <- spotify_features_legacy %>%
  summarise(
    raw_spotify_rows = n(),
    unique_spotify_songs_by_clean_key = n_distinct(song_key_relaxed, artist_primary_key),
    min_spotify_year = min(spotify_min_year, na.rm = TRUE),
    max_spotify_year = max(spotify_max_year, na.rm = TRUE),
    missing_song = sum(is.na(spotify_song_original) | str_trim(spotify_song_original) == ""),
    missing_artist = sum(is.na(spotify_artist_original) | str_trim(spotify_artist_original) == ""),
    across(all_of(spotify_audio_cols), ~ sum(is.na(.x)), .names = "missing_{.col}")
  )

print(spotify_features_diagnostics_legacy)

spotify_features_unique_legacy <- spotify_features_legacy %>%
  group_by(song_key_relaxed, artist_primary_key) %>%
  arrange(
    spotify_peak_rank_source,
    desc(spotify_total_weeks_source),
    spotify_feature_raw_id,
    .by_group = TRUE
  ) %>%
  summarise(
    spotify_feature_id = first(spotify_feature_raw_id),

    spotify_song_original = first(spotify_song_original),
    spotify_artist_original = first(spotify_artist_original),

    song_key_strict = first(song_key_strict),
    song_key_relaxed = first(song_key_relaxed),

    artist_full_key = first(artist_full_key),
    artist_primary_key = first(artist_primary_key),
    artist_core_key = first(artist_core_key),

    spotify_source_rows_collapsed = n(),

    across(all_of(c(spotify_feature_metadata_cols, spotify_audio_cols)), first_non_missing),

    .groups = "drop"
  )

spotify_dedup_diagnostics_legacy <- spotify_features_unique_legacy %>%
  summarise(
    unique_spotify_feature_songs = n(),
    duplicated_source_groups = sum(spotify_source_rows_collapsed > 1),
    max_rows_collapsed_into_one_song = max(spotify_source_rows_collapsed, na.rm = TRUE)
  )

print(spotify_dedup_diagnostics_legacy)

spotify_duplicate_examples_legacy <- spotify_features_unique_legacy %>%
  filter(spotify_source_rows_collapsed > 1) %>%
  arrange(desc(spotify_source_rows_collapsed)) %>%
  select(
    spotify_song_original,
    spotify_artist_original,
    spotify_source_rows_collapsed,
    spotify_peak_rank_source,
    spotify_total_weeks_source
  ) %>%
  slice_head(n = 30)

print(spotify_duplicate_examples_legacy)


# 5. Spotify-to-Billboard matching

make_spotify_crosswalk <- function(data, tier_name, score) {
  data %>%
    transmute(
      spotify_feature_id,
      weekly_song_id,
      spotify_match_tier = tier_name,
      spotify_match_score = score,
      spotify_song_distance = 0,
      spotify_artist_distance = 0,
      spotify_combined_distance = 0,

      spotify_song_original,
      spotify_artist_original,
      weekly_song_original,
      weekly_artist_original,
      weekly_peak_rank,
      weekly_weeks_observed
    )
}

spotify_tier1_legacy <- spotify_features_unique_legacy %>%
  inner_join(
    weekly_song_universe_legacy,
    by = c("song_key_strict" = "song_key_strict", "artist_full_key" = "artist_full_key"),
    suffix = c("_spotify", "_weekly")
  ) %>%
  make_spotify_crosswalk("1_exact_strict_song_full_artist", 1)

spotify_tier2_legacy <- spotify_features_unique_legacy %>%
  inner_join(
    weekly_song_universe_legacy,
    by = c("song_key_strict" = "song_key_strict", "artist_primary_key" = "artist_primary_key"),
    suffix = c("_spotify", "_weekly")
  ) %>%
  make_spotify_crosswalk("2_exact_strict_song_primary_artist", 2)

spotify_tier3_legacy <- spotify_features_unique_legacy %>%
  inner_join(
    weekly_song_universe_legacy,
    by = c("song_key_relaxed" = "song_key_relaxed", "artist_primary_key" = "artist_primary_key"),
    suffix = c("_spotify", "_weekly")
  ) %>%
  make_spotify_crosswalk("3_exact_relaxed_song_primary_artist", 3)

spotify_tier4_legacy <- spotify_features_unique_legacy %>%
  inner_join(
    weekly_song_universe_legacy,
    by = c("song_key_relaxed" = "song_key_relaxed", "artist_core_key" = "artist_core_key"),
    suffix = c("_spotify", "_weekly")
  ) %>%
  make_spotify_crosswalk("4_exact_relaxed_song_core_artist", 4)

spotify_exact_crosswalk_legacy <- bind_rows(
  spotify_tier1_legacy,
  spotify_tier2_legacy,
  spotify_tier3_legacy,
  spotify_tier4_legacy
) %>%
  arrange(
    weekly_song_id,
    spotify_match_score,
    spotify_combined_distance,
    weekly_peak_rank,
    desc(weekly_weeks_observed)
  ) %>%
  group_by(weekly_song_id) %>%
  slice(1) %>%
  ungroup()

spotify_exact_match_summary_legacy <- spotify_exact_crosswalk_legacy %>%
  count(spotify_match_tier, sort = TRUE)

print(spotify_exact_match_summary_legacy)

unmatched_spotify_features_legacy <- spotify_features_unique_legacy %>%
  filter(!spotify_feature_id %in% spotify_exact_crosswalk_legacy$spotify_feature_id)

unmatched_weekly_for_spotify_legacy <- weekly_song_universe_legacy %>%
  filter(!weekly_song_id %in% spotify_exact_crosswalk_legacy$weekly_song_id)

spotify_fuzzy_candidates_legacy <- unmatched_spotify_features_legacy %>%
  select(
    spotify_feature_id,
    spotify_song_original,
    spotify_artist_original,
    spotify_min_year,
    spotify_max_year,
    song_key_relaxed,
    artist_primary_key,
    artist_core_key
  ) %>%
  inner_join(
    unmatched_weekly_for_spotify_legacy %>%
      select(
        weekly_song_id,
        weekly_song_original,
        weekly_artist_original,
        weekly_entry_year,
        weekly_last_year,
        weekly_peak_rank,
        weekly_weeks_observed,
        song_key_relaxed,
        artist_primary_key,
        artist_core_key
      ),
    by = character(),
    relationship = "many-to-many",
    suffix = c("_spotify", "_weekly")
  ) %>%
  mutate(
    same_first_song_letter = str_sub(song_key_relaxed_spotify, 1, 1) == str_sub(song_key_relaxed_weekly, 1, 1),
    same_first_artist_letter = str_sub(artist_primary_key_spotify, 1, 1) == str_sub(artist_primary_key_weekly, 1, 1)
  ) %>%
  filter(same_first_song_letter, same_first_artist_letter) %>%
  mutate(
    spotify_song_distance = stringdist(song_key_relaxed_spotify, song_key_relaxed_weekly, method = "jw"),
    spotify_artist_distance = pmin(
      stringdist(artist_primary_key_spotify, artist_primary_key_weekly, method = "jw"),
      stringdist(artist_core_key_spotify, artist_core_key_weekly, method = "jw"),
      na.rm = TRUE
    ),
    spotify_artist_distance = ifelse(is.infinite(spotify_artist_distance), NA_real_, spotify_artist_distance),
    spotify_year_distance = pmin(
      abs(spotify_min_year - weekly_entry_year),
      abs(spotify_max_year - weekly_entry_year),
      abs(spotify_min_year - weekly_last_year),
      abs(spotify_max_year - weekly_last_year),
      na.rm = TRUE
    ),
    spotify_year_distance = ifelse(is.infinite(spotify_year_distance), NA_real_, spotify_year_distance),
    spotify_combined_distance = spotify_song_distance + spotify_artist_distance
  ) %>%
  filter(
    spotify_song_distance <= fuzzy_song_distance_cutoff,
    spotify_artist_distance <= fuzzy_artist_distance_cutoff,
    is.na(spotify_year_distance) | spotify_year_distance <= fuzzy_year_distance_cutoff
  ) %>%
  arrange(spotify_combined_distance, spotify_song_distance, spotify_artist_distance, weekly_peak_rank)

spotify_fuzzy_best_by_feature_legacy <- spotify_fuzzy_candidates_legacy %>%
  group_by(spotify_feature_id) %>%
  arrange(spotify_combined_distance, spotify_song_distance, spotify_artist_distance, weekly_peak_rank) %>%
  mutate(
    spotify_feature_candidate_rank = row_number(),
    spotify_feature_best_distance = first(spotify_combined_distance),
    spotify_feature_second_distance = nth(spotify_combined_distance, 2, default = Inf),
    spotify_feature_margin = spotify_feature_second_distance - spotify_feature_best_distance
  ) %>%
  ungroup() %>%
  filter(spotify_feature_candidate_rank == 1)

spotify_fuzzy_best_unique_legacy <- spotify_fuzzy_best_by_feature_legacy %>%
  group_by(weekly_song_id) %>%
  arrange(spotify_combined_distance, spotify_song_distance, spotify_artist_distance, weekly_peak_rank) %>%
  mutate(
    spotify_weekly_candidate_rank = row_number(),
    spotify_weekly_best_distance = first(spotify_combined_distance),
    spotify_weekly_second_distance = nth(spotify_combined_distance, 2, default = Inf),
    spotify_weekly_margin = spotify_weekly_second_distance - spotify_weekly_best_distance
  ) %>%
  ungroup() %>%
  filter(spotify_weekly_candidate_rank == 1) %>%
  filter(
    spotify_feature_margin >= fuzzy_margin_cutoff | is.infinite(spotify_feature_margin),
    spotify_weekly_margin >= fuzzy_margin_cutoff | is.infinite(spotify_weekly_margin)
  )

spotify_auto_fuzzy_crosswalk_legacy <- spotify_fuzzy_best_unique_legacy %>%
  transmute(
    spotify_feature_id,
    weekly_song_id,
    spotify_match_tier = "5_auto_fuzzy_conservative",
    spotify_match_score = 5,
    spotify_song_distance,
    spotify_artist_distance,
    spotify_combined_distance,

    spotify_song_original,
    spotify_artist_original,
    weekly_song_original,
    weekly_artist_original,
    weekly_peak_rank,
    weekly_weeks_observed
  )

spotify_fuzzy_review_examples_legacy <- spotify_auto_fuzzy_crosswalk_legacy %>%
  arrange(spotify_combined_distance) %>%
  select(
    spotify_song_original,
    spotify_artist_original,
    weekly_song_original,
    weekly_artist_original,
    spotify_song_distance,
    spotify_artist_distance,
    weekly_peak_rank
  ) %>%
  slice_head(n = 50)

print(spotify_fuzzy_review_examples_legacy)

check_unmatched_top10_spotify <- function(
    spotify_features_unique,
    current_crosswalk,
    weekly_song_universe,
    artist_distance_cutoff = 0.18,
    min_entry_year = 1990
) {
  unmatched_spotify <- spotify_features_unique %>%
    filter(!spotify_feature_id %in% current_crosswalk$spotify_feature_id)

  top10_billboard <- weekly_song_universe %>%
    filter(weekly_peak_rank <= 10, weekly_entry_year >= min_entry_year)

  possible_top10_matches <- unmatched_spotify %>%
    inner_join(
      top10_billboard,
      by = c("song_key_relaxed" = "song_key_relaxed"),
      suffix = c("_spotify", "_billboard")
    ) %>%
    mutate(
      artist_distance_primary = stringdist(artist_primary_key_spotify, artist_primary_key_billboard, method = "jw"),
      artist_distance_core = stringdist(artist_core_key_spotify, artist_core_key_billboard, method = "jw"),
      spotify_artist_distance = pmin(artist_distance_primary, artist_distance_core, na.rm = TRUE),
      spotify_artist_distance = ifelse(is.infinite(spotify_artist_distance), NA_real_, spotify_artist_distance)
    ) %>%
    filter(spotify_artist_distance <= artist_distance_cutoff) %>%
    arrange(spotify_feature_id, spotify_artist_distance, weekly_peak_rank) %>%
    group_by(spotify_feature_id) %>%
    slice(1) %>%
    ungroup()

  list(
    summary = tibble(
      unmatched_spotify_songs_total = n_distinct(unmatched_spotify$spotify_feature_id),
      possible_top10_unmatched_songs = n_distinct(possible_top10_matches$spotify_feature_id),
      share_possible_top10 = possible_top10_unmatched_songs / unmatched_spotify_songs_total
    ),
    possible_top10_matches = possible_top10_matches
  )
}

spotify_crosswalk_before_supplement_legacy <- bind_rows(
  spotify_exact_crosswalk_legacy,
  spotify_auto_fuzzy_crosswalk_legacy
) %>%
  arrange(weekly_song_id, spotify_match_score, spotify_combined_distance, weekly_peak_rank) %>%
  group_by(weekly_song_id) %>%
  slice(1) %>%
  ungroup()

spotify_top10_check_legacy <- check_unmatched_top10_spotify(
  spotify_features_unique = spotify_features_unique_legacy,
  current_crosswalk = spotify_crosswalk_before_supplement_legacy,
  weekly_song_universe = weekly_song_universe_legacy,
  artist_distance_cutoff = 0.18,
  min_entry_year = 1990
)

print(spotify_top10_check_legacy$summary)

spotify_supplemental_top10_legacy <- spotify_top10_check_legacy$possible_top10_matches %>%
  filter(isTRUE(accept_spotify_supplemental_top10)) %>%
  filter(!spotify_feature_id %in% spotify_crosswalk_before_supplement_legacy$spotify_feature_id) %>%
  filter(!weekly_song_id %in% spotify_crosswalk_before_supplement_legacy$weekly_song_id) %>%
  transmute(
    spotify_feature_id,
    weekly_song_id,
    spotify_match_tier = "6_supplemental_top10_reviewed",
    spotify_match_score = 6,
    spotify_song_distance = 0,
    spotify_artist_distance = spotify_artist_distance,
    spotify_combined_distance = spotify_artist_distance,

    spotify_song_original,
    spotify_artist_original,
    weekly_song_original,
    weekly_artist_original,
    weekly_peak_rank,
    weekly_weeks_observed
  )

spotify_billboard_crosswalk_legacy <- bind_rows(
  spotify_exact_crosswalk_legacy,
  spotify_auto_fuzzy_crosswalk_legacy,
  spotify_supplemental_top10_legacy
) %>%
  arrange(weekly_song_id, spotify_match_score, spotify_combined_distance, weekly_peak_rank) %>%
  group_by(weekly_song_id) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(spotify_feature_id, spotify_match_score, spotify_combined_distance, weekly_peak_rank) %>%
  group_by(spotify_feature_id) %>%
  slice(1) %>%
  ungroup()

stop_if_duplicates(spotify_billboard_crosswalk_legacy, "weekly_song_id", "Billboard weekly song IDs in Spotify crosswalk")
stop_if_duplicates(spotify_billboard_crosswalk_legacy, "spotify_feature_id", "Spotify feature IDs in Spotify crosswalk")

spotify_match_tier_summary_legacy <- spotify_billboard_crosswalk_legacy %>%
  count(spotify_match_tier, sort = TRUE)

spotify_crosswalk_summary_legacy <- spotify_billboard_crosswalk_legacy %>%
  summarise(
    matched_spotify_songs = n(),
    matched_top10_billboard_songs = sum(weekly_peak_rank <= 10, na.rm = TRUE),
    mean_song_distance = mean(spotify_song_distance, na.rm = TRUE),
    mean_artist_distance = mean(spotify_artist_distance, na.rm = TRUE)
  )

print(spotify_match_tier_summary_legacy)
print(spotify_crosswalk_summary_legacy)


# 6. Billboard + Spotify merge diagnostics

billboard_spotify_merged_legacy <- bb_weekly_legacy %>%
  left_join(
    spotify_billboard_crosswalk_legacy %>%
      select(
        weekly_song_id,
        spotify_feature_id,
        spotify_match_tier,
        spotify_match_score,
        spotify_song_distance,
        spotify_artist_distance,
        spotify_combined_distance
      ),
    by = "weekly_song_id"
  ) %>%
  left_join(
    spotify_features_unique_legacy %>%
      select(spotify_feature_id, all_of(spotify_feature_metadata_cols), all_of(spotify_audio_cols)),
    by = "spotify_feature_id"
  )

stopifnot(nrow(bb_weekly_legacy) == nrow(billboard_spotify_merged_legacy))

spotify_merge_diagnostics_legacy <- billboard_spotify_merged_legacy %>%
  summarise(
    weekly_rows_total = n(),
    weekly_rows_with_spotify = sum(!is.na(spotify_feature_id)),
    weekly_rows_without_spotify = sum(is.na(spotify_feature_id)),
    weekly_row_spotify_match_rate = weekly_rows_with_spotify / weekly_rows_total,

    unique_weekly_songs_total = n_distinct(weekly_song_id),
    unique_weekly_songs_with_spotify = n_distinct(weekly_song_id[!is.na(spotify_feature_id)]),
    unique_song_spotify_match_rate = unique_weekly_songs_with_spotify / unique_weekly_songs_total
  )

spotify_song_level_coverage_legacy <- billboard_spotify_merged_legacy %>%
  group_by(weekly_song_id) %>%
  summarise(
    song = first(weekly_song),
    artist = first(weekly_artist),
    entry_year = min(weekly_year, na.rm = TRUE),
    weeks_on_chart = n(),
    peak_rank = min(weekly_rank, na.rm = TRUE),
    has_spotify_features = any(!is.na(spotify_feature_id)),
    spotify_match_tier = first(spotify_match_tier),
    .groups = "drop"
  )

spotify_coverage_by_peak_group_legacy <- spotify_song_level_coverage_legacy %>%
  mutate(
    peak_group = case_when(
      peak_rank <= 10 ~ "Top 10 peak",
      peak_rank <= 40 ~ "Top 40 peak",
      peak_rank <= 100 ~ "Lower Hot 100",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(peak_group) %>%
  summarise(
    total_songs = n(),
    matched_spotify_songs = sum(has_spotify_features),
    spotify_match_rate = matched_spotify_songs / total_songs,
    .groups = "drop"
  )

print(spotify_merge_diagnostics_legacy)
print(spotify_coverage_by_peak_group_legacy)


# 7. Lyrics preparation and deduplication

lyrics_raw_legacy <- billboard_lyrics_spotify %>%
  rename(
    lyric_year_end_rank = rank,
    lyric_year_end_year = year
  ) %>%
  mutate(
    lyric_raw_id = row_number(),
    lyric_year_end_year = as.integer(lyric_year_end_year),

    song_key_strict = clean_song_strict(song),
    song_key_relaxed = clean_song_relaxed(song),

    artist_all_key = clean_artist_full(artist_all),
    artist_base_key = clean_artist_full(artist_base),
    artist_primary_key = clean_artist_primary(artist_base),
    artist_core_key = clean_artist_core(artist_base)
  )

if ("num_uniq_words" %in% names(lyrics_raw_legacy) && !"num_unique_words" %in% names(lyrics_raw_legacy)) {
  lyrics_raw_legacy <- lyrics_raw_legacy %>% rename(num_unique_words = num_uniq_words)
}

include_explicit_from_lyrics_file <- TRUE

lyric_cols <- c(
  "artist_all",
  "artist_base",
  "artist_featured",
  "lyrics",
  "num_words",
  "words_per_sec",
  "num_unique_words",
  "uniq_ratio"
)

if (isTRUE(include_explicit_from_lyrics_file)) {
  lyric_cols <- c(lyric_cols, "explicit")
}

lyric_cols <- intersect(lyric_cols, names(lyrics_raw_legacy))

lyrics_diagnostics_legacy <- lyrics_raw_legacy %>%
  summarise(
    raw_lyric_rows = n(),
    unique_lyric_songs_by_clean_key = n_distinct(song_key_relaxed, artist_primary_key),
    min_lyric_year = min(lyric_year_end_year, na.rm = TRUE),
    max_lyric_year = max(lyric_year_end_year, na.rm = TRUE),
    missing_song = sum(is.na(song) | str_trim(song) == ""),
    missing_artist_base = sum(is.na(artist_base) | str_trim(artist_base) == ""),
    missing_lyrics = sum(is.na(lyrics) | str_trim(lyrics) == "")
  )

print(lyrics_diagnostics_legacy)

lyrics_unique_legacy <- lyrics_raw_legacy %>%
  group_by(song_key_relaxed, artist_primary_key) %>%
  arrange(lyric_year_end_year, lyric_year_end_rank, .by_group = TRUE) %>%
  summarise(
    lyric_id = first(lyric_raw_id),

    lyric_song_original = first(song),
    lyric_artist_original = first(artist_base),

    lyric_source_years = paste(sort(unique(lyric_year_end_year)), collapse = ", "),
    lyric_min_year = safe_min(lyric_year_end_year),
    lyric_max_year = safe_max(lyric_year_end_year),
    lyric_best_year_end_rank = safe_min(lyric_year_end_rank),

    song_key_strict = first(song_key_strict),
    song_key_relaxed = first(song_key_relaxed),

    artist_all_key = first(artist_all_key),
    artist_base_key = first(artist_base_key),
    artist_primary_key = first(artist_primary_key),
    artist_core_key = first(artist_core_key),

    lyric_source_rows_collapsed = n(),

    across(any_of(lyric_cols), first_non_missing),

    .groups = "drop"
  )

lyrics_dedup_diagnostics_legacy <- lyrics_unique_legacy %>%
  summarise(
    unique_lyric_songs = n(),
    duplicated_lyric_source_groups = sum(lyric_source_rows_collapsed > 1),
    max_rows_collapsed_into_one_lyric_song = max(lyric_source_rows_collapsed, na.rm = TRUE)
  )

print(lyrics_dedup_diagnostics_legacy)


# 8. Lyrics-to-Billboard/Spotify matching
# Lyrics are matched only after Spotify has been merged onto the Billboard universe.

billboard_spotify_song_universe_legacy <- billboard_spotify_merged_legacy %>%
  group_by(weekly_song_id) %>%
  summarise(
    weekly_song_original = first(weekly_song),
    weekly_artist_original = first(weekly_artist),

    weekly_first_date = min(weekly_date, na.rm = TRUE),
    weekly_last_date = max(weekly_date, na.rm = TRUE),
    weekly_entry_year = year(weekly_first_date),
    weekly_last_year = year(weekly_last_date),
    weekly_peak_rank = min(weekly_rank, na.rm = TRUE),
    weekly_weeks_observed = n(),

    song_key_strict = first(song_key_strict),
    song_key_relaxed = first(song_key_relaxed),

    artist_full_key = first(artist_full_key),
    artist_primary_key = first(artist_primary_key),
    artist_core_key = first(artist_core_key),

    has_spotify_features = any(!is.na(spotify_feature_id)),
    .groups = "drop"
  )

lyrics_target_universe_legacy <- billboard_spotify_song_universe_legacy %>%
  filter(has_spotify_features)

lyrics_target_summary_legacy <- lyrics_target_universe_legacy %>%
  summarise(
    target_songs_for_lyrics = n(),
    target_top10_songs_for_lyrics = sum(weekly_peak_rank <= 10, na.rm = TRUE),
    min_entry_year = min(weekly_entry_year, na.rm = TRUE),
    max_entry_year = max(weekly_entry_year, na.rm = TRUE)
  )

print(lyrics_target_summary_legacy)

make_lyrics_crosswalk <- function(data, tier_name, score) {
  data %>%
    transmute(
      lyric_id,
      weekly_song_id,
      lyric_match_tier = tier_name,
      lyric_match_score = score,
      lyric_song_distance = 0,
      lyric_artist_distance = 0,
      lyric_combined_distance = 0,

      lyric_song_original,
      lyric_artist_original,
      weekly_song_original,
      weekly_artist_original,
      weekly_peak_rank,
      weekly_weeks_observed
    )
}

lyrics_tier1_legacy <- lyrics_unique_legacy %>%
  inner_join(
    lyrics_target_universe_legacy,
    by = c("song_key_strict" = "song_key_strict", "artist_base_key" = "artist_full_key"),
    suffix = c("_lyric", "_weekly")
  ) %>%
  make_lyrics_crosswalk("1_exact_strict_song_full_artist", 1)

lyrics_tier2_legacy <- lyrics_unique_legacy %>%
  inner_join(
    lyrics_target_universe_legacy,
    by = c("song_key_strict" = "song_key_strict", "artist_primary_key" = "artist_primary_key"),
    suffix = c("_lyric", "_weekly")
  ) %>%
  make_lyrics_crosswalk("2_exact_strict_song_primary_artist", 2)

lyrics_tier3_legacy <- lyrics_unique_legacy %>%
  inner_join(
    lyrics_target_universe_legacy,
    by = c("song_key_relaxed" = "song_key_relaxed", "artist_primary_key" = "artist_primary_key"),
    suffix = c("_lyric", "_weekly")
  ) %>%
  make_lyrics_crosswalk("3_exact_relaxed_song_primary_artist", 3)

lyrics_tier4_legacy <- lyrics_unique_legacy %>%
  inner_join(
    lyrics_target_universe_legacy,
    by = c("song_key_relaxed" = "song_key_relaxed", "artist_core_key" = "artist_core_key"),
    suffix = c("_lyric", "_weekly")
  ) %>%
  make_lyrics_crosswalk("4_exact_relaxed_song_core_artist", 4)

lyrics_exact_crosswalk_legacy <- bind_rows(
  lyrics_tier1_legacy,
  lyrics_tier2_legacy,
  lyrics_tier3_legacy,
  lyrics_tier4_legacy
) %>%
  arrange(
    weekly_song_id,
    lyric_match_score,
    lyric_combined_distance,
    weekly_peak_rank,
    desc(weekly_weeks_observed)
  ) %>%
  group_by(weekly_song_id) %>%
  slice(1) %>%
  ungroup()

lyrics_exact_match_summary_legacy <- lyrics_exact_crosswalk_legacy %>%
  count(lyric_match_tier, sort = TRUE)

print(lyrics_exact_match_summary_legacy)

unmatched_lyrics_legacy <- lyrics_unique_legacy %>%
  filter(!lyric_id %in% lyrics_exact_crosswalk_legacy$lyric_id)

unmatched_targets_for_lyrics_legacy <- lyrics_target_universe_legacy %>%
  filter(!weekly_song_id %in% lyrics_exact_crosswalk_legacy$weekly_song_id)

lyrics_fuzzy_candidates_legacy <- unmatched_lyrics_legacy %>%
  select(
    lyric_id,
    lyric_song_original,
    lyric_artist_original,
    lyric_min_year,
    lyric_max_year,
    song_key_relaxed,
    artist_primary_key,
    artist_core_key
  ) %>%
  inner_join(
    unmatched_targets_for_lyrics_legacy %>%
      select(
        weekly_song_id,
        weekly_song_original,
        weekly_artist_original,
        weekly_entry_year,
        weekly_last_year,
        weekly_peak_rank,
        weekly_weeks_observed,
        song_key_relaxed,
        artist_primary_key,
        artist_core_key
      ),
    by = character(),
    relationship = "many-to-many",
    suffix = c("_lyric", "_weekly")
  ) %>%
  mutate(
    same_first_song_letter = str_sub(song_key_relaxed_lyric, 1, 1) == str_sub(song_key_relaxed_weekly, 1, 1),
    same_first_artist_letter = str_sub(artist_primary_key_lyric, 1, 1) == str_sub(artist_primary_key_weekly, 1, 1)
  ) %>%
  filter(same_first_song_letter, same_first_artist_letter) %>%
  mutate(
    lyric_song_distance = stringdist(song_key_relaxed_lyric, song_key_relaxed_weekly, method = "jw"),
    lyric_artist_distance = pmin(
      stringdist(artist_primary_key_lyric, artist_primary_key_weekly, method = "jw"),
      stringdist(artist_core_key_lyric, artist_core_key_weekly, method = "jw"),
      na.rm = TRUE
    ),
    lyric_artist_distance = ifelse(is.infinite(lyric_artist_distance), NA_real_, lyric_artist_distance),
    lyric_year_distance = pmin(
      abs(lyric_min_year - weekly_entry_year),
      abs(lyric_max_year - weekly_entry_year),
      abs(lyric_min_year - weekly_last_year),
      abs(lyric_max_year - weekly_last_year),
      na.rm = TRUE
    ),
    lyric_year_distance = ifelse(is.infinite(lyric_year_distance), NA_real_, lyric_year_distance),
    lyric_combined_distance = lyric_song_distance + lyric_artist_distance
  ) %>%
  filter(
    lyric_song_distance <= fuzzy_song_distance_cutoff,
    lyric_artist_distance <= fuzzy_artist_distance_cutoff,
    is.na(lyric_year_distance) | lyric_year_distance <= fuzzy_year_distance_cutoff
  ) %>%
  arrange(lyric_combined_distance, lyric_song_distance, lyric_artist_distance, weekly_peak_rank)

lyrics_fuzzy_best_by_lyric_legacy <- lyrics_fuzzy_candidates_legacy %>%
  group_by(lyric_id) %>%
  arrange(lyric_combined_distance, lyric_song_distance, lyric_artist_distance, weekly_peak_rank) %>%
  mutate(
    lyric_candidate_rank = row_number(),
    lyric_best_distance = first(lyric_combined_distance),
    lyric_second_distance = nth(lyric_combined_distance, 2, default = Inf),
    lyric_margin = lyric_second_distance - lyric_best_distance
  ) %>%
  ungroup() %>%
  filter(lyric_candidate_rank == 1)

lyrics_fuzzy_best_unique_legacy <- lyrics_fuzzy_best_by_lyric_legacy %>%
  group_by(weekly_song_id) %>%
  arrange(lyric_combined_distance, lyric_song_distance, lyric_artist_distance, weekly_peak_rank) %>%
  mutate(
    weekly_candidate_rank = row_number(),
    weekly_best_distance = first(lyric_combined_distance),
    weekly_second_distance = nth(lyric_combined_distance, 2, default = Inf),
    weekly_margin = weekly_second_distance - weekly_best_distance
  ) %>%
  ungroup() %>%
  filter(weekly_candidate_rank == 1) %>%
  filter(
    lyric_margin >= fuzzy_margin_cutoff | is.infinite(lyric_margin),
    weekly_margin >= fuzzy_margin_cutoff | is.infinite(weekly_margin)
  )

lyrics_auto_fuzzy_crosswalk_legacy <- lyrics_fuzzy_best_unique_legacy %>%
  transmute(
    lyric_id,
    weekly_song_id,
    lyric_match_tier = "5_auto_fuzzy_conservative",
    lyric_match_score = 5,
    lyric_song_distance,
    lyric_artist_distance,
    lyric_combined_distance,

    lyric_song_original,
    lyric_artist_original,
    weekly_song_original,
    weekly_artist_original,
    weekly_peak_rank,
    weekly_weeks_observed
  )

lyrics_fuzzy_review_examples_legacy <- lyrics_auto_fuzzy_crosswalk_legacy %>%
  arrange(lyric_combined_distance) %>%
  select(
    lyric_song_original,
    lyric_artist_original,
    weekly_song_original,
    weekly_artist_original,
    lyric_song_distance,
    lyric_artist_distance,
    weekly_peak_rank
  ) %>%
  slice_head(n = 50)

print(lyrics_fuzzy_review_examples_legacy)

check_unmatched_top10_lyrics <- function(
    lyrics_unique,
    current_crosswalk,
    target_universe,
    artist_distance_cutoff = 0.18,
    min_entry_year = 1990
) {
  unmatched_lyrics <- lyrics_unique %>%
    filter(!lyric_id %in% current_crosswalk$lyric_id)

  top10_targets <- target_universe %>%
    filter(weekly_peak_rank <= 10, weekly_entry_year >= min_entry_year)

  possible_top10_matches <- unmatched_lyrics %>%
    inner_join(
      top10_targets,
      by = c("song_key_relaxed" = "song_key_relaxed"),
      suffix = c("_lyric", "_billboard")
    ) %>%
    mutate(
      artist_distance_primary = stringdist(artist_primary_key_lyric, artist_primary_key_billboard, method = "jw"),
      artist_distance_core = stringdist(artist_core_key_lyric, artist_core_key_billboard, method = "jw"),
      lyric_artist_distance = pmin(artist_distance_primary, artist_distance_core, na.rm = TRUE),
      lyric_artist_distance = ifelse(is.infinite(lyric_artist_distance), NA_real_, lyric_artist_distance)
    ) %>%
    filter(lyric_artist_distance <= artist_distance_cutoff) %>%
    arrange(lyric_id, lyric_artist_distance, weekly_peak_rank) %>%
    group_by(lyric_id) %>%
    slice(1) %>%
    ungroup()

  list(
    summary = tibble(
      unmatched_lyric_songs_total = n_distinct(unmatched_lyrics$lyric_id),
      possible_top10_unmatched_songs = n_distinct(possible_top10_matches$lyric_id),
      share_possible_top10 = possible_top10_unmatched_songs / unmatched_lyric_songs_total
    ),
    possible_top10_matches = possible_top10_matches
  )
}

lyrics_crosswalk_before_supplement_legacy <- bind_rows(
  lyrics_exact_crosswalk_legacy,
  lyrics_auto_fuzzy_crosswalk_legacy
) %>%
  arrange(weekly_song_id, lyric_match_score, lyric_combined_distance, weekly_peak_rank) %>%
  group_by(weekly_song_id) %>%
  slice(1) %>%
  ungroup()

lyrics_top10_check_legacy <- check_unmatched_top10_lyrics(
  lyrics_unique = lyrics_unique_legacy,
  current_crosswalk = lyrics_crosswalk_before_supplement_legacy,
  target_universe = lyrics_target_universe_legacy,
  artist_distance_cutoff = 0.18,
  min_entry_year = 1990
)

print(lyrics_top10_check_legacy$summary)

lyrics_supplemental_top10_legacy <- lyrics_top10_check_legacy$possible_top10_matches %>%
  filter(isTRUE(accept_lyrics_supplemental_top10)) %>%
  filter(!lyric_id %in% lyrics_crosswalk_before_supplement_legacy$lyric_id) %>%
  filter(!weekly_song_id %in% lyrics_crosswalk_before_supplement_legacy$weekly_song_id) %>%
  transmute(
    lyric_id,
    weekly_song_id,
    lyric_match_tier = "6_supplemental_top10_accepted",
    lyric_match_score = 6,
    lyric_song_distance = 0,
    lyric_artist_distance = lyric_artist_distance,
    lyric_combined_distance = lyric_artist_distance,

    lyric_song_original,
    lyric_artist_original,
    weekly_song_original,
    weekly_artist_original,
    weekly_peak_rank,
    weekly_weeks_observed
  )

lyrics_supplemental_reviewed_matches_legacy <- lyrics_supplemental_top10_legacy %>%
  arrange(lyric_artist_distance, weekly_peak_rank) %>%
  select(
    lyric_song_original,
    lyric_artist_original,
    weekly_song_original,
    weekly_artist_original,
    weekly_peak_rank,
    weekly_weeks_observed,
    lyric_artist_distance
  )

print(lyrics_supplemental_reviewed_matches_legacy)

lyrics_billboard_spotify_crosswalk_legacy <- bind_rows(
  lyrics_exact_crosswalk_legacy,
  lyrics_auto_fuzzy_crosswalk_legacy,
  lyrics_supplemental_top10_legacy
) %>%
  arrange(weekly_song_id, lyric_match_score, lyric_combined_distance, weekly_peak_rank) %>%
  group_by(weekly_song_id) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(lyric_id, lyric_match_score, lyric_combined_distance, weekly_peak_rank) %>%
  group_by(lyric_id) %>%
  slice(1) %>%
  ungroup()

stop_if_duplicates(lyrics_billboard_spotify_crosswalk_legacy, "weekly_song_id", "Billboard weekly song IDs in lyrics crosswalk")
stop_if_duplicates(lyrics_billboard_spotify_crosswalk_legacy, "lyric_id", "lyric IDs in lyrics crosswalk")

lyrics_match_tier_summary_legacy <- lyrics_billboard_spotify_crosswalk_legacy %>%
  count(lyric_match_tier, sort = TRUE)

lyrics_crosswalk_summary_legacy <- lyrics_billboard_spotify_crosswalk_legacy %>%
  summarise(
    matched_lyric_songs = n(),
    matched_top10_songs = sum(weekly_peak_rank <= 10, na.rm = TRUE),
    mean_song_distance = mean(lyric_song_distance, na.rm = TRUE),
    mean_artist_distance = mean(lyric_artist_distance, na.rm = TRUE)
  )

print(lyrics_match_tier_summary_legacy)
print(lyrics_crosswalk_summary_legacy)


# 9. Final merged weekly dataset

billboard_merged_legacy <- billboard_spotify_merged_legacy %>%
  left_join(
    lyrics_billboard_spotify_crosswalk_legacy %>%
      select(
        weekly_song_id,
        lyric_id,
        lyric_match_tier,
        lyric_match_score,
        lyric_song_distance,
        lyric_artist_distance,
        lyric_combined_distance
      ),
    by = "weekly_song_id"
  ) %>%
  left_join(
    lyrics_unique_legacy %>%
      select(
        lyric_id,
        lyric_source_years,
        lyric_min_year,
        lyric_max_year,
        lyric_best_year_end_rank,
        any_of(lyric_cols)
      ),
    by = "lyric_id"
  )

stopifnot(nrow(bb_weekly_legacy) == nrow(billboard_merged_legacy))

final_merge_diagnostics_legacy <- billboard_merged_legacy %>%
  summarise(
    weekly_rows_total = n(),

    weekly_rows_with_spotify = sum(!is.na(spotify_feature_id)),
    weekly_rows_with_lyrics = sum(!is.na(lyric_id)),
    weekly_rows_with_both = sum(!is.na(spotify_feature_id) & !is.na(lyric_id)),

    weekly_row_spotify_rate = weekly_rows_with_spotify / weekly_rows_total,
    weekly_row_lyrics_rate = weekly_rows_with_lyrics / weekly_rows_total,
    weekly_row_both_rate = weekly_rows_with_both / weekly_rows_total,

    unique_songs_total = n_distinct(weekly_song_id),
    unique_songs_with_spotify = n_distinct(weekly_song_id[!is.na(spotify_feature_id)]),
    unique_songs_with_lyrics = n_distinct(weekly_song_id[!is.na(lyric_id)]),
    unique_songs_with_both = n_distinct(weekly_song_id[!is.na(spotify_feature_id) & !is.na(lyric_id)]),

    unique_song_spotify_rate = unique_songs_with_spotify / unique_songs_total,
    unique_song_lyrics_rate = unique_songs_with_lyrics / unique_songs_total,
    unique_song_both_rate = unique_songs_with_both / unique_songs_total
  )

song_level_coverage_legacy <- billboard_merged_legacy %>%
  group_by(weekly_song_id) %>%
  summarise(
    song = first(weekly_song),
    artist = first(weekly_artist),
    entry_year = min(weekly_year, na.rm = TRUE),
    weeks_on_chart = n(),
    peak_rank = min(weekly_rank, na.rm = TRUE),

    has_spotify_features = any(!is.na(spotify_feature_id)),
    has_lyrics = any(!is.na(lyric_id)),
    has_both = any(!is.na(spotify_feature_id) & !is.na(lyric_id)),

    spotify_match_tier = first(spotify_match_tier),
    lyric_match_tier = first(lyric_match_tier),

    .groups = "drop"
  )

coverage_by_peak_group_legacy <- song_level_coverage_legacy %>%
  mutate(
    peak_group = case_when(
      peak_rank <= 10 ~ "Top 10 peak",
      peak_rank <= 40 ~ "Top 40 peak",
      peak_rank <= 100 ~ "Lower Hot 100",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(peak_group) %>%
  summarise(
    total_songs = n(),
    songs_with_spotify = sum(has_spotify_features),
    songs_with_lyrics = sum(has_lyrics),
    songs_with_both = sum(has_both),
    spotify_rate = songs_with_spotify / total_songs,
    lyrics_rate = songs_with_lyrics / total_songs,
    both_rate = songs_with_both / total_songs,
    .groups = "drop"
  )

coverage_top10_by_entry_year_legacy <- song_level_coverage_legacy %>%
  filter(peak_rank <= 10) %>%
  group_by(entry_year) %>%
  summarise(
    top10_songs = n(),
    top10_with_spotify = sum(has_spotify_features),
    top10_with_lyrics = sum(has_lyrics),
    top10_with_both = sum(has_both),
    both_rate = top10_with_both / top10_songs,
    .groups = "drop"
  ) %>%
  arrange(entry_year)

print(final_merge_diagnostics_legacy)
print(coverage_by_peak_group_legacy)
print(coverage_top10_by_entry_year_legacy)


# 10. Song-level trajectory construction
# time_to_peak is the observed chart-week index of the peak.
# It is not calendar distance from entry_date to peak_date, because chart re-entry
# gaps would otherwise make time_to_peak exceed observed chart life.

final_billboard_date_current <- max(bb_weekly_legacy$weekly_date, na.rm = TRUE)

analysis_feature_cols_legacy <- intersect(
  c(
    spotify_audio_cols,
    lyric_cols,
    "spotify_match_tier",
    "lyric_match_tier",
    "spotify_match_score",
    "lyric_match_score",
    "spotify_song_distance",
    "spotify_artist_distance",
    "lyric_song_distance",
    "lyric_artist_distance"
  ),
  names(billboard_merged_legacy)
)

analysis_song_level_legacy <- billboard_merged_legacy %>%
  group_by(weekly_song_id) %>%
  arrange(weekly_date, .by_group = TRUE) %>%
  mutate(
    chart_week_index = row_number(),
    gap_weeks = as.numeric(weekly_date - lag(weekly_date)) / 7,
    first_run_index = cumsum(if_else(is.na(gap_weeks) | gap_weeks <= 1, 0L, 1L)) + 1L
  ) %>%
  summarise(
    song = first(weekly_song),
    artist = first(weekly_artist),

    # Artist identity/audit variables.
    main_artist_key = first(artist_primary_key),
    artist_cluster_id = first(artist_primary_key),
    featured_artist_flag = first(detect_featured_artist_flag_current(weekly_artist)),
    n_credit_artists = first(count_credit_artists_current(weekly_artist)),

    entry_date = first(weekly_date),
    last_chart_date = last(weekly_date),
    entry_year = year(entry_date),

    weeks_on_chart = n(),

    entry_rank = first(weekly_rank),
    peak_rank = min(weekly_rank, na.rm = TRUE),
    final_rank = last(weekly_rank),

    peak_position = which.min(weekly_rank)[1],
    peak_date = weekly_date[peak_position],

    # Existing thesis outcome: observed chart-week index of the peak.
    # This is 1-based in this pipeline; zero-based timing is created later as weeks_to_peak_0.
    time_to_peak = peak_position,

    rise_speed = ifelse(
      time_to_peak > 1,
      (entry_rank - peak_rank) / (time_to_peak - 1),
      NA_real_
    ),

    decay_weeks = weeks_on_chart - time_to_peak,

    decay_rate = ifelse(
      decay_weeks > 0,
      (final_rank - peak_rank) / decay_weeks,
      NA_real_
    ),

    has_chart_gap = any(gap_weeks > 1, na.rm = TRUE),
    n_chart_gaps_gt_1 = sum(gap_weeks > 1, na.rm = TRUE),
    max_chart_gap_weeks = ifelse(all(is.na(gap_weeks)), 1, max(gap_weeks, na.rm = TRUE)),

    first_run_weeks_on_chart = sum(first_run_index == 1, na.rm = TRUE),
    first_run_decay_rate = {
      first_run_ranks <- weekly_rank[first_run_index == 1]
      if (length(first_run_ranks) == 0 || all(is.na(first_run_ranks))) {
        NA_real_
      } else {
        first_run_peak_index <- which.min(first_run_ranks)[1]
        first_run_peak_rank <- min(first_run_ranks, na.rm = TRUE)
        first_run_final_rank <- dplyr::last(first_run_ranks)
        first_run_decay_weeks <- length(first_run_ranks) - first_run_peak_index
        if (first_run_decay_weeks > 0) {
          (first_run_final_rank - first_run_peak_rank) / first_run_decay_weeks
        } else {
          NA_real_
        }
      }
    },

    has_spotify_features = any(!is.na(spotify_feature_id)),
    has_lyrics = any(!is.na(lyric_id)),
    has_both = any(!is.na(spotify_feature_id) & !is.na(lyric_id)),

    across(all_of(analysis_feature_cols_legacy), first_non_missing),

    .groups = "drop"
  ) %>%
  left_join(artist_history_lookup_current, by = "weekly_song_id") %>%
  mutate(
    decade = floor(entry_year / 10) * 10,
    decade_factor = as.factor(decade),
    entry_year_factor = as.factor(entry_year),
    entry_era = case_when(
      entry_year >= 2000 & entry_year <= 2007 ~ "2000_2007",
      entry_year >= 2008 & entry_year <= 2014 ~ "2008_2014",
      entry_year >= 2015 & entry_year <= 2021 ~ "2015_2021",
      TRUE ~ NA_character_
    ),
    entry_era = factor(entry_era, levels = c("2000_2007", "2008_2014", "2015_2021")),
    post_streaming_era_flag = as.integer(entry_year >= 2015),
    covid_era_flag = as.integer(entry_year >= 2020),
    sample_2000_2020_flag = as.integer(entry_year <= 2020),
    right_censored_flag = as.integer(last_chart_date == final_billboard_date_current),
    log_time_to_peak = log(time_to_peak),
    log_weeks_on_chart = log(weeks_on_chart),
    peak_week_index = time_to_peak,
    weeks_to_peak_0 = time_to_peak - 1,
    log1p_weeks_to_peak_0 = log1p(weeks_to_peak_0),
    peaked_on_entry_flag = as.integer(weeks_to_peak_0 == 0),
    rise_speed_observed_flag = as.integer(!is.na(rise_speed)),
    decay_rate_observed_flag = as.integer(!is.na(decay_rate)),
    decay_missing_flag = as.integer(is.na(decay_rate)),
    long_gap_8_flag = as.integer(max_chart_gap_weeks > 8),
    long_gap_12_flag = as.integer(max_chart_gap_weeks > 12),
    long_gap_flag = long_gap_8_flag
  )

if ("explicit" %in% names(analysis_song_level_legacy)) {
  analysis_song_level_legacy <- analysis_song_level_legacy %>%
    mutate(
      explicit_factor = case_when(
        is.na(explicit) ~ "Unknown",
        explicit == TRUE ~ "Explicit",
        explicit == FALSE ~ "Not explicit",
        TRUE ~ "Unknown"
      ),
      explicit_factor = factor(explicit_factor, levels = c("Not explicit", "Explicit", "Unknown"))
    )
} else {
  analysis_song_level_legacy <- analysis_song_level_legacy %>%
    mutate(
      explicit_factor = factor("Unknown", levels = c("Not explicit", "Explicit", "Unknown"))
    )
}

trajectory_diagnostics_all_legacy <- analysis_song_level_legacy %>%
  summarise(
    n_songs = n(),
    invalid_time_to_peak = sum(time_to_peak < 1 | time_to_peak > weeks_on_chart, na.rm = TRUE),
    negative_decay_weeks = sum(decay_weeks < 0, na.rm = TRUE),
    missing_rise_speed = sum(is.na(rise_speed)),
    peak_in_first_observed_week = sum(time_to_peak == 1, na.rm = TRUE),
    missing_decay_rate = sum(is.na(decay_rate)),
    no_post_peak_weeks = sum(decay_weeks == 0, na.rm = TRUE),
    songs_with_chart_gaps = sum(has_chart_gap, na.rm = TRUE),
    songs_with_long_gaps = sum(long_gap_flag, na.rm = TRUE),
    right_censored_songs = sum(right_censored_flag == 1, na.rm = TRUE),
    missing_artist_prior_top10 = sum(is.na(artist_prior_top10_count))
  )

print(trajectory_diagnostics_all_legacy)


# 11. Main sample construction

analysis_sample_top10_legacy <- analysis_song_level_legacy %>%
  filter(
    peak_rank <= 10,
    weeks_on_chart >= 3,
    has_spotify_features,
    has_lyrics,
    !is.na(danceability),
    !is.na(tempo),
    !is.na(lyrics)
  )

analysis_sample_main_model_legacy <- analysis_sample_top10_legacy %>%
  filter(entry_year >= 2000, entry_year <= 2021)

analysis_sample_2010_legacy <- analysis_sample_top10_legacy %>%
  filter(entry_year >= 2010, entry_year <= 2021)

analysis_sample_2000_2020_legacy <- analysis_sample_top10_legacy %>%
  filter(entry_year >= 2000, entry_year <= 2020)

analysis_sample_full_1990_legacy <- analysis_sample_top10_legacy %>%
  filter(entry_year >= 1990, entry_year <= 2021)

sample_summary_legacy <- bind_rows(
  analysis_sample_full_1990_legacy %>%
    summarise(sample = "Robustness: Top 10, 1990-2021", n_songs = n(), min_year = min(entry_year), max_year = max(entry_year)),
  analysis_sample_main_model_legacy %>%
    summarise(sample = "Main sample: Top 10, 2000-2021", n_songs = n(), min_year = min(entry_year), max_year = max(entry_year)),
  analysis_sample_2010_legacy %>%
    summarise(sample = "Robustness: Top 10, 2010-2021", n_songs = n(), min_year = min(entry_year), max_year = max(entry_year)),
  analysis_sample_2000_2020_legacy %>%
    summarise(sample = "Robustness: Top 10, 2000-2020", n_songs = n(), min_year = min(entry_year), max_year = max(entry_year))
)

main_missingness_key_legacy <- analysis_sample_main_model_legacy %>%
  summarise(
    n_songs = n(),
    missing_time_to_peak = sum(is.na(time_to_peak)),
    missing_rise_speed = sum(is.na(rise_speed)),
    missing_decay_rate = sum(is.na(decay_rate)),
    peak_in_first_week = sum(time_to_peak == 1, na.rm = TRUE),
    no_post_peak_decay = sum(decay_weeks == 0, na.rm = TRUE),
    across(
      all_of(intersect(c(spotify_audio_cols, lyric_cols), names(analysis_sample_main_model_legacy))),
      ~ sum(is.na(.x)),
      .names = "missing_{.col}"
    )
  )

missingness_all_legacy <- analysis_sample_main_model_legacy %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(n_total = nrow(analysis_sample_main_model_legacy), pct_missing = n_missing / n_total) %>%
  arrange(desc(n_missing))

variable_overview_legacy <- analysis_sample_main_model_legacy %>%
  summarise(across(everything(), ~ paste(class(.x), collapse = ", "))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "class") %>%
  left_join(missingness_all_legacy, by = "variable") %>%
  arrange(desc(n_missing))

print(sample_summary_legacy)
print(main_missingness_key_legacy)
print(missingness_all_legacy, n = Inf)
print(variable_overview_legacy, n = Inf)


# 12. NLP feature engineering
# These variables expand the lyric block beyond length and lexical diversity:
# word complexity, repetition/concentration, sentiment/emotionality, and pronoun perspective.

lyrics_base_legacy <- analysis_sample_main_model_legacy %>%
  select(weekly_song_id, song, artist, lyrics, num_words, uniq_ratio) %>%
  mutate(
    lyrics_raw = as.character(lyrics),
    lyrics_lines_text = str_replace_all(lyrics_raw, "\\\\n", "\n"),
    lyrics_clean = lyrics_lines_text %>%
      str_replace_all("\\[[^\\]]+\\]", " ") %>%
      str_replace_all("\\([^\\)]*\\)", " ") %>%
      str_replace_all("[^A-Za-z0-9'\\s\\n]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim()
  )

lyrics_tokens_legacy <- lyrics_base_legacy %>%
  select(weekly_song_id, lyrics_clean) %>%
  unnest_tokens(word, lyrics_clean) %>%
  filter(!is.na(word), word != "")

token_features_legacy <- lyrics_tokens_legacy %>%
  group_by(weekly_song_id) %>%
  summarise(
    token_count_check = n(),
    avg_word_length = mean(str_length(word), na.rm = TRUE),
    long_word_share = mean(str_length(word) >= 7, na.rm = TRUE),
    self_reference_share = mean(
      word %in% c("i", "me", "my", "mine", "myself", "we", "us", "our", "ours"),
      na.rm = TRUE
    ),
    second_person_share = mean(
      word %in% c("you", "your", "yours", "yourself"),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

data("stop_words", package = "tidytext")

lyrics_content_tokens_legacy <- lyrics_tokens_legacy %>%
  anti_join(stop_words, by = "word")

repetition_features_legacy <- lyrics_content_tokens_legacy %>%
  count(weekly_song_id, word, name = "word_n") %>%
  group_by(weekly_song_id) %>%
  arrange(desc(word_n), .by_group = TRUE) %>%
  summarise(
    content_token_count = sum(word_n),
    top_word_share = max(word_n) / sum(word_n),
    top_5_word_share = sum(head(word_n, 5)) / sum(word_n),
    content_unique_ratio = n_distinct(word) / sum(word_n),
    .groups = "drop"
  )

line_features_legacy <- lyrics_base_legacy %>%
  select(weekly_song_id, lyrics_lines_text) %>%
  separate_rows(lyrics_lines_text, sep = "\n") %>%
  mutate(
    lyric_line = lyrics_lines_text %>%
      str_to_lower() %>%
      str_replace_all("\\[[^\\]]+\\]", " ") %>%
      str_replace_all("[^a-z0-9'\\s]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim()
  ) %>%
  filter(!is.na(lyric_line), lyric_line != "") %>%
  group_by(weekly_song_id) %>%
  summarise(
    n_lyric_lines = n(),
    avg_words_per_line = mean(str_count(lyric_line, "\\S+"), na.rm = TRUE),
    repeated_line_share = 1 - (n_distinct(lyric_line) / n()),
    .groups = "drop"
  )

bing_lexicon_legacy <- tryCatch(
  tidytext::get_sentiments("bing"),
  error = function(e) {
    warning("Could not load Bing sentiment lexicon. Sentiment features will be set to NA.")
    NULL
  }
)

if (!is.null(bing_lexicon_legacy)) {
  sentiment_counts_legacy <- lyrics_tokens_legacy %>%
    inner_join(bing_lexicon_legacy, by = "word") %>%
    count(weekly_song_id, sentiment, name = "sentiment_n") %>%
    pivot_wider(names_from = sentiment, values_from = sentiment_n, values_fill = 0)

  if (!"positive" %in% names(sentiment_counts_legacy)) {
    sentiment_counts_legacy$positive <- 0
  }

  if (!"negative" %in% names(sentiment_counts_legacy)) {
    sentiment_counts_legacy$negative <- 0
  }

  sentiment_features_legacy <- sentiment_counts_legacy %>%
    right_join(lyrics_base_legacy %>% select(weekly_song_id, num_words), by = "weekly_song_id") %>%
    mutate(
      positive = replace_na(positive, 0),
      negative = replace_na(negative, 0),
      sentiment_balance = (positive - negative) / pmax(num_words, 1),
      emotionality_share = (positive + negative) / pmax(num_words, 1)
    ) %>%
    select(
      weekly_song_id,
      positive_word_count = positive,
      negative_word_count = negative,
      sentiment_balance,
      emotionality_share
    )
} else {
  sentiment_features_legacy <- lyrics_base_legacy %>%
    transmute(
      weekly_song_id,
      positive_word_count = NA_real_,
      negative_word_count = NA_real_,
      sentiment_balance = NA_real_,
      emotionality_share = NA_real_
    )
}

analysis_sample_main_model_legacy_lyrics_enriched <- analysis_sample_main_model_legacy %>%
  left_join(token_features_legacy, by = "weekly_song_id") %>%
  left_join(repetition_features_legacy, by = "weekly_song_id") %>%
  left_join(line_features_legacy, by = "weekly_song_id") %>%
  left_join(sentiment_features_legacy, by = "weekly_song_id") %>%
  mutate(
    no_content_tokens_flag = as.integer(is.na(content_token_count) | content_token_count == 0),
    very_short_lyrics_flag = as.integer(num_words < 100),
    top_5_word_share = replace_na(top_5_word_share, 0),
    top_word_share = replace_na(top_word_share, 0),
    content_unique_ratio = replace_na(content_unique_ratio, 0),
    content_token_count = replace_na(content_token_count, 0),
    sentiment_balance = replace_na(sentiment_balance, 0),
    emotionality_share = replace_na(emotionality_share, 0),
    self_reference_share = replace_na(self_reference_share, 0),
    second_person_share = replace_na(second_person_share, 0)
  )

if ("explicit" %in% names(analysis_sample_main_model_legacy_lyrics_enriched)) {
  analysis_sample_main_model_legacy_lyrics_enriched <- analysis_sample_main_model_legacy_lyrics_enriched %>%
    mutate(
      explicit_flag = case_when(
        is.na(explicit) ~ NA_integer_,
        explicit %in% c(TRUE, "TRUE", "true", "1", 1) ~ 1L,
        explicit %in% c(FALSE, "FALSE", "false", "0", 0) ~ 0L,
        TRUE ~ NA_integer_
      )
    )
} else {
  analysis_sample_main_model_legacy_lyrics_enriched <- analysis_sample_main_model_legacy_lyrics_enriched %>%
    mutate(explicit_flag = NA_integer_)
}

lyric_vars_candidate_legacy <- c(
  "num_words",
  "uniq_ratio",
  "avg_word_length",
  "top_word_share",
  "top_5_word_share",
  "repeated_line_share",
  "sentiment_balance",
  "emotionality_share",
  "self_reference_share",
  "second_person_share"
)

lyric_vars_candidate_legacy <- intersect(lyric_vars_candidate_legacy, names(analysis_sample_main_model_legacy_lyrics_enriched))

lyric_feature_missingness_legacy <- analysis_sample_main_model_legacy_lyrics_enriched %>%
  summarise(
    across(all_of(lyric_vars_candidate_legacy), ~ sum(is.na(.x)), .names = "missing_{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(
    variable = str_remove(variable, "^missing_"),
    n_total = nrow(analysis_sample_main_model_legacy_lyrics_enriched),
    pct_missing = n_missing / n_total
  ) %>%
  arrange(desc(n_missing))

candidate_predictors_legacy <- c(spotify_audio_cols, lyric_vars_candidate_legacy, "entry_year")

vif_candidate_legacy <- custom_vif_legacy(
  data = analysis_sample_main_model_legacy_lyrics_enriched,
  vars = candidate_predictors_legacy
) %>%
  arrange(desc(vif))

high_cor_pairs_candidate_legacy <- find_high_correlations_legacy(
  data = analysis_sample_main_model_legacy_lyrics_enriched,
  vars = candidate_predictors_legacy,
  cutoff = 0.70
)

# Final lyric decision:
# - repeated_line_share is not used because it had no useful variation in the working checks.
# - top_word_share is dropped because it overlaps strongly with top_5_word_share.
# - raw num_words is replaced by log_num_words in the final model.
lyric_vars_enriched_main_raw_legacy <- c(
  "num_words",
  "uniq_ratio",
  "avg_word_length",
  "top_5_word_share",
  "sentiment_balance",
  "emotionality_share",
  "self_reference_share",
  "second_person_share"
)

vif_enriched_raw_legacy <- custom_vif_legacy(
  data = analysis_sample_main_model_legacy_lyrics_enriched,
  vars = c(spotify_audio_cols, lyric_vars_enriched_main_raw_legacy, "entry_year")
) %>%
  arrange(desc(vif))

high_cor_pairs_enriched_raw_legacy <- find_high_correlations_legacy(
  data = analysis_sample_main_model_legacy_lyrics_enriched,
  vars = c(spotify_audio_cols, lyric_vars_enriched_main_raw_legacy, "entry_year"),
  cutoff = 0.70
)

print(lyric_feature_missingness_legacy)
print(vif_candidate_legacy, n = Inf)
print(high_cor_pairs_candidate_legacy)
print(vif_enriched_raw_legacy, n = Inf)
print(high_cor_pairs_enriched_raw_legacy)


# 13. Final feature engineering and standardization
# Reasoning:
# The final dataset is already cleaned and enriched. Before regression, we:
# 1) log-transform lyric length because raw word count is right-skewed;
# 2) standardize continuous predictors using z-scores;
# 3) use a binary instrumentalness flag in the main model because raw
#    instrumentalness is highly zero-inflated;
# 4) keep raw instrumentalness_z only for robustness checks;
# 5) create transformed outcomes and short-lyrics flags for robustness only.
#
# Important:
# - Only continuous predictors are standardized.
# - instrumentalness_any is a 0/1 dummy and is therefore not standardized.


# Variable blocks

audio_vars_continuous_main_raw_legacy <- c(
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "liveness",
  "valence",
  "tempo"
)

# Raw instrumentalness is zero-inflated, so it is not part of the main model.
# It is still standardized and kept for robustness checks only.
audio_vars_robustness_raw_legacy <- c(
  "instrumentalness"
)

lyric_vars_engineered_legacy <- c(
  "log_num_words",
  "uniq_ratio",
  "avg_word_length",
  "top_5_word_share",
  "sentiment_balance",
  "emotionality_share",
  "self_reference_share",
  "second_person_share"
)

control_vars_engineered_legacy <- c(
  "entry_year_c"
)

# Create engineered final dataset

final_sample_cleaned <- analysis_sample_main_model_legacy_lyrics_enriched %>%
  mutate(
    # Lyric length transformation
    log_num_words = log(num_words),
    
    # Centered year control for easier intercept interpretation
    entry_year_c = entry_year - mean(entry_year, na.rm = TRUE),
    
    # Main instrumentalness variable:
    # binary dummy because raw instrumentalness is highly zero-inflated.
    instrumentalness_any = as.integer(instrumentalness > 0.001),
    
    # Robustness-only outcome transformations
    log_time_to_peak = log(time_to_peak),
    log_weeks_on_chart = log(weeks_on_chart),
    log1p_rise_speed = log1p(rise_speed),
    log1p_decay_rate = log1p(decay_rate),
    
    # Robustness-only short-lyrics exclusion flag
    short_lyrics_flag = num_words < 100,

    # Matching-quality flags retained only for audit and later robustness checks.
    spotify_fuzzy_flag = as.integer(str_detect(str_to_lower(replace_na(spotify_match_tier, "")), "fuzzy|supplemental|accepted|reviewed")),
    lyric_fuzzy_or_supp_flag = as.integer(str_detect(str_to_lower(replace_na(lyric_match_tier, "")), "fuzzy|supplemental|accepted|reviewed"))
  ) %>%
  mutate(
    # Standardize continuous audio predictors used in the main model.
    across(
      all_of(audio_vars_continuous_main_raw_legacy),
      z_score,
      .names = "{.col}_z"
    ),
    
    # Standardize raw instrumentalness only for robustness checks.
    across(
      all_of(audio_vars_robustness_raw_legacy),
      z_score,
      .names = "{.col}_z"
    ),
    
    # Standardize engineered lyric predictors.
    across(
      all_of(lyric_vars_engineered_legacy),
      z_score,
      .names = "{.col}_z"
    ),
    
    # Standardize the centered year control.
    entry_year_c_z = z_score(entry_year_c),

    # Standardized internal artist-history controls for later modeling.
    log1p_artist_prior_top10_count_z = z_score(log1p_artist_prior_top10_count),
    artist_career_age_years_z = z_score(artist_career_age_years)
  )

# Creating Final predictor blocks

audio_vars_z_legacy <- paste0(audio_vars_continuous_main_raw_legacy, "_z")

audio_vars_robustness_z_legacy <- paste0(audio_vars_robustness_raw_legacy, "_z")


audio_vars_robustness_legacy <- c(
  "instrumentalness_any",
  audio_vars_robustness_z_legacy
)
# Main audio block:
# continuous audio features are standardized;
# instrumentalness_any remains a raw 0/1 dummy.
audio_vars_main_legacy <- c(
  "danceability_z",
  "energy_z",
  "loudness_z",
  "speechiness_z",
  "acousticness_z",
  "liveness_z",
  "valence_z",
  "tempo_z"
)

lyric_vars_z_legacy <- paste0(lyric_vars_engineered_legacy, "_z")

control_vars_z_legacy <- c(
  "entry_year_c_z"
)

main_predictors_legacy <- c(
  audio_vars_main_legacy,
  lyric_vars_z_legacy,
  control_vars_z_legacy,
  "log1p_artist_prior_top10_count_z",
  "artist_career_age_years_z"
)

# This object is used only to check that standardized continuous variables
# really have mean 0 and standard deviation 1.
standardized_predictors_check_legacy <- c(
  audio_vars_z_legacy,
  audio_vars_robustness_z_legacy,
  lyric_vars_z_legacy,
  control_vars_z_legacy
)

# Keep a full audit dataset and a slim modeling-ready dataset

# Full audit dataset keeps the construction and matching variables needed to verify the final sample.
final_sample_cleaned_audit <- final_sample_cleaned %>%
  select(everything())

# Keeping the thesis modeling variables plus the minimum additions needed for robustness checks
current_final_core_legacy <- c(
  "weekly_song_id",
  "song",
  "artist",
  "entry_year",
  "entry_year_c",
  "time_to_peak",
  "rise_speed",
  "decay_rate",
  "weeks_on_chart",
  audio_vars_continuous_main_raw_legacy,
  "instrumentalness",
  "instrumentalness_any",
  "num_words",
  "log_num_words",
  "uniq_ratio",
  "avg_word_length",
  "top_5_word_share",
  "sentiment_balance",
  "emotionality_share",
  "self_reference_share",
  "second_person_share",
  audio_vars_z_legacy,
  audio_vars_robustness_z_legacy,
  lyric_vars_z_legacy,
  control_vars_z_legacy,
  "log_time_to_peak",
  "log_weeks_on_chart",
  "log1p_rise_speed",
  "log1p_decay_rate",
  "short_lyrics_flag"
)

minimum_additions_current <- c(
  "main_artist_key",
  "artist_cluster_id",
  "featured_artist_flag",
  "n_credit_artists",
  "artist_prior_hot100_count",
  "artist_prior_top10_count",
  "log1p_artist_prior_top10_count",
  "log1p_artist_prior_top10_count_z",
  "artist_prior_weeks_on_chart",
  "artist_first_hot100_year",
  "artist_career_age_years",
  "artist_career_age_years_z",
  "entry_year_factor",
  "entry_era",
  "post_streaming_era_flag",
  "covid_era_flag",
  "sample_2000_2020_flag",
  "peak_week_index",
  "weeks_to_peak_0",
  "log1p_weeks_to_peak_0",
  "peaked_on_entry_flag",
  "rise_speed_observed_flag",
  "decay_rate_observed_flag",
  "decay_missing_flag",
  "entry_date",
  "last_chart_date",
  "right_censored_flag",
  "peak_date",
  "entry_rank",
  "peak_rank",
  "final_rank",
  "decay_weeks",
  "has_chart_gap",
  "n_chart_gaps_gt_1",
  "max_chart_gap_weeks",
  "long_gap_8_flag",
  "long_gap_12_flag",
  "long_gap_flag",
  "first_run_weeks_on_chart",
  "first_run_decay_rate",
  "spotify_match_tier",
  "spotify_match_score",
  "spotify_song_distance",
  "spotify_artist_distance",
  "lyric_match_tier",
  "lyric_match_score",
  "lyric_song_distance",
  "lyric_artist_distance",
  "spotify_fuzzy_flag",
  "lyric_fuzzy_or_supp_flag",
  "token_count_check",
  "content_token_count",
  "no_content_tokens_flag",
  "very_short_lyrics_flag",
  "explicit_flag"
)

final_sample_cleaned <- final_sample_cleaned_audit %>%
  select(any_of(unique(c(current_final_core_legacy, minimum_additions_current))))

# Robustness sample excluding very short-lyric songs.
final_sample_no_short_lyrics <- final_sample_cleaned %>%
  filter(!short_lyrics_flag)

# ----------------------------
# Instrumentalness checks
# ----------------------------

instrumentalness_distribution_legacy <- final_sample_cleaned %>%
  summarise(
    n = n(),
    instrumentalness_zero = sum(instrumentalness == 0, na.rm = TRUE),
    instrumentalness_nonzero = sum(instrumentalness > 0, na.rm = TRUE),
    instrumentalness_above_001 = sum(instrumentalness > 0.001, na.rm = TRUE),
    instrumentalness_above_01 = sum(instrumentalness > 0.01, na.rm = TRUE),
    instrumentalness_any_zero = sum(instrumentalness_any == 0, na.rm = TRUE),
    instrumentalness_any_one = sum(instrumentalness_any == 1, na.rm = TRUE),
    min_instrumentalness = min(instrumentalness, na.rm = TRUE),
    median_instrumentalness = median(instrumentalness, na.rm = TRUE),
    mean_instrumentalness = mean(instrumentalness, na.rm = TRUE),
    max_instrumentalness = max(instrumentalness, na.rm = TRUE)
  )

instrumentalness_high_examples_legacy <- final_sample_cleaned %>%
  arrange(desc(instrumentalness)) %>%
  select(
    song,
    artist,
    entry_year,
    instrumentalness,
    instrumentalness_z,
    instrumentalness_any
  ) %>%
  slice_head(n = 20)

# Standardization check

standardization_check_legacy <- final_sample_cleaned %>%
  summarise(
    across(
      all_of(standardized_predictors_check_legacy),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "stat",
    values_to = "value"
  )

# Let's print key Section 13 checks

print(instrumentalness_distribution_legacy)
print(instrumentalness_high_examples_legacy)
print(standardization_check_legacy, n = Inf)


# 14. Final regression datasets
# Outcome-specific datasets filter only on the outcome that is structurally observed; missing rise and decay values are not imputed.
# Outcome-specific datasets are created by filtering only on the relevant outcome.
# I will not impute rise_speed or decay_rate.
# I willreplace structural missing values with zero.

model_data_ttp_engineered_legacy <- make_model_data_legacy(
  final_sample_cleaned,
  outcome = "time_to_peak"
)

model_data_weeks_engineered_legacy <- make_model_data_legacy(
  final_sample_cleaned,
  outcome = "weeks_on_chart"
)

model_data_rise_engineered_legacy <- make_model_data_legacy(
  final_sample_cleaned,
  outcome = "rise_speed"
)

model_data_decay_engineered_legacy <- make_model_data_legacy(
  final_sample_cleaned,
  outcome = "decay_rate"
)

model_data_summary_engineered_legacy <- bind_rows(
  model_data_ttp_engineered_legacy %>%
    summarise(model_data = "model_data_ttp_engineered_legacy", n_songs = n()),
  
  model_data_weeks_engineered_legacy %>%
    summarise(model_data = "model_data_weeks_engineered_legacy", n_songs = n()),
  
  model_data_rise_engineered_legacy %>%
    summarise(model_data = "model_data_rise_engineered_legacy", n_songs = n()),
  
  model_data_decay_engineered_legacy %>%
    summarise(model_data = "model_data_decay_engineered_legacy", n_songs = n())
)

print(model_data_summary_engineered_legacy)


# 15. Final diagnostics and readiness checks
# Final diagnostics confirm that the sample is ready for the thesis regressions and robustness checks.

# Predictor correlation checks
# VIF calculation is intentionally deferred to the later modeling script, because this
# data-engineering script should not call modeling functions such as lm().

vif_engineered_legacy <- tibble(
  note = ""
)

high_cor_pairs_engineered_legacy <- find_high_correlations_legacy(
  data = final_sample_cleaned,
  vars = main_predictors_legacy,
  cutoff = 0.70
)

# Final regression-readiness checks

final_regression_readiness_legacy <- final_sample_cleaned %>%
  summarise(
    n_songs = n(),
    duplicate_weekly_song_ids = sum(duplicated(weekly_song_id)),
    
    min_entry_year = min(entry_year, na.rm = TRUE),
    max_entry_year = max(entry_year, na.rm = TRUE),
    
    invalid_time_to_peak = sum(time_to_peak < 1 | time_to_peak > weeks_on_chart, na.rm = TRUE),
    invalid_weeks_on_chart = sum(weeks_on_chart < 3, na.rm = TRUE),
    
    missing_time_to_peak = sum(is.na(time_to_peak)),
    missing_weeks_on_chart = sum(is.na(weeks_on_chart)),
    missing_rise_speed = sum(is.na(rise_speed)),
    missing_decay_rate = sum(is.na(decay_rate)),
    
    across(
      all_of(main_predictors_legacy),
      ~ sum(is.na(.x)),
      .names = "missing_{.col}"
    ),

    missing_log1p_artist_prior_top10_count = sum(is.na(log1p_artist_prior_top10_count)),
    missing_log1p_artist_prior_top10_count_z = sum(is.na(log1p_artist_prior_top10_count_z)),
    peaked_on_entry_count = sum(peaked_on_entry_flag == 1, na.rm = TRUE),
    long_gap_count = sum(long_gap_flag == 1, na.rm = TRUE),
    lyric_fuzzy_or_supp_count = sum(lyric_fuzzy_or_supp_flag == 1, na.rm = TRUE)
  )

# Structural missingness checks require decay_weeks, which is kept in the
# broader song-level sample but intentionally excluded from the final regression dataset.
structural_missingness_legacy <- final_sample_cleaned %>%
  summarise(
    missing_rise_speed = sum(is.na(rise_speed)),
    songs_peaking_in_first_observed_week = sum(peaked_on_entry_flag == 1, na.rm = TRUE),
    rise_speed_structural = missing_rise_speed == songs_peaking_in_first_observed_week,
    
    missing_decay_rate = sum(is.na(decay_rate)),
    songs_with_no_post_peak_weeks = sum(decay_weeks == 0, na.rm = TRUE),
    decay_rate_structural = missing_decay_rate == songs_with_no_post_peak_weeks
  )

trajectory_validity_checks_legacy <- final_sample_cleaned %>%
  summarise(
    n_songs = n(),
    time_to_peak_outside_observed_life =
      sum(time_to_peak < 1 | time_to_peak > weeks_on_chart, na.rm = TRUE),
    negative_decay_weeks = sum(decay_weeks < 0, na.rm = TRUE),
    
    rise_speed_formula_errors = sum(
      time_to_peak > 1 &
        abs(rise_speed - ((entry_rank - peak_rank) / (time_to_peak - 1))) > 1e-8,
      na.rm = TRUE
    ),
    
    decay_rate_formula_errors = sum(
      decay_weeks > 0 &
        abs(decay_rate - ((final_rank - peak_rank) / decay_weeks)) > 1e-8,
      na.rm = TRUE
    )
  )

final_missingness_all_legacy <- final_sample_cleaned %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    n_total = nrow(final_sample_cleaned),
    pct_missing = n_missing / n_total
  ) %>%
  arrange(desc(n_missing))

main_predictor_missing_cells_legacy <- final_sample_cleaned %>%
  summarise(across(all_of(main_predictors_legacy), ~ sum(is.na(.x)))) %>%
  as.matrix() %>%
  sum()

regression_ready_legacy <- final_regression_readiness_legacy %>%
  transmute(
    base_sample_ready =
      duplicate_weekly_song_ids == 0 &
      min_entry_year >= 2000 &
      max_entry_year <= 2021 &
      invalid_time_to_peak == 0 &
      invalid_weeks_on_chart == 0 &
      missing_time_to_peak == 0 &
      missing_weeks_on_chart == 0,
    
    no_missing_main_predictor_cells =
      main_predictor_missing_cells_legacy == 0,
    
    structural_rise_speed_missingness =
      structural_missingness_legacy$rise_speed_structural,
    
    structural_decay_rate_missingness =
      structural_missingness_legacy$decay_rate_structural,
    
    ready_for_main_regressions =
      base_sample_ready &
      no_missing_main_predictor_cells &
      structural_rise_speed_missingness &
      structural_decay_rate_missingness
  )

# Final diagnostic tables

# 1) Artist identity diagnostics.
diagnostic_artist_identity_summary_current <- final_sample_cleaned %>%
  summarise(
    diagnostic = "artist_identity_summary",
    n_songs = n(),
    unique_main_artist_keys = n_distinct(main_artist_key),
    featured_artist_songs = sum(featured_artist_flag == 1, na.rm = TRUE),
    featured_artist_share = featured_artist_songs / n_songs,
    min_n_credit_artists = min(n_credit_artists, na.rm = TRUE),
    median_n_credit_artists = median(n_credit_artists, na.rm = TRUE),
    mean_n_credit_artists = mean(n_credit_artists, na.rm = TRUE),
    max_n_credit_artists = max(n_credit_artists, na.rm = TRUE)
  )

diagnostic_artist_identity_top20_current <- final_sample_cleaned %>%
  count(main_artist_key, sort = TRUE, name = "n_songs") %>%
  slice_head(n = 20) %>%
  mutate(diagnostic = "top20_artists")

diagnostic_artist_identity_current <- bind_rows(
  diagnostic_artist_identity_summary_current %>% mutate(main_artist_key = NA_character_),
  diagnostic_artist_identity_top20_current %>%
    mutate(
      unique_main_artist_keys = NA_integer_,
      featured_artist_songs = NA_integer_,
      featured_artist_share = NA_real_,
      min_n_credit_artists = NA_integer_,
      median_n_credit_artists = NA_real_,
      mean_n_credit_artists = NA_real_,
      max_n_credit_artists = NA_integer_
    ) %>%
    select(names(diagnostic_artist_identity_summary_current), main_artist_key)
)

# 2) Artist-history diagnostics.
diagnostic_artist_history_summary_current <- final_sample_cleaned %>%
  summarise(
    diagnostic = "artist_history_summary",
    min_prior_hot100 = min(artist_prior_hot100_count, na.rm = TRUE),
    median_prior_hot100 = median(artist_prior_hot100_count, na.rm = TRUE),
    mean_prior_hot100 = mean(artist_prior_hot100_count, na.rm = TRUE),
    max_prior_hot100 = max(artist_prior_hot100_count, na.rm = TRUE),
    min_prior_top10 = min(artist_prior_top10_count, na.rm = TRUE),
    median_prior_top10 = median(artist_prior_top10_count, na.rm = TRUE),
    mean_prior_top10 = mean(artist_prior_top10_count, na.rm = TRUE),
    max_prior_top10 = max(artist_prior_top10_count, na.rm = TRUE),
    min_prior_weeks = min(artist_prior_weeks_on_chart, na.rm = TRUE),
    median_prior_weeks = median(artist_prior_weeks_on_chart, na.rm = TRUE),
    mean_prior_weeks = mean(artist_prior_weeks_on_chart, na.rm = TRUE),
    max_prior_weeks = max(artist_prior_weeks_on_chart, na.rm = TRUE),
    songs_zero_prior_top10 = sum(artist_prior_top10_count == 0, na.rm = TRUE),
    songs_at_least_one_prior_top10 = sum(artist_prior_top10_count > 0, na.rm = TRUE)
  )

diagnostic_artist_history_top20_current <- final_sample_cleaned %>%
  arrange(desc(artist_prior_top10_count), desc(artist_prior_hot100_count), song, artist) %>%
  select(
    song,
    artist,
    entry_year,
    artist_prior_hot100_count,
    artist_prior_top10_count,
    artist_prior_weeks_on_chart,
    artist_first_hot100_year,
    artist_career_age_years
  ) %>%
  slice_head(n = 20)

# 3) Entry-year and era diagnostics.
diagnostic_entry_year_current <- final_sample_cleaned %>%
  count(entry_year, name = "n_songs") %>%
  mutate(diagnostic = "entry_year")

diagnostic_entry_era_current <- final_sample_cleaned %>%
  count(entry_era, name = "n_songs") %>%
  mutate(diagnostic = "entry_era")

diagnostic_entry_year_era_current <- bind_rows(
  diagnostic_entry_year_current %>% mutate(entry_era = NA_character_) %>% select(diagnostic, entry_year, entry_era, n_songs),
  diagnostic_entry_era_current %>% mutate(entry_year = NA_integer_) %>% select(diagnostic, entry_year, entry_era, n_songs)
)

# 4) Time-to-peak definition diagnostics.
time_to_peak_zero_based_check_current <- any(final_sample_cleaned$time_to_peak == 0, na.rm = TRUE)

diagnostic_time_to_peak_definition_current <- tibble(
  n_songs = nrow(final_sample_cleaned),
  min_time_to_peak = min(final_sample_cleaned$time_to_peak, na.rm = TRUE),
  max_time_to_peak = max(final_sample_cleaned$time_to_peak, na.rm = TRUE),
  has_zero_time_to_peak = time_to_peak_zero_based_check_current,
  min_peak_week_index = min(final_sample_cleaned$peak_week_index, na.rm = TRUE),
  min_weeks_to_peak_0 = min(final_sample_cleaned$weeks_to_peak_0, na.rm = TRUE),
  max_weeks_to_peak_0 = max(final_sample_cleaned$weeks_to_peak_0, na.rm = TRUE),
  peaked_on_entry_count = sum(final_sample_cleaned$peaked_on_entry_flag == 1, na.rm = TRUE),
  missing_rise_speed = sum(is.na(final_sample_cleaned$rise_speed)),
  missing_rise_equals_peaked_on_entry = sum(is.na(final_sample_cleaned$rise_speed)) ==
    sum(final_sample_cleaned$peaked_on_entry_flag == 1, na.rm = TRUE),
  missing_decay_rate = sum(is.na(final_sample_cleaned$decay_rate))
)

# 5) Chart-gap diagnostics.
diagnostic_chart_gaps_summary_current <- final_sample_cleaned %>%
  summarise(
    diagnostic = "chart_gap_summary",
    n_songs = n(),
    songs_with_chart_gap = sum(has_chart_gap, na.rm = TRUE),
    share_with_chart_gap = songs_with_chart_gap / n_songs,
    min_max_chart_gap_weeks = min(max_chart_gap_weeks, na.rm = TRUE),
    median_max_chart_gap_weeks = median(max_chart_gap_weeks, na.rm = TRUE),
    mean_max_chart_gap_weeks = mean(max_chart_gap_weeks, na.rm = TRUE),
    max_max_chart_gap_weeks = max(max_chart_gap_weeks, na.rm = TRUE),
    songs_with_long_gap_8 = sum(long_gap_8_flag == 1, na.rm = TRUE),
    share_with_long_gap_8 = songs_with_long_gap_8 / n_songs,
    songs_with_long_gap_12 = sum(long_gap_12_flag == 1, na.rm = TRUE),
    share_with_long_gap_12 = songs_with_long_gap_12 / n_songs
  )

diagnostic_chart_gaps_top30_current <- final_sample_cleaned %>%
  arrange(desc(max_chart_gap_weeks), song, artist) %>%
  select(
    song,
    artist,
    entry_year,
    weeks_on_chart,
    time_to_peak,
    has_chart_gap,
    n_chart_gaps_gt_1,
    max_chart_gap_weeks,
    long_gap_flag
  ) %>%
  slice_head(n = 30)

# 6) Matching-quality diagnostics.
diagnostic_matching_spotify_current <- final_sample_cleaned %>%
  count(spotify_match_tier, spotify_fuzzy_flag, name = "n_songs") %>%
  mutate(source = "spotify")

diagnostic_matching_lyrics_current <- final_sample_cleaned %>%
  count(lyric_match_tier, lyric_fuzzy_or_supp_flag, name = "n_songs") %>%
  mutate(source = "lyrics")

diagnostic_matching_quality_current <- bind_rows(
  diagnostic_matching_spotify_current %>%
    transmute(source, match_tier = spotify_match_tier, flag = spotify_fuzzy_flag, n_songs),
  diagnostic_matching_lyrics_current %>%
    transmute(source, match_tier = lyric_match_tier, flag = lyric_fuzzy_or_supp_flag, n_songs)
)

# 7) Lyric QC diagnostics.
diagnostic_lyric_qc_current <- final_sample_cleaned %>%
  summarise(
    n_songs = n(),
    missing_token_count_check = sum(is.na(token_count_check)),
    missing_content_token_count = sum(is.na(content_token_count)),
    min_token_count_check = min(token_count_check, na.rm = TRUE),
    median_token_count_check = median(token_count_check, na.rm = TRUE),
    mean_token_count_check = mean(token_count_check, na.rm = TRUE),
    max_token_count_check = max(token_count_check, na.rm = TRUE),
    min_content_token_count = min(content_token_count, na.rm = TRUE),
    median_content_token_count = median(content_token_count, na.rm = TRUE),
    mean_content_token_count = mean(content_token_count, na.rm = TRUE),
    max_content_token_count = max(content_token_count, na.rm = TRUE),
    min_num_words = min(num_words, na.rm = TRUE),
    median_num_words = median(num_words, na.rm = TRUE),
    mean_num_words = mean(num_words, na.rm = TRUE),
    max_num_words = max(num_words, na.rm = TRUE),
    very_short_lyrics_count = sum(very_short_lyrics_flag == 1, na.rm = TRUE),
    no_content_tokens_count = sum(no_content_tokens_flag == 1, na.rm = TRUE),
    explicit_missing = sum(is.na(explicit_flag)),
    explicit_yes = sum(explicit_flag == 1, na.rm = TRUE),
    explicit_no = sum(explicit_flag == 0, na.rm = TRUE)
  )

# 8) Final variable missingness.
diagnostic_final_variable_missingness_current <- final_sample_cleaned %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(
    n_total = nrow(final_sample_cleaned),
    pct_missing = n_missing / n_total
  ) %>%
  arrange(desc(n_missing), variable)

# 9) Final validation summary.
final_validation_summary_current <- tibble(
  final_main_sample_rows = nrow(final_sample_cleaned),
  full_audit_dataset_columns = ncol(final_sample_cleaned_audit),
  slim_modeling_dataset_columns = ncol(final_sample_cleaned),
  duplicate_weekly_song_ids = sum(duplicated(final_sample_cleaned$weekly_song_id)),
  missing_time_to_peak = sum(is.na(final_sample_cleaned$time_to_peak)),
  missing_weeks_on_chart = sum(is.na(final_sample_cleaned$weeks_on_chart)),
  missing_rise_speed = sum(is.na(final_sample_cleaned$rise_speed)),
  missing_decay_rate = sum(is.na(final_sample_cleaned$decay_rate)),
  missing_existing_main_predictor_cells = main_predictor_missing_cells_legacy,
  missing_artist_prior_top10_count = sum(is.na(final_sample_cleaned$artist_prior_top10_count)),
  missing_log1p_artist_prior_top10_count = sum(is.na(final_sample_cleaned$log1p_artist_prior_top10_count)),
  peaked_on_entry_count = sum(final_sample_cleaned$peaked_on_entry_flag == 1, na.rm = TRUE),
  missing_rise_equals_peaked_on_entry = sum(is.na(final_sample_cleaned$rise_speed)) ==
    sum(final_sample_cleaned$peaked_on_entry_flag == 1, na.rm = TRUE),
  long_gap_count = sum(final_sample_cleaned$long_gap_flag == 1, na.rm = TRUE),
  lyric_fuzzy_or_supp_count = sum(final_sample_cleaned$lyric_fuzzy_or_supp_flag == 1, na.rm = TRUE)
)

# Save all new diagnostic tables.
safe_write_csv_current(diagnostic_artist_identity_current, file.path(diagnostics_dir, "diagnostic_artist_identity.csv"))
safe_write_csv_current(diagnostic_artist_history_summary_current, file.path(diagnostics_dir, "diagnostic_artist_history.csv"))
safe_write_csv_current(diagnostic_artist_history_top20_current, file.path(diagnostics_dir, "diagnostic_artist_history_top20.csv"))
safe_write_csv_current(diagnostic_entry_year_era_current, file.path(diagnostics_dir, "diagnostic_entry_year_era.csv"))
safe_write_csv_current(diagnostic_time_to_peak_definition_current, file.path(diagnostics_dir, "diagnostic_time_to_peak_definition.csv"))
safe_write_csv_current(diagnostic_chart_gaps_summary_current, file.path(diagnostics_dir, "diagnostic_chart_gaps.csv"))
safe_write_csv_current(diagnostic_chart_gaps_top30_current, file.path(diagnostics_dir, "diagnostic_chart_gaps_top30.csv"))
safe_write_csv_current(diagnostic_matching_quality_current, file.path(diagnostics_dir, "diagnostic_matching_quality.csv"))
safe_write_csv_current(diagnostic_lyric_qc_current, file.path(diagnostics_dir, "diagnostic_lyric_qc.csv"))
safe_write_csv_current(diagnostic_final_variable_missingness_current, file.path(diagnostics_dir, "diagnostic_final_variable_missingness.csv"))
safe_write_csv_current(final_validation_summary_current, file.path(diagnostics_dir, "diagnostic_final_validation_summary.csv"))

# Print final diagnostics

print(vif_engineered_legacy, n = Inf)
print(high_cor_pairs_engineered_legacy)
print(final_regression_readiness_legacy)
print(structural_missingness_legacy)
print(trajectory_validity_checks_legacy)
print(final_missingness_all_legacy, n = Inf)
print(regression_ready_legacy)
print(final_validation_summary_current)

# Save a backup of any existing slim final dataset before overwriting it.
final_slim_output_path_current <- file.path(output_dir, "final_sample_cleaned.csv")
final_audit_output_path_current <- file.path(output_dir, "final_sample_cleaned_audit.csv")

if (isTRUE(save_outputs) && file.exists(final_slim_output_path_current)) {
  backup_path_current <- file.path(
    output_dir,
    paste0(
      "final_sample_cleaned_backup_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".csv"
    )
  )
  file.copy(final_slim_output_path_current, backup_path_current, overwrite = FALSE)
}

safe_write_csv_current(
  final_sample_cleaned_audit,
  final_audit_output_path_current
)

safe_write_csv_current(
  final_sample_cleaned,
  final_slim_output_path_current
)


