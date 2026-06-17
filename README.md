# Thesis_code_643280


This repository contains the R code used for my master thesis analysis on Billboard Hot 100 Top 10 songs. The project examines whether audio and lyrical features help explain differences in successful songs’ chart trajectories.

The analysis combines three data sources: weekly Billboard Hot 100 chart data(https://github.com/TaylorVellios/Spotify_and_the_Billboard_100/tree/main), Spotify audio features(https://github.com/TaylorVellios/Spotify_and_the_Billboard_100/tree/main), and lyric-based variables (https://www.kaggle.com/datasets/pat143u/spotify-dataset-1960-2022-with-audio-features). After cleaning and matching the data, the final sample is restricted to songs that reached the Billboard Hot 100 Top 10 between 2000 and 2021 and have both Spotify and lyrics information available.

## Repository structure

The code is split into three scripts that should be run in order.

### `01_clean_final_sample.R`

This script prepares the final analysis dataset.

It imports the three raw datasets, standardizes song titles and artist names, and matches Billboard songs to Spotify audio features and lyric data. Since the Billboard data are weekly chart observations, the script first converts them into song-level chart trajectories. It then constructs the main trajectory outcomes, control variables, audio variables, lyric variables, and robustness-related indicators.

The main output of this step is:

```text
final_sample_cleaned.csv
```

This is the final song-level dataset used in the regression and machine-learning analysis.

### `02_model_regression_ml.R`

This script runs the main empirical analysis.

It loads `final_sample_cleaned.csv`, checks that the final sample and variables are correctly defined, and estimates the main OLS feature-block models. The models compare controls-only, audio, lyrics, and combined specifications across four trajectory outcomes:

```text
time_to_peak
weeks_on_chart
rise_speed
decay_rate
```

The script also produces descriptive statistics, correlation and VIF diagnostics, robustness checks, alternative model specifications, and machine-learning comparisons against simple baseline models.

Main outputs are saved in the `outputs/` folder, including model fit tables, coefficient tables, robustness results, and machine-learning comparison files.

### `03_results_tables_figures.R`

This script prepares the final tables and figures used for reporting the results.

It reads the outputs created by the modeling script and formats them into thesis-ready tables and figures. The purpose of this script is only presentation: it does not change the analysis or re-estimate the models.

## Data folder

The raw datasets should be placed in a folder called:

```text
master_thesis_data/
```

The expected input files are:

```text
Clean_Billboard.csv
SpotifyFeatures_All_1990_to_2021.csv
billboard-lyrics-spotify.csv
```

## How to run the code

Run the scripts in this order from the project root folder:

```r
source("01_clean_final_sample.R")
source("02_model_regression_ml.R")
source("03_results_tables_figures.R")
```

The scripts assume that the raw data files are stored in `master_thesis_data/`.

## Main analysis design

The analysis is designed to study variation among songs that already achieved major chart success. For this reason, the code does not try to classify songs as hits or non-hits. Instead, it constructs a final sample of Billboard Hot 100 Top 10 songs and examines how their chart trajectories differ after entering the chart.

The code follows three main stages: preparing the final sample, estimating the empirical models, and formatting the final results.

### 1. Preparing the final sample

The first script, `01_clean_final_sample.R`, builds the final analysis dataset from the raw data files.

The script starts by importing the three datasets used in the thesis: weekly Billboard Hot 100 chart data, Spotify audio features, and lyric data. The Billboard file is the main source for constructing chart trajectories because it records songs week by week. The Spotify file provides audio characteristics, while the lyrics file provides the text information used to create lyric-based variables.

After importing the data, the script cleans the song and artist names. This step is necessary because the same song can be written differently across datasets. For example, a title may include punctuation, brackets, remix labels, or version information in one file but not in another. Artist names can also differ because of featured artists, collaborations, spelling differences, or different separator formats. To deal with this, the script creates several cleaned matching keys, including strict song keys, relaxed song keys, full artist keys, primary artist keys, and core artist keys.

The Billboard data are then converted from weekly observations into song-level observations. Since the thesis analyzes song-level trajectories, each song needs to appear once in the final sample. For each song, the script identifies the first chart date, last chart date, entry year, entry rank, best rank, first week of peak rank, final observed rank, and total number of observed chart weeks. These values are later used to construct the main outcome variables.

The script also constructs artist-history variables from the Billboard data. For each song, it calculates the artist’s prior Hot 100 history and prior Top 10 history before that song entered the chart. Same-date entries by the same artist are not counted as prior history, because they do not represent earlier chart experience. This produces the main artist-history control used in the regression models.

The Spotify data are prepared separately before matching. The script cleans Spotify song and artist names using the same logic as for Billboard. It also handles duplicate Spotify records by keeping one cleaned song-artist record. When multiple Spotify rows refer to the same cleaned song and artist, the script prioritizes the record with the strongest source-reported chart information and keeps diagnostic information on how many rows were collapsed.

The Spotify-to-Billboard match is then carried out in tiers. The code first attempts stricter exact matches, then gradually uses more relaxed exact matches. Only after these steps does it use conservative fuzzy matching for remaining unmatched cases. This structure is used so that the safest matches are prioritized before approximate matches are considered. Match tiers and distance measures are kept as diagnostics and later used for robustness checks.

The lyrics data are prepared and matched in a similar way. The script cleans song and artist names, removes duplicate lyric records where needed, and matches lyrics to the Billboard song-level data. Exact matching is attempted first, followed by conservative fuzzy matching and reviewed supplemental matches where applicable. The script also keeps flags for fuzzy or supplemental lyric matches so that these cases can be excluded in robustness checks.

After Billboard, Spotify, and lyrics data are matched, the script constructs the main trajectory outcomes. `time_to_peak` measures how long it takes a song to reach its best chart position. `weeks_on_chart` measures total observed chart duration. `rise_speed` captures how quickly a song moves upward before reaching its peak. `decay_rate` captures how quickly it declines after its peak. Some outcomes are structurally missing for certain songs. For example, `rise_speed` is not defined for songs that peak in their first observed week, and `decay_rate` is not defined when no post-peak week is observed.

The script then creates the final explanatory variables. Spotify audio features are standardized for comparability. Lyric variables are engineered from the lyric text, including measures such as lyric length, lexical uniqueness, repetition, sentiment balance, emotionality, self-reference, and second-person address. Control variables are also prepared, including centered and standardized entry year, prior artist Top 10 history, and featured-artist status.

Finally, the script applies the main sample restriction. The final dataset keeps songs that reached the Billboard Hot 100 Top 10, entered between 2000 and 2021, and have the required Spotify and lyric information. The final output is saved as:

```text
final_sample_cleaned.csv
```

This is the dataset used by the modeling script.

### 2. Estimating the main models

The second script, `02_model_regression_ml.R`, runs the empirical analysis using `final_sample_cleaned.csv`.

The script first loads the final sample and checks that the required variables are present. It verifies that the sample has the expected structure, including one row per song, no duplicated song IDs, no missing values in the main predictors, and the expected outcome-specific sample sizes.

The four main outcomes are:

```text
time_to_peak
weeks_on_chart
rise_speed
decay_rate
```

The script then defines the predictor groups used in the feature-block regressions. The control variables are entry year, prior artist Top 10 history, and featured-artist status. The audio block contains standardized Spotify audio features. The lyrics block contains standardized lyric-based variables. These groups are combined into the model specifications used in the main OLS analysis.

The main OLS analysis compares five model blocks for each outcome:

```text
null model
controls-only model
audio model
lyrics model
combined model
```

The null model contains no predictors and serves as a baseline. The controls-only model includes only the control variables. The audio model adds Spotify audio features to the controls. The lyrics model adds lyric variables to the controls. The combined model includes controls, audio features, and lyric variables together.

Each model block is estimated separately for each trajectory outcome. This produces a structured comparison of whether audio features, lyric features, or their combination explain additional variation beyond the controls. The script saves model fit statistics such as R-squared, adjusted R-squared, RMSE, MAE, AIC, and BIC.

The script also extracts coefficient tables from the combined models. Standard errors are calculated in multiple ways, including conventional standard errors, HC3 robust standard errors, and artist-clustered standard errors where possible. The preferred inference method is artist-clustered standard errors when available, otherwise HC3 robust standard errors are used.

Because many coefficients are tested across multiple outcomes and predictors, the script applies Benjamini-Hochberg correction. This separates stronger coefficient-level patterns from weaker exploratory associations. The output includes both raw and adjusted significance indicators.

### 3. Robustness checks and alternative specifications

After the main OLS models, the script runs several robustness checks. These checks are included to see whether the main findings depend heavily on one specific modeling choice or sample definition.

The robustness checks include alternative time controls, such as era controls and year fixed effects. They also include sample restrictions, such as excluding 2021 entrants, excluding right-censored observations, excluding songs with long chart gaps, excluding fuzzy or supplemental lyric matches, and excluding Spotify fuzzy matches. These checks help assess whether the findings are sensitive to recent chart entries, incomplete chart trajectories, interrupted chart runs, or less certain matches.

The script also estimates alternative specifications using related controls and alternative outcome definitions. These are not meant to replace the main models, but to check whether the same broad patterns appear when reasonable changes are made to the empirical setup.

### 4. Machine-learning extension

The modeling script also includes a machine-learning extension. This part of the analysis is predictive rather than explanatory.

The machine-learning models are used to test whether more flexible methods improve out-of-sample prediction of the four trajectory outcomes. Their performance is compared with a simple null mean baseline and cross-validated OLS. The purpose is to assess whether audio, lyric, and control variables contain enough signal to predict trajectory differences among already successful songs.

The output compares predictive performance using metrics such as RMSE, MAE, and out-of-sample R-squared.

### 5. Formatting tables and figures

The third script, `03_results_tables_figures.R`, prepares the final tables and figures.

This script does not rerun the analysis or change the results. It reads the model outputs from the second script, applies readable labels, formats the tables, and creates the final figures used for reporting. Keeping this step separate makes the workflow clearer because the statistical analysis and the presentation of results are handled in different scripts.
