###--------------------------------------------------------------------------###
###   IPL Impact Player Rule — Synthetic Control Analysis                    ###
###   Script: IPL_02_Synthetic_Control.R                                     ###
###                                                                          ###
###   Question: What would IPL run rates, all-rounder usage, and            ###
###   competitive balance have looked like WITHOUT the Impact Player rule?   ###
###                                                                          ###
###   Method: Synthetic Control (Abadie et al.)                             ###
###   Treatment unit: IPL (treated 2023)                                    ###
###   Donor pool: BBL, PSL, CPL (never adopted Impact Player rule)          ###
###   Outcomes: run_rate, allrounders_per_match, avg_margin                 ###
###--------------------------------------------------------------------------###

rm(list = ls())

# ---- 0. Packages ----
pacman::p_load(
  tidyverse, glue, cricketdata,
  Synth,       # synthetic control
  ggthemes, scales, patchwork
)


## portion on synthetic control --> synthetic IPL
# Check what competitions cricketdata has
cricketdata::fetch_cricsheet(
  competition = "bbl",
  gender = "male",
  type = "match"
) %>% 
  count(season)

cricketdata::fetch_cricsheet(
  competition = "psl", 
  gender = "male",
  type = "match"
) %>%
  count(season)


cricketdata::fetch_cricsheet(
  competition = "cpl",
  gender = "male",
  type = "match"
) %>% 
  count(season)

cricketdata::fetch_cricsheet(
  competition = "sa20",
  gender = "male",
  type = "match"
) %>%
  count(season)

cricketdata::fetch_cricsheet(
  competition = "hundred_men",
  gender = "male",
  type = "match"
) %>%
  count(season)

cricketdata::fetch_cricsheet(
  competition = "hundred_men",
  gender = "male",
  type = "match"
) %>%
  count(season)

###--------------------------------------------------------------------------###
###   SECTION 1: Load & Process All Leagues                                  ###
###--------------------------------------------------------------------------###

# message("Loading ball-by-ball data for all leagues...")
# 
# # Helper: fetch bbb + compute season-level metrics
# get_league_metrics <- function(competition, league_name) {
#   
#   message(glue("Fetching {league_name}..."))
#   
#   # Ball by ball
#   bbb <- fetch_cricsheet(competition = competition,
#                          gender = "male",
#                          type = "bbb")
#   
#   # Match level
#   matches <- fetch_cricsheet(competition = competition,
#                              gender = "male",
#                              type = "match")
#   
#   # Clean season to integer year (use end year for split seasons)
#   clean_yr_start <- function(s) {
#     case_when(
#       str_detect(s, "/") ~ as.integer(str_extract(s, "^\\d{4}")),
#       TRUE ~ as.integer(s)
#     )
#   }
#   
#   bbb <- bbb %>% mutate(season_yr = clean_yr(season))
#   matches <- matches %>% mutate(season_yr = clean_yr(season))
#   
#   # Season run rate (legal deliveries only)
#   run_rate_df <-
#     bbb %>%
#     filter(!extra_ball) %>%
#     group_by(season_yr) %>%
#     summarise(
#       run_rate = (sum(runs_off_bat, na.rm = TRUE) / n()) * 6,
#       wickets_per_over = (sum(wicket, na.rm = TRUE) / n()) * 6,
#       .groups = "drop"
#     )
#   
#   # All-rounder usage per match
#   allrounder_df <-
#     bbb %>%
#     group_by(match_id, season_yr) %>%
#     summarise(
#       batters = list(unique(striker)),
#       bowlers = list(unique(bowler)),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       n_allrounders = map2_int(batters, bowlers, 
#                                ~ length(intersect(.x, .y)))
#     ) %>%
#     group_by(season_yr) %>%
#     summarise(
#       avg_allrounders = mean(n_allrounders, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   # Match margins
#   margin_df <-
#     matches %>%
#     mutate(
#       winner_runs    = as.numeric(winner_runs),
#       winner_wickets = as.numeric(winner_wickets),
#       margin_runs = case_when(
#         !is.na(winner_runs)    ~ winner_runs,
#         !is.na(winner_wickets) ~ (10 - winner_wickets) * 10,
#         TRUE ~ NA_real_
#       )
#     ) %>%
#     group_by(season_yr) %>%
#     summarise(
#       avg_margin = mean(margin_runs, na.rm = TRUE),
#       n_matches  = n(),
#       .groups = "drop"
#     )
#   
#   # Merge all metrics
#   run_rate_df %>%
#     left_join(allrounder_df, by = "season_yr") %>%
#     left_join(margin_df, by = "season_yr") %>%
#     mutate(league = league_name)
# }


get_league_metrics <- function(competition, league_name) {
  
  message(glue("Fetching {league_name}..."))
  
  bbb <- fetch_cricsheet(competition = competition,
                         gender = "male",
                         type = "bbb")
  
  matches <- fetch_cricsheet(competition = competition,
                             gender = "male",
                             type = "match")
  
  clean_yr <- function(s) {
    case_when(
      str_detect(s, "/") ~ as.integer(str_extract(s, "^\\d{4}")),
      TRUE ~ as.integer(s)
    )
  }
  
  bbb <- bbb %>% mutate(season_yr = clean_yr(season))
  matches <- matches %>% mutate(season_yr = clean_yr(season))
  
  run_rate_df <-
    bbb %>%
    filter(!extra_ball) %>%
    group_by(season_yr) %>%
    summarise(
      run_rate = (sum(runs_off_bat, na.rm = TRUE) / n()) * 6,
      wickets_per_over = (sum(wicket, na.rm = TRUE) / n()) * 6,
      .groups = "drop"
    )
  
  allrounder_df <-
    bbb %>%
    group_by(match_id, season_yr) %>%
    summarise(
      batters = list(unique(striker)),
      bowlers = list(unique(bowler)),
      .groups = "drop"
    ) %>%
    mutate(
      n_allrounders = map2_int(batters, bowlers,
                               ~ length(intersect(.x, .y)))
    ) %>%
    group_by(season_yr) %>%
    summarise(
      avg_allrounders = mean(n_allrounders, na.rm = TRUE),
      .groups = "drop"
    )
  
  margin_df <-
    matches %>%
    mutate(
      winner_runs    = as.numeric(winner_runs),
      winner_wickets = as.numeric(winner_wickets),
      margin_runs = case_when(
        !is.na(winner_runs)    ~ winner_runs,
        !is.na(winner_wickets) ~ (10 - winner_wickets) * 10,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(season_yr) %>%
    summarise(
      avg_margin = mean(margin_runs, na.rm = TRUE),
      n_matches  = n(),
      .groups = "drop"
    )
  
  run_rate_df %>%
    left_join(allrounder_df, by = "season_yr") %>%
    left_join(margin_df, by = "season_yr") %>%
    mutate(league = league_name)
}


# Fetch all leagues
ipl_metrics <- get_league_metrics("ipl", "IPL")
bbl_metrics <- get_league_metrics("bbl", "BBL")
psl_metrics <- get_league_metrics("psl", "PSL")
cpl_metrics <- get_league_metrics("cpl", "CPL")

# Combine
all_leagues <-
  bind_rows(ipl_metrics, bbl_metrics, psl_metrics, cpl_metrics) %>%
  arrange(league, season_yr)

message("All leagues loaded.")
print(all_leagues %>% count(league, season_yr) %>% print(n = 60))

###--------------------------------------------------------------------------###
###   SECTION 2: Align Seasons & Build Panel                                 ###
###--------------------------------------------------------------------------###

# For synthetic control we need:
# - A balanced panel (same years for all units)
# - Pre-treatment period: before 2023
# - Post-treatment period: 2023 onwards
# - IPL is the treated unit

# Find common years across all leagues
common_years <-
  all_leagues %>%
  group_by(season_yr) %>%
  summarise(n_leagues = n_distinct(league), .groups = "drop") %>%
  filter(n_leagues >= 3,   # at least 3 leagues present
         season_yr >= 2015, # enough pre-period
         season_yr <= 2025  # cap at 2025 for complete seasons
  ) %>%
  pull(season_yr)

message(glue("Common years for synthetic control: {paste(common_years, collapse=', ')}"))

# Filter to common years
panel_df <-
  all_leagues %>%
  filter(season_yr %in% common_years) %>%
  # Fill missing values with league-year median
  group_by(league) %>%
  mutate(across(c(run_rate, avg_allrounders, avg_margin),
                ~ if_else(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# Assign numeric unit IDs for Synth package
panel_df <-
  panel_df %>%
  mutate(unit_id = case_when(
    league == "IPL" ~ 1L,
    league == "BBL" ~ 2L,
    league == "PSL" ~ 3L,
    league == "CPL" ~ 4L
  ))

message("Panel built:")
print(panel_df %>% select(league, unit_id, season_yr, 
                           run_rate, avg_allrounders, avg_margin))

###--------------------------------------------------------------------------###
###   SECTION 3: Synthetic Control — Run Rate                                ###
###--------------------------------------------------------------------------###

message("\nRunning synthetic control for Run Rate...")

# Prepare Synth dataprep object
treatment_yr <- 2023
pre_years  <- common_years[common_years < treatment_yr]
post_years <- common_years[common_years >= treatment_yr]
donor_ids  <- c(2L, 3L, 4L)  # BBL, PSL, CPL

# dataprep_runrate <- dataprep(
#   foo                   = as.data.frame(panel_df),
#   predictors            = "run_rate",
#   predictors.op         = "mean",
#   dependent             = "run_rate",
#   unit.variable         = "unit_id",
#   time.variable         = "season_yr",
#   treatment.identifier  = 1L,
#   controls.identifier   = donor_ids,
#   time.predictors.prior = pre_years,
#   time.optimize.ssr     = pre_years,
#   unit.names.variable   = "league",
#   time.plot             = common_years
# )

# Check what's missing
panel_df %>% 
  select(league, season_yr) %>%
  complete(league, season_yr) %>%
  filter(is.na(league)) %>%
  print()

# Force balance — fill missing league-years with interpolated values
panel_balanced <-
  panel_df %>%
  complete(league, season_yr) %>%
  group_by(league) %>%
  mutate(
    # Fill unit_id
    unit_id = case_when(
      league == "IPL" ~ 1L,
      league == "BBL" ~ 2L,
      league == "PSL" ~ 3L,
      league == "CPL" ~ 4L
    ),
    # Interpolate missing numeric values
    run_rate        = zoo::na.approx(run_rate, na.rm = FALSE),
    avg_allrounders = zoo::na.approx(avg_allrounders, na.rm = FALSE),
    avg_margin      = zoo::na.approx(avg_margin, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  filter(season_yr %in% common_years)

# Verify balanced
panel_balanced %>% count(league) %>% print()
# All should show n = 11

dataprep_runrate <- dataprep(
  foo                   = as.data.frame(panel_balanced),
  predictors            = "run_rate",
  predictors.op         = "mean",
  dependent             = "run_rate",
  unit.variable         = "unit_id",
  time.variable         = "season_yr",
  treatment.identifier  = 1L,
  controls.identifier   = donor_ids,
  time.predictors.prior = pre_years,
  time.optimize.ssr     = pre_years,
  unit.names.variable   = "league",
  time.plot             = common_years
)

synth_runrate <- synth(dataprep_runrate)

### 2024 is collapsing near zero, so remove that and run again
# Remove 2024 from common years — PSL missing causes interpolation failure
common_years <- common_years[common_years != 2024]

# Rebuild balanced panel
panel_balanced <-
  panel_df %>%
  filter(season_yr %in% common_years) %>%
  complete(league, season_yr) %>%
  group_by(league) %>%
  mutate(
    unit_id = case_when(
      league == "IPL" ~ 1L,
      league == "BBL" ~ 2L,
      league == "PSL" ~ 3L,
      league == "CPL" ~ 4L
    ),
    run_rate        = zoo::na.approx(run_rate, na.rm = FALSE),
    avg_allrounders = zoo::na.approx(avg_allrounders, na.rm = FALSE),
    avg_margin      = zoo::na.approx(avg_margin, na.rm = FALSE)
  ) %>%
  ungroup()

# Verify — all leagues should have same n
panel_balanced %>% count(league)

# Update pre/post years
pre_years  <- common_years[common_years < 2023]
post_years <- common_years[common_years >= 2023]

# Check what unit IDs actually exist in the panel
panel_df %>% 
  count(unit_id, league, season_yr) %>% 
  print(n = 60)

# Check common years vs what each league actually has
panel_df %>%
  group_by(league, unit_id) %>%
  summarise(
    seasons = paste(sort(season_yr), collapse = ", "),
    n = n(),
    .groups = "drop"
  ) %>%
  print()

# Extract synthetic weights
synth_weights_rr <- 
  tibble(
    league = c("BBL", "PSL", "CPL"),
    weight = round(synth_runrate$solution.w, 3)
  )

message("Synthetic control weights (Run Rate):")
print(synth_weights_rr)

# Build synthetic IPL run rate
synthetic_rr <-
  panel_df %>%
  filter(league != "IPL") %>%
  left_join(synth_weights_rr, by = "league") %>%
  group_by(season_yr) %>%
  summarise(
    synthetic_run_rate = sum(run_rate * weight, na.rm = TRUE),
    .groups = "drop"
  )

actual_rr <-
  panel_df %>%
  filter(league == "IPL") %>%
  select(season_yr, actual_run_rate = run_rate)

synth_rr_combined <-
  actual_rr %>%
  left_join(synthetic_rr, by = "season_yr") %>%
  mutate(gap = actual_run_rate - synthetic_run_rate)

###--------------------------------------------------------------------------###
###   SECTION 4: Synthetic Control — All-rounders                            ###
###--------------------------------------------------------------------------###

message("\nRunning synthetic control for All-rounder Usage...")

dataprep_ar <- dataprep(
  foo                   = as.data.frame(panel_balanced),
  predictors            = "avg_allrounders",
  predictors.op         = "mean",
  dependent             = "avg_allrounders",
  unit.variable         = "unit_id",
  time.variable         = "season_yr",
  treatment.identifier  = 1L,
  controls.identifier   = donor_ids,
  time.predictors.prior = pre_years,
  time.optimize.ssr     = pre_years,
  unit.names.variable   = "league",
  time.plot             = common_years
)

synth_ar <- synth(dataprep_ar)

synth_weights_ar <-
  tibble(
    league = c("BBL", "PSL", "CPL"),
    weight = round(synth_ar$solution.w, 3)
  )

message("Synthetic control weights (All-rounders):")
print(synth_weights_ar)

synthetic_ar <-
  panel_df %>%
  filter(league != "IPL") %>%
  left_join(synth_weights_ar, by = "league") %>%
  group_by(season_yr) %>%
  summarise(
    synthetic_ar = sum(avg_allrounders * weight, na.rm = TRUE),
    .groups = "drop"
  )

actual_ar <-
  panel_df %>%
  filter(league == "IPL") %>%
  select(season_yr, actual_ar = avg_allrounders)

synth_ar_combined <-
  actual_ar %>%
  left_join(synthetic_ar, by = "season_yr") %>%
  mutate(gap = actual_ar - synthetic_ar)


panel_balanced %>% 
  filter(season_yr == 2024) %>%
  select(league, season_yr, run_rate, avg_allrounders, unit_id)

# Update years
pre_years  <- common_years[common_years < 2023]
post_years <- common_years[common_years >= 2023]

message(glue("Pre years: {paste(pre_years, collapse=', ')}"))
message(glue("Post years: {paste(post_years, collapse=', ')}"))

# Re-run dataprep and synth for run rate
dataprep_runrate <- dataprep(
  foo                   = as.data.frame(panel_balanced),
  predictors            = "run_rate",
  predictors.op         = "mean",
  dependent             = "run_rate",
  unit.variable         = "unit_id",
  time.variable         = "season_yr",
  treatment.identifier  = 1L,
  controls.identifier   = donor_ids,
  time.predictors.prior = pre_years,
  time.optimize.ssr     = pre_years,
  unit.names.variable   = "league",
  time.plot             = common_years
)

synth_runrate <- synth(dataprep_runrate)

synth_weights_rr <- 
  tibble(
    league = c("BBL", "PSL", "CPL"),
    weight = as.numeric(synth_runrate$solution.w)
  )

synthetic_rr <-
  panel_balanced %>%
  filter(league != "IPL") %>%
  left_join(synth_weights_rr, by = "league") %>%
  group_by(season_yr) %>%
  summarise(
    synthetic_run_rate = sum(run_rate * weight, na.rm = TRUE),
    .groups = "drop"
  )

actual_rr <-
  panel_balanced %>%
  filter(league == "IPL") %>%
  select(season_yr, actual_run_rate = run_rate)

synth_rr_combined <-
  actual_rr %>%
  left_join(synthetic_rr, by = "season_yr") %>%
  mutate(gap = actual_run_rate - synthetic_run_rate)

# Check
synth_rr_combined %>% 
  filter(season_yr >= 2023) %>%
  print()

# Replot
print(fig6_synth_rr %+% 
        (synth_rr_combined %>%
           pivot_longer(cols = c(actual_run_rate, synthetic_run_rate),
                        names_to = "series", values_to = "run_rate") %>%
           mutate(series = if_else(series == "actual_run_rate",
                                   "Actual IPL", "Synthetic IPL (counterfactual)"))))

###--------------------------------------------------------------------------###
###   SECTION 5: Visualisations                                              ###
###--------------------------------------------------------------------------###

theme_ipl <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major = element_line(color = "#E0E0E0"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 11, color = "gray30"),
      plot.caption     = element_text(size = 8, color = "gray50", face = "italic"),
      axis.title       = element_text(face = "bold", size = 10),
      axis.text        = element_text(size = 9),
      legend.position  = "top"
    )
}

# --- Fig 6: Synthetic Control — Run Rate ---
fig6_synth_rr <-
  synth_rr_combined %>%
  pivot_longer(cols = c(actual_run_rate, synthetic_run_rate),
               names_to = "series",
               values_to = "run_rate") %>%
  mutate(series = if_else(series == "actual_run_rate",
                          "Actual IPL", "Synthetic IPL (counterfactual)")) %>%
  ggplot(aes(x = season_yr, y = run_rate,
             color = series, linetype = series)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("rect",
           xmin = 2022.5, xmax = max(common_years) + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "#CE1141", alpha = 0.05) +
  annotate("text", x = 2023, y = min(synth_rr_combined$synthetic_run_rate) + 0.1,
           label = "Treatment\nPeriod",
           color = "#CE1141", size = 3, hjust = 0) +
  scale_color_manual(values = c("Actual IPL" = "#CE1141",
                                "Synthetic IPL (counterfactual)" = "#17408B")) +
  scale_linetype_manual(values = c("Actual IPL" = "solid",
                                   "Synthetic IPL (counterfactual)" = "dashed")) +
  scale_x_continuous(breaks = common_years) +
  labs(
    title    = "Synthetic Control: IPL Run Rate vs Counterfactual",
    subtitle = "What would run rates have looked like without the Impact Player rule?",
    x = "Season", y = "Run Rate (per over)",
    color = NULL, linetype = NULL,
    caption  = "Synthetic IPL = weighted combination of BBL, PSL, CPL | Data: Cricsheet"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Re-run dataprep and synth for all-rounders
dataprep_ar <- dataprep(
  foo                   = as.data.frame(panel_balanced),
  predictors            = "avg_allrounders",
  predictors.op         = "mean",
  dependent             = "avg_allrounders",
  unit.variable         = "unit_id",
  time.variable         = "season_yr",
  treatment.identifier  = 1L,
  controls.identifier   = donor_ids,
  time.predictors.prior = pre_years,
  time.optimize.ssr     = pre_years,
  unit.names.variable   = "league",
  time.plot             = common_years
)

synth_ar <- synth(dataprep_ar)

synth_weights_ar <-
  tibble(
    league = c("BBL", "PSL", "CPL"),
    weight = as.numeric(synth_ar$solution.w)
  )

synthetic_ar <-
  panel_balanced %>%
  filter(league != "IPL") %>%
  left_join(synth_weights_ar, by = "league") %>%
  group_by(season_yr) %>%
  summarise(
    synthetic_ar = sum(avg_allrounders * weight, na.rm = TRUE),
    .groups = "drop"
  )

actual_ar <-
  panel_balanced %>%
  filter(league == "IPL") %>%
  select(season_yr, actual_ar = avg_allrounders)

synth_ar_combined <-
  actual_ar %>%
  left_join(synthetic_ar, by = "season_yr") %>%
  mutate(gap = actual_ar - synthetic_ar)

# Check
synth_ar_combined %>%
  filter(season_yr >= 2023) %>%
  print()

# Replot fig 7
print(fig7_synth_ar %+%
        (synth_ar_combined %>%
           pivot_longer(cols = c(actual_ar, synthetic_ar),
                        names_to = "series", values_to = "allrounders") %>%
           mutate(series = if_else(series == "actual_ar",
                                   "Actual IPL", "Synthetic IPL (counterfactual)"))))

# --- Fig 7: Synthetic Control — All-rounders ---
fig7_synth_ar <-
  synth_ar_combined %>%
  pivot_longer(cols = c(actual_ar, synthetic_ar),
               names_to = "series",
               values_to = "allrounders") %>%
  mutate(series = if_else(series == "actual_ar",
                          "Actual IPL", "Synthetic IPL (counterfactual)")) %>%
  ggplot(aes(x = season_yr, y = allrounders,
             color = series, linetype = series)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("rect",
           xmin = 2022.5, xmax = max(common_years) + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "#CE1141", alpha = 0.05) +
  scale_color_manual(values = c("Actual IPL" = "#CE1141",
                                "Synthetic IPL (counterfactual)" = "#17408B")) +
  scale_linetype_manual(values = c("Actual IPL" = "solid",
                                   "Synthetic IPL (counterfactual)" = "dashed")) +
  scale_x_continuous(breaks = common_years) +
  labs(
    title    = "Synthetic Control: All-rounder Usage vs Counterfactual",
    subtitle = "Would all-rounder decline have happened anyway without the rule?",
    x = "Season", y = "Avg All-rounders per Match",
    color = NULL, linetype = NULL,
    caption  = "Synthetic IPL = weighted combination of BBL, PSL, CPL | Data: Cricsheet"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# --- Fig 8: Gap plots (treatment effect over time) ---
fig8_gap_rr <-
  synth_rr_combined %>%
  ggplot(aes(x = season_yr, y = gap)) +
  geom_line(color = "#CE1141", linewidth = 1.3) +
  geom_point(color = "#CE1141", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "gray50", linewidth = 0.8) +
  annotate("text", x = 2023, y = -0.05,
           label = "Rule introduced →",
           color = "gray30", size = 3, hjust = 0) +
  scale_x_continuous(breaks = common_years) +
  labs(
    title    = "Treatment Effect: Run Rate Gap (Actual − Synthetic IPL)",
    subtitle = "Positive = Impact Player rule increased run rates above counterfactual",
    x = "Season", y = "Gap (runs per over)",
    caption  = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

fig9_gap_ar <-
  synth_ar_combined %>%
  ggplot(aes(x = season_yr, y = gap)) +
  geom_line(color = "#17408B", linewidth = 1.3) +
  geom_point(color = "#17408B", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "gray50", linewidth = 0.8) +
  annotate("text", x = 2023, y = 0.1,
           label = "Rule introduced →",
           color = "gray30", size = 3, hjust = 0) +
  scale_x_continuous(breaks = common_years) +
  labs(
    title    = "Treatment Effect: All-rounder Gap (Actual − Synthetic IPL)",
    subtitle = "Negative = Impact Player rule reduced all-rounder usage below counterfactual",
    x = "Season", y = "Gap (all-rounders per match)",
    caption  = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Print all figures
print(fig6_synth_rr)
print(fig7_synth_ar)
print(fig8_gap_rr)
print(fig9_gap_ar)

###--------------------------------------------------------------------------###
###   SECTION 6: Summary                                                     ###
###--------------------------------------------------------------------------###

cat("\n\n========== SYNTHETIC CONTROL SUMMARY ==========\n")

cat("\nRun Rate Weights:\n")
print(synth_weights_rr)

cat("\nAll-rounder Weights:\n")
print(synth_weights_ar)

cat("\nRun Rate Gap (Actual - Synthetic) post-2023:\n")
synth_rr_combined %>%
  filter(season_yr >= 2023) %>%
  select(season_yr, actual_run_rate, synthetic_run_rate, gap) %>%
  print()

cat("\nAll-rounder Gap (Actual - Synthetic) post-2023:\n")
synth_ar_combined %>%
  filter(season_yr >= 2023) %>%
  select(season_yr, actual_ar, synthetic_ar, gap) %>%
  print()

cat("\nLimitations:\n")
cat("- Only 3 donor leagues — limited donor pool\n")
cat("- PSL and CPL have shorter histories than ideal\n")
cat("- Donor leagues may have their own confounders\n")
cat("- No placebo tests run yet — add for robustness\n")
cat("=================================================\n")

###--------------------------------------------------------------------------###
save.image("IPL_02_Synthetic_Control_Results.RData")
message("Saved to IPL_02_Synthetic_Control_Results.RData")
