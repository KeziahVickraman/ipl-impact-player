###--------------------------------------------------------------------------###
###   IPL Impact Player Rule — Causal Analysis                               ###
###   Script: IPL_01_Causal_DiD.R                                            ###
###                                                                          ###
###   Research Question:                                                     ###
###   Should the BCCI keep, modify, or scrap the Impact Player rule?         ###
###   A causal analysis of its effect on:                                    ###
###     1. Run rates (batting dominance)                                     ###
###     2. Wicket-taking rates (bowling efficacy)                            ###
###     3. Competitive balance (match margins)                               ###
###     4. All-rounder value                                                 ###
###                                                                          ###
###   Method: Difference-in-Differences (DiD)                                ###
###   Treatment: Impact Player rule introduction (IPL 2023)                  ###
###   Data: Cricsheet ball-by-ball data via cricketdata package              ###
###--------------------------------------------------------------------------###

rm(list = ls())

# ---- 0. Set Dependencies ----
pacman::p_load(
  tidyverse, glue, skimr,
  cricketdata,
  lmtest, sandwich,      # robust SEs
  broom, jtools,         # tidy model outputs
  huxtable,              # regression tables
  ggthemes, scales,      # viz
  zoo                    # rolling averages
)

# ---- 1. Collect & Import Data ----
## ---- 1.1 Importation and Cleaning of Data ----
###--------------------------------------------------------------------------###
###   SECTION 1: Data Import & Season Cleaning                               ###
###--------------------------------------------------------------------------###

# Load ball-by-ball data
ipl_bbb <- fetch_cricsheet(competition = "ipl",
                           gender = "male",
                           type = "bbb")

# Load match-level data
ipl_matches <- fetch_cricsheet(competition = "ipl",
                               gender = "male",
                               type = "match")

# Clean season labels — handle split-year formats
clean_season <- function(s) {
  case_when(
    s == "2007/08" ~ 2008L,
    s == "2009/10" ~ 2010L,
    s == "2020/21" ~ 2021L,
    TRUE ~ as.integer(s)
  )
}

ipl_bbb <- 
  ipl_bbb %>%
  mutate(
    season_yr = clean_season(season),
    # Treatment indicator: Impact Player rule introduced in 2023
    post = as.integer(season_yr >= 2023),
    post_factor = factor(post, levels = c(0, 1), 
                         labels = c("Pre (2008-2022)", "Post (2023-2026)"))
  )

ipl_matches <- 
  ipl_matches %>%
  mutate(season_yr = clean_season(season))

message(glue("Ball-by-ball rows: {nrow(ipl_bbb)}"))
message(glue("Match rows: {nrow(ipl_matches)}"))
message(glue("Seasons: {paste(sort(unique(ipl_bbb$season_yr)), collapse=', ')}"))

# ---- 2. Wrangle Data ----
###--------------------------------------------------------------------------###
###   SECTION 2: Outcome Variables                                           ###
###   Build season-level aggregates for DiD                                  ###
###--------------------------------------------------------------------------###

## --- 2.1 Run rate per season (batting dominance) ----
season_run_rate <-
  ipl_bbb %>%
  filter(!extra_ball) %>%           # exclude wide/no-ball deliveries
  group_by(season_yr, post, post_factor) %>%
  summarise(
    total_balls   = n(),
    total_runs    = sum(runs_off_bat, na.rm = TRUE),
    total_wickets = sum(wicket, na.rm = TRUE),
    run_rate      = (total_runs / total_balls) * 6,
    wickets_per_over = (total_wickets / total_balls) * 6,
    .groups = "drop"
  )

## --- 2.2 Match-level outcomes (competitive balance) ----
match_outcomes <-
  ipl_matches %>%
  mutate(
    # Win margin in runs (batting first wins) or wickets (chasing wins)
    winner_runs    = as.numeric(winner_runs),
    winner_wickets = as.numeric(winner_wickets),
    # Standardise margin — use runs for batting wins, invert wickets for chase wins
    margin_runs    = case_when(
      !is.na(winner_runs)    ~ winner_runs,
      !is.na(winner_wickets) ~ (10 - winner_wickets) * 10,  # proxy
      TRUE ~ NA_real_
    ),
    close_match = if_else(margin_runs <= 20, 1L, 0L)  # within 20 runs = close
  ) %>%
  group_by(season_yr, post = as.integer(season_yr >= 2023)) %>%
  summarise(
    n_matches       = n(),
    avg_margin      = mean(margin_runs, na.rm = TRUE),
    pct_close       = mean(close_match, na.rm = TRUE) * 100,
    .groups = "drop"
  )

## --- 2.3 All-rounder proxy (IMPORTANT) ----
# All-rounders are players who BOTH bat and bowl in the same match
# Proxy: count of unique bowlers who also appear as striker in same match
allrounder_usage <-
  ipl_bbb %>%
  group_by(match_id, season_yr, post) %>%
  summarise(
    batters = list(unique(striker)),
    bowlers = list(unique(bowler)),
    .groups = "drop"
  ) %>%
  mutate(
    # All-rounders = players who both batted AND bowled
    n_allrounders = map2_int(batters, bowlers, ~ length(intersect(.x, .y)))
  ) %>%
  group_by(season_yr, post) %>%
  summarise(
    avg_allrounders_per_match = mean(n_allrounders, na.rm = TRUE),
    .groups = "drop"
  )

message("Season-level aggregates built.")

# ---- 3. Model ----
###--------------------------------------------------------------------------###
###   SECTION 3: Average Treatment Effect (ATE) — Simple Before/After        ###
###   Note: This is a descriptive A-B comparison (IPL pre vs post 2023)      ###
###   Causal identification comes later via Synthetic Control (Section 5)    ###
###--------------------------------------------------------------------------###

model_runrate <- 
  lm(run_rate ~ post, data = season_run_rate)

model_wickets <- 
  lm(wickets_per_over ~ post, data = season_run_rate)

model_margin <- 
  lm(avg_margin ~ post, data = match_outcomes)

model_allrounder <- 
  lm(avg_allrounders_per_match ~ post, data = allrounder_usage)

# Function: to extract ATE and CIs cleanly
get_ate <- function(model, outcome_label) {
  ci <- confint(model)["post", ]
  tibble(
    outcome    = outcome_label,
    ATE        = round(coef(model)["post"], 3),
    CI_lower   = round(ci[1], 3),
    CI_upper   = round(ci[2], 3),
    CI_range   = glue("[{round(ci[1],3)}, {round(ci[2],3)}]")
  )
}

ate_table <-
  bind_rows(
    get_ate(model_runrate,    "Run Rate (per over)"),
    get_ate(model_wickets,    "Wickets per Over"),
    get_ate(model_margin,     "Match Margin (runs)"),
    get_ate(model_allrounder, "All-rounders per Match")
  )

cat("\n========== ATE: IPL Before vs After Impact Player Rule ==========\n")
print(ate_table)
cat("\nHow to read:\n")
cat("ATE      = average change in outcome post-rule vs pre-rule\n")
cat("CI range = 95% confidence interval\n")
cat("Note: This is a simple A-B comparison — not causal.\n")
cat("      Causal identification via Synthetic Control in IPL_02_SyntheticControl Script\n")
cat("=================================================================\n")

## ---- 3.5 Model Validity ----
###--------------------------------------------------------------------------###
###   SECTION 3.5: Parallel Trends Check                                     ###
###   Required assumption check for DiD validity                             ###
###--------------------------------------------------------------------------###

# Pre-trend: was run rate already rising before 2023?
# If yes — rule effect may be overstated
# Check by fitting a trend line on pre-period only

pre_period <- 
  season_run_rate %>% 
  filter(post == 0)

pre_trend_model <- 
  lm(run_rate ~ season_yr, data = pre_period)

pre_trend_tidy <- tidy(pre_trend_model, conf.int = TRUE)

message("\nPre-period trend:")
print(pre_trend_tidy)

# If season_yr coefficient is small and insignificant → parallel trends holds
# If significant → need to control for time trend in main model

# --- Model 1b: Run Rate ~ Post + season_yr (controlling for time trend) ---
model_runrate_trend <- 
  lm(run_rate ~ post + season_yr, data = season_run_rate)

model_runrate_trend_robust <- 
  coeftest(model_runrate_trend, vcov = vcovHC(model_runrate_trend, type = "HC3"))

cat("\n=== Model 1b: Run Rate (controlling for time trend) ===\n")
print(model_runrate_trend_robust)

# ---- 4. Visualize ----
###--------------------------------------------------------------------------###
###   SECTION 4: Visualisations                                              ###
###--------------------------------------------------------------------------###

# Custom IPL theme
theme_ipl <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major = element_line(color = "#E0E0E0"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 14, color = "black"),
      plot.subtitle    = element_text(size = 11, color = "gray30"),
      plot.caption     = element_text(size = 8, color = "gray50", face = "italic"),
      axis.title       = element_text(face = "bold", size = 10),
      axis.text        = element_text(size = 9),
      legend.position  = "top"
    )
}

### --- Fig 1: Run Rate by Season ----
fig1_runrate <-
  season_run_rate %>%
  ggplot(aes(x = season_yr, y = run_rate, fill = post_factor)) +
  geom_col() +
  geom_vline(xintercept = 2022.5, linetype = "dashed", 
             color = "#CE1141", linewidth = 0.8) +
  geom_smooth(aes(group = post_factor), 
              method = "lm", se = TRUE, 
              color = "black", linewidth = 0.6, linetype = "dotted") +
  annotate("text", x = 2020.5, y = 9.3,
           label = "Impact Player Rule →",
           color = "#CE1141", size = 3.5, fontface = "italic") +
  scale_fill_manual(values = c("Pre (2008-2022)" = "#17408B",
                               "Post (2023-2026)" = "#CE1141")) +
  scale_x_continuous(breaks = seq(2008, 2026, by = 1)) +
  scale_y_continuous(limits = c(6.5, 9.5), oob = scales::squish) +
  labs(
    title    = "IPL Run Rate by Season — Impact of the Impact Player Rule",
    subtitle = "Average runs per over (legal deliveries only)",
    x = "Season", y = "Run Rate (per over)", fill = "Era",
    caption  = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

### --- Fig 2: Wickets per over by Season ----
fig2_wickets <-
  season_run_rate %>%
  ggplot(aes(x = season_yr, y = wickets_per_over, fill = post_factor)) +
  geom_col() +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "#CE1141", linewidth = 0.8) +
  annotate("text", x = 2020.5, y = 0.32,
           label = "Impact Player Rule →",
           color = "#CE1141", size = 3.5, fontface = "italic") +
  scale_fill_manual(values = c("Pre (2008-2022)" = "#17408B",
                               "Post (2023-2026)" = "#CE1141")) +
  scale_x_continuous(breaks = seq(2008, 2026, by = 1)) +
  labs(
    title    = "IPL Wickets per Over by Season",
    subtitle = "Did the Impact Player rule help or hurt bowlers?",
    x = "Season", y = "Wickets per Over", fill = "Era",
    caption  = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

### --- Fig 3: Competitive Balance (Match Margins) ----
fig3_balance <-
  match_outcomes %>%
  mutate(post_factor = factor(post, levels = c(0,1),
                              labels = c("Pre (2008-2022)", 
                                         "Post (2023-2026)"))) %>%
  ggplot(aes(x = season_yr, y = avg_margin, fill = post_factor)) +
  geom_col() +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "#CE1141", linewidth = 0.8) +
  annotate("text", x = 2020, y = max(match_outcomes$avg_margin, na.rm=TRUE) * 0.95,
           label = "Impact Player Rule →",
           color = "#CE1141", size = 3.5, fontface = "italic") +
  scale_fill_manual(values = c("Pre (2008-2022)" = "#17408B",
                               "Post (2023-2026)" = "#CE1141")) +
  scale_x_continuous(breaks = seq(2008, 2026, by = 1)) +
  labs(
    title    = "IPL Average Match Margin by Season",
    subtitle = "Higher margin = less competitive matches",
    x = "Season", y = "Average Win Margin (runs proxy)", fill = "Era",
    caption  = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

### --- Fig 4: All-rounder Usage ----
fig4_allrounders <-
  allrounder_usage %>%
  mutate(post_factor = factor(post, levels = c(0,1),
                              labels = c("Pre (2008-2022)", 
                                         "Post (2023-2026)"))) %>%
  ggplot(aes(x = season_yr, y = avg_allrounders_per_match, 
             fill = post_factor)) +
  geom_col() +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "#CE1141", linewidth = 0.8) +
  annotate("text", x = 2020, 
           y = max(allrounder_usage$avg_allrounders_per_match, na.rm=TRUE) * 0.95,
           label = "Impact Player Rule →",
           color = "#CE1141", size = 3.5, fontface = "italic") +
  scale_fill_manual(values = c("Pre (2008-2022)" = "#17408B",
                               "Post (2023-2026)" = "#CE1141")) +
  scale_x_continuous(breaks = seq(2008, 2026, by = 1)) +
  labs(
    title    = "All-rounder Usage per Match by Season",
    subtitle = "Players who both batted AND bowled in the same match",
    x = "Season", y = "Avg All-rounders per Match", fill = "Era",
    caption  = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

### --- Fig 5: Parallel Trends Check (simple) ----
fig5_parallel_only_ipl <-
  season_run_rate %>%
  ggplot(aes(x = season_yr, y = run_rate, 
             color = post_factor, group = 1)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  geom_smooth(data = . %>% filter(post == 0),
              method = "lm", se = TRUE,
              color = "#17408B", fill = "#17408B", alpha = 0.1,
              linetype = "dotted") +
  scale_color_manual(values = c("Pre (2008-2022)" = "#17408B",
                                "Post (2023-2026)" = "#CE1141")) +
  scale_x_continuous(breaks = seq(2008, 2026, by = 1)) +
  scale_y_continuous(limits = c(6.5, 9.5), oob = scales::squish) +
  annotate("text", x = 2023.2, y = 7,
           label = "Post-rule\nacceleration",
           color = "#CE1141", size = 3, fontface = "italic") +
  labs(
    title    = "Parallel Trends Check — Run Rate Pre vs Post Impact Player Rule",
    subtitle = "Dotted line shows pre-period trend extrapolated forward. But this is merely what is...",
    x = "Season", y = "Run Rate (per over)", color = "Era",
    caption  = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Print all figures
print(fig1_runrate)
print(fig2_wickets)
print(fig3_balance)
print(fig4_allrounders)
print(fig5_parallel_only_ipl)

# Valid parallel trends checks with all leagues
get_league_metrics <- function(competition, league_name) {
  message(glue("Fetching {league_name}..."))
  
  bbb <- fetch_cricsheet(competition = competition, gender = "male", type = "bbb")
  matches <- fetch_cricsheet(competition = competition, gender = "male", type = "match")
  
  clean_yr <- function(s) {
    case_when(
      str_detect(s, "/") ~ as.integer(str_extract(s, "^\\d{4}")),
      TRUE ~ as.integer(s)
    )
  }
  
  bbb     <- bbb     %>% mutate(season_yr = clean_yr(season))
  matches <- matches %>% mutate(season_yr = clean_yr(season))
  
  run_rate_df <-
    bbb %>%
    filter(!extra_ball) %>%
    group_by(season_yr) %>%
    summarise(
      run_rate         = (sum(runs_off_bat, na.rm = TRUE) / n()) * 6,
      wickets_per_over = (sum(wicket,       na.rm = TRUE) / n()) * 6,
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
    mutate(n_allrounders = map2_int(batters, bowlers, ~ length(intersect(.x, .y)))) %>%
    group_by(season_yr) %>%
    summarise(avg_allrounders = mean(n_allrounders, na.rm = TRUE), .groups = "drop")
  
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
    summarise(avg_margin = mean(margin_runs, na.rm = TRUE), .groups = "drop")
  
  run_rate_df %>%
    left_join(allrounder_df, by = "season_yr") %>%
    left_join(margin_df,     by = "season_yr") %>%
    mutate(league = league_name)
}

ipl_metrics <- get_league_metrics("ipl", "IPL")
bbl_metrics <- get_league_metrics("bbl", "BBL")
psl_metrics <- get_league_metrics("psl", "PSL")
cpl_metrics <- get_league_metrics("cpl", "CPL")

# merge into one dataframe
all_leagues <- bind_rows(ipl_metrics, 
                         bbl_metrics, 
                         psl_metrics, 
                         cpl_metrics)

# Parallel trends amongst all leagues 
parallel_all_leagues <-
  all_leagues %>%
  mutate(
    season_yr = case_when(
      str_detect(season_yr, "/") ~ as.integer(str_extract(season_yr, "^\\d{4}")),
      TRUE ~ as.integer(season_yr)
    ),
    league_type = if_else(league == "IPL", 
                          "IPL (Treated)", 
                          "Donor Leagues (Control)")
  ) %>%
  filter(season_yr >= 2015, season_yr <= 2025) %>%
  group_by(season_yr, league, league_type) %>%
  summarise(run_rate = mean(run_rate, na.rm = TRUE), .groups = "drop")

### --- Fig 5b: Parallel Trends Check (Valid across all leagues) ----
fig5b_parallel_all <-
  parallel_all_leagues %>%
  mutate(alpha_val = if_else(league_type == "IPL (Treated)", 
                             1, 0.4)
         ) %>%
  ggplot(aes(x = season_yr, y = run_rate,
             color = league_type, group = league,
             linetype = league_type,
             alpha = alpha_val)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2022.5, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("text", x = 2023.1, y = 7.2,
           label = "Impact Player\nRule ->",
           color = "#CE1141", size = 3, hjust = 0) +
  scale_color_manual(values = c("IPL (Treated)"           = "#CE1141",
                                "Donor Leagues (Control)" = "#17408B")) +
  scale_linetype_manual(values = c("IPL (Treated)"           = "solid",
                                   "Donor Leagues (Control)" = "dashed")) +
  scale_alpha_identity() +
  scale_x_continuous(breaks = seq(2015, 2025, by = 1)) +
  labs(
    title    = "Parallel Trends Check — IPL vs Donor Leagues",
    subtitle = "Pre-2023 trends do not strictly satisfy parallel trends \n— justifying the Synthetic Control approach. What IF analysis...",
    x = "Season", y = "Run Rate (per over)",
    color = NULL, linetype = NULL,
    caption = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)
        )

print(fig5_parallel_only_ipl)
print(fig5b_parallel_all)

# ---- 5. Report ----
###--------------------------------------------------------------------------###
###   SECTION 5: Summary of Findings — A-B (IPL Before vs After)            ###
###   Note: Causal SC gaps reported in IPL_02_Synthetic_Control.R           ###
###--------------------------------------------------------------------------###

cat("\n========== ATE SUMMARY: IPL Before vs After Impact Player Rule ==========\n")
print(ate_table)

cat("\n========== KEY OBSERVATIONS ==========\n")
cat("1. Run rates increased post-rule (+1.3 runs/over)\n")
cat("2. All-rounder usage declined (-1.2 per match)\n")
cat("3. Competitive balance largely unchanged\n")
cat("4. Parallel trends assumption not satisfied — see IPL_02 for SC\n")

cat("\n========== LIMITATIONS (this script) ==========\n")
cat("- Simple A-B comparison only — not causal\n")
cat("- Only 3 post-treatment seasons (2023-2025)\n")
cat("- All-rounder proxy based on batting + bowling participation per match\n")
cat("- Match margin proxy combines runs and wickets — imperfect\n")
cat("- Causal identification via Synthetic Control in IPL_02\n")
cat("================================================\n")
###--------------------------------------------------------------------------###
# ---- 6. Save RAM space into hardisk ----
save.image("IPL_01_Causal_Results.RData")
message("\nSaved to IPL_01_Causal_Results.RData")

# ---- 7. Report Dependencies ----
sessionInfo()
