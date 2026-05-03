###--------------------------------------------------------------------------###
###   IPL Impact Player Study                                                ###
###   00 — Data Scoping Script                                               ###
###--------------------------------------------------------------------------###

rm(list = ls())

# ---- 0. Set Dependencies ----
# Package for cricket data in R 
# cricketdata pulls from Cricsheet — free, clean, well maintained
install.packages("cricketdata")

pacman::p_load(tidyverse,
               glue,
               skimr,
               tidymodels,
               cricketdata)

# ---- 1. Collect & Import Data ----
## ---- 1.1 Fetch IPL Match-Level Data ----
###--------------------------------------------------------------------------###
###   SECTION 1.1: Fetch IPL Match-Level Data ----                           ###
###--------------------------------------------------------------------------###
message("\n--- Fetching IPL ball-by-ball data ---")

# competition = "ipl" fetches all Indian Premier League (IPL) seasons available

tryCatch({
  ipl_raw <- fetch_cricsheet(competition = "ipl",
                             gender = "male",
                             type = "match")  # match-level first
  
  message(glue("IPL match rows: {nrow(ipl_raw)}"))
  message(glue("IPL match cols: {ncol(ipl_raw)}"))
  message(glue("Seasons available: {paste(sort(unique(ipl_raw$season)), collapse=', ')}"))
  
  glimpse(ipl_raw)
  
}, error = function(e) message(glue("IPL match ERROR: {e$message}")))

## ---- 1.2 Fetch IPL Ball-by-Ball Data ----
###--------------------------------------------------------------------------###
###   SECTION 1.2: Fetch IPL Ball-by-Ball Data                               ###
###--------------------------------------------------------------------------###

message("\n--- Fetching IPL ball-by-ball data ---")

tryCatch({
  ipl_bbb <- fetch_cricsheet(competition = "ipl",
                             gender = "male", 
                             type = "bbb")  # ball by ball
  
  message(glue("IPL ball-by-ball rows: {nrow(ipl_bbb)}"))
  message(glue("IPL ball-by-ball cols: {ncol(ipl_bbb)}"))
  message(glue("Seasons available: {paste(sort(unique(ipl_bbb$season)), collapse=', ')}"))
  
  glimpse(ipl_bbb)
  
}, error = function(e) message(glue("IPL ball-by-ball ERROR: {e$message}")))

# ---- 2. Wrangle and EDA ----
## ---- 2.1 Check Key Variables for Analysis ----
###--------------------------------------------------------------------------###
###   SECTION 2.1: Check Key Variables for Analysis                          ###
###--------------------------------------------------------------------------###

message("\n--- Checking key variables ---")

if (exists("ipl_bbb")) {
  
  # Check seasons — we need pre-2023 (control) and 2023+ (treatment)
  season_counts <- 
    ipl_bbb %>%
    count(season) %>%
    mutate(period = if_else(season >= 2023, "Post Impact Player", "Pre Impact Player"))
  
  print(season_counts)
  
  # Check batting metrics available
  message("\nBatting columns:")
  ipl_bbb %>% 
    select(contains("run") | contains("bat") | contains("strike")) %>% 
    names() %>% 
    paste(collapse = ", ") %>% 
    message()
  
  # Check bowling metrics available  
  message("\nBowling columns:")
  ipl_bbb %>% 
    select(contains("bowl") | contains("wicket") | contains("economy")) %>% 
    names() %>% 
    paste(collapse = ", ") %>% 
    message()
  
  # Check if player names are available
  message("\nPlayer identifier columns:")
  ipl_bbb %>%
    select(contains("player") | contains("batter") | contains("bowler")) %>%
    names() %>%
    paste(collapse = ", ") %>%
    message()
}

# ---- 3. Model (Using DID) ----
###--------------------------------------------------------------------------###
###   SECTION 3.1: Build Pre/Post Treatment Split                            ###
###--------------------------------------------------------------------------###

message("\n--- Pre/Post Impact Player split ---")

if (exists("ipl_bbb")) {
  
  # Impact Player rule introduced in IPL 2023
  ipl_bbb <- 
    ipl_bbb %>%
    mutate(
      impact_player_era = if_else(season >= 2023, "Post", "Pre"),
      impact_player_era = factor(impact_player_era, levels = c("Pre", "Post"))
    )
  
  pre_post_summary <- 
    ipl_bbb %>%
    group_by(impact_player_era, season) %>%
    summarise(
      total_balls = n(),
      total_runs = sum(runs_off_bat, na.rm = TRUE),
      total_wickets = sum(wicket, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      run_rate = (total_runs / total_balls) * 6,  # per over
      wickets_per_over = (total_wickets / total_balls) * 6
    )
  
  print(pre_post_summary)
}

# ---- 4. Visualize ----

###--------------------------------------------------------------------------###
###   SECTION 4.1: Quick Sanity Check Viz                                    ###
###--------------------------------------------------------------------------###

message("\n--- Quick viz: Run rate by season ---")

if (exists("ipl_bbb")) {
  
  run_rate_by_season <-
    ipl_bbb %>%
    group_by(season, impact_player_era) %>%
    summarise(
      run_rate = (sum(runs_off_bat, na.rm = TRUE) / n()) * 6,
      .groups = "drop"
    )
  
  run_rate_plot <-
    run_rate_by_season %>%
    ggplot(aes(x = season, y = run_rate, fill = impact_player_era)) +
    geom_col() +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = 2022.7, y = max(run_rate_by_season$run_rate) * 0.95,
             label = "Impact Player\nRule Introduced",
             color = "red", hjust = 0, size = 3.5) +
    scale_fill_manual(values = c("Pre" = "#17408B", "Post" = "#CE1141")) +
    labs(
      title = "IPL Run Rate by Season — Pre vs Post Impact Player Rule",
      x = "Season",
      y = "Average Run Rate (per over)",
      fill = "Era"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  print(run_rate_plot)
}

run_rate_by_season <- 
  ipl_bbb %>%
  mutate(season_clean = case_when(
    season == "2007/08" ~ 2008,
    season == "2009/10" ~ 2010,
    season == "2020/21" ~ 2021,
    TRUE ~ as.numeric(season)
  )) %>%
  group_by(season_clean, impact_player_era) %>%
  summarise(
    run_rate = (sum(runs_off_bat, na.rm = TRUE) / n()) * 6,
    .groups = "drop"
  )

run_rate_plot <-
  run_rate_by_season %>%
  ggplot(aes(x = season_clean, y = run_rate, fill = impact_player_era)) +
  geom_col() +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 2020, y = 9.2,
           label = "Impact Player\nRule Introduced",
           color = "red", hjust = 0, size = 3.5) +
  scale_x_continuous(breaks = seq(2008, 2026, by = 1)) +
  scale_fill_manual(values = c("Pre" = "#17408B", "Post" = "#CE1141")) +
  labs(
    title = "IPL Run Rate by Season — Pre vs Post Impact Player Rule",
    x = "Season",
    y = "Average Run Rate (per over)",
    fill = "Era"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

print(run_rate_plot)

run_rate_plot +
  scale_y_continuous(limits = c(6.5, 9.5), oob = scales::squish) +
  scale_x_continuous(
    breaks = seq(2008, 2026, by = 1),
    labels = c("2007/08", as.character(2009:2019), 
               "2020/21", as.character(2021:2026))
  )

# ---- 5. Report ----
###--------------------------------------------------------------------------###
###   SECTION 5: Scoping Summary                                             ###
###--------------------------------------------------------------------------###

message("\n\n========== IPL SCOPING SUMMARY ==========")
message("Check the following before proceeding:")
message("1. Are seasons 2008-2024 all present?")
message("2. Are batter/bowler names available for player-level analysis?")
message("3. Is there a wicket indicator column?")
message("4. Is runs_off_bat the right runs column?")
message("5. Does the run rate plot show a visible shift post-2023?")
message("==========================================\n")

###--------------------------------------------------------------------------###
# ---- 6. Save RAM space into hardisk ----
save.image("IPL_00_Scoping.RData")
message("Saved to IPL_00_Scoping.RData")

# ---- 7. Report Dependencies ----
sessionInfo()
