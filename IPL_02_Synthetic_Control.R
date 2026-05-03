###--------------------------------------------------------------------------###
###   IPL Impact Player Rule — Synthetic Control Analysis                    ###
###   Script: IPL_02_Synthetic_Control.R                                     ###
###   Method: Synthetic Control                                              ###
###   Treatment: IPL 2023 (Impact Player rule)                               ###
###   Donor pool: BBL, PSL, CPL                                              ###
###   Outcomes: run_rate, avg_allrounders_per_match                          ###
###--------------------------------------------------------------------------###

rm(list = ls())

# ---- 0. Set Dependencies ----
pacman::p_load(tidyverse, glue, cricketdata, Synth, scales)

# ---- 1. Collect & Import Data ----
## ---- 1.1 Importation of Data Across all major Cricket Leagues ----
### From now on, major cricket leagues will be references as follows:
### - Indian Premiere League (IPL)
### - Big Bash League (BBL)
### - Pakistan Super League (PSL)
### - Caribbean Premiere League (CPL)
###--------------------------------------------------------------------------###
###   SECTION 1: Load & Process All Leagues                                  ###
###--------------------------------------------------------------------------###

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

all_leagues <- bind_rows(ipl_metrics, bbl_metrics, psl_metrics, cpl_metrics)

# ---- 2. Wrangle Data ----
## Missing years of data (PSL) causes interpolation failures --> dropping that year
###--------------------------------------------------------------------------###
###   SECTION 2: Build Balanced Panel                                        ###
###--------------------------------------------------------------------------###

common_years <-
  all_leagues %>%
  group_by(season_yr) %>%
  summarise(n_leagues = n_distinct(league), .groups = "drop") %>%
  filter(n_leagues >= 3,
         season_yr >= 2015,
         season_yr <= 2025,
         season_yr != 2024) %>%
  pull(season_yr)

message(glue("Panel years: {paste(common_years, collapse=', ')}"))

panel_balanced <-
  all_leagues %>%
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
    run_rate        = zoo::na.approx(run_rate,        na.rm = FALSE),
    avg_allrounders = zoo::na.approx(avg_allrounders, na.rm = FALSE),
    avg_margin      = zoo::na.approx(avg_margin,      na.rm = FALSE)
  ) %>%
  ungroup()

message("League row counts (should all be equal):")
print(panel_balanced %>% count(league))

pre_years  <- common_years[common_years < 2023]
post_years <- common_years[common_years >= 2023]
donor_ids  <- c(2L, 3L, 4L)

# ---- 3. Model (Here, Synthetic Control) ----
## We determined that impact player DID infact cause some differences -- favour for batsmen relative to bowlers
## But also coming at a hidden cost to all-rounders.
## However, DiD alone cannot rule out the
# possibility that these trends were already underway globally across
# all T20 leagues — independent of the rule itself.

## Now we investigate, what if? 
## What if, there were no policy in place, how would that have affected the league
# To isolate the rule's causal effect we employ a Synthetic Control
# approach (Abadie et al., 2010). This method constructs a weighted
# "counterfactual IPL" from leagues that never adopted the Impact
# Player rule — BBL (Australia), PSL (Pakistan), and CPL (Caribbean).
# The counterfactual answers the question: what would IPL run rates
# and all-rounder usage have looked like in 2023-2025 had the rule
# never been introduced?

# Crucially, this donor pool also serves a secondary analytical purpose.
# The IPL is played in India under hot, dry conditions on flat pitches
# that inherently favour batters. By contrast, BBL pitches offer
# lateral movement for seamers, PSL surfaces provide pace and bounce,
# and CPL conditions are more varied. If the rule's batting bias is
# driven partly by Indian conditions rather than the rule alone, the
# synthetic control will underestimate the true effect — because the
# donor leagues would show less batting dominance even under the same
# rule. Any gap we find between actual and synthetic IPL is therefore
# a conservative lower-bound estimate of the rule's true impact in
# Indian conditions.

## This segment aims to quantify that.

## ---- 3.1 Synthetic Control — Run Rate ----
###--------------------------------------------------------------------------###
###   SECTION 3.1: Synthetic Control — Run Rate (for batsmen)                ###
###--------------------------------------------------------------------------###

message("\nRunning synthetic control: Run Rate...")

dataprep_rr <- dataprep(
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

synth_rr <- synth(dataprep_rr)

weights_rr <-
  tibble(league = c("BBL", "PSL", "CPL"),
         weight = as.numeric(synth_rr$solution.w))

synth_rr_combined <-
  panel_balanced %>%
  filter(league == "IPL") %>%
  select(season_yr, actual = run_rate) %>%
  left_join(
    panel_balanced %>%
      filter(league != "IPL") %>%
      left_join(weights_rr, by = "league") %>%
      group_by(season_yr) %>%
      summarise(synthetic = sum(run_rate * weight, na.rm = TRUE), .groups = "drop"),
    by = "season_yr"
  ) %>%
  mutate(gap = actual - synthetic)

## ---- 3.2 Synthetic Control — All-rounders  ----

###--------------------------------------------------------------------------###
###   SECTION 3.2: Synthetic Control — All-rounders                          ###
###--------------------------------------------------------------------------###

message("\nRunning synthetic control: All-rounders...")

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

weights_ar <-
  tibble(league = c("BBL", "PSL", "CPL"),
         weight = as.numeric(synth_ar$solution.w))

synth_ar_combined <-
  panel_balanced %>%
  filter(league == "IPL") %>%
  select(season_yr, actual = avg_allrounders) %>%
  left_join(
    panel_balanced %>%
      filter(league != "IPL") %>%
      left_join(weights_ar, by = "league") %>%
      group_by(season_yr) %>%
      summarise(synthetic = sum(avg_allrounders * weight, na.rm = TRUE), .groups = "drop"),
    by = "season_yr"
  ) %>%
  mutate(gap = actual - synthetic)

# ---- 4. Visualize  ----

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
      plot.caption     = element_text(size = 8,  color = "gray50", face = "italic"),
      axis.title       = element_text(face = "bold", size = 10),
      axis.text        = element_text(size = 9),
      legend.position  = "top"
    )
}

plot_synth <- function(data, y_label, title, subtitle) {
  data %>%
    pivot_longer(cols = c(actual, synthetic),
                 names_to = "series", values_to = "value") %>%
    mutate(series = if_else(series == "actual",
                            "Actual IPL",
                            "Synthetic IPL (counterfactual)")) %>%
    ggplot(aes(x = season_yr, y = value,
               color = series, linetype = series)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 3) +
    geom_vline(xintercept = 2022.5, linetype = "dashed",
               color = "black", linewidth = 0.8) +
    annotate("rect",
             xmin = 2022.5, xmax = max(data$season_yr) + 0.5,
             ymin = -Inf,   ymax = Inf,
             fill = "#CE1141", alpha = 0.05) +
    annotate("text", x = 2023.1,
             y = min(data$synthetic, na.rm = TRUE) * 1.02,
             label = "Treatment\nPeriod",
             color = "#CE1141", size = 3, hjust = 0) +
    scale_color_manual(values = c("Actual IPL" = "#CE1141",
                                  "Synthetic IPL (counterfactual)" = "#17408B")) +
    scale_linetype_manual(values = c("Actual IPL" = "solid",
                                     "Synthetic IPL (counterfactual)" = "dashed")) +
    scale_x_continuous(breaks = common_years) +
    labs(title = title, subtitle = subtitle,
         x = "Season", y = y_label,
         color = NULL, linetype = NULL,
         caption = "Synthetic IPL = weighted combination of BBL, PSL, CPL | Data: Cricsheet") +
    theme_ipl() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

plot_gap <- function(data, y_label, title, subtitle, line_color) {
  data %>%
    ggplot(aes(x = season_yr, y = gap)) +
    geom_line(color = line_color, linewidth = 1.3) +
    geom_point(color = line_color, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 2022.5, linetype = "dashed",
               color = "gray50", linewidth = 0.8) +
    annotate("text", x = 2023.1, y = 0,
             label = "Rule introduced →",
             color = "gray30", size = 3, hjust = 0) +
    scale_x_continuous(breaks = common_years) +
    labs(title = title, subtitle = subtitle,
         x = "Season", y = y_label,
         caption = "Data: Cricsheet via cricketdata R package") +
    theme_ipl() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

fig6 <- plot_synth(
  synth_rr_combined,
  y_label  = "Run Rate (per over)",
  title    = "Synthetic Control: IPL Run Rate vs Counterfactual",
  subtitle = "What would run rates have looked like without the Impact Player rule?"
)

fig7 <- plot_synth(
  synth_ar_combined,
  y_label  = "Avg All-rounders per Match",
  title    = "Synthetic Control: All-rounder Usage vs Counterfactual",
  subtitle = "Would the all-rounder decline have happened anyway without the rule?"
)

fig8 <- plot_gap(
  synth_rr_combined,
  y_label    = "Gap (runs per over)",
  title      = "Treatment Effect: Run Rate Gap (Actual − Synthetic IPL)",
  subtitle   = "Positive = Impact Player rule increased run rates above counterfactual",
  line_color = "#CE1141"
)

fig9 <- plot_gap(
  synth_ar_combined,
  y_label    = "Gap (all-rounders per match)",
  title      = "Treatment Effect: All-rounder Gap (Actual − Synthetic IPL)",
  subtitle   = "Negative = Impact Player rule reduced all-rounder usage below counterfactual",
  line_color = "#17408B"
)

print(fig6)
print(fig7)
print(fig8)
print(fig9)

# ---- 5. Report  ----
## ---- 5.1 Summary of findings  ----
###--------------------------------------------------------------------------###
###   SECTION 5.1: Summary                                                   ###
###--------------------------------------------------------------------------###

cat("\n========== SYNTHETIC CONTROL SUMMARY ==========\n")
cat("\nRun Rate Weights:\n");    print(weights_rr)
cat("\nAll-rounder Weights:\n"); print(weights_ar)

cat("\nRun Rate Gap post-2023:\n")
synth_rr_combined %>% filter(season_yr >= 2023) %>% print()

cat("\nAll-rounder Gap post-2023:\n")
synth_ar_combined %>% filter(season_yr >= 2023) %>% print()

cat("\nLimitations:\n")
cat("- 3 donor leagues only\n")
cat("- 2024 excluded due to missing PSL data\n")
cat("- No placebo tests yet\n")
cat("================================================\n")

## ---- 5.2 Real world validation: Pundits' critique  ----
# The aggregate findings from the DiD and Synthetic Control analyses are
# further corroborated by qualitative evidence from cricket practitioners.
# Former Indian international batter Sanjay Manjrekar articulated the
# structural concern most precisely:
#
# "In Indian conditions, an Impact Sub batter has a far greater impact
#  than an Impact Sub bowler, and that has added to everything being in
#  favour of the batting side. Imagine the Impact Player rule in New
#  Zealand where the ball is swinging — an extra seamer would do wonders.
#  However, in Indian conditions, Impact Player hasn't worked because an
#  extra batter is having more impact than an extra bowler, which is also
#  a reason why we are seeing such high scores because we are seeing pure
#  batters like Ashutosh Sharma playing at number 8."
#  — Sanjay Manjrekar, Sportstar Insight Edge Podcast (2026)
#
# Manjrekar's observation identifies two testable claims:
#   (i)  Lower-order batters (positions 7-9) are hitting at higher
#        strike rates post-rule — consistent with pure batting
#        specialists replacing genuine all-rounders in those slots
#   (ii) Those same lower-order players are contributing less with
#        the ball — confirming the all-rounder archetype is being
#        systematically displaced

# We test both claims directly using ball-by-ball data.
# The results are unambiguous:
#
#   Strike Rate at Position 8:
#   Pre-rule:  97.9  →  Post-rule: 114.0  (+16.1 points)
#
#   % of Position 8 Batters Who Also Bowled:
#   Pre-rule:  44.4%  →  Post-rule: 34.9%  (-9.5 percentage points)
#
# These findings provide empirical confirmation of what practitioners
# have observed anecdotally. The Impact Player rule has not merely
# changed tactics at the margins — it has restructured the fundamental
# player archetype that occupies the lower middle order.
#
# The all-rounder — historically cricket's most strategically valuable
# player type precisely because of their dual contribution — is being
# rendered redundant in the IPL context. A player who bats at 8 and
# bowls 3 overs is now less valuable to a franchise than a specialist
# batter who hits at 150+ strike rate and never bowls at all.
#
# This has downstream implications beyond the IPL itself. If franchise
# cricket's most lucrative and visible league no longer rewards
# all-round ability, the incentive for young cricketers globally to
# develop both skills diminishes. The player development pipeline —
# particularly in India where the IPL is the dominant career aspiration
# — may increasingly produce batting specialists at the expense of the
# all-rounders that make Test cricket and ODI cricket strategically rich.
#
# This is the hidden cost the BCCI must weigh in its post-IPL 2026 review.
###--------------------------------------------------------------------------###

### ---- 5.2.1 Analysis of Pundit's Claim  ----
###--------------------------------------------------------------------------###
###   5.2.1 Case of Ashutosh (quality batsman at number 8)                   ###
###--------------------------------------------------------------------------###
# The Ashutosh Sharma Case
# Sanjay Manjrekar specifically names Ashutosh Sharma batting at number 8 as the symptom. 
#Your data can test this directly.
# The argument:
#   Pre-rule, a number 8 batter was typically a genuine 
# all-rounder — someone who could bowl 3-4 overs AND contribute with the bat. 
# Post-rule, teams can slot in a pure batter at number 8 because they don't need him to bowl 
# — the Impact Player substitution has already covered the bowling slot.
###--------------------------------------------------------------------------###

# Using ipl_bbb — track batting position distribution over time
# Specifically: what is the average balls faced / strike rate
# for batters coming in at position 7, 8, 9 pre vs post rule?

ipl_bbb <- fetch_cricsheet(competition = "ipl",
                           gender = "male",
                           type = "bbb")
batting_position <-
  ipl_bbb %>%
  filter(!extra_ball) %>%
  group_by(match_id, season, batting_team, striker) %>%
  summarise(
    balls_faced  = n(),
    runs_scored  = sum(runs_off_bat, na.rm = TRUE),
    strike_rate  = (runs_scored / balls_faced) * 100,
    .groups = "drop"
  ) %>%
  group_by(match_id, season, batting_team) %>%
  mutate(batting_position = row_number()) %>%
  ungroup()

# Compare lower order (pos 7-9) strike rates pre vs post
lower_order <-
  batting_position %>%
  filter(batting_position %in% 7:9) %>%
  mutate(era = if_else(season >= 2023, "Post", "Pre")) %>%
  group_by(era, batting_position) %>%
  summarise(
    avg_strike_rate = mean(strike_rate, na.rm = TRUE),
    avg_balls_faced = mean(balls_faced, na.rm = TRUE),
    .groups = "drop"
  )

print(lower_order)

# For each player who batted at position 7-9,
# did they also bowl in the same match?

lower_order_players <-
  batting_position %>%
  filter(batting_position %in% 7:9) %>%
  select(match_id, season, striker, batting_position)

bowler_appearances <-
  ipl_bbb %>%
  distinct(match_id, bowler) %>%
  rename(striker = bowler) %>%
  mutate(bowled = 1)

lower_order_allrounder <-
  lower_order_players %>%
  left_join(bowler_appearances, by = c("match_id", "striker")) %>%
  mutate(
    bowled = replace_na(bowled, 0),
    era = if_else(season >= 2023, "Post", "Pre")
  ) %>%
  group_by(era, batting_position) %>%
  summarise(
    pct_also_bowled = mean(bowled) * 100,
    .groups = "drop"
  )

print(lower_order_allrounder)

# Run these first
print(lower_order)
print(lower_order_allrounder)

# Plot A — Strike Rate Pre vs Post by Position
plot_A <-
  lower_order %>%
  mutate(batting_position = factor(batting_position,
                                   labels = c("Position 7",
                                              "Position 8", 
                                              "Position 9")),
         era = factor(era, levels = c("Pre", "Post"))) %>%
  ggplot(aes(x = batting_position, y = avg_strike_rate, 
             fill = era)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(avg_strike_rate, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Pre" = "#17408B", 
                               "Post" = "#CE1141")) +
  labs(
    title    = "Lower Order Strike Rates — Pre vs Post Impact Player Rule",
    subtitle = "Position 8 saw the biggest jump (+16 points) — pure batters replacing all-rounders",
    x = "Batting Position", 
    y = "Average Strike Rate",
    fill = "Era",
    caption = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl()

# Plot B — % Also Bowled Pre vs Post by Position  
plot_B <-
  lower_order_allrounder %>%
  mutate(batting_position = factor(batting_position,
                                   labels = c("Position 7",
                                              "Position 8",
                                              "Position 9")),
         era = factor(era, levels = c("Pre", "Post"))) %>%
  ggplot(aes(x = batting_position, y = pct_also_bowled,
             fill = era)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(pct_also_bowled, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Pre" = "#17408B",
                               "Post" = "#CE1141")) +
  labs(
    title    = "% of Lower Order Batters Who Also Bowled — Pre vs Post Impact Player Rule",
    subtitle = "All-rounder deployment at positions 7–9 declined across the board post-2023",
    x = "Batting Position",
    y = "% Who Also Bowled in Same Match",
    fill = "Era",
    caption = "Data: Cricsheet via cricketdata R package"
  ) +
  theme_ipl()

gridExtra::grid.arrange(plot_A, plot_B)
###--------------------------------------------------------------------------###
# ---- 6. Save RAM space into hardisk ----
save.image("IPL_02_Synthetic_Control_Results.RData")
message("\nSaved to IPL_02_Synthetic_Contro.RData")

# ---- 7. Report Dependencies ----
sessionInfo()