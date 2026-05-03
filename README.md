# IPL Impact Player Rule — Causal Analysis

**Author:** Keziah Vickraman  
**Supervisor:**  

## Overview
Causal analysis of the IPL Impact Player rule (introduced 2023) examining 
its effect on run rates, competitive balance, and all-rounder deployment 
across IPL, BBL, PSL, and CPL using Difference-in-Differences and 
Synthetic Control methods.

## Files
- `IPL_00_Data_Scoping.R` — data validation across all leagues
- `IPL_01_Causal_DiD.R` — DiD analysis + visualisations
- `IPL_02_Synthetic_Control.R` — synthetic control counterfactual + Manjrekar hypothesis test
- `context_of_paper.Rmd` — paper context and background for non-cricket audiences

## Key Findings
1. Run rates increased +1.3 runs/over post-rule (significant)
2. All-rounder deployment fell by 1.2 players per match (significant)
3. Synthetic control confirms all-rounder decline cannot be explained 
   by global T20 trends alone
4. Position 8 strike rate jumped +16 points while bowling contribution 
   fell 9.5 percentage points — empirical confirmation of specialist 
   displacement

## Data
Ball-by-ball data via `cricketdata` R package (Cricsheet)

## Policy Context
This analysis is timed to the BCCI's live review of the Impact Player rule,
announced immediately after IPL 2026. The following sources informed the 
policy framing:

- [BCCI to review Impact Player rule after IPL 2026 — India Today](https://www.indiatoday.in/sports/cricket/story/ipl-impact-player-rule-to-be-reviewed-bcci-devajit-saikia-2904850-2026-05-02)
- [Manjrekar, Washington Sundar, Ashutosh Sharma on the Impact Player rule — Sportstar](https://sportstar.thehindu.com/cricket/ipl/ipl-2026-impact-player-rule-ashutosh-washington-dube-sanjay-manjrekar-quotes/article70931888.ece)
- [Will BCCI change the Impact Player rule? — Odisha TV](https://odishatv.in/sports/ipl-2026-will-bcci-change-impact-player-rule-11792291)
- [BCCI to review controversial Impact Player rule — LiveMint](https://www.livemint.com/sports/cricket-news/ipl-2026-bcci-to-review-controversial-impact-player-rule-after-end-of-ongoing-season-says-devajit-saikia-11777744679335.html)
