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
