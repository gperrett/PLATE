library(baseballr)
library(tidyverse)
# load in mlb stats from baseballr
mlb_pitching_stats <- mlb_stats(
  stat_type = "season", 
  stat_group = "pitching", 
  season = 2025, 
  player_pool = "All"
)


# clean and compute stats just like in get_pitching_stats.R but keep mlb and aaa seperate!
mlb_pitching_stats <- mlb_pitching_stats |>
  select(player_full_name, earned_runs, era, whip, home_runs, base_on_balls, hit_by_pitch, strike_outs, innings_pitched, strikeouts_per9inn, games_pitched, games_started)

mlb_pitching_stats[2:length(mlb_pitching_stats)] <- apply(mlb_pitching_stats[2:length(mlb_pitching_stats)], 2, as.numeric)
mlb_pitching_stats <- mlb_pitching_stats |> mutate(league = 'mlb')


mlb_pitching_stats <- mlb_pitching_stats |> 
  group_by(league) |>
  mutate(lgERA = (sum(earned_runs)*9)/sum(innings_pitched), 
         lgHR = sum(home_runs), 
         lgBB = sum(base_on_balls), 
         lgHBP = sum(hit_by_pitch), 
         lgK = sum(strike_outs), 
         lgIP = sum(innings_pitched),
         constant = lgERA - (((13*lgHR) + (3*(lgBB + lgHBP)) - (2*lgK))/lgIP)
  ) |>
  ungroup() |>
  mutate(fip = (((13*home_runs) + (3*(base_on_balls + hit_by_pitch)) - (2*strike_outs))/innings_pitched) + constant)

mlb_pitching_stats <- mlb_pitching_stats |> select(-contains('lg'))

mlb_pitching_stats <- mlb_pitching_stats |> 
  rename(matchup.pitcher.fullName = player_full_name)



aaa_pitching_stats <- mlb_stats(
  stat_type = "season", 
  stat_group = "pitching", 
  season = 2025, 
  sport_id = 11,  
  player_pool = "All"
)

aaa_pitching_stats <- aaa_pitching_stats |>
  select(player_full_name, earned_runs, era, whip, home_runs, base_on_balls, hit_by_pitch, strike_outs, innings_pitched, strikeouts_per9inn, games_pitched, games_started)

aaa_pitching_stats[2:length(aaa_pitching_stats)] <- apply(aaa_pitching_stats[2:length(aaa_pitching_stats)], 2, as.numeric)
aaa_pitching_stats <- aaa_pitching_stats |> mutate(league = 'mlb')


aaa_pitching_stats <- aaa_pitching_stats |> 
  group_by(league) |>
  mutate(lgERA = (sum(earned_runs)*9)/sum(innings_pitched), 
         lgHR = sum(home_runs), 
         lgBB = sum(base_on_balls), 
         lgHBP = sum(hit_by_pitch), 
         lgK = sum(strike_outs), 
         lgIP = sum(innings_pitched),
         constant = lgERA - (((13*lgHR) + (3*(lgBB + lgHBP)) - (2*lgK))/lgIP)
  ) |>
  ungroup() |>
  mutate(fip = (((13*home_runs) + (3*(base_on_balls + hit_by_pitch)) - (2*strike_outs))/innings_pitched) + constant)

aaa_pitching_stats <- aaa_pitching_stats |> select(-contains('lg'))

aaa_pitching_stats <- aaa_pitching_stats |> 
  rename(matchup.pitcher.fullName = player_full_name)

names(aaa_pitching_stats)[2:15] <- paste0('aaa_',names(aaa_pitching_stats)[2:15])





