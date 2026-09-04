library(baseballr)
library(tidyverse)

# load mlb 2025 stats from baseballr
mlb_pitching_stats <- mlb_stats(
  stat_type = "season", 
  stat_group = "pitching", 
  season = 2025, 
  player_pool = "All"
)

# select the stats we will consider
mlb_pitching_stats <- mlb_pitching_stats |>
  select(player_full_name, earned_runs, era, whip, home_runs, base_on_balls, hit_by_pitch, strike_outs, innings_pitched, strikeouts_per9inn, games_pitched, games_started)

# we need to compute FIP by hand but first make things numeric
mlb_pitching_stats[2:length(mlb_pitching_stats)] <- apply(mlb_pitching_stats[2:length(mlb_pitching_stats)], 2, as.numeric)

# this will be usefull for bookeeping
mlb_pitching_stats <- mlb_pitching_stats |> mutate(league = 'mlb')

# now do the same for triple A (repeate the same process)
aaa_pitching_stats <- mlb_stats(
  stat_type = "season", 
  stat_group = "pitching", 
  season = 2025, 
  sport_id = 11,  
  player_pool = "All"
)

aaa_pitching_stats <- aaa_pitching_stats |>
  select(player_full_name, earned_runs, era, whip, home_runs, base_on_balls, hit_by_pitch, strike_outs, innings_pitched, strikeouts_per9inn, games_pitched,  games_started)

aaa_pitching_stats[2:length(aaa_pitching_stats)] <- apply(aaa_pitching_stats[2:length(aaa_pitching_stats)], 2, as.numeric)
aaa_pitching_stats <- aaa_pitching_stats |> mutate(league = 'aaa')

# combine into a single df and clean up the global env
pitching_stats <- rbind(mlb_pitching_stats, aaa_pitching_stats)
rm(mlb_pitching_stats); rm(aaa_pitching_stats)

# now we will compute FIP
pitching_stats <- pitching_stats |> 
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

# clean up the df by removing unessesary variables
pitching_stats <- pitching_stats |> select(-contains('lg'))

# rename for merging
pitching_stats <- pitching_stats |> 
  rename(matchup.pitcher.fullName = player_full_name)

# now we will get WAR stats from baseballr too
# agin we will need to rename the key
get_war <- baseballr::fg_pitcher_leaders(startseason = 2025, endseason = 2025)
get_war <- get_war |>
  rename(matchup.pitcher.fullName = PlayerName) |>
  select(WAR, matchup.pitcher.fullName)

# now we will do the merge
pitching_stats <- pitching_stats |> left_join(get_war)

# load in PLATE estimates (see model.R) for computation of these
theta <- read_rds('data/theta_mean.rds')
theta_q10 <- read_rds('data/theta_q10.rds')

# load in pitching data (collected in pull_data.R)
pitch_dat <- read_csv('data/all_data_2025.csv.zip')

# compute player time spent in the majors and select only the varibales we need
pitcher_map <- pitch_dat |>
  group_by(matchup.pitcher.id) |> 
  mutate(avg_mlb = mean(league)
         ) |>
  ungroup() |>
  dplyr::select(matchup.pitcher.id, matchup.pitcher.fullName, avg_mlb) |>
  dplyr::distinct() |>
  mutate(pitcher = 1:n())

# connect PLATE to pitcher id map
pitcher_map$theta <- theta
pitcher_map$theta_q10 <- theta_q10

# do the join 
pitching_stats <- pitching_stats |> 
  left_join(pitcher_map, relationship = 'many-to-many')

# record pitching awards
pitching_stats <-  pitching_stats |>
  mutate(award = case_when(
  matchup.pitcher.fullName == 'Tarik Skubal' ~ 1,
  matchup.pitcher.fullName == 'Garrett Crochet' ~ 1,
  matchup.pitcher.fullName == 'Hunter Brown' ~ 1,
  matchup.pitcher.fullName == 'Max Fried' ~ 1,
  matchup.pitcher.fullName == 'Bryan Woo' ~ 1, 
  matchup.pitcher.fullName == 'Hunter Brown' ~ 1,
  matchup.pitcher.fullName == 'Carlos Rodón' ~ 1,
  matchup.pitcher.fullName == 'Aroldis Chapman' ~ 1,
  matchup.pitcher.fullName == 'Jacob deGrom' ~ 1,
  matchup.pitcher.fullName == 'Drew Rasmussen' ~ 1,
  matchup.pitcher.fullName == 'Trevor Rogers' ~ 1,
  matchup.pitcher.fullName == 'Paul Skenes' ~ 1,
  matchup.pitcher.fullName == 'Cristopher Sánchez' ~ 1,
  matchup.pitcher.fullName == 'Yoshinobu Yamamoto' ~ 1,
  matchup.pitcher.fullName == 'Logan Webb' ~ 1,
  matchup.pitcher.fullName == 'Freddy Peralta' ~ 1,
  matchup.pitcher.fullName == 'Noah Cameron' ~ 2,
  matchup.pitcher.fullName == 'Jack Leiter' ~ 2,
  matchup.pitcher.fullName == 'Will Warren' ~ 2,
  matchup.pitcher.fullName == 'Braydon Fisher' ~ 2,
  matchup.pitcher.fullName == 'Shane Smith' ~ 2,
  matchup.pitcher.fullName == 'Luis Morales' ~ 2,
  matchup.pitcher.fullName == 'Cade Horton' ~ 2,
  matchup.pitcher.fullName == 'Chad Patrick' ~ 2,
  matchup.pitcher.fullName == 'Jack Dreyer' ~ 2,
  matchup.pitcher.fullName == 'Nolan McLean' ~ 2,
  matchup.pitcher.fullName == 'Jacob Misiorowski' ~ 2,
  matchup.pitcher.fullName == 'Cade Smith' ~ 3,
  matchup.pitcher.fullName == 'Bryan Abreu' ~ 3,
  matchup.pitcher.fullName == 'Aroldis Chapman' ~ 3,
  matchup.pitcher.fullName == 'David Bednar' ~ 3,
  matchup.pitcher.fullName == 'Edwin Díaz' ~ 3, 
  matchup.pitcher.fullName == 'Jhoan Duran' ~ 3, 
  matchup.pitcher.fullName == 'Abner Uribe' ~ 3, 
  matchup.pitcher.fullName == 'Mason Miller' ~ 3, 
  matchup.pitcher.fullName == 'Adrian Morejon' ~ 3, 
  matchup.pitcher.fullName =='Andrés Muñoz' ~ 3,
  matchup.pitcher.fullName == 'Nick Pivetta' ~ 1,
  matchup.pitcher.fullName == 'Andrew Abbott' ~ 1,
  matchup.pitcher.fullName == 'Zack Wheeler' ~ 1,
  matchup.pitcher.fullName == 'Nick Pivetta' ~ 1,
  matchup.pitcher.fullName == 'Jesús Luzardo' ~ 1, 
  T ~ 0
))


# special players to look at
special <- c('Tarik Skubal', 'Garrett Crochet', 'Jonah Tong', 'Andrew Abbott',
             'Michael Wacha', 'Paul Skenes',
             'Jacob Misiorowski', 'Trey Yesavage', 'Chase Burns', 
             'Mason Miller', 'Aroldis Chapman', 
              'Yoshinobu Yamamoto', 'Zack Wheeler', 
             'Kyle Freeland'
)

