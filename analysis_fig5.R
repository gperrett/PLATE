library(tidyverse)
library(lubridate)
# load in the PLATE estimates from the validation modle
## see model_validate.R for the estimation of these parameters
## fitting the validation model will take ~ 12-14 hours
## relevant parameters were written to rds files and can be loaded in
validation_theta_mean <- readr::read_rds('validation_theta_mean.rds')
validation_theta_q10 <- readr::read_rds('validation_theta_10.rds')
## load in pitch data so we can link PLATE to pitchers and get conventional pitching stats
pitch_dat <- readr::read_csv('all_data_2025.csv')

# validation data
pre_september_dat <- pitch_dat |>
  filter(ymd(game_date) < ymd(20250831))
# test data
september_dat <-  pitch_dat |>
  filter(ymd(game_date) > ymd(20250831))

# see who was in the minor leagues in August
pre_september_dat <- pre_september_dat |>
  mutate(month = month(game_date)) |>
  group_by(matchup.pitcher.fullName, month) |>
  mutate(month_avg_mlb = mean(league)) |>
  ungroup()

# and who was in the majors as of September
september_dat <- september_dat |>
  mutate(month = month(game_date)) |>
  group_by(matchup.pitcher.fullName, month) |>
  mutate(month_avg_mlb = mean(league)) |>
  ungroup()

# create pitcher map to link PLATE
pre_september_pitcher_map <- pre_september_dat |>
  dplyr::select(matchup.pitcher.id) |>
  dplyr::distinct() |>
  mutate(pitcher = 1:n())

pre_september_pitcher_map <- pre_september_pitcher_map |>
  mutate(validation_theta_mean = validation_theta_mean, 
         validation_theta_q10 = validation_theta_q10)

# get pitchers who were in the minors
minors_in_aug <- pre_september_dat |>
  filter(month == 8) |>
  filter(month_avg_mlb == 0) |>
  select(matchup.pitcher.fullName, league, month_avg_mlb)  |>
  distinct() 
minors_in_aug <- minors_in_aug$matchup.pitcher.fullName

# now see who got called up
called_up <- september_dat |>
  filter(month > 8) |>
  filter(league == 1) |>
  select(matchup.pitcher.fullName)  |>
  distinct()
called_up <- called_up$matchup.pitcher.fullName
called_up <- called_up[called_up %in% minors_in_aug]

# limit our forcast to only minor league players that got called up
callup_theta <- pre_september_dat |>
  ungroup() |>
  select(matchup.pitcher.fullName, matchup.pitcher.id) |>
  distinct() |>
  left_join(pre_september_pitcher_map) |>
  filter(matchup.pitcher.fullName %in% called_up)

# gather conventional pitching stats for the major league. 
source('get_mlb_pitching_stats.R')

# outcomes
callup_theta <- callup_theta |>
  left_join(mlb_pitching_stats)

# predictors for reference models
callup_theta <- callup_theta |>
  left_join(aaa_pitching_stats)

# remove pitchers who were not in the minor leagues
callup_theta <- callup_theta |> filter(!is.na(aaa_era))

# run LOO forcasts
# we'll weight by games played to accont for varaice in ERA etc for only playing 1 game
callup_theta$k <- 1:nrow(callup_theta)
plate_era_results <- vector()
minor_era_results <- vector()
plate_era_hat <- vector()
for (i in 1:nrow(callup_theta)) {
  test_df <- callup_theta |> filter(k == i)
  forcast <- lm(era ~ validation_theta_q10 + validation_theta_mean, data = callup_theta, subset = k!=i, weights = games_pitched)
  aaa_forcast <- lm(era ~ aaa_era, data = callup_theta, subset = k!=i, weights = games_pitched)
  plate_era_results[i] <- sqrt(mean(((callup_theta$era[callup_theta$k == i] - predict(forcast, test_df)))**2))
  minor_era_results[i] <- sqrt(mean(((callup_theta$era[callup_theta$k == i] - predict(aaa_forcast, test_df)))**2))
  plate_era_hat[i] <- predict(forcast, test_df)
}

plate_fip_results <- vector()
minor_fip_results <- vector()
for (i in 1:nrow(callup_theta)) {
  test_df <- callup_theta |> filter(k == i)
  forcast <- lm(fip ~ validation_theta_q10 + validation_theta_mean, data = callup_theta, subset = k!=i, weights = games_pitched)
  aaa_forcast <- lm(fip ~ aaa_fip, data = callup_theta, subset = k!=i, weights = games_pitched)
  plate_fip_results[i] <- sqrt(mean(((callup_theta$fip[callup_theta$k == i] - predict(forcast, test_df)))**2))
  minor_fip_results[i] <- sqrt(mean(((callup_theta$fip[callup_theta$k == i] - predict(aaa_forcast, test_df)))**2))
}


plate_whip_results <- vector()
minor_whip_results <- vector()
for (i in 1:nrow(callup_theta)) {
  test_df <- callup_theta |> filter(k == i)
  forcast <- lm(whip ~ validation_theta_mean, data = callup_theta, subset = k!=i, weights = games_pitched)
  aaa_forcast <- lm(whip ~ aaa_whip, data = callup_theta, subset = k!=i, weights = games_pitched)
  plate_whip_results[i] <- sqrt(mean(((callup_theta$whip[callup_theta$k == i] - predict(forcast, test_df)))**2))
  minor_whip_results[i] <- sqrt(mean(((callup_theta$whip[callup_theta$k == i] - predict(aaa_forcast, test_df)))**2))
  }


# create Figrue 5
results <- data.frame(
  ERA = c(mean(plate_era_results), mean(minor_era_results))/sd(callup_theta$era),
  FIP = c(mean(plate_fip_results), mean(minor_fip_results))/sd(callup_theta$fip), 
  WHIP = c(mean(plate_whip_results), mean(minor_whip_results))/sd(callup_theta$whip), 
  stat = c('PLATE', 'minor league performance')
)

results |>
  pivot_longer(1:3) |>
  ggplot(aes(value, name, col = stat)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c(4, 2)) + 
  theme_bw() + 
  labs(color = NULL, 
       title = 'Leave One Out Cross Validation',
       x = 'LOO normalized root mean square error', 
       y = 'Performance in major league') + 
  theme(legend.position = 'bottom')

ggsave('validate.pdf', width = 10, height = 4.7)

# compute medians for write up
median(callup_theta$innings_pitched)
median(callup_theta$games_pitched)
