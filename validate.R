library(tidyverse)
library(rstan)
library(lubridate)
# load all pitching data
pitch_dat <- readr::read_csv('all_data_2025.csv')

# but only train on pre-september data!
pre_september_dat <- pitch_dat |>
  filter(ymd(game_date) < ymd(20250831))

# create outcomes same as in model.R
pre_september_dat <- pre_september_dat |>
  mutate(outcome1 = ifelse(details.isOut == T & details.isStrike == T, 1, 0), 
         platoon = paste0(matchup.pitchHand.code, matchup.batSide.code))

pre_september_dat <- pre_september_dat |> 
  mutate(outcome2 = case_when(
    outcome1 == 0 & details.isOut == T ~ 1,
    outcome1 == 0 & details.isOut == F ~ 0,
    TRUE ~ -1
  ))


pre_september_dat <- pre_september_dat |> 
  mutate(outcome3 = case_when(
    outcome2 == 1 & result.rbi == 0 ~ 1,
    outcome2 == 1 & result.rbi > 0 ~ 0,
    TRUE ~ -1
  ))

pre_september_dat <- pre_september_dat |> 
  mutate(outcome4 = case_when(
    outcome2 == 0 & result.eventType == 'hit_by_pitch' ~ 1, 
    outcome2 == 0 & result.eventType == 'walk' ~ 1, 
    outcome2 == 0 & result.eventType == 'single' ~ 1, 
    outcome2 == 0 & result.eventType == 'double' ~ 0, 
    outcome2 == 0 & result.eventType == 'triple' ~ 0, 
    outcome2 == 0 & result.eventType == 'home_run' ~ 0, 
    TRUE ~ -1
  ))


pre_september_dat <- pre_september_dat |> 
  mutate(outcome5 = case_when(
    outcome4 == 1 & result.rbi == 0 ~ 1,  
    outcome4 == 1 & result.rbi > 0 ~ 0,  
    TRUE ~ -1
  ))


pre_september_dat <- pre_september_dat |> 
  mutate(outcome6 = case_when(
    outcome4 == 0 & result.rbi == 0 ~ 4,  
    outcome4 == 0 & result.rbi  == 1 ~ 3,  
    outcome4 == 0 & result.rbi  == 2 ~ 2,  
    outcome4 == 0 & result.rbi  > 2 ~ 1,  
    TRUE ~ -1
  ))


pre_september_pitcher_map <- pre_september_dat |>
  dplyr::select(matchup.pitcher.id) |>
  dplyr::distinct() |>
  mutate(pitcher = 1:n())

pre_september_batter_map <- pre_september_dat |>
  dplyr::select(matchup.batter.id) |>
  dplyr::distinct() |>
  mutate(batter = 1:n())


pre_september_dat <- pre_september_dat |> left_join(pre_september_pitcher_map)
pre_september_dat <- pre_september_dat |> left_join(pre_september_batter_map)
pre_september_dat$pitch_count <- (pre_september_dat$pitch_count - mean(pre_september_dat$pitch_count))/sd(pre_september_dat$pitch_count)
index2 <- which(pre_september_dat$outcome1 == 0)
index3 <- which(pre_september_dat$outcome2 == 1)
index4 <- which(pre_september_dat$outcome2 == 0 & pre_september_dat$outcome4 != -1)
index5 <- which(pre_september_dat$outcome4 == 1)
index6 <- which(pre_september_dat$outcome4 == 0)

pre_september_dat$home_ind <- ifelse(pre_september_dat$about.isTopInning == T, 1, 0)
stan_dat <- list(n_pitchers = nrow(pre_september_pitcher_map), 
                 n_batters = nrow(pre_september_batter_map), 
                 N = nrow(pre_september_dat), # strike outs 
                 batter = pre_september_dat$batter, 
                 pitcher = pre_september_dat$pitcher, 
                 pitch_count = pre_september_dat$pitch_count, 
                 home = pre_september_dat$home_ind, 
                 outcome1 = pre_september_dat$outcome1, 
                 index2 = index2, 
                 index3 = index3, 
                 index4 = index4, 
                 index5 = index5, 
                 index6 = index6, 
                 M = length(index2), # out or not (given non strike out)
                 outcome2 = pre_september_dat$outcome2[pre_september_dat$outcome1 == 0],
                 O = length(index3), # rbi given out
                 outcome3 = pre_september_dat$outcome3[index3],
                 P = length(index4), 
                 outcome4 = pre_september_dat$outcome4[index4],
                 Q = length(index5), 
                 outcome5 = pre_september_dat$outcome5[index5],
                 R= length(index6), 
                 K_rbi = 4, 
                 outcome6 = pre_september_dat$outcome6[index6]
)

# fit the validation model
validate <- rstan::stan(file = 'light_model.stan', 
                   data = stan_dat,
                   cores = 4, 
                   chains = 4, 
                   seed = 64,
                   iter = 3000, 
                   warmup = 1000,
                   control = list(adapt_delta = .99))

# save params needed for analysis_fig5.R
readr::write_rds(validate, 'validate.rds')
pars <- paste0('theta[', 1:nrow(pre_september_pitcher_map), ']')
validation_post <- as_tibble(as.matrix(validate, pars = pars))
validation_theta_mean <- apply(validation_post, 2, mean)
validation_theta_q10 <- apply(validation_post, 2, function(i) quantile(i, .1))
readr::write_rds(validation_theta_mean, 'validation_theta_mean.rds')
readr::write_rds(validation_theta_q10, 'validation_theta_10.rds')
rm(list = ls())

