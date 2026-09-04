library(tidyverse)
library(rstan)

# read in every plate appearacne from 2025 mlb and aaa
pitch_dat <- readr::read_csv('all_data_2025.csv')

# Assign node outcomes
# Higher Values = Always better for pitchers
# node 1 outcome strike out or not
pitch_dat <- pitch_dat |>
  mutate(outcome1 = ifelse(details.isOut == T & details.isStrike == T, 1, 0))

# node 2 outcome, given not a strike out is the batter out
pitch_dat <- pitch_dat |> 
  mutate(outcome2 = case_when(
    outcome1 == 0 & details.isOut == T ~ 1,
    outcome1 == 0 & details.isOut == F ~ 0,
    TRUE ~ -1
  ))

# node 3 outcome, given an out was there an RBI?
pitch_dat <- pitch_dat |> 
  mutate(outcome3 = case_when(
    outcome2 == 1 & result.rbi == 0 ~ 1,
    outcome2 == 1 & result.rbi > 0 ~ 0,
    TRUE ~ -1
  ))

# node 4 outcome, need to do some recording
# given there is not an out does the batter move to 1st? Or did they get a power hit?
pitch_dat <- pitch_dat |> 
  mutate(outcome4 = case_when(
    outcome2 == 0 & result.eventType == 'hit_by_pitch' ~ 1, 
    outcome2 == 0 & result.eventType == 'walk' ~ 1, 
    outcome2 == 0 & result.eventType == 'single' ~ 1, 
    outcome2 == 0 & result.eventType == 'double' ~ 0, 
    outcome2 == 0 & result.eventType == 'triple' ~ 0, 
    outcome2 == 0 & result.eventType == 'home_run' ~ 0, 
    TRUE ~ -1
  ))

# node 5 outcome, given the batter moved to first was there an RBI?
pitch_dat <- pitch_dat |> 
  mutate(outcome5 = case_when(
    outcome4 == 1 & result.rbi == 0 ~ 1,  
    outcome4 == 1 & result.rbi > 0 ~ 0,  
    TRUE ~ -1
  ))

# node 6 outcome, given there was a power hit how bad was it?
pitch_dat <- pitch_dat |> 
  mutate(outcome6 = case_when(
    outcome4 == 0 & result.rbi == 0 ~ 4,  
    outcome4 == 0 & result.rbi  == 1 ~ 3,  
    outcome4 == 0 & result.rbi  == 2 ~ 2,  
    outcome4 == 0 & result.rbi  > 2 ~ 1,  
    TRUE ~ -1
  ))


## Create pitcher and batter maps
# we will need these to link theta estimate to pitchers
pitcher_map <- pitch_dat |>
  dplyr::select(matchup.pitcher.id) |>
  dplyr::distinct() |>
  mutate(pitcher = 1:n())

batter_map <- pitch_dat |>
  dplyr::select(matchup.batter.id) |>
  dplyr::distinct() |>
  mutate(batter = 1:n())


pitch_dat <- pitch_dat |> left_join(pitcher_map)
pitch_dat <- pitch_dat |> left_join(batter_map)

# standardize pitch count to help sampler converge
pitch_dat$pitch_count <- (pitch_dat$pitch_count - mean(pitch_dat$pitch_count))/sd(pitch_dat$pitch_count)

# create index vector for Stan 
index2 <- which(pitch_dat$outcome1 == 0)
index3 <- which(pitch_dat$outcome2 == 1)
index4 <- which(pitch_dat$outcome2 == 0 & pitch_dat$outcome4 != -1)
index5 <- which(pitch_dat$outcome4 == 1)
index6 <- which(pitch_dat$outcome4 == 0)

# create stan data
stan_dat <- list(n_pitchers = nrow(pitcher_map), 
                 n_batters = nrow(batter_map), 
                 N = nrow(pitch_dat), # strike outs 
                 batter = pitch_dat$batter, 
                 pitcher = pitch_dat$pitcher, 
                 pitch_count = pitch_dat$pitch_count, 
                 outcome1 = pitch_dat$outcome1, 
                 index2 = index2, 
                 index3 = index3, 
                 index4 = index4, 
                 index5 = index5, 
                 index6 = index6, 
                 M = length(index2), # out or not (given non strike out)
                 outcome2 = pitch_dat$outcome2[pitch_dat$outcome1 == 0],
                 O = length(index3), # rbi given out
                 outcome3 = pitch_dat$outcome3[index3],
                 P = length(index4), 
                 outcome4 = pitch_dat$outcome4[index4],
                 Q = length(index5), 
                 outcome5 = pitch_dat$outcome5[index5],
                 R= length(index6), 
                 K_rbi = 4, 
                 outcome6 = pitch_dat$outcome6[index6]
)


# fit model this will take 12-20 hours!
fit <- rstan::stan(file = 'light_model.stan', 
                   data = stan_dat,
                   cores = 4, 
                   chains = 4, 
                   iter = 3000, 
                   warmup = 1000,
                   control = list(adapt_delta = .99))
# save model
write_rds(fit, 'light_model_all_2025.rds')

# extract PLATE posterior
pars <- paste0('theta[', 1:nrow(pitcher_map), ']')
post <- as_tibble(as.matrix(fit, pars = pars))
theta <- apply(post, 2, mean)
theta_q10 <- apply(post, 2, function(i) quantile(.1))
readr::write_rds(theta, 'theta_mean.rds')
readr::write_rds(theta_q10, 'theta_q10.rds')

# get Abbott and Tong and others for fig 3
pitcher_map |>
  left_join(pitch_dat) |>
  select(pitcher, matchup.pitcher.fullName) |>
  distinct() |>
  filter(matchup.pitcher.fullName == 'Jonah Tong'|
         matchup.pitcher.fullName == 'Andrew Abbott' |
         matchup.pitcher.fullName == 'Shohei Ohtani'|
        matchup.pitcher.fullName == 'Garrett Crochet')
  filter(matchup.pitcher.id)
  
Crochet <- post[,43]; readr::write_rds(Crochet, 'Crochet.rds')
Abbott <- post[,452]; readr::write_rds(Abbott, 'Abbott.rds')
Ohtani <- post[,697]; readr::write_rds(Ohtani, 'Ohtani.rds')
Tong <- post[,823]; readr::write_rds(Tong, 'Tong.rds')


# extract batter pars for fig 6
beta_so <- apply(as_tibble(as.matrix(fit, pars = paste0('beta_so[', 1:nrow(batter_map), ']'))), 2, mean)
beta_m <- apply(as_tibble(as.matrix(fit, pars = paste0('beta_m[', 1:nrow(batter_map), ']'))), 2, mean)
readr::write_rds(beta_so, "beta_node1.rds")
readr::write_rds(beta_m, 'beta_node2.rds')



