library(tidyverse)

source('get_pitching_stats.R') # get 2025 pitching stats 
# note get_pitching_stats.R also gathers PLATE estimates and joins them with conventional stats

# limit pitchers to pitchers who have pitched more than 10 innings (reduce extreme outliers)
stat_plt <- pitching_stats |>
  filter(innings_pitched > 10) |>
  select(era, fip, whip, strikeouts_per9inn, theta, WAR, award, innings_pitched) |>
  rename(ERA = era, FIP = fip, WHIP = whip, K9 = strikeouts_per9inn) |>
  mutate(r_era = cor(ERA, theta, method = 'spearman'),
         r_fip = cor(FIP, theta, method = 'spearman'),
         r_whip = cor(WHIP, theta, method = 'spearman'),
         r_k9 = cor(K9, theta, method = 'spearman')) |>
  pivot_longer(1:4) |>
  mutate(name = case_when(
    name == 'ERA' ~ paste0('ERA, r = ', round(unique(r_era), 2)),
    name == 'WHIP' ~ paste0('WHIP, r = ', round(unique(r_whip), 2)),
    name == 'FIP' ~ paste0('FIP, r = ', round(unique(r_fip), 2)),
    name == 'K9' ~ paste0('K9, r = ', round(unique(r_k9), 2))
  ))

# plot it
ggplot(data = stat_plt, aes(theta, value)) + 
  geom_point(data = stat_plt |> filter(award == 0), alpha = .7) +   
  geom_point(data = stat_plt |> filter(award == 1), 
             aes(col = 'Cy Young finalist')) + 
  geom_point(data = stat_plt |> filter(award == 2), 
             aes(col = 'Rookie of year finalist')) + 
  geom_point(data = stat_plt|> filter(award == 3), 
             aes(col = 'Reliever of the year finalist')) +
  scale_color_manual(values = c(4, 7, 2)) + 
    facet_wrap( ~ name, scales = 'free_y', nrow = 1) + 
  labs(color = NULL, 
       title = expression(paste(hat(theta)[i], ' by Pitching Statistics')), 
       x = expression(hat(theta)[i]), 
       ) + 
  theme_bw() + 
  theme(legend.position = 'bottom')


ggsave('stats.pdf', width = 10, height = 4.7)
rm(list = ls())

