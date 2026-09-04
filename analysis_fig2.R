library(tidyverse)
library(patchwork)
library(ggrepel)
library(latex2exp)
source('get_pitching_stats.R')

# remove players who dont have a WAR rating
war_plt <- pitching_stats |>
  filter(!is.na(WAR)) |>
  select(matchup.pitcher.fullName, WAR, theta, award) |>
  distinct()

p1 <- ggplot(data = war_plt, aes(theta, WAR)) + 
  geom_point(data = war_plt |> filter(award == 0), alpha = .7) +   
  geom_point(data = war_plt |> filter(award == 1), 
             aes(col = 'Cy Young finalist')) + 
  geom_point(data = war_plt |> filter(award == 2), 
             aes(col = 'Rookie of year finalist')) + 
  geom_point(data = war_plt |> filter(award == 3), 
             aes(col = 'Reliever of the year finalist')) +
  scale_color_manual(values = c(4, 7, 2)) + 
  labs(color = NULL, 
       title = expression(paste(hat(theta)[i], ' by WAR')), 
       x = expression(hat(theta)[i]), 
       subtitle = paste0('r = ', round(cor(war_plt$WAR, war_plt$theta, method = 'spearman'), 2))) + 
  theme_bw()


x_lims <- layer_scales(p1)$x$get_limits()
y_lims <- layer_scales(p1)$y$get_limits()

war_plt <- war_plt |> filter(matchup.pitcher.fullName %in% special)
p2 <- ggplot(data = war_plt, aes(theta, WAR)) + 
  geom_point(data = war_plt |> filter(award == 0), alpha = .7) +   
  geom_point(data = war_plt |> filter(award == 1), 
             aes(col = 'Cy Young finalist')) + 
  geom_point(data = war_plt |> filter(award == 2), 
             aes(col = 'Rookie of year finalist')) + 
  geom_point(data = war_plt |> filter(award == 3), 
             aes(col = 'Reliever of the year finalist')) +
  geom_label_repel(
    data = war_plt, 
    aes(theta, WAR, label = matchup.pitcher.fullName),
    size = 2.4, max.iter = 5000,
    box.padding = unit(1, "lines"),   # Adds generous spacing around the label box
    point.padding = unit(0, "lines"), # Adds generous spacing around the data point
    min.segment.length = 0            # Forces segments/lines to draw even with large gaps
  ) + 
  coord_cartesian(xlim = x_lims, ylim = y_lims) + 
  #geom_label_repel(data = special, aes(theta, WAR, label = matchup.pitcher.fullName)) + 
  scale_color_manual(values = c(4, 7, 2)) + 
  labs(color = NULL, 
       title = expression(paste(hat(theta)[i], ' by WAR')), 
       x = expression(hat(theta)[i]), 
       subtitle = paste0('r = ', round(cor(war_plt$WAR, war_plt$theta), 2)) 
  ) + 
  theme_bw()

p1 <- p1 + theme(legend.position = 'bottom')
p2 <- p2 + theme(legend.position = 'none',axis.text.y = element_blank(), axis.ticks.y = element_blank())
p2 <- p2 + labs(title = NULL, subtitle = NULL, y = NULL)

p1 + p2
ggsave('fig1.pdf', width = 10, height = 5.8)


# what happens if we only concider starting pitcher workloads? 
war_plt <- pitching_stats |>
  filter(!is.na(WAR)) |>
  select(matchup.pitcher.fullName, WAR, theta, award, innings_pitched) |>
  distinct() |>
  filter(innings_pitched >= 100)
p3 <- ggplot(data = war_plt, aes(theta, WAR)) + 
  geom_point(data = war_plt |> filter(award == 0), alpha = .7) +   
  geom_point(data = war_plt |> filter(award == 1), 
             aes(col = 'Cy Young finalist')) + 
  geom_point(data = war_plt |> filter(award == 2), 
             aes(col = 'Rookie of year finalist')) + 
  geom_point(data = war_plt |> filter(award == 3), 
             aes(col = 'Reliever of the year finalist')) +
  scale_color_manual(values = c(4, 7, 2)) + 
  labs(color = NULL, 
       title = expression(paste(hat(theta)[i], ' by WAR')), 
       x = expression(hat(theta)[i]), 
       subtitle = paste0('r = ', round(cor(war_plt$WAR, war_plt$theta, method = 'spearman'), 2))) + 
  theme_bw()
p3
