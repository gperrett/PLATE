library(tidyverse)

# load in batter strength params of interest
beta_node1 <- readr::read_rds('beta_node1.rds')
beta_node2 <- readr::read_rds('beta_node2.rds')

# read in every plate appearance from 2025 mlb and aaa and get batter map to link params to names
pitch_dat <- readr::read_csv('all_data_2025.csv')

# here is our map for battes
batter_map <- pitch_dat |>
  dplyr::select(matchup.batter.id) |>
  dplyr::distinct() |>
  mutate(batter = 1:n())

node1_top_10 <- batter_map |>
  mutate(beta_node1= beta_node1, 
         beta_node2= beta_node2) |>
  left_join(pitch_dat |> 
              select(matchup.batter.id,
                     matchup.batter.fullName)) |>
  distinct() |>
  arrange(desc(beta_node1)) |>
  slice(1:10)

node2_top_10 <- batter_map |>
  mutate(beta_node1= beta_node1,
         beta_node2= beta_node2) |>
  left_join(pitch_dat |> 
              select(matchup.batter.id,
                     matchup.batter.fullName)) |>
  distinct() |>
  arrange(desc(beta_node2)) |>
  slice(1:10)


rbind(node1_top_10, node2_top_10) |>
ggplot(aes(beta_node1, beta_node2, label = matchup.batter.fullName)) + 
  geom_point() + 
    geom_label_repel(
      max.overlaps = 20,
      size = 2.4, max.iter = 8000,
      box.padding = unit(1, "lines"),   # Adds generous spacing around the label box
      point.padding = unit(0, "lines"), # Adds generous spacing around the data point
      min.segment.length = 0            # Forces segments/lines to draw even with large gaps
    ) + 
  labs(color = NULL, 
       #title = expression(paste(hat(theta)[i], ' by WAR')), 
       x = 'Batter Strength (Strike Outs)', 
       y = 'Batter Strength (On Base|No Strike Out)') + 
  theme_bw()

ggsave('batters.pdf', width = 10, height = 4)

