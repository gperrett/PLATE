library(tidyverse)

Crochet <- read_rds('Crochet.rds')[[1]]
Abbott <- read_rds('Abbott.rds')[[1]]
Ohtani <- read_rds('Ohtani.rds')[[1]]
Tong <- read_rds('Tong.rds')[[1]]


rbind(
  data.frame(theta = Abbott, pitcher = 'Andrew Abbott', ten = quantile(Abbott, .1, names = F)), 
  data.frame(theta = Tong, pitcher = 'Jonah Tong', ten = quantile(Tong, .1, names = F)), 
  data.frame(theta = Ohtani, pitcher = 'Shohei Ohtani', ten = quantile(Ohtani, .1, names = F)), 
  data.frame(theta = Crochet, pitcher = 'Garrett Crochet', ten = quantile(Crochet, .1, names = F))
) |>
  mutate(pitcher = factor(pitcher, 
                          levels = c('Andrew Abbott', 'Garrett Crochet', 'Jonah Tong', 'Shohei Ohtani'))) |>
  ggplot(aes(theta, fill = pitcher)) + 
  geom_histogram(alpha = .7, col = 'black', position = 'identity') + 
  #scale_fill_manual(values = c('#009E73', '#D55E00')) + 
  geom_vline(aes(xintercept = ten), linetype = 2) + 
  labs(x = expression(hat(theta))) + 
  facet_wrap(~pitcher, ncol = 2) + 
  theme_bw()

ggsave('compare.pdf', height = 3, width = 6)

