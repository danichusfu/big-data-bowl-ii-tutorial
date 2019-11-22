library(tidyverse)

source("read_train_data.R")
source("helper_functions/plot_field.R")


train <- read_train_data()

train <-
  train %>%
  mutate(temp = x,
         x = 160/3 - y,
         y = temp,
         x_end = x + velocity * cos((180 - dir_std_2) * pi / 180),
         y_end = y + velocity * sin((180 - dir_std_2) * pi / 180))

one_play <-
  train %>%
  group_by(game_id, play_id) %>%
  filter(group_indices() == 1)
 # filter(play_id == 20170910001102)


plot_field(y_min = 30, y_max = 60) +
  geom_point(data = one_play, aes(x = x, y = y, colour = team), size = 7) +
  geom_segment(data = one_play, aes(x = x, y = y, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.4, "cm")),
               size = 0.75, color = "black") +
  geom_text(data = one_play, aes(x = x, y = y, group = nfl_id, label = jersey_number), color = 'black', size = 4) +
  scale_color_manual(values = c("lightblue", "red"))

