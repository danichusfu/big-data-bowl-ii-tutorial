library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

first <- 2009 #first season to grab. min available=2009
last <- 2018 # most recent season

datalist = list()

for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards, -blocked_player_id, -fumble_recovery_2_player_id)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_all <- dplyr::bind_rows(datalist)
pbp_all <- pbp_all %>%
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  )))
pbp_rp <- pbp_all %>%
  filter(!is_na(epa), play_type == "pass" | play_type == "run" | play_type == "no_play")
pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  )
pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)
pbp_wpa <- pbp_rp %>% filter(!is.na(wpa) & wp > 0.2 & wp < 0.8 & down == 3)
pbp_wpa %>%
  ggplot() +
  geom_point(aes(x = ydstogo, y = (wpa*pass)), alpha = 0.2, color = "blue") +
  geom_point(aes(x = ydstogo, y = (wpa*rush)), alpha = 0.2, color = "orange") +
  labs(
    title = "WPA distribution by Pass/Rush on 3rd and x",
    y = "WPA",
    x = "Yards to Go",
    caption = "@nflscrapR",
    subtitle = "2009-2018 | Blue = Pass | Orange = Rush"
  ) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 12))
chart <- pbp_rp %>% filter(!is.na(wpa) & wp > 0.2 & wp < 0.8 & down == 3) %>%
  group_by(ydstogo) %>%
  summarize(
    n_pass = sum(pass),
    n_rush = sum(rush),
    wpa_pass = sum(wpa*pass) / n_pass,
    wpa_rush = sum(wpa*rush) / n_rush,
    epa_pass = sum(epa*pass) / n_pass,
    epa_rush = sum(epa*rush) / n_rush,
  )
chart %>%
  ggplot() +
  geom_line(aes(x = ydstogo, y = wpa_pass), color = "blue") +
  geom_line(aes(x = ydstogo, y = wpa_rush), color = "orange") +
  labs(title = "WPA on 3rd and x (Neutral WP)",
       subtitle = "2009-2018 | Blue = Pass | Orange = Run",
       caption = "@nflscrapR",
       x = "Yards to Go",
       y = "WPA") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 12)) +
  scale_x_continuous(breaks = c(1:15)) +
  coord_cartesian(xlim = c(1:15)) 


chart %>%
  ggplot() +
  geom_line(aes(x = ydstogo, y = wpa_pass), color = "blue") +
  geom_line(aes(x = ydstogo, y = wpa_rush), color = "orange") +
  labs(title = "EPA on 3rd and x (Neutral WP)",
       subtitle = "2009-2018 | Blue = Pass | Orange = Run",
       caption = "@nflscrapR",
       x = "Yards to Go",
       y = "EPA") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 12)) +
  scale_x_continuous(breaks = c(1:15)) +
  coord_cartesian(xlim = c(1:15)) 