##############################
# Load Libraries
##############################
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

##############################
# Define your seasons
##############################
first <- 2009 # first season to grab. min available=2009
last <- 2018  # most recent season


# define an empty list
datalist = list()

# read in the game and play by play data for each year
# join them together
for (yr in first:last) {
  pbp   <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp   <- 
    pbp %>% 
    inner_join(games %>% 
                 distinct(game_id, week, season)) %>% 
    select(-fumble_recovery_2_yards, -blocked_player_id, -fumble_recovery_2_player_id)
  datalist[[yr]] <- pbp # add it to your list
}


# get all the years in one data frame
pbp_all <- dplyr::bind_rows(datalist)


# replace team names for teams that have moved cities
# or just changed their abbreviations
pbp_all <- 
  pbp_all %>%
  mutate_at(
    vars(home_team, away_team, posteam, defteam), 
    funs(
      case_when(
        . %in% "JAX" ~ "JAC",
        . %in% "STL" ~ "LA",
        . %in% "SD" ~ "LAC",
        TRUE ~ .
  )))

# get rid of clock plays
pbp_rp <- 
  pbp_all %>%
  filter(!is_na(epa), play_type == "pass" | play_type == "run" | play_type == "no_play")

# determine if a play is a run or a pass
pbp_rp <- 
  pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa > 0, 1 , 0)
  )

# Grab only the runs and passes
pbp_rp <- 
  pbp_rp %>% 
  filter(pass == 1 | rush == 1)

# grab only the situation you want
pbp_wpa <- 
  pbp_rp %>% 
  filter(!is.na(wpa) & wp > 0.2 & wp < 0.8 & down == 3)

# distribution for 6 7 8 yards to go
pbp_wpa %>% 
  select(ydstogo, wpa, pass, rush) %>%
  mutate(pass_or_run = if_else(pass == 1, "pass", "run")) %>%
  filter(ydstogo < 11) %>%
  ggplot() +
  ggridges::geom_density_ridges(alpha = 0.25, aes(fill = pass_or_run, x = wpa, y = factor(ydstogo))) +
  theme_bw()

#  break it down by field position
pbp_wpa %>%
  mutate(pass_or_run = if_else(pass == 1, "pass", "run")) %>%
  mutate(field_pos = case_when(yardline_100 < 20 ~ "red_zone",
                               20 <= yardline_100  & yardline_100 < 80 ~ "between_20s",
                               80 <= yardline_100 & yardline_100 < 100 ~ "own_end")) %>%
  filter(ydstogo < 11) %>%
  group_by(pass_or_run, ydstogo, field_pos) %>%
  summarise(ep = mean(ep, na.rm = T)) %>%
  ggplot(aes(x = ydstogo, colour = pass_or_run, y = ep)) +
  geom_point() +
  geom_line() +
  facet_grid(~ field_pos) +
  theme_bw()

chart <- 
  pbp_rp %>% 
  filter(!is.na(wpa) & wp > 0.2 & wp < 0.8 & down == 3) %>%
  mutate(pass_or_run = if_else(pass == 1, "pass", "run")) %>%
  group_by(ydstogo, play_type) %>%
  summarize( n = n(),
             wpa = mean(wpa, na.rm = T),
             epa = mean(epa, na.rm = T))
