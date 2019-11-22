library(tidyverse)
library(snakecase)

read_train_data <- function(){
  train <- 
    read_csv('Data/train.csv', col_types = list(WindSpeed = col_character())) %>%
    rename(velocity = S, accel = A)  %>%
    rename_all(to_snake_case) %>% 
    mutate(to_left = play_direction == "left", 
           is_ball_carrier = nfl_id == nfl_id_rusher,
           team_on_offense = ifelse(possession_team == home_team_abbr, "home", "away"),
           is_on_offense = team == team_on_offense,  ## Is player on offense?
           yards_from_own_goal = if_else(as.character(field_position) == possession_team, 
                                         yard_line, 
                                         50 + (50 - yard_line)),
           yards_from_own_goal = if_else(yard_line == 50, 50, yards_from_own_goal),
           x_old = x,
           y_old = y,
           x           = if_else(to_left, 120 - x, x), ## Standardizes X
           y           = if_else(to_left, 160/3 - y, y),
           orient_std_1= if_else(to_left & orientation < 90, orientation + 360, orientation),
           orient_std_1= if_else(!to_left & orientation > 270, orientation - 360, orient_std_1),
           orient_std_2= if_else(to_left, orient_std_1 - 180, orient_std_1),
           dir_std_1   = if_else(to_left & dir < 90, dir + 360, dir),
           dir_std_1   = if_else(!to_left & dir > 270, dir - 360, dir_std_1),
           dir_std_2   = if_else(to_left, dir_std_1 - 180, dir_std_1),
           end_yard_line = yards_from_own_goal + yards)    ## Standardized Y
  
  return(train)
  
}

