library(tidyverse)

plot_field <- function(y_min = 38, y_max = 78, paint_size = 7){
  y_min_nums <- ceiling(y_min/10) * 10
  y_min_5    <- ceiling(y_min/5) * 5
  
  ggplot() +
    # Every 5 yards bold line
    geom_line(data = tibble(y = seq(y_min_5, y_max, by = 5), 
                            x = list(c(0, 160/3))) %>% 
                mutate(group = row_number()) %>% 
                filter(!y %in% c(5, 115)) %>%
                unnest(cols= c(x)), 
              aes(x = x, y = y, group = group), linetype = "solid") +
    # Sidelines
    geom_line(data = tibble(x     = c(0, 160/3),
                            y     = list(c(y_min, y_max)),
                            group = c(1, 2)) %>%
                unnest(cols = c(y)),
              aes(x = x, y = y, group = group)) +
    # Numbers
    geom_text(data = tibble(x     = list(c(12, 124/3)),
                            y     = seq(y_min_nums, y_max, by = 10),
                            angle = list(c(-90, 90))) %>%
                mutate(label = 50 -abs(y - 60)) %>%
                filter(label > 0) %>%
                unnest(cols = c(x, angle)) %>%
                mutate(hjust = case_when(label == 50 ~ 0.5,
                                         y > 60 & x == 12    ~ 0.65,
                                         y < 60 & x == 124/3 ~ 0.65,
                                         T ~ 0.35),
                       label = case_when(label == 50 ~ as.character(label),
                                         y > 60 & x == 12    ~ paste0("\u02F1", label),
                                         y < 60 & x == 124/3 ~ paste0("\u02F1", label),
                                         T ~ paste0(label, "\u02F2"))),
              aes(label = label, x = x, y = y, angle = angle, hjust = hjust), size = paint_size) +
    # LOS
    #annotate("text", x = -4.5, y = 0, label = "LOS:", size = paint_size, vjust = 0.3) +
    # Hashes
    geom_text(data = tibble(x     = list(c(0, 71/3, 89/3, 160/3)),
                            y     = y_min:y_max,
                            hjust = list(c(0, 0, 1, 1))) %>%
                unnest(cols = c(x, hjust)) %>%
                mutate(label = case_when(y %% 5 == 0 & x %in% c(71/3, 89/3)  ~ "\u2503",
                                         y %% 5 != 0                         ~ "\u2501",
                                         T                                   ~ "")) %>%
                filter(y > 10, y < 110),
              aes(x = x, y = y, hjust = hjust, label = label), size = paint_size/1.9, vjust = 0.25, alpha = 0.5) +
    # Scales
    scale_x_continuous(limits = c(-10, 55), expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    scale_linetype_identity() +
    coord_fixed() +
    # Theme
    theme_void()
  
}

