
library(ggplot2)
# install.packages("gganimate")
# install.packages("ggsoccer")
# install.packages("StatsBombR")
library(gganimate)
library(ggsoccer)
library(dplyr)

wwc_passes <- wwc_passes %>%
  group_by(team_name) |>
  mutate(
    prev_period = lag(period),
    # start of a new game
    new_game = if_else(period == 1 & lag(period) %in% c(2, 3, 4), 1, 0),
    new_game = replace_na(new_game, 1),
    # game numbers
    game = cumsum(new_game) +1
  ) %>%
  select(game, everything(), -prev_period, -new_game) |>
  ungroup()
# Sample pass data (replace this with your real data)
# df <- your data frame with start_x, start_y, end_x, end_y, position
spain_animate <- spain_g1
# Create base pass plot
pass_plot <- spain_animate |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.7) +
  coord_fixed() +
  xlim(0, 120) + ylim(0, 80) +  # or match your field dimensions
  theme_minimal() +
  labs(title = "Passes by Position: {closest_state}", x = "X", y = "Y") +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Add animation by position
animated <- pass_plot +
  transition_states(position_name, transition_length = 2, state_length = 1, wrap = FALSE) +
  ease_aes('cubic-in-out')

# Save as GIF
animate(animated, width = 800, height = 600, renderer = gifski_renderer("passes_by_position.gif"))

spain_g1_gk <- filter(spain_passes, game == 1, position_name == "Goalkeeper")
cr_g1_rb <- filter(cr_passes, game == 1, position_name == "Right Back")

UNICE <- filter(wwc_passes, player_name == "Elena Linari")
UNICE |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y, colour = pass_outcome_name)) +
  coord_flip() +
  annotate_pitch(
    dimensions = pitch_statsbomb, #,
    colour = "white",             # Pitch lines
    fill = "#7fc47f") +             # Pitch colour+
  geom_segment(aes(color = pass_outcome_name, linetype = under_pressure), size = 0.5, alpha = 0.5) +
  scale_color_manual("Pass Outcome", values = c("blue", "darkred", "yellow", "hotpink")) +
  geom_point(aes(x = location_x, y = location_y), color = "green", size = 0.5) 

filter(spain_passes, game == 1, position_name == "Goalkeeper", pass_outcome_name == "Incomplete")
table(wwc_passes$pass_outcome_name)

table(wwc_passes$position_name)

germany <- filter(wwc_passes, team_name == "Germany")

germ_games <- filter(germany, game %in% c(1, 2, 3)) |>
  group_by(game) |>
  mutate(
    complete_so_far = cumsum(pass_outcome_name == "Complete"),
    total_so_far = row_number(),
    completion_pct = complete_so_far / total_so_far,
    pressure_rate = (sum(under_pressure == TRUE)/n())
  ) |>
  ungroup()
germ_games |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = as.factor(game))) +
  geom_line() + 
  geom_line(aes(x = total_so_far, y = pressure_rate, color = as.factor(game))) +
  xlim(15, 700)

morocco <- filter(wwc_passes, team_name == "Morocco")

morocco_group <- filter(morocco, game %in% c(1, 2, 3)) |>
  group_by(game) |>
  mutate(
    complete_so_far = cumsum(pass_outcome_name == "Complete"),
    total_so_far = row_number(),
    completion_pct = complete_so_far / total_so_far,
    pass_completion_up = round((cumsum(under_pressure == TRUE & pass_outcome_name == "Complete") 
                                / cumsum(under_pressure == TRUE) * 100), 2),
    opponent = factor(ifelse(game == 1, "Germany", ifelse(game == 2, "South Korea", "Colombia")),levels = c("Germany", "South Korea", "Colombia"))
  ) |>
  ungroup()

morocco_group |>
ggplot(aes(x = total_so_far, y = completion_pct, color = opponent)) +
  geom_line() + 
  xlim(15, 700)

cr_group <- filter(wwc_passes, team_name == "Costa Rica", game %in% c(1, 2, 3)) |>
  group_by(game) |>
  mutate(
    complete_so_far = cumsum(pass_outcome_name == "Complete"),
    total_so_far = row_number(),
    completion_pct = complete_so_far / total_so_far,
    pass_completion_up = round((cumsum(under_pressure == TRUE & pass_outcome_name == "Complete") 
                                / cumsum(under_pressure == TRUE) * 100), 2),
    opponent = factor(ifelse(game == 1, "vs. Each Other", ifelse(game == 2, "Japan", "Zambia")),levels = c("Zambia", "Japan", "vs. Each Other"))
  ) |>
  ungroup()
  
spain_group <- filter(wwc_passes, team_name == "Spain", game %in% c(1, 2, 3)) |>
  group_by(game) |>
  mutate(
    complete_so_far = cumsum(pass_outcome_name == "Complete"),
    total_so_far = row_number(),
    completion_pct = complete_so_far / total_so_far,
    pass_completion_up = round((cumsum(under_pressure == TRUE & pass_outcome_name == "Complete") 
                                / cumsum(under_pressure == TRUE) * 100), 2),
    opponent = factor(ifelse(game == 1, "vs. Each Other", ifelse(game == 2, "Zambia", "Japan")),levels = c("vs. Each Other", "Japan", "Zambia"))
  ) |>
  ungroup()
cr_spain <- 
  rbind(cr_group, spain_group)
cr_group |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = opponent)) +
  geom_line()

spain_group |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = opponent)) +
  geom_line()

cr_spain |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = opponent)) +
  geom_line(linewidth = 0.6) +
  xlim(20, 800) +
  ylim(0.4, 1.00) +
  facet_wrap(~team_name, scale = "free_y", nrow = 2) +
  theme(legend.position = "bottom")


  cr_spain |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = team_name)) +
  geom_line(linewidth = 0.6) +
  xlim(20, 800) +
  ylim(0.45, 0.95) +
  geom_text(data = ann_text, aes(x = total_so_far, y = completion_pct, label = completion_pct), color = "black", position = position_dodge() +
  facet_wrap(~opponent, scale = "free_y", nrow = 3) +
  theme(legend.position = "bottom") 
  

ann_text <- data.frame(total_so_far = c(770, 429, 916, 423, 655, 153), completion_pct = c(0.871, 0.748, 0.905, 0.825, 0.860, 0.556))  


svz <- cr_group |>
filter(opponent == "vs. Each Other") 
  
max(svz$total_so_far)
svz$completion_pct[153]

player_stats_clustered |>
  mutate(up_diff = pass_completion_rate - pass_completion_up) |>
  arrange(desc(up_diff))

elena_lanari <- wwc_passes |>
  filter(player_name == "Elena Linari")
wieke_kaptein <- wwc_passes |>
  filter(player_name == 	"Wieke Hendrikje Maria Kaptein")
rapinah <- wwc_passes |>
  filter(player_name == "Megan Anna Rapinoe")

elena_lanari |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y, colour = pass_outcome_name)) +
  coord_flip() +
  annotate_pitch(
    dimensions = pitch_statsbomb, #,
    colour = "white",             # Pitch lines
    fill = "#7fc47f") +             # Pitch colour+
  geom_segment(aes(color = pass_outcome_name, linetype = under_pressure), size = 0.5, alpha = 0.5) +
  scale_color_manual("Pass Outcome", values = c("blue", "darkred", "yellow", "hotpink")) +
  geom_point(aes(x = location_x, y = location_y), color = "green", size = 0.5) 

wieke_kaptein |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y, colour = pass_outcome_name)) +
  coord_flip() +
  annotate_pitch(
    dimensions = pitch_statsbomb, #,
    colour = "white",             # Pitch lines
    fill = "#7fc47f") +             # Pitch colour+
  geom_segment(aes(color = pass_outcome_name, linetype = under_pressure), size = 0.5, alpha = 0.5) +
  scale_color_manual("Pass Outcome", values = c("blue", "darkred", "yellow", "hotpink")) +
  geom_point(aes(x = location_x, y = location_y), color = "green", size = 0.5) 

rapinah |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y, colour = pass_outcome_name)) +
  coord_flip() +
  annotate_pitch(
    dimensions = pitch_statsbomb, #,
    colour = "white",             # Pitch lines
    fill = "#7fc47f") +             # Pitch colour+
  geom_segment(aes(color = pass_outcome_name, linetype = under_pressure), size = 0.5, alpha = 0.5) +
  scale_color_manual("Pass Outcome", values = c("blue", "darkred", "yellow", "hotpink")) +
  geom_point(aes(x = location_x, y = location_y), color = "green", size = 0.5) 
