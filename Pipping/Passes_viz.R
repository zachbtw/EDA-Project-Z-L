
library(ggplot2)
# install.packages("gganimate")
# install.packages("ggsoccer")
# install.packages("StatsBombR")
library(gganimate)
library(ggsoccer)
library(dplyr)

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
