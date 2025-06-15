library(ggplot2)
# install.packages("gganimate")
# install.packages("ggsoccer")
# install.packages("StatsBombR")
library(gganimate)
library(ggsoccer)
library(dplyr)
library(ggpubr)
wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_passes.csv")


# Data cleaning -----------------------------------------------------------

# Cleaning the names 
wwc_passes <- wwc_passes |> 
  janitor::clean_names()

## Cleaning the data
# Replacing NAs with FALSE for pass columns
wwc_passes_2 <- wwc_passes |>
  select(under_pressure, counterpress, pass_switch:pass_miscommunication)

wwc_passes_2[is.na(wwc_passes_2)] <- FALSE
wwc_passes[c(5, 6, 11:23)] <- wwc_passes_2

# Replacing NAs in pass outcomes with "Complete"
wwc_passes_3 <- wwc_passes |> 
  select(pass_outcome_name) |>
  mutate(pass_outcome_name = ifelse(is.na(pass_outcome_name == TRUE), 
                                    "Complete", pass_outcome_name))

wwc_passes[28] <- wwc_passes_3

# Cleaning team names
wwc_passes <- wwc_passes |> 
  mutate(team_name = str_remove_all(team_name, " Women's"))

# Adding game number
wwc_passes <- wwc_passes |> 
  group_by(team_name) |>
  mutate(
    prev_period = lag(period),
    # start of a new game
    new_game = if_else(period == 1 & lag(period) %in% c(2, 3, 4), 1, 0),
    new_game = replace_na(new_game, 1),
    # game numbers
    game = cumsum(new_game) +1
  ) %>%
  select(game, everything(), -prev_period, -new_game)


# Passing spreads ---------------------------------------------------------

# Only want passes that are complete, incomplete, or out
wwc_passes <- wwc_passes |> 
  filter(pass_outcome_name %in% c("Complete", "Incomplete", "Out"))


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
  geom_segment(aes(color = pass_outcome_name, lty = under_pressure), size = 0.7, alpha = 0.7) +
  scale_color_manual("Pass Outcome", values = c("#4292f6", "#f5c951", "black")) +
  geom_point(aes(x = location_x, y = location_y), color = "grey2", size = 1, alpha = .9) +
  labs(title = "Elena Lanari passing spread",
       subtitle = "Italian right center back",
       lty = "Under Pressure?") +
  theme_void() +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5,
                                  size = 15,
                                  vjust = -4),
        legend.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic",
                                     hjust = .5,
                                     size = 15,
                                     vjust = -4))

wieke_kaptein |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y, colour = pass_outcome_name)) +
  coord_flip() +
  annotate_pitch(
    dimensions = pitch_statsbomb, #,
    colour = "white",             # Pitch lines
    fill = "#7fc47f") +             # Pitch colour+
  geom_segment(aes(color = pass_outcome_name, lty = under_pressure), size = 0.7, alpha = 0.7) +
  scale_color_manual("Pass Outcome", values = c("#4292f6", "#f5c951", "black")) +
  geom_point(aes(x = location_x, y = location_y), color = "grey2", size = 1, alpha = .9) +
  labs(title = "Wieke Hendrikje Maria Kaptein passing spread",
       subtitle = "Dutch left center midfield",
       lty = "Under Pressure?") +
  theme_void() +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5,
                                  size = 15,
                                  vjust = -4),
        legend.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic",
                                     hjust = .5,
                                     size = 15,
                                     vjust = -4))

rapinah |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y, colour = pass_outcome_name)) +
  coord_flip() +
  annotate_pitch(
    dimensions = pitch_statsbomb, #,
    colour = "white",             # Pitch lines
    fill = "#7fc47f") +             # Pitch colour+
  geom_segment(aes(color = pass_outcome_name, lty = under_pressure), size = 0.7, alpha = 0.7) +
  scale_color_manual("Pass Outcome", values = c("#4292f6", "#f5c951", "black")) +
  geom_point(aes(x = location_x, y = location_y), color = "grey2", size = 1, alpha = .9) +
  labs(title = "Megan Anna Rapinoe passing spread",
       subtitle = "American left winger",
       lty = "Under Pressure?") +
  theme_void() +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5,
                                  size = 15,
                                  vjust = -4),
        legend.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic",
                                     hjust = .5,
                                     size = 15,
                                     vjust = -4))

# Faceting
three_players <- rbind(elena_lanari, wieke_kaptein, rapinah) |> 
  mutate(player_name = ifelse(player_name == "Elena Linari", "Elena Linari\nItalian Right Center Back",
                              ifelse(player_name == "Wieke Hendrikje Maria Kaptein",
                                     "Wieke Hendrikje Maria Kaptein\nDutch Left Center Midfielder",
                                     "Megan Anna Rapinoe\nAmerican Left Winger")))

three_players |>
  ggplot(aes(x = location_x, y = location_y, xend = pass_end_location_x, yend = pass_end_location_y, colour = pass_outcome_name)) +
  coord_flip() +
  annotate_pitch(
    dimensions = pitch_statsbomb, #,
    colour = "white",             # Pitch lines
    fill = "#7fc47f") +             # Pitch colour+
  geom_segment(aes(color = pass_outcome_name, lty = under_pressure), size = 0.7, alpha = 0.7) +
  scale_color_manual("Pass Outcome", values = c("#4292f6", "#f5c951", "black")) +
  geom_point(aes(x = location_x, y = location_y), color = "grey2", size = 1, alpha = .9) +
  facet_wrap(~ player_name) +
  theme_bw() +
  labs(lty = "Under Pressure?") +
  theme(legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic",
                                  size = 15),
        strip.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# Spain vs. Costa Rica ----------------------------------------------------

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

cr_spain <- rbind(cr_group, spain_group)

cr_group |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = opponent)) +
  geom_line()

spain_group |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = opponent)) +
  geom_line()

cr_spain |>
  ggplot(aes(x = total_so_far, y = (completion_pct*100), color = opponent)) +
  geom_line(linewidth = 0.7) +
  xlim(20, 800) +
  ylim(50, 100) +
  facet_wrap(~team_name, scale = "free_y", nrow = 2) +
  scale_color_manual("Oponnent", values = c("#7fc47f","#4292f6", "#f5c951")) +
  labs(title = "Spain (2-0-1) is consistent in passing completion percentage \nthrough group stage compared to Costa Rica (0-3)",
       caption = "Data courtesy of StatsBomb",
       y = "Passing completion rate (%)",
       x = "Number of passes") +
  theme(plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = .5),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        legend.title = element_text(size = 15,
                                    face = "bold"),
        legend.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic",
                                  size = 15),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(size = 10))
  

        