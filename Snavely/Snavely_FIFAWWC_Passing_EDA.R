##################################################
# Title: WWC Passing EDA
# By: Luke and Zach
##################################################

# Loading in packages/data
library(tidyverse)
# install.packages("janitor")
# install.packages("ggpubr")
library(ggpubr)
library(janitor)
wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_passes.csv")
theme_set(theme_bw())

# Cleaning Data -----------------------------------------------------------


## Cleaning the data
# Cleaning the names
wwc_passes <- wwc_passes |> 
  clean_names()

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

# Removing "Women's" for the teams
table(wwc_passes$team_name)
wwc_passes <- wwc_passes |> 
  mutate(team_name = str_remove_all(team_name, " Women's"))

# Adding a "knockout_stage" column to differentiate between playoff and non-playoff teams
wwc_passes <- wwc_passes |>
  mutate(knockout_stage = ifelse(team_name %in% c("Switzerland", "Spain", 
                                                  "Netherlands", "South Africa",
                                                  "Japan", "Norway",
                                                  "Sweden", "United States",
                                                  "Australia", "Denmark",
                                                  "France", "Morocco",
                                                  "England", "Nigeria",
                                                  "Colombia", "Jamaica"),
                                 TRUE, FALSE))

# Adding a total time in possession column
wwc_passes <- wwc_passes |> 
  mutate(tot_possession = time_in_poss + time_to_poss_end)




# Initial EDA ---------------------------------------------------------------------
## EDA
# Looking at pass outcome due to pressure
wwc_passes |> 
  group_by(pass_outcome_name, under_pressure) |> 
  count(pass_outcome_name, name = "Passes") |> 
  filter(pass_outcome_name != "Unknown") |> 
  ggplot(aes(y = reorder(pass_outcome_name, Passes), x = Passes, fill = under_pressure)) +
  geom_col() +
  scale_fill_manual("Under Pressure?", values = c("forestgreen", "goldenrod")) +
  theme_bw() +
  scale_x_sqrt() +
  labs(title = "WWC pass outcomes and their pressure rates",
       y = "Pass Outcome")

# Looking at pressure rate and pass completion for teams
team_stats |> 
  ggplot(aes(x = pressure_rate, y = pass_completion_rate, color = knockout_stage)) +
  geom_point(size = 3, alpha = .6) +
  scale_color_manual("Knockout stage?", values = c("forestgreen", "goldenrod")) +
  geom_vline(aes(xintercept = mean(pressure_rate)), lty = 2, lwd = 1, color = "red") +
  geom_hline(aes(yintercept = mean(pass_completion_rate)), lty = 2, lwd = 1, color = "red")


# Answering Question 1 ----------------------------------------------------
# How does a team’s passing ability generally change as the game goes on? 
wwc_PassingChanges <- wwc_passes |> 
  filter(period %in% c(1, 2)) |> 
  mutate(time_range = factor(ifelse(minute < 5, "0 to 5",
                             ifelse(period == 1 & 5 <= minute & minute < 10, "5 to 10",
                             ifelse(period == 1 & 10 <= minute & minute < 15, "10 to 15",
                             ifelse(period == 1 & 15 <= minute & minute < 20, "15 to 20",
                             ifelse(period == 1 & 20 <= minute & minute < 25, "20 to 25",
                             ifelse(period == 1 & 25 <= minute & minute < 30, "25 to 30",
                             ifelse(period == 1 & 30 <= minute & minute < 35, "30 to 35",
                             ifelse(period == 1 & 35 <= minute & minute < 40, "35 to 40",
                             ifelse(period == 1 & 40 <= minute & minute < 45, "40 to 45",
                             ifelse(period == 2 & 45 <= minute & minute < 50, "45 to 50",
                             ifelse(period == 2 & 50 <= minute & minute < 55, "50 to 55",
                             ifelse(period == 2 & 55 <= minute & minute < 60, "55 to 60",
                             ifelse(period == 2 & 60 <= minute & minute < 65, "60 to 65",
                             ifelse(period == 2 & 65 <= minute & minute < 70, "65 to 70",
                             ifelse(period == 2 & 70 <= minute & minute < 75, "70 to 75",
                             ifelse(period == 2 & 75 <= minute & minute < 80, "75 to 80",
                             ifelse(period == 2 & 80 <= minute & minute < 85, "80 to 85",
                             ifelse(period == 2 & 85 <= minute & minute < 90, "85 to 90",
                             ifelse(period == 2 & 90 <= minute, "90+", NA))))))))))))))))))))) |>
  group_by(time_range, period) |> 
  summarize(pressure_rate = (sum(under_pressure == TRUE)/n() * 100),
            pass_completion_rate = (sum(pass_outcome_name == "Complete")/n() * 100),
            mean_pass_length = mean(pass_length),
            mean_pass_duration = mean(duration)) |>
  na.omit()

# Changing factor levels
wwc_PassingChanges$time_range <-factor(wwc_PassingChanges$time_range, 
                                      levels = c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25",
                                      "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50",
                                      "50 to 55", "55 to 60", "60 to 65", "65 to 70", "70 to 75",
                                      "75 to 80", "80 to 85", "85 to 90", "90+"))

# Rearranging columns
wwc_PassingChanges[1:19, ] <- wwc_PassingChanges[c(1, 10, 2:9, 11:19), ]

# Pivot longer to facet
wwc_PassingChanges_long <- wwc_PassingChanges |> 
  select(time_range, pressure_rate, pass_completion_rate) |> 
  pivot_longer(pressure_rate:pass_completion_rate,
               names_to = "Stat",
               values_to = "Val")

# Facet label names
facet_labels <- c("Pass completion", "Pressure on pass")
names(facet_labels) <- c("pass_completion_rate", "pressure_rate")

# 2 variable graphing
wwc_PassingChanges_long |> 
  ggplot(aes(x = time_range, y = Val, fill = Stat)) +
  geom_col(alpha = 1) +
  scale_fill_manual(values = c("#419153", "#d5d4c6")) +
  facet_wrap(~Stat, nrow = 2, scale = "free_y",
             labeller = labeller(Stat = facet_labels)) +
  labs(title = "Average rates throughout 2023 WWC matches",
       x = "Time of game (minutes)",
       y = "Rate (%)",
       caption = "Data courtesy of StatsBomb") +



## Solo Graphs
# Pass Length
pass_length_graph <- {
wwc_PassingChanges |> 
  ggplot(aes(x = time_range, y = mean_pass_length)) + 
  geom_col(fill = "#419153") +
  labs(x = "Time of game (minutes)",
       y = "Pass length (feet)") +
  theme(axis.text.x = element_text(angle = 45,
                             vjust = .5),
        axis.text = element_text(size = 10,
                                 color = "black"),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.title.x = element_blank())
}

pass_duration_graph <- {
wwc_PassingChanges |> 
  ggplot(aes(x = time_range, y = mean_pass_duration)) + 
  geom_col(fill = "#d5d4c6") +
  labs(x = "Time of game (minutes)",
       y = "Pass duration (seconds)") +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = .5),
        axis.text = element_text(size = 10,
                                 color = "black"),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.title.x = element_blank())
}

pass_completion_graph <- {  
wwc_PassingChanges |> 
  ggplot(aes(x = time_range, y = pass_completion_rate)) + 
  geom_col(fill = "#4292f6") +
  labs(x = "Time of game (minutes)",
       y = "Pass completion rate (%)") +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = .5),
        axis.text = element_text(size = 10,
                                 color = "black"),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.title.x = element_blank())
}

pressure_rate_graph <- {
wwc_PassingChanges |> 
  ggplot(aes(x = time_range, y = pressure_rate)) + 
  geom_col(fill = "#f5c951") +
  labs(y = "Pressure rate (%)") +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = .5),
        axis.text = element_text(size = 10,
                                 color = "black"),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.title.x = element_blank())
}

title <- "Mean passing statistics over 2023 WWC matches"
x_axis <- "Time of game (minutes)"
caption <- "Data courtesy of StatsBomb"

plot <- ggarrange(pass_length_graph, pass_duration_graph, pass_completion_graph, pressure_rate_graph,
          ncol = 2, nrow = 2)
annotate_figure(plot,
                top = text_grob(title, color = "black", face = "bold", size = 20),
                bottom = text_grob(x_axis, color = "black", face = "bold", size = 15),
                fig.lab = caption,
                fig.lab.pos = "bottom.right",
                fig.lab.face = "italic")

# Is there a difference between knockout and non-knockout teams in pass success throughout a game?
wwc_knockout_diff <- wwc_passes |> 
  filter(period %in% c(1,2)) |> 
    mutate(time_range = factor(ifelse(minute < 5, "0 to 5",
                             ifelse(period == 1 & 5 <= minute & minute < 10, "5 to 10",
                             ifelse(period == 1 & 10 <= minute & minute < 15, "10 to 15",
                             ifelse(period == 1 & 15 <= minute & minute < 20, "15 to 20",
                             ifelse(period == 1 & 20 <= minute & minute < 25, "20 to 25",
                             ifelse(period == 1 & 25 <= minute & minute < 30, "25 to 30",
                             ifelse(period == 1 & 30 <= minute & minute < 35, "30 to 35",
                             ifelse(period == 1 & 35 <= minute & minute < 40, "35 to 40",
                             ifelse(period == 1 & 40 <= minute & minute < 45, "40 to 45",
                             ifelse(period == 2 & 45 <= minute & minute < 50, "45 to 50",
                             ifelse(period == 2 & 50 <= minute & minute < 55, "50 to 55",
                             ifelse(period == 2 & 55 <= minute & minute < 60, "55 to 60",
                             ifelse(period == 2 & 60 <= minute & minute < 65, "60 to 65",
                             ifelse(period == 2 & 65 <= minute & minute < 70, "65 to 70",
                             ifelse(period == 2 & 70 <= minute & minute < 75, "70 to 75",
                             ifelse(period == 2 & 75 <= minute & minute < 80, "75 to 80",
                             ifelse(period == 2 & 80 <= minute & minute < 85, "80 to 85",
                             ifelse(period == 2 & 85 <= minute & minute < 90, "85 to 90",
                             ifelse(period == 2 & 90 <= minute, "90+", NA))))))))))))))))))))) |> 
  group_by(knockout_stage, time_range) |> 
  summarize(pressure_rate = (sum(under_pressure == TRUE)/n() * 100),
            pass_completion_rate = (sum(pass_outcome_name == "Complete")/n() * 100),
            mean_pass_length = mean(pass_length),
            mean_pass_duration = mean(duration)) |>
  na.omit()

# Changing factor levels
wwc_knockout_diff$time_range <-factor(wwc_knockout_diff$time_range, 
                                       levels = c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25",
                                                  "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50",
                                                  "50 to 55", "55 to 60", "60 to 65", "65 to 70", "70 to 75",
                                                  "75 to 80", "80 to 85", "85 to 90", "90+"))

# Rearranging columns
wwc_knockout_diff[1:19, ] <-wwc_knockout_diff[c(1, 10, 2:9, 11:19), ]
wwc_knockout_diff[20:38, ] <-wwc_knockout_diff[c(20, 29, 21:28, 30:38), ]
  
## Visualizations
# Passing rates
wwc_knockout_diff |> 
  ggplot(aes(x = time_range, y = pass_completion_rate, fill = knockout_stage)) +
  geom_col(position = "dodge") +
  scale_fill_manual("Knockout Stage?", values = c("#419153", "#f5c951")) +
  labs(title = "Knockout stage vs. non-knockout stage team mean pass completion rates",
       subtitle = "Analyzing 2023 Women's World Cup teams",
       x = "Time of game (minutes)",
       y = "Pass completion rate (%)",
       caption = "Data courtesy of StatsBomb") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5,
                                  size = 20),
        legend.position = "bottom",
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = .5),
        legend.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic",
                                     size = 15,
                                     hjust = .5))

# Pressure rates
wwc_knockout_diff |> 
  ggplot(aes(x = time_range, y = pressure_rate, fill = knockout_stage)) +
  geom_col(position = "dodge") +
  scale_fill_manual("Knockout Stage?", values = c("#419153", "#f5c951")) +
  labs(title = "Knockout stage vs. non-knockout stage team mean pressure rates",
       subtitle = "Analyzing 2023 Women's World Cup teams",
       x = "Time of game (minutes)",
       y = "Pressure rate (%)",
       caption = "Data courtesy of StatsBomb") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5,
                                  size = 20),
        legend.position = "bottom",
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = .5),
        legend.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic",
                                     size = 15,
                                     hjust = .5))

# Looking at statistics by team -------------------------------------------
wwc_spain_costa <- wwc_passes |> 
  filter(period %in% c(1,2),
         team_name %in% c("Spain", "Costa Rica")) |> 
    mutate(time_range = factor(ifelse(minute < 5, "0 to 5",
                             ifelse(period == 1 & 5 <= minute & minute < 10, "5 to 10",
                             ifelse(period == 1 & 10 <= minute & minute < 15, "10 to 15",
                             ifelse(period == 1 & 15 <= minute & minute < 20, "15 to 20",
                             ifelse(period == 1 & 20 <= minute & minute < 25, "20 to 25",
                             ifelse(period == 1 & 25 <= minute & minute < 30, "25 to 30",
                             ifelse(period == 1 & 30 <= minute & minute < 35, "30 to 35",
                             ifelse(period == 1 & 35 <= minute & minute < 40, "35 to 40",
                             ifelse(period == 1 & 40 <= minute & minute < 45, "40 to 45",
                             ifelse(period == 2 & 45 <= minute & minute < 50, "45 to 50",
                             ifelse(period == 2 & 50 <= minute & minute < 55, "50 to 55",
                             ifelse(period == 2 & 55 <= minute & minute < 60, "55 to 60",
                             ifelse(period == 2 & 60 <= minute & minute < 65, "60 to 65",
                             ifelse(period == 2 & 65 <= minute & minute < 70, "65 to 70",
                             ifelse(period == 2 & 70 <= minute & minute < 75, "70 to 75",
                             ifelse(period == 2 & 75 <= minute & minute < 80, "75 to 80",
                             ifelse(period == 2 & 80 <= minute & minute < 85, "80 to 85",
                             ifelse(period == 2 & 85 <= minute & minute < 90, "85 to 90",
                             ifelse(period == 2 & 90 <= minute, "90+", NA))))))))))))))))))))) |> 
  group_by(team_name, time_range) |> 
  summarize(pressure_rate = (sum(under_pressure == TRUE)/n() * 100),
            pass_completion_rate = (sum(pass_outcome_name == "Complete")/n() * 100),
            mean_pass_length = mean(pass_length),
            mean_pass_duration = mean(duration)) |>
  na.omit()

# Changing factor levels
wwc_spain_costa$time_range <-factor(wwc_spain_costa$time_range, 
                                      levels = c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25",
                                                 "25 to 30", "30 to 35", "35 to 40", "40 to 45", "45 to 50",
                                                 "50 to 55", "55 to 60", "60 to 65", "65 to 70", "70 to 75",
                                                 "75 to 80", "80 to 85", "85 to 90", "90+"))

# Rearranging columns
wwc_spain_costa[1:19, ] <- wwc_spain_costa[c(1, 10, 2:9, 11:19), ]
wwc_spain_costa[20:38, ] <- wwc_spain_costa[c(20, 29, 21:28, 30:38), ]

# Pivoting longer to facet by team and passing statistic
wwc_spain_costa_long <- wwc_spain_costa |> 
  select(team_name, time_range, pressure_rate, pass_completion_rate) |> 
  pivot_longer(pressure_rate:pass_completion_rate,
               names_to = "Stat",
               values_to = "Val") |>
  mutate(Stat = ifelse(Stat == "pressure_rate", "Pressure Rate", "Pass Completion Rate"))

# Looking at differences between Spain and Costa Rica
wwc_spain_costa_long |> 
  ggplot(aes(x = time_range, y = Val, fill = team_name)) +
  geom_col(alpha = 1, position = "dodge") +
  scale_fill_manual("Team name", values = c("#419153", "#f5c951")) +
  facet_wrap(~Stat, scale = "free_y") +
  labs(title = "Spain vs. Costa Rica mean passing statistics throughout 2023 WWC games",
       x = "Time of game (minutes)",
       y = "Rate (%)",
       caption = "Data courtesy of StatsBomb") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5, 
                                  face = "bold",
                                  size = 20),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45,
                                   vjust = .5),
        axis.text = element_text(size = 10,
                                 color = "black"),
        plot.caption = element_text(size = 10,
                                    face = "italic"),
        strip.background = element_blank(),
        strip.text = element_text(size = 15,
                                  face = "italic",
                                  color = "grey3"),
        legend.title = element_text(face = "bold"))
  
  
  

?facet_grid
# Clustering --------------------------------------------------------------
## Team statistics df
team_stats <- wwc_passes |> 
  group_by(team_name) |> 
  summarize(pressure_rate = (sum(under_pressure == TRUE)/n() * 100),
            pass_completion_rate = (sum(pass_outcome_name == "Complete")/n() * 100),
            switch_rate = (sum(pass_switch == TRUE)/n() * 100),
            pass_miscommunication_rate = (sum(pass_miscommunication == TRUE)/n() * 100),
            mean_duration = mean(duration),
            mean_pass_length = mean(pass_length)) |> 
  mutate(knockout_stage = ifelse(team_name %in% c("Switzerland", "Spain", 
                                                  "Netherlands", "South Africa",
                                                  "Japan", "Norway",
                                                  "Sweden", "United States",
                                                  "Australia", "Denmark",
                                                  "France", "Morocco",
                                                  "England", "Nigeria",
                                                  "Colombia", "Jamaica"),
                                 TRUE, FALSE))

# Are pressure_rate and completion_rate correlated?
team_stats |> 
  ggplot(aes(x = pressure_rate, y = pass_completion_rate)) +
  geom_point()

cor(team_stats$pressure_rate, team_stats$pass_completion_rate)
