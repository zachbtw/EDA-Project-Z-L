library(tidyverse)
# install.packages("janitor")
# install.packages("ggpubr")
# install.packages("RColorBrewer")
# install.packages("gt")
# install.packages("gtExtras")
library(factoextra)
library(ggpubr)
library(janitor)
library(plotly)
library(RColorBrewer)
library(gt)
library(gtExtras)
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

# Clustering --------------------------------------------------------------
## Team statistics df

player_stats <- wwc_passes |> 
  filter(position_name != "Goalkeeper") |> 
  group_by(player_name, team_name) |> 
  summarize(pressure_rate = round((sum(under_pressure == TRUE)/n() * 100), 2),
            pass_completion_rate = round((sum(pass_outcome_name == "Complete")/n() * 100), 2),
            pass_completion_up = round((sum(under_pressure == TRUE & pass_outcome_name == "Complete") 
                                        / sum(under_pressure == TRUE) * 100), 2),
            passes_attempted = as.numeric(sum(pass_outcome_name %in% c("Complete", "Incomplete", "Out"))),
            mean_duration = mean(duration),
            mean_pass_length = round(mean(pass_length), 2)) |> 
  mutate(knockout_stage = ifelse(team_name %in% c("Switzerland", "Spain", 
                                                  "Netherlands", "South Africa",
                                                  "Japan", "Norway",
                                                  "Sweden", "United States",
                                                  "Australia", "Denmark",
                                                  "France", "Morocco",
                                                  "England", "Nigeria",
                                                  "Colombia", "Jamaica"),
                                 TRUE, FALSE)) |> 
  ungroup() 

# 5 number summary of passes completed per player
summary(player_stats$passes_attempted) # Minimum of 24 passes attempted to filter out low sample size players

# 5 number summary of pressure rate for playets
summary(player_stats$pressure_rate) # Minimum pressure rate of 12%

# Also filtering out all goalkeepers because passing patterns are different than field players

# Cleaning the data set
player_stats_clean <- player_stats |> 
  filter(passes_attempted > 24,
         pressure_rate >= 12)

str(player_stats)

# Scaling the data
player_stats_std <- player_stats_clean |> 
  select(pass_completion_rate, pass_completion_up, mean_pass_length) |>
  scale()

# Elbow plot to select number of cluster
player_stats_std |> 
  fviz_nbclust(FUNcluster = kmeans, method = "wss") # Elbow plot --> 5 clusters is ideal

# k-means clustering
player_stats_kmeans <- player_stats_std |> 
  kmeans(centers = 5, nstart = 100, algorithm = "Lloyd")

# Adding the clusters back to the df
player_stats_clustered <- player_stats_clean |> 
  mutate(cluster = as.factor(player_stats_kmeans$cluster))

# Plotting through plotly
plot_ly(player_stats_clustered,
        x = ~pass_completion_rate,
        y = ~pass_completion_up,
        z = ~mean_pass_length,
        color = ~as.factor(cluster),
        type = "scatter3d",
        mode = "markers",
        colors = c("#419153", "#f5c951", "#4292f6", "#d5d4c6", "grey3"),
        text = ~player_name,
        hovertemplate = paste("<i>%{text}</i><br>",
                              "<b>Pass completion</b>: %{x}%",
                              "<br><b>Pass completion UP</b>: %{y}%<br>",
                              "<b>Mean pass length</b>: %{z}"),
        opacity = .8,
        marker = list(size = 7)) |> 
  layout(title = list(text = "<b>2023 WWC passing statistics by player<b>", y = .95,
                      font = list(size = 30))) |> 
  layout(scene = list(xaxis = list(title = "<b>Pass completion rate (%)</b>"),
                      yaxis = list(title = "<b>Pass competion rate under pressure (%)</b>"),
                      zaxis = list(title = "<b>Mean pass length</b>"))) |>
  layout(scene = list(xaxis = list(titlefont = list(size = 17), tickfont = list(size = 15)),
                      yaxis = list(titlefont = list(size = 17), tickfont = list(size = 15)),
                      zaxis = list(titlefont = list(size = 17), tickfont = list(size = 15)))) |>
  layout(scene = list(xaxis = list(gridcolor = "darkgrey", gridwidth = 2),
                      yaxis = list(gridcolor = "darkgrey", gridwidth = 2),
                      zaxis = list(gridcolor = "darkgrey", gridwidth = 2))) |>
  layout(scene = list(aspectmode = "auto")) |> 
  layout(legend = list(title = list(text = "Cluster"))) |> 
  layout(legend = list(font = list(size = 20, face = "bold")))

# Are the variables correlated?
cor(player_stats_clustered$pass_completion_rate, player_stats_clustered$pass_completion_up)
cor(player_stats_clustered$pass_completion_rate, player_stats_clustered$mean_pass_length)
cor(player_stats_clustered$mean_pass_length, player_stats_clustered$pass_completion_up)

# Creating a clustered statistics df
cluster_stats <- player_stats_clustered |> 
  group_by(cluster) |> 
  summarize(pass_completion_rate = median(pass_completion_rate),
         pass_completion_up = median(pass_completion_up),
         pass_length = median(mean_pass_length))

# Creating a tibble
Cluster_tibble <- cluster_stats |>
  gt() |>
  tab_header(title = md("**Median passing measures per cluster**")) |>
  cols_label(cluster = "Cluster",
             pass_completion_rate = "Pass completion rate", 
             pass_completion_up = "Pass completion rate under pressure",
             pass_length = "Pass length") |>
  data_color(columns = c(pass_completion_rate, pass_completion_up, pass_length),
             fn = scales::col_numeric(palette = c("white","#419153", "darkgreen"), domain = NULL)) |>
  gtExtras::gt_theme_espn()

Cluster_tibble