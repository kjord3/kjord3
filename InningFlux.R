library(tidyverse)
library(baseballr)
library(lubridate)
library(ggthemes)

###################################################################################################################
# Dates and Player Types
june26 = "2025-06-26"
july01 = "2025-07-01"
player_type = "batter"

# Query
base_url = "https://baseballsavant.mlb.com/statcast_search/csv?"
query = paste0(
  "all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZtwos=&hfGT=R%7CPO%7CS%7C",
  "&hfC=&hfSea=&hfSit=&hfOuts=&opptwont=&pitcher_throws=&batter_stands=&hfSA=",
  "&player_type=", player_type,
  "&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=",
  "&game_date_gt=", june26,
  "&game_date_lt=", july01,
  "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0",
  "&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc",
  "&min_abs=0&type=details"
)

july01_query = paste0(base_url, query)
july01_temp = tempfile(fileext = ".csv")
download.file(july01_query, july01_temp, mode = "wb")
july01_cast = read_csv(july01_temp)

statcast = rbind(july01_cast)
###################################################################################################################
# Create the scorecard via statcast
statcast_flux = statcast %>%
  mutate(
    half_inning = paste(game_pk, inning, if_else(inning_topbot == "Top", "T", "B"), sep = "_"),
    events = tolower(events),
    description = tolower(description),
    flux_score = case_when(
      events == "home_run" ~ 10,
      events == "triple" ~ 8,
      events == "double" ~ 6,
      events == "single" ~ 4,
      events %in% c("walk", "hit_by_pitch") & outs_when_up == 2 ~ 5,
      events %in% c("walk", "hit_by_pitch") ~ 3,
      description == "stolen_base_2b" ~ 5,
      description == "stolen_base_3b" ~ 7,
      events == "strikeout" & outs_when_up == 2 ~ -6,
      events == "strikeout" ~ -4,
      events %in% c("grounded_into_double_play", "double_play") ~ -10,
      events %in% c("field_out", "force_out") & is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ -2,
      events %in% c("field_out", "force_out") ~ -4,
      TRUE ~ 0
    ),
    batting_team = if_else(inning_topbot == "Top", away_team, home_team)
  )
# Create inning_flux
inning_flux = statcast_flux %>%
  rename(date = game_date, inn_half = inning_topbot) %>%
  group_by(game_pk, date, inning, inn_half, batting_team) %>%
  summarise(
    total_flux = sum(flux_score, na.rm = TRUE),
    home = first(home_team),
    away = first(away_team),
    home_score = first(home_score),
    away_score = first(away_score),
    .groups = "drop"
  ) %>%
  arrange(
    home,
    away,
    game_pk,
    inning,
    factor(inn_half, levels = c("Top", "Bot"))
  ) %>%
  group_by(game_pk, batting_team) %>%
  mutate(cumulative_flux = cumsum(total_flux)) %>%
  ungroup() %>%
  mutate(
    flux_signed = case_when(
      batting_team == home ~ total_flux,
      batting_team == away ~ -total_flux,
      TRUE ~ 0
    )
  ) %>%
  group_by(game_pk, home) %>%
  mutate(net_flux = cumsum(flux_signed),
         score = paste(home_score,"-",away_score)) %>%
  ungroup() %>%
  select(1:10, 13:14)

# Random Game
inning_flux %>% filter(game_pk == 777288)

# Visualization
inning_flux %>%
  filter(game_pk == 777288) %>%
  mutate(half_inning = paste("Inning", inning, inn_half)) %>%
  ggplot(aes(x = factor(half_inning, levels = unique(half_inning)), y = net_flux)) +
  geom_line(group = 1, color = "gold", size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Net  Flux — STL vs. PIT (Jul 1, 2025)",
    subtitle = "PIT beats STL 1–0",
    x = "Half-Inning",
    y = paste("Net Flux (", 
              inning_flux %>% filter(game_pk == 777288) %>% pull(home) %>% unique(), 
              "Perspective)")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
