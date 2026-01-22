library(tidyverse)
library(lubridate)

statcast <- readRDS("inning_flux.rds")

# Create Flux
statcast_flux <- statcast %>%
  mutate(
    events = tolower(events),
    description = tolower(description),
    batting_team = if_else(inning_topbot == "Top", away_team, home_team),
    flux_score = case_when(
      events == "home_run" ~ 10,
      events == "triple" ~ 8,
      events == "double" ~ 6,
      events == "single" ~ 4,
      events == "sac_fly" ~ 3,
      events %in% c("walk", "intent_walk", "hit_by_pitch") & outs_when_up == 2 ~ 5,
      events %in% c("walk", "intent_walk", "hit_by_pitch") ~ 3,
      events == "field_error" ~ 4,
      description == "stolen_base_2b" ~ 5,
      description == "stolen_base_3b" ~ 7,
      events == "strikeout_double_play" ~ -12,
      events == "strikeout" & outs_when_up == 2 ~ -6,
      events == "strikeout" ~ -4,
      events %in% c("grounded_into_double_play", "double_play") ~ -10,
      events == "triple_play" ~ -15,
      events %in% c("field_out", "force_out") &
        is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ -2,
      events %in% c("field_out", "force_out") ~ -4,
      TRUE ~ 0
    )
  )

# Inning-level Flux
inning_flux <- statcast_flux %>%
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
  arrange(game_pk, inning, factor(inn_half, levels = c("Top", "Bot"))) %>%
  mutate(
    flux_signed = if_else(batting_team == home, total_flux, -total_flux)
  ) %>%
  group_by(game_pk) %>%
  mutate(
    net_flux = cumsum(flux_signed),
    score = paste(home_score, "-", away_score)
  ) %>%
  ungroup()

# Random Game
inning_flux  %>%
  filter(game_pk == 813032) %>% select(2:12) %>% print(n=100) 

# Visualization
inning_flux %>%
  filter(game_pk == 813032) %>%
  mutate(half_inning = paste("Inning", inning, inn_half)) %>%
  ggplot(aes(x = factor(half_inning, levels = unique(half_inning)), y = net_flux)) +
  geom_line(group = 1, color = "gold", size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Net  Flux — TOR vs. LAD (Nov 1, 2025)",
    subtitle = "LAD beats TOR 5–4",
    x = "Half-Inning",
    y = paste("Net Flux (", 
              inning_flux %>% filter(game_pk == 813032) %>% pull(home) %>% unique(), 
              "Perspective)")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

inning_flux <- inning_flux %>%
  group_by(game_pk) %>%
  mutate(
    # Flag extra innings
    extra_innings = max(inning) > 9,
    
    # Identify final half-inning
    is_final_half = row_number() == n(),
    
    # If home team bats last in extras, increment home score by 1
    home_extra_run = if_else(
      extra_innings & inn_half == "Bot" & home_score <= away_score & is_final_half,
      1, 0
    ),
    
    # Adjust home_score
    home_score = home_score + home_extra_run,
    
    # Recalculate net_flux if needed
    flux_signed = case_when(
      batting_team == home ~ total_flux,
      batting_team == away ~ -total_flux,
      TRUE ~ 0
    ),
    net_flux = cumsum(flux_signed)
  ) %>%
  ungroup()
