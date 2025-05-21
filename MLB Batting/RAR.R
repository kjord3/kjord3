library(tidyverse)

# Load the data
bat = read_csv("C:/Users/kaleb/Downloads/projs/MLB Batting/bat_stats.csv")
fielding = read_csv("C:/Users/kaleb/Downloads/projs/MLB Batting/field_stats.csv")

# NA values
colSums(is.na(bat))

# Make all NA values 0 for fielding
fielding[is.na(fielding)] = 0

# Rename Players
bat = bat %>% rename(name = `last_name, first_name`)

# Combine Bat and Fielding Stats
df = bat %>%
  left_join(fielding)

outs_cols = df %>% select(outs_2:outs_9)

df$max_outs_col = colnames(outs_cols)[max.col(outs_cols, ties.method = "first")]

# Find Positional Adjustments
pos_adjustments = c(
  outs_2 = 12.5,
  outs_6 = 7.5,
  outs_4 = 2.5,
  outs_5 = 2.5,
  outs_8 = 2.5,
  outs_9 = -7.5,
  outs_7 = -7.5,
  outs_3 = -12.5
)

df$positional_adjustment = pos_adjustments[df$max_outs_col]

outs_cols = paste0("outs_", 2:9)

df = df %>%
  rowwise() %>%
  mutate(
    total_positional_adjustment = sum(
      c_across(all_of(outs_cols)) / 4350 * unname(pos_adjustments[outs_cols])
    )
  ) %>%
  ungroup()

# Standard WAR Replacement Factor
df = df %>%
  mutate(replacement_adj = (pa / 600) * (-20))

# Find the necessary stats
df = df %>%
  mutate(
    outs = ab - hit - strikeout,
    bat_runs = 0.9*single + 1.25*double + 1.6*triple + 1.95*home_run + 
      0.7*(walk + b_hit_by_pitch + b_catcher_interf + b_intent_walk) - 0.25*outs - 0.35*strikeout,
    run_runs = 0.2*r_total_stolen_base - 0.4*r_total_caught_stealing,
    field_runs = run_value + total_positional_adjustment
  ) %>%
  replace_na(list(field_runs = -15)) %>%
  mutate(RAR = bat_runs + run_runs + field_runs - replacement_adj)

# 2015-2024 exclude 2020
output = df %>%
  filter(year < 2025 & year != 2020 & year >) %>%
  select(name, player_id, year, bat_runs, run_runs, field_runs, replacement_adj, RAR) %>%
  mutate(WAR = RAR / 10)

# 2025
output25 = df %>%
  filter(year == 2025) %>%
  select(name, player_id, year, bat_runs, run_runs, field_runs, replacement_adj, RAR) %>%
  mutate(WAR = RAR / 10)

head(output, 10)

summary(output$RAR)

med_rar = median(output$RAR)
med_rar

output = output %>%
  mutate(RAR_centered = RAR - med_rar)

ggplot(output, aes(x = RAR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Runs Above Replacement (RAR)",
       x = "RAR", y = "Count") +
  theme_minimal()

output = output %>%
  mutate(RAR_centered = RAR - med_rar,
         across(4:10, ~ round(.x, 2))
  )

output = output %>%
  mutate(RAR_tier = case_when(
    RAR >= 150 ~ "Superstar",
    RAR >= 105 ~ "All-Star",
    RAR >= 50  ~ "Starter",
    RAR >= 10  ~ "Role Player",
    TRUE       ~ "Below Replacement"
  ))

output = output %>%
  mutate(RAR_tier = factor(RAR_tier, levels = c(
    "Below Replacement", "Role Player", "Starter", "All-Star", "Superstar"
  )))

ggplot(output, aes(x = RAR_tier)) +
  geom_bar(fill = "darkred") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Player Season Tiers by RAR",
       x = "Tier", y = "Number of Seasons") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

output %>%
  count(RAR_tier) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = RAR_tier, y = prop)) +
  geom_col(fill = "darkred") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Player Season Tier Proportions", y = "Share of Seasons", x = "Tier") +
  theme_minimal()

med_rar25 = median(output25$RAR)
med_rar25

output25 = output25 %>%
  mutate(RAR_centered = RAR - med_rar25,
         across(4:10, ~ round(.x, 2))
  )

ggplot(output25, aes(x = RAR)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Runs Above Replacement (RAR)",
       x = "RAR", y = "Count") +
  theme_minimal()

output25 = output25 %>%
  mutate(RAR_centered = RAR - med_rar,
         across(4:10, ~ round(.x, 2))
  )

output25 = output25 %>%
  mutate(RAR_tier = case_when(
    RAR >= 55 ~ "Superstar",
    RAR >= 40 ~ "All-Star",
    RAR >= 25  ~ "Starter",
    RAR >= 4.5   ~ "Role Player",
    TRUE       ~ "Below Replacement"
  ))

output25 = output25 %>%
  mutate(RAR_tier = factor(RAR_tier, levels = c(
    "Below Replacement", "Role Player", "Starter", "All-Star", "Superstar"
  )))

ggplot(output25, aes(x = RAR_tier)) +
  geom_bar(fill = "darkred") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Player Season Tiers by RAR",
       x = "Tier", y = "Number of Seasons") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

output25 %>%
  count(RAR_tier) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = RAR_tier, y = prop)) +
  geom_col(fill = "darkred") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Player Season Tier Proportions", y = "Share of Seasons", x = "Tier") +
  theme_minimal()

head_output = output %>% arrange(desc(RAR)) %>% head(10)

knitr::kable(head(output, 10), caption = "Top 10 Players by RAR")

