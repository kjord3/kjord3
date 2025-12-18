library(tidyverse)
library(bbd)
library(car)
library(Lahman)

bat_war <- bref_war_daily_batter()
pit_war <- bref_war_daily_pitcher()

team_bat_war <- bat_war %>%
  group_by(year_ID, team_ID) %>%
  summarise(
    team_bat_WAR = sum(WAR, na.rm = TRUE),
    .groups = "drop"
  )

team_pit_war <- pit_war %>%
  group_by(year_ID, team_ID) %>%
  summarise(
    team_pit_WAR = sum(WAR, na.rm = TRUE),
    .groups = "drop"
  )

team_total_war <- team_bat_war %>%
  full_join(team_pit_war, by = c("year_ID", "team_ID")) %>%
  mutate(
    team_bat_WAR = replace_na(team_bat_WAR, 0),
    team_pit_WAR = replace_na(team_pit_WAR, 0),
    team_WAR = team_bat_WAR + team_pit_WAR
  )

team_total_war %>%
  filter(year_ID == 2025) %>%
  arrange(desc(team_WAR))

lahman = Lahman::Teams %>% select(teamIDBR, yearID, W, L, G)

team_war_wins <- team_total_war %>%
  left_join(
    lahman,
    by = c("year_ID" = "yearID", "team_ID" = "teamIDBR")
  ) %>% drop_na() %>% filter(year_ID >= 2000, G >= 140)

mod <- lm(W ~ team_WAR, data = team_war_wins)
summary(mod)


### WAR has a relationship that a team of replacements would win 49 (48.9) games with each
### 1-WAR increase adding approximately 1 win (0.96299)


###########################################################################################
### Are Batting and Pitching WAR weighted the same?

summary(lm(W ~ team_bat_WAR + team_pit_WAR, data = team_war_wins))

### Batting and pitching WAR are approximately equally valued in terms of wins, 
### with a slight edge to pitching.

###########################################################################################
### Are there non-linear effects at the extremes?

summary(lm(W ~ poly(team_WAR, 2), data = team_war_wins))

### The relationship between team WAR and wins is predominantly linear, with modest 
### diminishing returns at the extremes.

###########################################################################################
### Residual Analysis

team_war_wins %>%
  mutate(resid = residuals(mod)) %>%
  arrange(desc(abs(resid)))


###########################################################################################
###########################################################################################
###########################################################################################
### Step 1: Define Realized WAR

team_war_wins <- team_war_wins %>%
  mutate(
    expected_W = predict(mod),
    WAR_realization = W - expected_W
  )

### WAR_realization > 0: team outperformed its WAR
### WAR_realization < 0: team underperformed its WAR


### Step 2: Create Roster Churn

player_team_war <- bat_war %>%
  select(player_ID, year_ID, team_ID, WAR) %>%
  bind_rows(pit_war %>% select(player_ID, year_ID, team_ID, WAR)) %>%
  filter(WAR > 0) %>%
  arrange(player_ID, year_ID) %>%
  group_by(player_ID) %>%
  mutate(prev_team = lag(team_ID),
         prev_WAR = lag(WAR, default = 0)) %>%
  ungroup()

team_churn <- player_team_war %>%
  group_by(year_ID, team_ID) %>%
  summarise(
    WAR_lost_from_exits = sum(prev_WAR[prev_team != team_ID], na.rm = TRUE),
    WAR_gained_from_new  = sum(WAR[prev_team != team_ID], na.rm = TRUE),
    WAR_total            = sum(WAR, na.rm = TRUE),
    WAR_continuity       = sum(WAR[prev_team == team_ID], na.rm = TRUE) / WAR_total,
    n_players            = n_distinct(player_ID),
    top3_WAR_share       = sum(sort(WAR, decreasing = TRUE)[1:3]) / WAR_total,
    .groups = "drop"
  ) %>%
  mutate(
    pct_WAR_lost = WAR_lost_from_exits / WAR_total,
    pct_WAR_new  = WAR_gained_from_new / WAR_total
  )

team_churn <- team_churn %>%
  mutate(pct_WAR_lost = 1 - pct_WAR_lost)

### Step 3: Merge into existing df
team_war_extended <- team_war_wins %>%
  left_join(team_churn, by = c("year_ID", "team_ID")) %>%
  drop_na()


### Step 4: Test Hypothesis
mod_ext <- lm(
  W ~ team_WAR + pct_WAR_new + top3_WAR_share,
  data = team_war_extended
)

summary(mod_ext)


### Step 5: Check Residuals
team_war_extended %>%
  mutate(resid_ext = residuals(mod_ext)) %>%
  arrange(desc(abs(resid_ext))) %>%
  select(1:8, 10, 14) %>%
  head(10)


### Step 6: Check Validation
train <- team_war_extended %>% filter(year_ID <= 2015)
test  <- team_war_extended %>% filter(year_ID > 2015)

mod_train <- lm(
  W ~ team_WAR + pct_WAR_new + top3_WAR_share,
  data = train
)

pred_test <- predict(mod_train, test)

sqrt(mean((test$W - pred_test)^2))


###########################################################################################
###########################################################################################
### Build an enhanced model

mod_ext2 <- lm(
  W ~ team_WAR + pct_WAR_lost + WAR_continuity + top3_WAR_share,
  data = team_war_extended
)
summary(mod_ext2)


### Explore interactions

mod_ext3 <- lm(
  W ~ team_WAR + WAR_continuity + top3_WAR_share * pct_WAR_lost,
  data = team_war_extended
)
summary(mod_ext3)


### New Validation
train <- team_war_extended %>% filter(year_ID <= 2015)
test  <- team_war_extended %>% filter(year_ID > 2015)

mod_train <- lm(W ~ team_WAR + WAR_continuity + pct_WAR_lost + top3_WAR_share, data = train)
pred_test <- predict(mod_train, test)
sqrt(mean((test$W - pred_test)^2))  # RMSE





team_depth <- player_team_war %>%
  group_by(year_ID, team_ID) %>%
  summarise(
    WAR_top3 = sum(sort(WAR, decreasing = TRUE)[1:3], na.rm = TRUE),
    WAR_4to9 = sum(sort(WAR, decreasing = TRUE)[4:9], na.rm = TRUE),
    WAR_rest = sum(WAR, na.rm = TRUE) - WAR_top3 - WAR_4to9,
    n_players = n_distinct(player_ID),
    .groups = "drop"
  )

team_war_split <- team_war_wins %>%
  select(year_ID, team_ID, team_bat_WAR, team_pit_WAR)

teams_pf <- Lahman::Teams %>%
  select(teamIDBR, yearID, park_factor = BPF)  # placeholder if you have actual PF

team_war_extended <- team_war_extended %>%
  left_join(teams_pf, by = c("team_ID" = "teamIDBR", "year_ID" = "yearID"))

team_war_final <- team_war_extended %>%
  left_join(team_depth, by = c("year_ID", "team_ID"))

mod_final <- lm(
  W ~ team_bat_WAR + team_pit_WAR + pct_WAR_new +
    WAR_continuity + pct_WAR_lost + top3_WAR_share +
    park_factor + WAR_rest,
  data = team_war_final
)
summary(mod_final)
vif(mod_final)

train <- team_war_final %>% filter(year_ID <= 2015)
test  <- team_war_final %>% filter(year_ID > 2015)

mod_train <- lm(W ~ team_bat_WAR + team_pit_WAR +
                  WAR_continuity + pct_WAR_lost + top3_WAR_share +
                  park_factor + WAR_rest,
                data = train)

pred_test <- predict(mod_train, test)
rmse <- sqrt(mean((test$W - pred_test)^2))
rmse

orioles2026_war <- tibble(
  WAR = c(2.7, 5.6, 2.1, 3.2, 3.5, 2.3, 1.7, 0.9, 1.0, 1.1, 0.5, 0.2, 0.4, -0.1, 0.1, 0.1,
          0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0.2, 0.7, 0.4, 0, 0.3, 0, 0.4, 0.2,
          0.1, 0.1, 0.9, 0.5, 2.4, -0.1, 1.2, 3.2, 1.3, 0, -0.1, 0, 0, 0.2, 0.5, 0.1, 0),
  type = c("bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat", 
           "bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat",
           "bat", "bat", "bat", "bat", "bat", "bat", "bat", "bat", "pit", "pit", "pit", 
           "pit", "pit", "pit", "pit", "pit", "pit", "pit", "pit", "pit", 
           "pit", "pit", "pit", "pit", "pit", "pit", "pit", "pit", "pit", "pit", "pit",
           "pit", "pit", "pit"),
  returning = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, 
                TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

orioles2026_input <- orioles2026_war %>%
  arrange(desc(WAR)) %>%
  mutate(rank = row_number()) %>%
  summarise(
    team_bat_WAR = sum(WAR[type == "bat"], na.rm = TRUE),
    team_pit_WAR = sum(WAR[type == "pit"], na.rm = TRUE),
    WAR_4to9 = sum(WAR[rank %in% 4:9], na.rm = TRUE),
    WAR_rest = sum(WAR[rank > 9], na.rm = TRUE),
    total_WAR = sum(WAR, na.rm = TRUE),
    top3_WAR_share = sum(WAR[rank <= 3], na.rm = TRUE) / total_WAR,
    WAR_continuity = sum(WAR[returning == TRUE], na.rm = TRUE) / total_WAR,
    pct_WAR_lost = 1 - sum(WAR[returning == TRUE], na.rm = TRUE) / total_WAR,
    pct_WAR_new = sum(WAR[returning == FALSE], na.rm = TRUE) / total_WAR,
    park_factor = 0.98
  ) %>%
  mutate(
    `team_bat_WAR:team_pit_WAR` = team_bat_WAR * team_pit_WAR,
    `WAR_continuity:top3_WAR_share` = WAR_continuity * top3_WAR_share
  )
orioles2026_input

predicted_2026_wins <- predict(mod_final, orioles2026_input)
predicted_2026_wins
