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

train <- team_war_wins %>% filter(year_ID <= 2015)
test  <- team_war_wins %>% filter(year_ID > 2015)

mod_train <- lm(
  W ~ team_WAR,
  data = train
)
summary(mod_train)

pred_test <- predict(mod_train, test)
pred_train = predict(mod_train, train)

sqrt(mean((train$W - pred_train)^2))
sqrt(mean((test$W - pred_test)^2))

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
  mutate(
    WAR_continuity = 1 - pct_WAR_lost
  )

### Step 3: Merge into existing df
team_war_extended <- team_war_wins %>%
  left_join(team_churn, by = c("year_ID", "team_ID")) %>%
  drop_na()


team_war_extended <- team_war_extended %>%
  arrange(team_ID, year_ID) %>%
  group_by(team_ID) %>%
  mutate(lag_W = lag(W)) %>%
  ungroup() %>%
  group_by(year_ID) %>%
  mutate(
    team_type = case_when(
      lag_W >= 90 ~ "Contender",
      lag_W >= 80 & lag_W < 90  ~ "Wildcard",
      lag_W >= 70 & lag_W < 80 ~ "Retooling",
      TRUE ~ "Rebuilding"
    )
  ) %>%
  ungroup() %>%
  mutate(team_type = factor(team_type, levels = c("Contender", "Wildcard", "Retooling", "Rebuilding")))

mod_churn <- lm(
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost,
  data = team_war_extended
)
summary(mod_churn)

train <- team_war_extended %>% filter(year_ID <= 2015)
test  <- team_war_extended %>% filter(year_ID > 2015)

mod_train <- lm(
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost,
  data = train
)
summary(mod_train)

pred_test <- predict(mod_train, test)
pred_train = predict(mod_train, train)

sqrt(mean((train$W - pred_train)^2))
sqrt(mean((test$W - pred_test)^2))

### Step 4: Test Hypothesis
mod_ext <- lm(
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost + team_type:team_WAR + team_WAR:pct_WAR_new,
  data = team_war_extended
)

summary(mod_ext)
vif(mod_ext)

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
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost + team_type:team_WAR + team_WAR:pct_WAR_new,
  data = train
)
summary(mod_train)

pred_test <- predict(mod_train, test)
pred_train = predict(mod_train, train)

sqrt(mean((train$W - pred_train)^2))
sqrt(mean((test$W - pred_test)^2))

# Scatter plot: Churn vs WAR Realization
team_war_extended %>%
  ggplot(aes(x = pct_WAR_new, y = WAR_realization)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Roster Churn vs WAR Realization",
    x = "Percent of WAR from New Players",
    y = "Wins Above / Below WAR Expectation"
  ) +
  theme_minimal()

# Wins lost per 10% increase in new WAR
wins_lost_per_10pct_churn <- coef(mod_ext)["pct_WAR_new"] * 0.1
wins_lost_per_10pct_churn

# Scatter plot with top 25% WAR teams highlighted
team_war_extended %>%
  mutate(team_strength = ifelse(team_WAR >= quantile(team_WAR, 0.75), "Contender", "Other")) %>%
  ggplot(aes(x = pct_WAR_new, y = WAR_realization)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  facet_wrap(~ team_strength) +
  labs(
    title = "Roster Churn vs WAR Realization by Team Strength",
    x = "Percent of WAR from New Players",
    y = "Wins Above / Below WAR Expectation"
  ) +
  theme_minimal()

# Median team_WAR and pct_WAR_lost for each team type
median_churn <- team_war_extended %>%
  group_by(team_type) %>%
  summarise(
    median_WAR = median(team_WAR, na.rm = TRUE),
    median_pct_WAR_lost = median(pct_WAR_lost, na.rm = TRUE),
    .groups = "drop"
  )

# Churn scenario predictions
churn_scenarios <- expand.grid(
  pct_WAR_new = seq(0, 0.5, by = 0.05),
  team_type = unique(team_war_extended$team_type)
) %>%
  left_join(median_churn, by = "team_type") %>%
  mutate(
    team_WAR = median_WAR,
    pct_WAR_lost = median_pct_WAR_lost
  )

churn_scenarios$predicted_W <- predict(mod_ext, newdata = churn_scenarios)

# Plot predicted wins under different churn scenarios
ggplot(churn_scenarios, aes(x = pct_WAR_new, y = predicted_W, color = team_type)) +
  geom_line(size = 1.2) +
  labs(
    title = "Sensitivity of Wins to Roster Churn",
    x = "Percent of WAR from New Players",
    y = "Predicted Wins",
    color = "Team Type"
  ) +
  theme_minimal()

# Calculate wins lost per 10% increase in churn for each team type
churn_scenarios %>%
  group_by(team_type) %>%
  summarise(
    wins_lost_per_10pct_churn = (predicted_W[pct_WAR_new == 0.1] - predicted_W[pct_WAR_new == 0]) / 0.1,
    .groups = "drop"
  )

# Focus on title race teams
title_race <- team_war_extended %>%
  filter(team_ID %in% c("BAL", "TBR") & year_ID >= 2022 & year_ID < 2024)

title_race %>% select(team_ID, year_ID, W, L, team_WAR, pct_WAR_new)

# Predicted wins for title race teams
predict(
  mod_ext,
  newdata = title_race,
  interval = "prediction",
  level = 0.95
)

# Plot raw W vs pct_WAR_new
team_war_extended %>%
  ggplot(aes(x = pct_WAR_new, y = W)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(
    title = "Roster Churn vs Actual Wins",
    x = "Percent of WAR from New Players",
    y = "Wins"
  ) +
  theme_minimal()

set.seed(88)
sigma_hat <- sqrt(mean(residuals(mod_ext)^2))
sigma_hat

simulate_season <- function(team_row, model, sigma, n_sims = 10000) {
  mu <- predict(model, newdata = team_row)
  
  tibble(
    sim = 1:n_sims,
    wins = rnorm(n_sims, mean = mu, sd = sigma)
  )
}

mc_results <- title_race %>%
  group_by(team_ID, year_ID) %>%
  group_modify(~ simulate_season(.x, mod_ext, sigma_hat)) %>%
  ungroup()

division_odds <- mc_results %>%
  group_by(year_ID, sim) %>%
  slice_max(wins, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  count(year_ID, team_ID) %>%
  group_by(year_ID) %>%
  mutate(prob_division_win = n / sum(n))
division_odds


win_ci <- mc_results %>%
  group_by(team_ID, year_ID) %>%
  summarise(
    mean_wins = mean(wins),
    lo_80 = quantile(wins, 0.10),
    hi_80 = quantile(wins, 0.90),
    lo_95 = quantile(wins, 0.025),
    hi_95 = quantile(wins, 0.975),
    .groups = "drop"
  )

win_ci

ggplot(mc_results, aes(x = wins, fill = team_ID)) +
  geom_density(alpha = 0.5) +
  geom_vline(
    data = win_ci,
    aes(xintercept = mean_wins, color = team_ID),
    linetype = "dashed",
    linewidth = 1
  ) +
  facet_wrap(~ year_ID) +
  labs(
    title = "Monte Carlo Win Distributions with Confidence Ranges",
    x = "Wins",
    y = "Density"
  ) +
  theme_minimal()




normalize_team_id <- function(team_id) {
  case_when(
    team_id %in% c("OAK", "ATH") ~ "OAK",
    TRUE ~ team_id
  )
}

team_total_war <- team_total_war %>%
  mutate(team_ID = normalize_team_id(team_ID))

team_churn <- team_churn %>%
  mutate(team_ID = normalize_team_id(team_ID))

team_war_extended <- team_war_extended %>%
  mutate(team_ID = normalize_team_id(team_ID))

mlb_2024 <- team_war_extended %>%
  filter(year_ID == 2024) %>%
  select(
    team_ID, year_ID, W, team_WAR,
    pct_WAR_new, pct_WAR_lost
  )

team_type_2025 <- mlb_2024 %>%
  mutate(
    team_type = case_when(
      W >= 90 ~ "Contender",
      W >= 80 & W < 90 ~ "Wildcard",
      W >= 70 & W < 80 ~ "Retooling",
      TRUE ~ "Rebuilding"
    )
  ) %>%
  select(team_ID, team_type)

mlb_2025_team_WAR <- team_total_war %>% filter(year_ID == 2025) %>% pull(team_WAR)

mlb_2025_churn <- team_churn %>%
  filter(year_ID == 2025) %>%
  select(team_ID, pct_WAR_new, pct_WAR_lost)

mlb_2025 <- mlb_2025_churn %>%
  left_join(team_total_war %>% filter(year_ID == 2025),
            by = "team_ID") %>%
  left_join(team_type_2025, by = "team_ID") %>%
  mutate(year_ID = 2025)

pred_80 <- predict(
  mod_train,
  newdata = mlb_2025,
  interval = "prediction",
  level = 0.80
)

pred_95 <- predict(
  mod_train,
  newdata = mlb_2025,
  interval = "prediction",
  level = 0.95
)

mlb_2025_pred <- mlb_2025 %>%
  mutate(
    fit = pred_80[, "fit"],
    
    lo_80 = pred_80[, "lwr"],
    hi_80 = pred_80[, "upr"],
    
    lo_95 = pred_95[, "lwr"],
    hi_95 = pred_95[, "upr"]
  )



mlb_2025_pred = mlb_2025_pred %>% mutate(W = c(80,76,76,75,89,92,60,83,88,43,87,87,82,72,93,79,97,70,
                                                      83,94,96,71,90,81,90,78,77,81,94,66))

sqrt(mean((mlb_2025_pred$W - mlb_2025_pred$fit)^2))


sigma_hat <- sqrt(mean(residuals(mod_ext)^2))

simulate_season <- function(team_row, model, sigma, n_sims = 10000) {
  mu <- predict(model, newdata = team_row)
  tibble(wins = rnorm(n_sims, mean = mu, sd = sigma))
}

mlb_mc <- mlb_2025 %>%
  mutate(
    mu = predict(mod_train, newdata = mlb_2025)
  ) %>%
  rowwise() %>%
  mutate(
    wins = list(rnorm(10000, mu, sigma_hat))
  ) %>%
  ungroup() %>%
  unnest(wins)

mlb_mc_summary <- mlb_mc %>%
  left_join(
    mlb_2025_pred %>% select(team_ID, W),
    by = "team_ID"
  ) %>%
  group_by(team_ID) %>%
  summarise(
    actual_wins = first(W),
    mean_wins = mean(wins),
    lo_80 = quantile(wins, 0.10),
    hi_80 = quantile(wins, 0.90),
    lo_95 = quantile(wins, 0.025),
    hi_95 = quantile(wins, 0.975),
    .groups = "drop"
  )
mlb_mc_summary

mlb_mc_summary <- mlb_mc_summary %>%
  mutate(
    in_80_CI = actual_wins >= lo_80 & actual_wins <= hi_80,
    in_95_CI = actual_wins >= lo_95 & actual_wins <= hi_95
  )

# Count the number of teams in each CI
num_in_80_CI <- sum(mlb_mc_summary$in_80_CI)
num_in_95_CI <- sum(mlb_mc_summary$in_95_CI)

num_in_80_CI
num_in_95_CI


train <- train %>%
  mutate(
    fitted = fitted(mod_train),
    resid  = resid(mod_train)
  )

ggplot(train, aes(fitted, resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Wins",
    y = "Residuals"
  ) +
  theme_minimal()

qqnorm(train$resid)
qqline(train$resid, col = "red")

ggplot(train, aes(fitted, abs(resid))) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Absolute Residuals vs Fitted",
    x = "Fitted Wins",
    y = "|Residuals|"
  ) +
  theme_minimal()

library(lmtest)
bptest(mod_train)

dwtest(mod_train)

library(sandwich)

coeftest(mod_train, vcov = vcovCL(mod_train, cluster = train$team_ID))






team_war_extended %>%
  ggplot(aes(x = team_WAR, y = W)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Team WAR vs Actual Wins",
    x = "Team WAR (Batting + Pitching)",
    y = "Wins"
  ) +
  theme_minimal()

ggplot(train, aes(fitted, resid)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, color = "red") +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Wins",
    y = "Residuals"
  ) +
  theme_minimal()

team_war_extended %>%
  ggplot(aes(x = pct_WAR_new, y = WAR_realization)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Roster Churn vs WAR Realization",
    x = "Percent of WAR from New Players",
    y = "WAR Realization (Wins - Exp. Wins)"
  ) +
  theme_minimal()

eval_2224 <- team_war_extended %>%
  filter(year_ID %in% 2022:2024) %>%
  mutate(expected_W = round(expected_W, 0)) %>%
  mutate(
    resid = W - expected_W,
    abs_resid = abs(resid)
  )

total_abs_resid_2224 <- eval_2224 %>%
  group_by(year_ID) %>%
  summarise(
    total_abs_resid = sum(abs_resid),
    n_obs = n()
  )

total_abs_resid_2224

mae_2224 <- eval_2224 %>%
  group_by(year_ID) %>%
  summarise(
    MAE = mean(abs_resid)
  )

mae_2224

library(glmnet)

X <- model.matrix(
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost +
    team_type:team_WAR + team_WAR:pct_WAR_new,
  data = team_war_extended
)[, -1]

y <- team_war_extended$W

set.seed(88)
cv_enet <- cv.glmnet(
  X, y,
  alpha = 0.5,   # Elastic Net
  nfolds = 10
)

enet_mod <- glmnet(X, y, alpha = 0.5, lambda = cv_enet$lambda.min)
coef(enet_mod)










library(lme4)

lmer_mod <- lmer(
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost +
    team_WAR:team_type +
    (1 | team_ID),
  data = team_war_extended
)

summary(lmer_mod)


lmer_mod2 <- lmer(
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost +
    (team_WAR | team_ID),
  data = team_war_extended
)
summary(lmer_mod2)












library(xgboost)

features <- team_war_extended %>%
  select(team_WAR, pct_WAR_new, pct_WAR_lost, WAR_continuity, top3_WAR_share)

X <- as.matrix(features)
y <- team_war_extended$W

set.seed(88)
xgb_mod <- xgboost(
  data = X,
  label = y,
  nrounds = 500,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.03,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)



library(brms)

bayes_mod <- brm(
  W ~ team_WAR + pct_WAR_new + pct_WAR_lost +
    team_WAR:team_type +
    (1 | team_ID),
  data = team_war_extended,
  family = gaussian(),
  cores = 4
)
summary(bayes_mod)