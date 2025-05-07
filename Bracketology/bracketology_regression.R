# Import libraries
library(tidyverse)
library(ordinal)
library(caret)
library(lmtest)
library(yardstick)
library(ggthemes)
library(purrr)

# Load the data
df = read_csv("small_data.xlsx.csv")
new_df = read_csv("C:/Users/kaleb/Downloads/bracketology_updated.csv")
teams2025 = read_csv("teams2025.csv")

df = cbind(df, new_df)

df = df %>%
  select(1:11, 20:22)

# Factor and level Seed
df$Seed = factor(df$Seed, levels = c('1', '2', '3', '4', '5', '6',
                                     '7', '8', '9', '10', '11', '12') , ordered = TRUE)

teams2025$Seed = factor(teams2025$Seed, levels = c('1', '2', '3', '4', '5', '6',
                                            '7', '8', '9', '10', '11', '12') , ordered = TRUE)

df = df %>%
  mutate(
    Win_Pct = W / W + L,
    Opp = W + L)

bpi0 = df %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

bpi1 = df %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

kpi0 = df %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kpi1 = df %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

sor0 = df %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

sor1 = df %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

df = df %>%
  mutate(BPI_cwg = as.numeric(ifelse(`Power Conf` == 0, BPI - bpi0, BPI - bpi1)),
         KPI_cwg = as.numeric(ifelse(`Power Conf` == 0, KPI - kpi0, KPI - kpi1)),
         SOR_cwg = as.numeric(ifelse(`Power Conf` == 0, SOR - sor0, SOR - sor1)))

df = df %>%
  mutate(Win_Pct = W / (W + L),
         Win_Pct = ifelse(is.nan(Win_Pct), 0, Win_Pct))

df = df %>%
  mutate(NET_group = case_when(
    NET <= 25 ~ "Top25",
    NET <= 50 ~ "26to50",
    NET <= 100 ~ "51to100",
    TRUE ~ "100plus"
  )) %>%
  mutate(NET_group = as.factor(NET_group))

# Train & Test sets
set.seed(88)

# Baseline LM model
base_mod = lm(as.numeric(Seed) ~ BPI + KPI + SOR, df)
summary(base_mod)

# Baseline Predictions
base_preds = predict(base_mod, newdata = df, type = "response")
base_preds = round(as.numeric(base_preds), 0)

# New baseline df
comps_base = data.frame(Team = df$Team, NET = df$NET, BPI = df$BPI, KPI = df$KPI, SOR = df$SOR,
                   Actual = df$Seed, Predicted = base_preds)

# Baseline Accuracy
num_matches = sum(comps_base$Actual == comps_base$Predicted)
total_obs = nrow(comps_base)
percent_same = (num_matches / total_obs)
percent_same # 32% accurate

# Baseline 2025
base_2025 = predict(base_mod, newdata = teams2025, type = "response")
base_2025 = round(as.numeric(base_2025), 0)

# New baseline df 2025
comps_base_25 = data.frame(Team = teams2025$Team, BPI = teams2025$BPI, KPI = teams2025$KPI, SOR = teams2025$SOR,
                        Actual = teams2025$Seed, Predicted = base_2025)

# Baseline Accuracy 2025
num_matches_25 = sum(comps_base_25$Actual == comps_base_25$Predicted)
total_obs_25 = nrow(comps_base_25)
percent_same_25 = (num_matches_25 / total_obs_25)
percent_same_25 # 30% accurate

# Ordinal Model
ordinal_mod = clm(Seed ~ BPI + KPI + SOR, data = df)
summary(ordinal_mod)

new_ord = clm(Seed ~ NET + KPI + SOR + W + BPI_cwg + W:Opp + KP:`Conf Champ` + KP:SOR_cwg, data = df)
summary(new_ord)

# Predictions
overall_preds = predict(ordinal_mod, newdata = df, type = "class")
overall_preds_new = predict(new_ord, newdata = df, type = "class")

# Confusion Matrix for overall dataset
confusionMatrix(overall_preds$fit, df$Seed) # 44%
confusionMatrix(overall_preds_new$fit, df$Seed) # 53.5%

# New df
comps = data.frame(Team = df$Team, NET = df$NET, BPI = df$BPI, KPI = df$KPI, SOR = df$SOR,
                   Actual = df$Seed, Predicted = overall_preds$fit)

# Likelihood Ratio Test
lrtest(ordinal_mod, new_ord) # This indicates that the difference in fit between original ordinal_mod and new new_ord is statistically significant

# Log-Likelihood of Full & Baseline model
logLik_new = logLik(new_ord)
logLik_orig = logLik(ordinal_mod)

# McFadden R-Squared
mcfadden_r2 = 1 - (logLik_new / logLik_orig)
mcfadden_r2 * 100 # This shows that the new model variation explained by the data increases by 15.77% from the baseline ordinal model

# Confusion Matrix Accuracy for being off by Absolute Value(1)
pred_int = as.integer(as.character(overall_preds_new$fit))
true_int = as.integer(as.character(df$Seed))

within_1 = abs(pred_int - true_int) <= 1
within_1

plus_minus1_accuracy = mean(within_1)
plus_minus1_accuracy # 89%

# Confusion Matrix Heatmap
true = df$Seed
pred = overall_preds_new$fit

conf_mat = table(True = true, Predicted = pred)

conf_df = as.data.frame(conf_mat)

ggplot(conf_df, aes(x = Predicted, y = True, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Confusion Matrix Heatmap (2021-2024 Data)", x = "Predicted", y = "Actual") +
  theme_economist()
####################################################################################################################################
bpi0_new = teams2025 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

bpi1_new = teams2025 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

kpi0_new = teams2025 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kpi1_new = teams2025 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

sor0_new = teams2025 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

sor1_new = teams2025 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

teams2025 = teams2025 %>%
  mutate(BPI_cwg = as.numeric(ifelse(`Power Conf` == 0, BPI - bpi0, BPI - bpi1)),
         KPI_cwg = as.numeric(ifelse(`Power Conf` == 0, KPI - kpi0, KPI - kpi1)),
         SOR_cwg = as.numeric(ifelse(`Power Conf` == 0, SOR - sor0, SOR - sor1)))

teams2025 = teams2025 %>%
  mutate(Win_Pct = W / (W + L),
         Win_Pct = ifelse(is.nan(Win_Pct), 0, Win_Pct))

teams2025 = teams2025 %>%
  mutate(NET_group = case_when(
    NET <= 25 ~ "Top25",
    NET <= 50 ~ "26to50",
    NET <= 100 ~ "51to100",
    TRUE ~ "100plus"
  )) %>%
  mutate(NET_group = as.factor(NET_group))

preds_2025 = predict(new_ord, newdata = teams2025, type = "class")

confusionMatrix(preds_2025$fit, teams2025$Seed) # 64% accuracy

comb_2025 = data.frame(Team = teams2025$Team, BPI = teams2025$BPI, KPI = teams2025$KPI, SOR = teams2025$SOR,
                       Actual = teams2025$Seed, Predicted = preds_2025$fit)

# Confusion Matrix Accuracy for being off by Absolute Value(1)
pred_int_25 = as.integer(as.character(preds_2025$fit))
true_int_25 = as.integer(as.character(teams2025$Seed))

within_1_25 = abs(pred_int_25 - true_int_25) <= 1
within_1_25

plus_minus1_accuracy_25 = mean(within_1_25)
plus_minus1_accuracy_25
# 92%

# Confusion Matrix Heatmap
true_25 = teams2025$Seed
pred_25 = preds_2025$fit

conf_mat_25 = table(True = true_25, Predicted = pred_25)

conf_df_25 = as.data.frame(conf_mat_25)

ggplot(conf_df_25, aes(x = Predicted, y = True, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Confusion Matrix Heatmap (2025 Data)", x = "Predicted", y = "Actual") +
  theme_economist()

actual_seeds = teams2025$Seed

predicted_seeds = data.frame(Team = teams2025$Team, Actual_Seed = actual_seeds, Predicted_Seed = preds_2025)

ord_function = function(`Conf Champ`, NET, KPI, SOR, W, BPI_cwg, SOR_cwg, Opp) {
  
  # Create the interaction terms
  W_Opp = W * Opp
  KP_Conf_Champ = KPI * `Conf Champ`
  KP_SOR_cwg = KPI * SOR_cwg
  
  # Linear predictor (eta) using your final model's coefficients
  eta = 0.0672448 * NET +
    0.0683249 * KPI +
    0.2314032 * SOR +
    (-1.1293652) * W +
    0.0614721 * BPI_cwg +
    0.0351165 * W_Opp +
    0.0498408 * KP_Conf_Champ +
    (-0.0025185) * KP_SOR_cwg
  
  # Thresholds from your final model
  thresholds = c(`1|2` = -4.929, `2|3` = -1.978, `3|4` = 0.450,
                 `4|5` = 2.557, `5|6` = 4.535, `6|7` = 6.202,
                 `7|8` = 7.732, `8|9` = 9.159, `9|10` = 10.663,
                 `10|11` = 12.814, `11|12` = 17.475)
  
  # Calculate cumulative probabilities using logit link
  cum_probs = plogis(thresholds - eta)
  
  # Get probabilities for each seed
  probs = c(cum_probs[1], diff(cum_probs), 1 - tail(cum_probs, 1))
  
  # Name the probabilities
  names(probs) = paste0("Seed ", 1:12)
  
  # Return rounded probabilities
  return(round(probs, 4))
}

sub_df25 = teams2025 %>%
  select(`Conf Champ`, NET, KPI, SOR, W, BPI_cwg, SOR_cwg, Opp)

# Apply the prediction function row-by-row
predicted_probs = pmap_dfr(sub_df25, function(`Conf Champ`, NET, KPI, SOR, W, BPI_cwg, SOR_cwg, Opp) {
  ord_function(`Conf Champ`, NET, KPI, SOR, W, BPI_cwg, SOR_cwg, Opp)
})

# Add the Team names
predicted_probs$Team = teams2025$Team

# Rearrange the columns: Team first, then seeds
predicted_probs = predicted_probs %>%
  select(Team, starts_with("Seed"))

# Assign predicted seed based on highest probability
predicted_probs$Predicted_Seed = apply(predicted_probs[, 2:13], 1, function(row) {
  which.max(row)
})

seed1 = predicted_probs %>%
  arrange(desc(`Seed 1`)) %>%
  select(Team, `Seed 1`) %>%
  head(4)
seed1 # Auburn, Houston, Florida, Alabama

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed1$Team))

seed2 = predicted_probs %>%
  mutate(prob2 = `Seed 1` + `Seed 2`) %>%
  arrange(desc(prob2)) %>%
  select(Team, prob2) %>%
  head(4)
seed2 # Duke, Tennessee, Michigan State, Kentucky

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed2$Team))

seed3 = predicted_probs %>%
  mutate(prob3 = `Seed 1` + `Seed 2` + `Seed 3`) %>%
  arrange(desc(prob3)) %>%
  select(Team, prob3) %>%
  head(4)
seed3 # Texas Tech, St. John's, Wisconsin, Michigan

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed3$Team))

seed4 = predicted_probs %>%
  mutate(prob4 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4`) %>%
  arrange(desc(prob4)) %>%
  select(Team, prob4) %>%
  head(4)
seed4 # Maryland, Arizona, Texas A&M, Iowa State

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed4$Team))

seed5 = predicted_probs %>%
  mutate(prob5 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5`) %>%
  arrange(desc(prob5)) %>%
  select(Team, prob5) %>%
  head(4)
seed5 # Clemson, Oregon, Louisville, Mississippi

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed5$Team))

seed6 = predicted_probs %>%
  mutate(prob6 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6`) %>%
  arrange(desc(prob6)) %>%
  select(Team, prob6) %>%
  head(4)
seed6 # Kansas, BYU, Purdue, UCLA

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed6$Team))

seed7 = predicted_probs %>%
  mutate(prob7 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7`) %>%
  arrange(desc(prob7)) %>%
  select(Team, prob7) %>%
  head(4)
seed7 # Illinois, Saint Mary's, Missouri, Marquette

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed7$Team))

seed8 = predicted_probs %>%
  mutate(prob8 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8`) %>%
  arrange(desc(prob8)) %>%
  select(Team, prob8) %>%
  head(4)
seed8 # Memphis, Connecticut, Creighton, Gonzaga

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed8$Team))

seed9 = predicted_probs %>%
  mutate(prob9 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9`) %>%
  arrange(desc(prob9)) %>%
  select(Team, prob9) %>%
  head(4)
seed9 # Georgia, Mississippi State, Oklahoma, Baylor

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed9$Team))

seed10 = predicted_probs %>%
  mutate(prob10 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9` + `Seed 10`) %>%
  arrange(desc(prob10)) %>%
  select(Team, prob10) %>%
  head(4)
seed10 # New Mexico, Utah State, Arkansas, Texas

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed10$Team))

seed11 = predicted_probs %>%
  mutate(prob11 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9` + `Seed 10` + `Seed 11`) %>%
  arrange(desc(prob11)) %>%
  select(Team, prob11) %>%
  head(6)
seed11 # North Carolina, Vanderbilt, Xavier, San Diego State, VCU, UC San Diego

predicted_probs = predicted_probs %>%
  filter(!(Team %in% seed11$Team))

seed12 = predicted_probs %>%
  mutate(prob12 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9` + `Seed 10` + `Seed 11` + `Seed 12`) %>%
  arrange(desc(prob12)) %>%
  select(Team, prob12) %>%
  head(4)
seed12 # Drake, McNeese State, Liberty, Colorado State

set.seed(88)
folds = createFolds(df$Seed, k = 5, returnTrain = TRUE)

cv_clm = function(train_idx, test_idx) {
  train_data = df[train_idx, ]
  test_data = df[test_idx, ]
  
  # Fit the model
  model = clm(Seed ~ BPI_cwg:SOR_cwg + `Conf Champ` + NET + KPI + SOR + BPI + W + L:Opp + KPI_cwg:L, 
              data = train_data, link = "logit")
  
  # Predict
  preds = predict(model, newdata = test_data, type = "class")
  
  # Convert predictions to integers
  pred_int = as.integer(as.character(preds$fit))
  true_int = as.integer(as.character(test_data$Seed))
  
  # Calculate accuracies
  exact = mean(pred_int == true_int)
  plus_minus1 = mean(abs(pred_int - true_int) <= 1)
  
  return(c(exact = exact, plus_minus1 = plus_minus1))
}

cv_results = map(folds, ~cv_clm(train_idx = .x, test_idx = setdiff(1:nrow(df), .x)))

# Convert to df
cv_results_df = do.call(rbind, cv_results) %>% as.data.frame()
cv_results_df

colMeans(cv_results_df)

# Add Indiana and West Virginia and remove Texas and Xavier from 2025
#############################################################################################################
new25 = teams2025 %>%
  filter(Team != 'Texas' & Team != 'Xavier')

added = data.frame(
  Team = c('West Virginia', 'Indiana'),
  Conference = c('Big 12', 'Big 10'),
  `Conf Champ` = c(0, 0),
  `Power Conf` = c(1, 1),
  Year = c(2025, 2025),
  NET = c(51, 54),
  KPI = c(48, 35),
  SOR = c(47, 50),
  BPI = c(51, 52),
  KP = c(53, 48),
  Seed = c(11, 11),
  W = c(4, 2),
  L = c(7, 6),
  Opp = c(11, 8)
  )

added = added %>%
  rename(`Conf Champ` = Conf.Champ,
         `Power Conf` = Power.Conf)
added = added %>%
  mutate(BPI_cwg = as.numeric(ifelse(`Power Conf` == 0, BPI - bpi0, BPI - bpi1)),
         KPI_cwg = as.numeric(ifelse(`Power Conf` == 0, KPI - kpi0, KPI - kpi1)),
         SOR_cwg = as.numeric(ifelse(`Power Conf` == 0, SOR - sor0, SOR - sor1)))

added = added %>%
  mutate(Win_Pct = W / (W + L),
         Win_Pct = ifelse(is.nan(Win_Pct), 0, Win_Pct))

added = added %>%
  mutate(NET_group = case_when(
    NET <= 25 ~ "Top25",
    NET <= 50 ~ "26to50",
    NET <= 100 ~ "51to100",
    TRUE ~ "100plus"
  )) %>%
  mutate(NET_group = as.factor(NET_group))

new25 = rbind(new25, added)

preds_new25 = predict(new_ord, newdata = new25, type = "class")

confusionMatrix(preds_new25$fit, new25$Seed) # 64% accuracy

comb_new25 = data.frame(Team = new25$Team, BPI = new25$BPI, KPI = new25$KPI, SOR = new25$SOR,
                       Actual = new25$Seed, Predicted = preds_new25$fit)

# Confusion Matrix Accuracy for being off by Absolute Value(1)
pred_int_25 = as.integer(as.character(preds_new25$fit))
true_int_25 = as.integer(as.character(new25$Seed))

within_1_25 = abs(pred_int_25 - true_int_25) <= 1
within_1_25

plus_minus1_accuracy_25 = mean(within_1_25)
plus_minus1_accuracy_25
# 92%




sub_new25 = new25 %>%
  select(`Conf Champ`, NET, KPI, SOR, W, BPI_cwg, SOR_cwg, Opp)

# Apply the prediction function row-by-row
predicted_probs_new = pmap_dfr(sub_new25, function(`Conf Champ`, NET, KPI, SOR, W, BPI_cwg, SOR_cwg, Opp) {
  ord_function(`Conf Champ`, NET, KPI, SOR, W, BPI_cwg, SOR_cwg, Opp)
})

# Add the Team names
predicted_probs_new$Team = new25$Team

# Rearrange the columns: Team first, then seeds
predicted_probs_new = predicted_probs_new %>%
  select(Team, starts_with("Seed"))

# Assign predicted seed based on highest probability
predicted_probs_new$Predicted_Seed = apply(predicted_probs_new[, 2:13], 1, function(row) {
  which.max(row)
})

seed1_new = predicted_probs_new %>%
  arrange(desc(`Seed 1`)) %>%
  select(Team, `Seed 1`) %>%
  head(4)
seed1_new # Auburn, Houston, Florida, Alabama

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed1_new$Team))

seed2_new = predicted_probs_new %>%
  mutate(prob2 = `Seed 1` + `Seed 2`) %>%
  arrange(desc(prob2)) %>%
  select(Team, prob2) %>%
  head(4)
seed2_new # Duke, Tennessee, Michigan State, Kentucky

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed2_new$Team))

seed3_new = predicted_probs_new %>%
  mutate(prob3 = `Seed 1` + `Seed 2` + `Seed 3`) %>%
  arrange(desc(prob3)) %>%
  select(Team, prob3) %>%
  head(4)
seed3_new # Texas Tech, St. John's, Wisconsin, Michigan

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed3_new$Team))

seed4_new = predicted_probs_new %>%
  mutate(prob4 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4`) %>%
  arrange(desc(prob4)) %>%
  select(Team, prob4) %>%
  head(4)
seed4_new # Maryland, Arizona, Texas A&M, Iowa State

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed4_new$Team))

seed5_new = predicted_probs_new %>%
  mutate(prob5 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5`) %>%
  arrange(desc(prob5)) %>%
  select(Team, prob5) %>%
  head(4)
seed5_new # Clemson, Oregon, Louisville, Mississippi

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed5_new$Team))

seed6_new = predicted_probs_new %>%
  mutate(prob6 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6`) %>%
  arrange(desc(prob6)) %>%
  select(Team, prob6) %>%
  head(4)
seed6_new # Kansas, BYU, Purdue, UCLA

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed6_new$Team))

seed7_new = predicted_probs_new %>%
  mutate(prob7 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7`) %>%
  arrange(desc(prob7)) %>%
  select(Team, prob7) %>%
  head(4)
seed7_new # Illinois, Saint Mary's, Missouri, Marquette

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed7_new$Team))

seed8_new = predicted_probs_new %>%
  mutate(prob8 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8`) %>%
  arrange(desc(prob8)) %>%
  select(Team, prob8) %>%
  head(4)
seed8_new # Memphis, Connecticut, Creighton, Gonzaga

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed8_new$Team))

seed9_new = predicted_probs_new %>%
  mutate(prob9 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9`) %>%
  arrange(desc(prob9)) %>%
  select(Team, prob9) %>%
  head(4)
seed9_new # Georgia, Mississippi State, Oklahoma, Baylor

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed9_new$Team))

seed10_new = predicted_probs_new %>%
  mutate(prob10 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9` + `Seed 10`) %>%
  arrange(desc(prob10)) %>%
  select(Team, prob10) %>%
  head(4)
seed10_new # New Mexico, Utah State, Arkansas, North Carolina

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed10_new$Team))

seed11_new = predicted_probs_new %>%
  mutate(prob11 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9` + `Seed 10` + `Seed 11`) %>%
  arrange(desc(prob11)) %>%
  select(Team, prob11) %>%
  head(6)
seed11_new # West Virginia, Vanderbilt, Indiana, San Diego State, VCU, UC San Diego

predicted_probs_new = predicted_probs_new %>%
  filter(!(Team %in% seed11_new$Team))

seed12_new = predicted_probs_new %>%
  mutate(prob12 = `Seed 1` + `Seed 2` + `Seed 3` + `Seed 4` + `Seed 5` + `Seed 6` + `Seed 7` + `Seed 8` +
           `Seed 9` + `Seed 10` + `Seed 11` + `Seed 12`) %>%
  arrange(desc(prob12)) %>%
  select(Team, prob12) %>%
  head(4)
seed12_new # Drake, McNeese State, Liberty, Colorado State

