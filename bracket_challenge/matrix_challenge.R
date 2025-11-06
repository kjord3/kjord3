############################################################################################################
# Import libraries
library(tidyverse)
library(ordinal)
library(caret)
library(lmtest)
library(yardstick)
library(ggthemes)
library(purrr)

# Load the data
df = read_csv("matrix.csv")

df = df %>%
  rename(`Avg Resume` = `Avg...10`,
         `Avg Quality` = `Avg...13`)

# Factor and level Seed
df$Seed = factor(df$Seed, levels = c('1', '2', '3', '4', '5', '6',
                                     '7', '8', '9', '10', '11', '12', '13', '14', 
                                     '15', '16', '17') , ordered = TRUE)
###########################################################################################
df25 = df %>%
  filter(Year == 2025)

bpi0_25 = df25 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

bpi1_25 = df25 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

kpi0_25 = df25 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kpi1_25 = df25 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kpi_25 = df25 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

kpi_25 = df25 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

sor0_25 = df25 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

sor1_25 = df25 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

df25 = df25 %>%
  mutate(BPI_cwg = as.numeric(ifelse(`Power Conf` == 0, BPI - bpi0_25, BPI - bpi1_25)),
         KPI_cwg = as.numeric(ifelse(`Power Conf` == 0, KPI - kpi0_25, KPI - kpi1_25)),
         SOR_cwg = as.numeric(ifelse(`Power Conf` == 0, SOR - sor0_25, SOR - sor1_25)),
         Q1_Opp = `Q1 W` + `Q1 L`)

colSums(is.na(df25))

# Train & Test sets | Who makes it?
set.seed(88)

auto_bids25 = df25 %>%
  filter(`Conf Champ` == 1)

df = df %>%
  mutate(Tourney = ifelse(as.numeric(Seed) <= 16, 1, 0),
         Q1_Opp = `Q1 W` + `Q1 L`)

bids25 = df25 %>%
  mutate(Tourney = ifelse(as.numeric(Seed) <= 16, 1, 0)) %>%
  filter(`Conf Champ` != 1)

real25 = df25 %>%
  filter(as.numeric(Seed) <= 16) %>%
  select(Team, `Conf Champ`)
#######################################################################################
# Split into training (2021-2024) and test (2025)
train_df <- df %>% filter(Year >= 2021 & Year <= 2024)
test_df  <- df %>% filter(Year == 2025)

# Exclude auto-bid teams from test evaluation
test_df_eval <- test_df %>% filter(`Conf Champ` != 1)

# Actual teams that made the tournament
real25 <- df %>%
  filter(Year == 2025, as.numeric(Seed) <= 16) %>%
  select(Team, `Conf Champ`)

##############################################################################################
# Remove rows with missing predictor values
predictor_cols <- c("NET", "KPI", "SOR", "BPI", "KP", "Avg Quality", "Avg Resume", 
                    "Q1_Opp", "Power Conf", "Tourney")
train_df <- train_df %>%
  select(all_of(predictor_cols)) %>%
  drop_na()

test_df_eval <- test_df_eval %>%
  select(all_of(predictor_cols), Team, `Conf Champ`) %>%
  drop_na()

# Convert target to factor for caret
train_df$Tourney <- factor(ifelse(train_df$Tourney == 1, "Yes", "No"))
test_df_eval$Tourney  <- factor(ifelse(test_df_eval$Tourney == 1, "Yes", "No"))

##########################################################################################
# GBM formula
gbm_formula <- Tourney ~ NET + KPI + SOR + BPI + KP + `Avg Quality` + 
  `Avg Resume` + Q1_Opp + `Power Conf`

# Cross-validation setup
train_ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

###########################################################################################
# Train GBM
gbm_model <- train(
  gbm_formula,
  data = train_df,
  method = "gbm",
  trControl = train_ctrl,
  metric = "ROC",
  verbose = FALSE,
  tuneGrid = expand.grid(
    n.trees = seq(100, 500, by = 100),
    interaction.depth = c(1, 2, 3),
    shrinkage = c(0.01, 0.05),
    n.minobsinnode = c(5, 10)
  )
)
summary(gbm_model)

gbm_model$results %>% 
  arrange(desc(ROC)) %>% 
  head(1)
############################################################################################
# Predict probabilities on test data
test_df_eval <- test_df_eval %>%
  mutate(pred_prob = predict(gbm_model, 
                             newdata = test_df_eval, type = "prob")[, "Yes"])

# Select top 37 predicted teams
top_predicted <- test_df_eval %>%
  arrange(desc(pred_prob)) %>%
  head(37) %>%
  select(Team, pred_prob, `Conf Champ`) %>%
  mutate(Top37 = TRUE)

first4 = test_df_eval %>%
  arrange(desc(pred_prob)) %>%
  head(41) %>%
  tail(4) %>%
  select(Team, pred_prob)
first4

# Add auto-bid teams
auto_bids25 <- test_df %>%
  filter(`Conf Champ` == 1) %>%
  select(Team, `Conf Champ`) %>%
  mutate(pred_prob = NA, Top37 = FALSE)

# Combine predicted + auto-bid teams
predicted_teams_df <- bind_rows(top_predicted, auto_bids25) %>%
  distinct(Team, .keep_all = TRUE)

# Add Actual tournament flag
predicted_teams_df <- predicted_teams_df %>%
  mutate(Actual = Team %in% real25$Team)

#####################################################################################
# Count matches between top 37 predicted and actual teams (excluding auto-bids)
num_matches_top37 <- sum(top_predicted$Team %in% real25$Team)

#####################################################################################
# Output results
cat("Correctly predicted March Madness Teams:", num_matches_top37 + 31, "\n")
num_matches_25 = num_matches_top37 + 31

# View the final predicted teams data frame
predicted_teams_df

bids25 = df25 %>% filter(`Conf Champ` != 1)

conf_champs = df25 %>% filter(`Conf Champ` == 1)

predicted_teams = predicted_teams_df %>% select(Team)

predicted_teams25 <- bids25 %>% filter(Team %in% predicted_teams$Team)

bids25 = rbind(conf_champs, predicted_teams25)

# Texas and Xavier miss out (Made it in reality)
# West Virginia and UC Irvine made it (Missed in reality)
##################################################################################
train = df %>% filter(Year < 2025)

bpi0_train = train %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

bpi1_train = train %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

kpi0_train = train %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kpi1_train = train %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kp0_train = train %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

kp1_train = train %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

sor0_train = train %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

sor1_train = train %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

train = train %>%
  mutate(BPI_cwg = as.numeric(ifelse(`Power Conf` == 0, 
                                     BPI - bpi0_train, BPI - bpi1_train)),
         KPI_cwg = as.numeric(ifelse(`Power Conf` == 0, 
                                     KPI - kpi0_train, KPI - kpi1_train)),
         SOR_cwg = as.numeric(ifelse(`Power Conf` == 0, 
                                     SOR - sor0_train, SOR - sor1_train)),
         KP_cwg = as.numeric(ifelse(`Power Conf` == 0, 
                                    KP - kp0_train, KP - kp1_train)))

colSums(is.na(train))

kp0_25 = bids25 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

kp1_25 = bids25 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

train = train %>% filter(Seed <= 16 & `Conf Champ` != 1) %>%
  mutate(NET_SOR = log(NET+SOR), SOR_KPI = SOR*KPI+KP, BPI_KPI = 
           (BPI*SOR+SOR_cwg)*(`Avg Quality`)+`Power Conf`,
         Res_Qual = NET*(KPI+SOR) + NET*(KP+BPI),
         BPI_ = (KP*BPI_cwg / KPI_cwg)+NET,
         NET_ = NET*SOR,
         SOR_ = (`Avg Resume`)*`Power Conf` + SOR*SOR,
         SOR_Qual = `Avg Resume`*`Avg Quality`+(`Power Conf`*NET)
  )

colSums(is.na(train))
ord = clm(Seed ~ scale(`Avg Quality`) + scale(log(KP)) + scale(log(NET_SOR)) + 
            scale(log(SOR_KPI)) + scale(BPI_) + `Q1 W`,
          data = train) # 382.97 | 
summary(ord)

bids25 = bids25 %>%
  mutate(NET_SOR = log(NET+SOR), SOR_KPI = SOR*KPI+KP, 
         Res_Qual = NET*(KPI+SOR) + NET*(KP+BPI),
         BPI_ = (KP*BPI_cwg / KPI_cwg),
         NET_ = NET*SOR,
         SOR_ = (NET+SOR+BPI+KP),
         BPI_KPI = (BPI*SOR+SOR_cwg)*(`Avg Quality`) + `Power Conf`,
         SOR_Qual = `Avg Resume`*`Avg Quality`+ (`Power Conf`*NET)
  )

# Predictions
bids25 = bids25 %>% mutate(
  eta = 
    3.73917*scale(`Avg Quality`) +
    (-2.47023)*scale(log(KP)) +
    5.65983*scale(log(NET_SOR)) +
    2.51152*scale(log(SOR_KPI)) +
    0.81884*scale(BPI_) +
    (-0.25437)*`Q1 W`
)

correct25 = bids25

seed1 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 1) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed1 # Auburn, Houston, Duke, Florida

correct25 = correct25 %>%
  filter(!(Team %in% seed1$Team))

seed2 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 2) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed2 # Tennessee, Alabama, Michigan State, St. Johns

correct25 = correct25 %>%
  filter(!(Team %in% seed2$Team))

seed3 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 3) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed3 # Texas Tech, Kentucky, Wisconsin, Iowa St.

correct25 = correct25 %>%
  filter(!(Team %in% seed3$Team))

seed4 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 4) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed4 # Michigan, Maryland, Louisville, Arizona

correct25 = correct25 %>%
  filter(!(Team %in% seed4$Team))

seed5 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 5) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed5 # Texas A&M, Clemson, Oregon, Purdue

correct25 = correct25 %>%
  filter(!(Team %in% seed5$Team))

seed6 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 6) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed6 # BYU, Mississippi, Illinois, Kansas

correct25 = correct25 %>%
  filter(!(Team %in% seed6$Team))

seed7 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 7) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed7 # UCLA, Marquette, Memphis, Saint Mary's

correct25 = correct25 %>%
  filter(!(Team %in% seed7$Team))

seed8 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 8) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed8 # Missouri, Creighton, Mississippi St., Connecticut

correct25 = correct25 %>%
  filter(!(Team %in% seed8$Team))

seed9 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 9) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed9 # Gonzaga, Georgia, Oklahoma, New Mexico

correct25 = correct25 %>%
  filter(!(Team %in% seed9$Team))

seed10 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 10) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed10 # Baylor, Utah St., West Virginia, Arkansas

correct25 = correct25 %>%
  filter(!(Team %in% seed10$Team))

seed11 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 11) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(6)
seed11 # Vanderbilt, VCU, North Carolina, Drake, UC San Diego, Colorado St.

correct25 = correct25 %>%
  filter(!(Team %in% seed11$Team))

seed12 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 12) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed12 # San Diego St, UC Irvine, Liberty, McNeese St.

correct25 = correct25 %>%
  filter(!(Team %in% seed12$Team))

seed13 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 13) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed13 # Yale, High Point, Akron, Grand Canyon

correct25 = correct25 %>%
  filter(!(Team %in% seed13$Team))

seed14 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 14) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed14 # UNC Wilmington, Troy, Lipscomb, Montana

correct25 = correct25 %>%
  filter(!(Team %in% seed14$Team))

seed15 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 15) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(4)
seed15 # Robert Morris, Wofford, Nebraska Omaha, Bryant

correct25 = correct25 %>%
  filter(!(Team %in% seed15$Team))

seed16 = correct25 %>%
  arrange(eta) %>%
  mutate(Pred_Seed = 16) %>%
  select(Team, Seed, Pred_Seed) %>%
  head(6)
seed16 # Norfolk St., Mount St. Mary's SIU Edwardsville, Alabama St., American, Saint Francis

matrix25 = (num_matches_25 * 2) + (51 * 2) + (63 * 2) + 2
round(matrix25, 0) # 362

