############################################################################################################
# Import libraries
library(tidyverse)
library(ordinal)
library(caret)
library(lmtest)
library(yardstick)
library(ggthemes)
library(purrr)
library(splines)

# Load the data
df26 = read_csv("torvik26.csv")


bpi0_26 = df26 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

bpi1_26 = df26 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_BPI = mean(BPI)) %>%
  pull(Avg_BPI)

kpi0_26 = df26 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kpi1_26 = df26 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KPI = mean(KPI)) %>%
  pull(Avg_KPI)

kpi_26 = df26 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

kpi_26 = df26 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

sor0_26 = df26 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

sor1_26 = df26 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_SOR = mean(SOR)) %>%
  pull(Avg_SOR)

df26 = df26 %>%
  mutate(BPI_cwg = as.numeric(ifelse(`Power Conf` == 0, BPI - bpi0_26, BPI - bpi1_26)),
         KPI_cwg = as.numeric(ifelse(`Power Conf` == 0, KPI - kpi0_26, KPI - kpi1_26)),
         SOR_cwg = as.numeric(ifelse(`Power Conf` == 0, SOR - sor0_26, SOR - sor1_26)))

colSums(is.na(df26))


# Train & Test sets | Who makes it?
set.seed(88)

bids26 = df26

#######################################################################################
df = read_csv("C:/Users/kaleb/Downloads/matrix.csv")

df = df %>%
  rename(`Avg Resume` = `Avg...10`,
         `Avg Quality` = `Avg...13`)%>%
  select(1:13)

# Factor and level Seed
df$Seed = factor(df$Seed, levels = c('1', '2', '3', '4', '5', '6',
                                     '7', '8', '9', '10', '11', '12', '13', '14', 
                                     '15', '16', '17') , ordered = TRUE)


auto_bids26 = df26 %>%
  filter(`Conf Champ` == 1)

df = df %>%
  mutate(Tourney = ifelse(as.numeric(Seed) <= 16, 1, 0))

# Split into training (2021-2024) and test (2026)
train_df <- df %>% filter(Year >= 2021 & Year <= 2025)

##############################################################################################
# Remove rows with missing predictor values
predictor_cols <- c("NET", "KPI", "SOR", "BPI", "KP", "Avg Quality", "Avg Resume", 
                    "Power Conf")


train_df <- train_df %>%
  select(all_of(predictor_cols), Tourney) %>%
  drop_na()

test_df_eval <- df26 %>%
  select(all_of(predictor_cols), Team) %>%
  drop_na()

# Convert target to factor for caret
train_df$Tourney <- factor(ifelse(train_df$Tourney == 1, "Yes", "No"))

train_df = train_df %>%
  mutate(xNET = NET / mean(NET),
         xKPI = KPI / mean(KPI),
         xSOR = SOR / mean(SOR),
         xBPI = BPI / mean(BPI),
         xKP =  KP / mean(KP)
  ) %>%
  mutate(NET_SOR = log(NET+SOR), SOR_KPI = SOR*KPI+KP, 
         Res_Qual = NET*(KPI+SOR) + NET*(KP+BPI),
         BPI_ = (KP*BPI / KPI),
         NET_ = NET*SOR,
         SOR_ = (NET+SOR+BPI+KP),
         BPI_KPI = (BPI*SOR+SOR)*(`Avg Quality`) + `Power Conf`,
         SOR_Qual = `Avg Resume`*`Avg Quality`+ (`Power Conf`*NET)
  )

test_df_eval = test_df_eval %>%
  mutate(xNET = NET / mean(NET),
         xKPI = KPI / mean(KPI),
         xSOR = SOR / mean(SOR),
         xBPI = BPI / mean(BPI),
         xKP =  KP / mean(KP)
  ) %>%
  mutate(NET_SOR = log(NET+SOR), SOR_KPI = SOR*KPI+KP, 
         Res_Qual = NET*(KPI+SOR) + NET*(KP+BPI),
         BPI_ = (KP*BPI / KPI),
         NET_ = NET*SOR,
         SOR_ = (NET+SOR+BPI+KP),
         BPI_KPI = (BPI*SOR+SOR)*(`Avg Quality`) + `Power Conf`,
         SOR_Qual = `Avg Resume`*`Avg Quality`+ (`Power Conf`*NET)
  )

##################################################################################
train = df %>% filter(Year < 2026)

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


kp0_26 = bids26 %>%
  filter(`Power Conf` == 0) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

kp1_26 = bids26 %>%
  filter(`Power Conf` == 1) %>%
  summarize(Avg_KP = mean(KP)) %>%
  pull(Avg_KP)

train = train %>% filter(Seed <= 16 & `Conf Champ` != 1) %>%
  mutate(NET_SOR = log(NET+SOR), SOR_KPI = (SOR*KPI+KP), 
         BPI_KPI = (BPI*SOR+SOR_cwg)*(`Avg Quality`) +`Power Conf`,
         Res_Qual = NET*(KPI+SOR) + NET*(KP+BPI),
         BPI_ = ((KP*BPI_cwg / KPI_cwg)+NET) + 90,
         NET_ = NET*SOR,
         SOR_ = (`Avg Resume`)*`Power Conf` + SOR*SOR,
         SOR_Qual = `Avg Resume`*`Avg Quality`+(`Power Conf`*NET),
         sos = log(NET + KP + BPI) * log(SOR + 1),
         confidence = log(NET + 1) * log(SOR + 1) + 0.5 * log(BPI + KP),
         conf_inf = log(BPI + KP + NET) * (`Power Conf` + 1),
         manual = 3*sos + log(SOR)*log(KP)*log(BPI)*log(KPI) / log(`Power Conf` + 2) + (2*BPI_ + 9*log(SOR_)) +
           100*NET_SOR,
         manual2 =
           log(NET_SOR)^2 + log(SOR_) + log(NET_SOR) * scale(`Avg Resume`) +
           log(SOR_KPI) * `Avg Quality` -
           log(1 + Res_Qual)^2 +
           0.5 * (`Power Conf`) * log(BPI + KP),
         manual3 = 3*manual + 0.5*manual2 + (log(NET_SOR))*BPI_
  )


ord = clm(Seed ~ scale(log(KP)) + scale(log(NET_SOR)) + scale(log(manual3)) +
            scale(log(SOR_KPI)) + scale(manual2),
          data = train) # 471.49 | 
summary(ord)

bids26 = bids26 %>%
  mutate(NET_SOR = log(NET+SOR), SOR_KPI = (SOR*KPI+KP), 
         BPI_KPI = (BPI*SOR+SOR_cwg)*(`Avg Quality`) +`Power Conf`,
         Res_Qual = NET*(KPI+SOR) + NET*(KP+BPI),
         BPI_ = ((KP*BPI_cwg / KPI_cwg)+NET) + 90,
         NET_ = NET*SOR,
         SOR_ = (`Avg Resume`)*`Power Conf` + SOR*SOR,
         SOR_Qual = `Avg Resume`*`Avg Quality`+(`Power Conf`*NET),
         sos = log(NET + KP + BPI) * log(SOR + 1),
         confidence = log(NET + 1) * log(SOR + 1) + 0.5 * log(BPI + KP),
         conf_inf = log(BPI + KP + NET) * (`Power Conf` + 1),
         manual = 3*sos + log(SOR)*log(KP)*log(BPI)*log(KPI) / log(`Power Conf` + 2) + (2*BPI_ + 9*log(SOR_)) +
           100*NET_SOR,
         manual2 =
           log(NET_SOR)^2 + log(SOR_) + log(NET_SOR) * scale(`Avg Resume`) +
           log(SOR_KPI) * `Avg Quality` -
           log(1 + Res_Qual)^2 +
           0.5 * (`Power Conf`) * log(BPI + KP),
         manual3 = 3*manual + 0.5*manual2 + (log(NET_SOR))*BPI_
  )

# Predictions
bids26 = bids26 %>% mutate(
  eta = 
    (-2.0408)*scale(log(KP)) +
    3.8067*scale(log(NET_SOR)) +
    3.3609*scale(log(manual3)) +
    1.7785*scale(log(SOR_KPI)) +
    3.3593*scale(manual2)
)

correct26 = bids26 %>% arrange(eta)

model_rank = correct26 %>% select(Team, eta) %>% head(n=25) %>% mutate(model_rank = 1:25)

ap_rank <- tibble(
  ap_rank = 1:25,
  Team = c(
    "Arizona", "Connecticut", "Michigan", "Purdue", "Duke",
    "Houston", "Nebraska", "Gonzaga", "Iowa St.", "Michigan St.",
    "Illinois", "Texas Tech", "BYU", "Virginia",
    "Vanderbilt", "Florida", "Alabama", "Clemson",
    "Kansas", "Arkansas", "Georgia", "North Carolina",
    "Louisville", "Saint Louis", "Miami OH"
  )
)

ap_rank

rank_compare <- model_rank %>%
  left_join(ap_rank, by = "Team") %>%
  mutate(ap_rank = if_else(is.na(ap_rank), 26L, ap_rank))

cor(rank_compare$model_rank, rank_compare$ap_rank, method = "kendall")
mean(abs(rank_compare$model_rank - rank_compare$ap_rank))
mean(rank_compare$model_rank == rank_compare$ap_rank)



power37 = correct26 %>% select(Team, eta) %>% head(n=37) %>% mutate(model_rank = 1:37)
