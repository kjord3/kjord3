library(nycflights13)
library(tidyverse)
library(ggplot2)
library(caret)
library(xgboost)
library(gridExtra)
library(kableExtra)
library(htmltools)
library(knitr)

data("flights")
data("airlines")
data("airports")
data("planes")
data("weather")

all_flights = flights %>%
  left_join(weather) %>%
  select(1:24, 26:28) %>%
  left_join(airlines)

all_flights = na.omit(all_flights)
flights = flights %>%
  left_join(airlines)
nrow(all_flights)
summary(all_flights)
# Data on 284,550 different flights in the year 2013

length(unique(flights$carrier))
# There are 16 unique carriers (airlines)

top_air = all_flights %>%
  group_by(carrier) %>%
  mutate(num_flights = n()) %>%
  left_join(airlines) %>%
  select(carrier, name, num_flights) %>%
  unique() %>%
  arrange(desc(num_flights))
top_air
# The airline with the most and least flights are United Air Lines (49,271) & Skywest Airlines (27)
ggplot(top_air, aes(x = reorder(name, -num_flights), y = num_flights)) +
  geom_bar(stat = "identity", col = 'black', fill = 'steelblue') +
  labs(title = "Number of Flights by Carrier",
       x = "Carrier",
       y = "Number of Flights") +
  scale_y_continuous(breaks = seq(0, max(top_air$num_flights), by = 5000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of departure delays for every Airline
top_del = all_flights %>%
  filter(dep_delay > 0) %>%
  group_by(carrier) %>%
  mutate(num_delays = n()) %>%
  left_join(airlines) %>%
  select(carrier, name, num_delays) %>%
  unique() %>%
  arrange(desc(num_delays))
top_del

# The airline with the most and least flights are United Air Lines (22,555) & Skywest Airlines (8)
ggplot(top_del, aes(x = reorder(name, -num_delays), y = num_delays)) +
  geom_bar(stat = "identity", col = 'black', fill = 'steelblue') +
  labs(title = "Number of Departure Delays by Carrier",
       x = "Carrier",
       y = "Number of Departure Delays") +
  scale_y_continuous(breaks = seq(0, max(top_del$num_delays), by = 2000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Percent of Flight Departures are Delayed per Airline
per_del = cbind(top_air, top_del) %>%
  select(2, 6, 3) %>%
  rename(carrier = name...2) %>%
  mutate(per_delays = round(num_delays / num_flights, 2))

# The airline with the most and least percentage of flights delayed are AirTran Airways Corporation (52%) & Hawaiian Airlines Inc. (21%)
plot6 = ggplot(per_del, aes(x = reorder(carrier, -per_delays), y = per_delays)) +
  geom_bar(stat = "identity", col = 'black', fill = 'steelblue') +
  labs(title = "Percent of Departure Delays by Carrier",
       x = "Carrier",
       y = "Percent of Departure Delays") +
  scale_y_continuous(breaks = seq(0, max(per_del$per_delays), by = 0.05)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot6

# Mean time of Flight Departures, SkyWest Airlines Inc. (19) & United Air Lines Inc. (2.84)
mean_del = all_flights %>%
  group_by(carrier) %>%
  summarize(mean_delay = round(mean(dep_delay), 2)) %>%
  left_join(airlines) %>%
  select(carrier, name, mean_delay) %>%
  unique() %>%
  arrange(desc(mean_delay))
mean_del

plot4 = ggplot(mean_del, aes(x = reorder(name, -mean_delay), y = mean_delay)) +
  geom_bar(stat = "identity", col = 'black', fill = 'steelblue') +
  labs(title = "Mean time of Departure Delays by Carrier (In Minutes)",
       x = "Carrier",
       y = "Mean time of Departure Delays ") +
  scale_y_continuous(breaks = seq(0, max(mean_del$mean_delay), by = 5)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot4

# Departure Delay Boxplot
ggplot(all_flights, aes(name, dep_delay, col=name)) +
  geom_boxplot() +
  coord_trans() +
  theme_minimal() +
  labs(title = "Departure Delay by Carrier in 2013",
       x = 'Airline',
       y = 'Departure Delay by Minutes') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

############################################################################################################################################
# Questions I'm looking  to answer
############################################################################################################################################

# Question 1: What are the predicted departure delay times per airline?
# Question 2: What is the percent that a flight is likely to be delayed using Bayesian methodologies?

###################################################################################################################
# Question 1: What are the predicted departure delay times per airline?
###################################################################################################################
# Relevant flight information for the RF model
rel_flights = all_flights %>%
  select(dep_time, dep_delay, arr_time, air_time, distance, name, month, day, hour, 
         minute, arr_delay, temp, dewp, humid, wind_dir, wind_speed, precip, pressure)

# Factor carrier since its non-numerical
rel_flights$name <- as.factor(rel_flights$name)

# Train and test model
set.seed(123)
trainIndex <- createDataPartition(rel_flights$dep_delay, p = .8, 
                                  list = FALSE, 
                                  times = 1)
flightTrain <- rel_flights[trainIndex,]
flightTest  <- rel_flights[-trainIndex,]

dummy <- dummyVars(~ ., data = flightTrain)
flightTrain <- predict(dummy, newdata = flightTrain)
flightTest <- predict(dummy, newdata = flightTest)

# Prepare the data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(flightTrain[, -2]), label = flightTrain[, 2])
dtest <- xgb.DMatrix(data = as.matrix(flightTest[, -2]), label = flightTest[, 2])

# Train an xgboost model
params <- list(
  objective = "reg:squarederror", 
  eval_metric = "rmse",           
  eta = 0.1,                      
  max_depth = 10,                  
  subsample = 0.8,               
  colsample_bytree = 0.8          
)

bst_model <- xgb.train(params, dtrain, nrounds = 100)

# Predict departure delays on the test data
predictions <- predict(bst_model, dtest)

# Evaluate the model's performance
model_performance <- postResample(pred = predictions, obs = flightTest[, 2])
print(model_performance)

# Plot the actual vs predicted departure delays
results <- data.frame(actual = flightTest[, 2], predicted = predictions)

ggplot(results, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Departure Delays",
       x = "Actual Departure Delay",
       y = "Predicted Departure Delay") +
  theme_minimal()

######################################################################################################################
# Airline Predictions
######################################################################################################################

# AirTran
airtran = cbind(results, flightTest) %>%
  select(8, actual, predicted) %>%
  rename(AirTran = "name.AirTran Airways Corporation") %>%
  filter(AirTran > 0) %>%
  mutate(airtran_del = mean(predicted), index = 1:546) %>% unique()
summary(airtran)  

airtran_only = airtran %>%
  mutate(Airline = "AirTran Airways Corporation") %>%
  rename(Average_delay = airtran_del) %>%
  select(Airline, Average_delay) %>%
  unique()
  
plot_at <- ggplot(data = airtran, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-20, 500)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_at2 <- ggplot(data = airtran, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-20, 500), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_at, plot_at2, ncol = 1)

# Alaska
alaska = cbind(results, flightTest) %>%
  select(9, actual, predicted) %>%
  rename(Alaska = "name.Alaska Airlines Inc.") %>%
  filter(Alaska > 0) %>%
  mutate(alaska_del = mean(predicted), index = 1:118) %>% unique()
summary(alaska)  

alaska_only = alaska %>%
  mutate(Airline = "Alaska Airlines Inc.") %>%
  rename(Average_delay = alaska_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_aa <- ggplot(data = alaska, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-25, 180)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_aa2 <- ggplot(data = alaska, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-25, 180), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_aa, plot_aa2, ncol = 1)

# American
american = cbind(results, flightTest) %>%
  select(10, actual, predicted) %>%
  rename(American = "name.American Airlines Inc.") %>%
  filter(American > 0) %>%
  mutate(american_del = mean(predicted), index = 1:5611) %>% unique()
summary(american)  

american_only = american %>%
  mutate(Airline = "American Airlines Inc.") %>%
  rename(Average_delay = american_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_ama <- ggplot(data = american, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-25, 1100)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_ama2 <- ggplot(data = american, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-25, 1100), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_ama, plot_ama2, ncol = 1)

# Delta
delta = cbind(results, flightTest) %>%
  select(11, actual, predicted) %>%
  rename(Delta = "name.Delta Air Lines Inc.") %>%
  filter(Delta > 0) %>%
  mutate(delta_del = mean(predicted), index = 1:8404) %>% unique()
summary(delta)  

delta_only = delta %>%
  mutate(Airline = "Delta Air Lines Inc.") %>%
  rename(Average_delay = delta_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_da <- ggplot(data = delta, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-35, 750)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_da2 <- ggplot(data = delta, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-35, 750), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_da, plot_da2, ncol = 1)

# Endeavor
endeavor = cbind(results, flightTest) %>%
  select(12, actual, predicted) %>%
  rename(Endeavor = "name.Endeavor Air Inc.") %>%
  filter(Endeavor > 0) %>%
  mutate(endeavor_del = mean(predicted), index = 1:3126) %>% unique()
summary(endeavor)  

endeavor_only = endeavor %>%
  mutate(Airline = "Endeavor Air Inc.") %>%
  rename(Average_delay = endeavor_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_ea <- ggplot(data = endeavor, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-25, 325)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_ea2 <- ggplot(data = endeavor, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-25, 325), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_ea, plot_ea2, ncol = 1)

# Envoy
envoy = cbind(results, flightTest) %>%
  select(13, actual, predicted) %>%
  rename(Envoy = "name.Envoy Air") %>%
  filter(Envoy > 0) %>%
  mutate(envoy_del = mean(predicted), index = 1:4295) %>% unique()
summary(envoy)  

envoy_only = envoy %>%
  mutate(Airline = "Envoy Air") %>%
  rename(Average_delay = envoy_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_enva <- ggplot(data = envoy, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-25, 1150)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_enva2 <- ggplot(data = envoy, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-25, 1150), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_enva, plot_enva2, ncol = 1)

# Express
express = cbind(results, flightTest) %>%
  select(14, actual, predicted) %>%
  rename(Express = "name.ExpressJet Airlines Inc.") %>%
  filter(Express > 0) %>%
  mutate(express_del = mean(predicted), index = 1:8759) %>% unique()
summary(express)  

express_only = express %>%
  mutate(Airline = "ExpressJet Airlines Inc.") %>%
  rename(Average_delay = express_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_exa <- ggplot(data = express, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-20, 500)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_exa2 <- ggplot(data = express, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-20, 500), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_exa, plot_exa2, ncol = 1)

# Frontier
frontier = cbind(results, flightTest) %>%
  select(15, actual, predicted) %>%
  rename(Frontier = "name.Frontier Airlines Inc.") %>%
  filter(Frontier > 0) %>%
  mutate(frontier_del = mean(predicted), index = 1:121) %>% unique()
summary(frontier)  

frontier_only = frontier %>%
  mutate(Airline = "Frontier Airlines Inc.") %>%
  rename(Average_delay = frontier_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_fa <- ggplot(data = frontier, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-15, 325)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_fa2 <- ggplot(data = frontier, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-15, 325), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_fa, plot_fa2, ncol = 1)

# Hawaiian
hawaiian = cbind(results, flightTest) %>%
  select(16, actual, predicted) %>%
  rename(Hawaiian = "name.Hawaiian Airlines Inc.") %>%
  filter(Hawaiian > 0) %>%
  mutate(hawaiian_del = mean(predicted), index = 1:53) %>% unique()
summary(hawaiian)  

hawaiian_only = hawaiian %>%
  mutate(Airline = "Hawaiian Airlines Inc.") %>%
  rename(Average_delay = hawaiian_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_ha <- ggplot(data = hawaiian, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-15, 20)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_ha2 <- ggplot(data = hawaiian, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-15, 20), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_ha, plot_ha2, ncol = 1)

# JetBlue
jetblue = cbind(results, flightTest) %>%
  select(17, actual, predicted) %>%
  rename(JetBlue = "name.JetBlue Airways") %>%
  filter(JetBlue > 0) %>%
  mutate(jetblue_del = mean(predicted), index = 1:9619) %>% unique()
summary(jetblue)  

jetblue_only = jetblue %>%
  mutate(Airline = "JetBlue Airways") %>%
  rename(Average_delay = jetblue_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_jba <- ggplot(data = jetblue, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-50, 420)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_jba2 <- ggplot(data = jetblue, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-50, 420), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_jba, plot_jba2, ncol = 1)

# Mesa
mesa = cbind(results, flightTest) %>%
  select(18, actual, predicted) %>%
  rename(Mesa = "name.Mesa Airlines Inc.") %>%
  filter(Mesa > 0) %>%
  mutate(mesa_del = mean(predicted), index = 1:102) %>% unique()
summary(mesa)  

mesa_only = mesa %>%
  mutate(Airline = "Mesa Airlines Inc.") %>%
  rename(Average_delay = mesa_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_ma <- ggplot(data = mesa, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-15, 170)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_ma2 <- ggplot(data = mesa, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-15, 170), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_ma, plot_ma2, ncol = 1)

# SkyWest
skywest = cbind(results, flightTest) %>%
  select(19, actual, predicted) %>%
  rename(SkyWest = "name.SkyWest Airlines Inc.") %>%
  filter(SkyWest > 0) %>%
  mutate(skywest_del = mean(predicted), index = 1:7) %>% unique()
summary(skywest)  

skywest_only = skywest %>%
  mutate(Airline = "SkyWest Airlines Inc.") %>%
  rename(Average_delay = skywest_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_ska <- ggplot(data = skywest, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-15, 150)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_ska2 <- ggplot(data = skywest, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-15, 150), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_ska, plot_ska2, ncol = 1)

# Southwest
southwest = cbind(results, flightTest) %>%
  select(20, actual, predicted) %>%
  rename(Southwest = "name.Southwest Airlines Co.") %>%
  filter(Southwest > 0) %>%
  mutate(southwest_del = mean(predicted), index = 1:2096) %>% unique()
summary(southwest)  

southwest_only = southwest %>%
  mutate(Airline = "Southwest Airlines Co.") %>%
  rename(Average_delay = southwest_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_swa <- ggplot(data = southwest, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-15, 415)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_swa2 <- ggplot(data = southwest, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-15, 415), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_swa, plot_swa2, ncol = 1)

# US
us = cbind(results, flightTest) %>%
  select(22, actual, predicted) %>%
  rename(US = "name.US Airways Inc.") %>%
  filter(US > 0) %>%
  mutate(us_del = mean(predicted), index = 1:3430) %>% unique()
summary(us)  

us_only = us %>%
  mutate(Airline = "US Airways Inc.") %>%
  rename(Average_delay = us_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_usa <- ggplot(data = us, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-25, 500)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_usa2 <- ggplot(data = us, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-25, 500), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_usa, plot_usa2, ncol = 1)

# United
united = cbind(results, flightTest) %>%
  select(21, actual, predicted) %>%
  rename(United = "name.United Air Lines Inc.") %>%
  filter(United > 0) %>%
  mutate(united_del = mean(predicted), index = 1:9737) %>% unique()
summary(united)  

united_only = united %>%
  mutate(Airline = "United Air Lines Inc.") %>%
  rename(Average_delay = united_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_una <- ggplot(data = united, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-20, 500)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_una2 <- ggplot(data = united, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-20, 500), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_una, plot_una2, ncol = 1)

# Virgin
virgin = cbind(results, flightTest) %>%
  select(23, actual, predicted) %>%
  rename(Virgin = "name.Virgin America") %>%
  filter(Virgin > 0) %>%
  mutate(virgin_del = mean(predicted), index = 1:884) %>% unique()
summary(virgin)  

virgin_only = virgin %>%
  mutate(Airline = "Virgin America") %>%
  rename(Average_delay = virgin_del) %>%
  select(Airline, Average_delay) %>%
  unique()

plot_va <- ggplot(data = virgin, aes(x = index, y = actual)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(-20, 715)) +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

plot_va2 <- ggplot(data = virgin, aes(x = index, y = predicted)) +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(-20, 715), position = "left") +
  labs(x = 'Flight Index', y = "Predicted Delay") +
  theme_minimal()

grid.arrange(plot_va, plot_va2, ncol = 1)

pred_del = rbind(airtran_only, alaska_only, american_only, delta_only, endeavor_only, envoy_only,
                 express_only, frontier_only, hawaiian_only, jetblue_only, mesa_only,
                 skywest_only, southwest_only, us_only, united_only, virgin_only)

plot3 = ggplot(pred_del, aes(x = reorder(Airline, -Average_delay), y = Average_delay)) +
  geom_bar(stat = "identity", col = 'black', fill = 'steelblue') +
  labs(title = "Mean Delay Time Predicted for Each Carrier (In Minutes)",
       x = "Carrier",
       y = "Average Predicted Delay Time") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot3

grid.arrange(plot4, plot3, ncol = 2)

##############################################################################################################
# Question 2: What is the percent that a flight is likely to be delayed using Bayesian methodologies?
##############################################################################################################
library(rstanarm)
library(caret)

bay_flights <- rel_flights %>%
  mutate(delayed = ifelse(dep_delay > 0, 1, 0)) %>%
  select(-dep_delay)

bay_flights$name <- as.factor(bay_flights$name)

set.seed(123)
bayIndex <- createDataPartition(bay_flights$delayed, p = .8, 
                                  list = FALSE, 
                                  times = 1)
bayTrain <- bay_flights[bayIndex,]
bayTest  <- bay_flights[-bayIndex,]

fit <- stan_glm(delayed ~ ., data = bayTrain, family = binomial(link = "logit"),
                prior = normal(0, 5), prior_intercept = normal(0, 5),
                chains = 2, cores = 2, iter = 800, warmup = 400)
summary(fit)

bay_probs <- posterior_epred(fit, newdata = bayTest)
dim(bay_probs)


if (length(dim(bay_probs)) > 1) {
  bay_probs <- colMeans(bay_probs)
}

# Convert probabilities to binary outcomes (threshold = 0.5)
bay_classes <- ifelse(bay_probs > 0.5, 1, 0)
# Evaluate the model's performance
confusionMatrix(as.factor(bay_classes), as.factor(bayTest$delayed))

bayTest <- bayTest %>%
  mutate(bay_probs = bay_probs)

# Plot predicted probabilities
ggplot(bayTest, aes(x = bay_probs)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  labs(title = "Predicted Probabilities of Delays",
       x = "Predicted Probability",
       y = "Count")

pred_bay = bayTest %>%
  group_by(name) %>%
  mutate(bay_tot = n()) %>%
  filter(bay_probs > 0.5) %>%
  group_by(name) %>%
  reframe(name, bay_num = n(), bay_tot, bay_per = round(bay_num / bay_tot, 2)) %>%
  unique()


ggplot(pred_bay, aes(x = reorder(name, -bay_num), y = bay_num)) +
  geom_bar(stat = "identity", col = 'black', fill = 'steelblue') +
  labs(title = "Bayesian Estimation of Flights Delayed for Each Carrier",
       x = "Carrier",
       y = "Number of Estimated Delays") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot5 = ggplot(pred_bay, aes(x = reorder(name, -bay_per), y = bay_per)) +
  geom_bar(stat = "identity", col = 'black', fill = 'steelblue') +
  labs(title = "Bayesian Estimation of Percentage of Flights Delayed for Each Carrier",
       x = "Carrier",
       y = "Percentage of Estimated Delays") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot5

grid.arrange(plot6, plot5, ncol = 2)
