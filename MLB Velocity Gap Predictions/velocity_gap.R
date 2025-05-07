library(tidyverse)
library(car)
library(data.table)
library(caret)

# Load the data
pitch_data = read_csv("Solutions Data Project - Data.csv")
head(pitch_data)
dim(pitch_data)

# Check for missing values
colSums(is.na(pitch_data))

# We should remove columns 33:36 as too much data is missing
pitch_data = pitch_data[, 1:32]
head(pitch_data)

pitch_data = pitch_data %>%
  mutate(season = as.factor(season))
pitch_data$season = relevel(pitch_data$season, ref = "2024")

unique(pitch_data$pitch_type)

fastballs = c("FF", "SI")  # four-seam and sinker
breaking_balls = c("CB", "CT", "SL", "SW", "SV")  # curveball, cutter, slider, sweeper, slurve

# Total pitches thrown per pitcher-season
pit_tot = pitch_data %>%
  group_by(pitcher_id, pitcher, season) %>%
  summarise(total_pitches = n())

# Set a threshold at the 25th percentile
pit_tot_thres = quantile(pit_tot$total_pitches, 0.25)

# Keep only pitcher-seasons above this threshold
pit_tot = pit_tot %>%
  filter(total_pitches >= pit_tot_thres)

# Find the total number of pitches per pitch type by pitcher-season
pitch_type_sum = pitch_data %>%
  group_by(pitcher_id, pitcher, season, pitch_type) %>%
  summarise(pitch_type_sum = n()) %>%
  ungroup()

# Keep pitch types with at least 30 pitches
pitch_type_sum = pitch_type_sum %>%
  filter(pitch_type_sum >= 30)

colSums(is.na(pitch_data))

# We are going to remove the missing release_velo & extension entries since its less than 0.01% of the data
pitch_data = pitch_data %>%
  filter(!is.na(release_velo))

colSums(is.na(pitch_data))

pitch_data = pitch_data %>%
  filter(!is.na(extension))

colSums(is.na(pitch_data))

pitch_data = pitch_data %>%
  filter(!is.na(break_x))

colSums(is.na(pitch_data))

pitch_data = pitch_data %>%
  filter(!is.na(break_z))

colSums(is.na(pitch_data))

# Dimensions after cleaning missing data
dim(pitch_data)

# Remove irrelevant pitch_types
pitch_data = pitch_data %>%
  filter(pitch_type != "EP" & pitch_type != "KN" & pitch_type != "UN" & pitch_type != "AB" &
           pitch_type != "CH" & pitch_type != "SP")

# Factor pitch_type
pitch_data = pitch_data %>%
  mutate(pitch_type = as.factor(pitch_type))
  
# Copy original data
pitch_data_impute = pitch_data

# Loop through each season
for (yr in unique(pitch_data$season)) {
  
  # Subset data for the current season
  season_data = pitch_data %>% filter(season == yr)
  
  # Rows with non-missing spin_rate for model fitting
  train_data = season_data %>% filter(!is.na(spin_rate))
  
  # Rows needing imputation
  missing_data = season_data %>% filter(is.na(spin_rate))
  
  # Only proceed if there are missing rows to impute
  if (nrow(missing_data) > 0) {
    
    # Fit the regression model
    spin_model = lm(spin_rate ~ throws:break_x + pitch_type:break_z + release_velo +
                      haa, data = train_data)
    
    # Predict missing values
    predicted_spins = predict(spin_model, newdata = missing_data)
    
    # Impute into the main data frame
    pitch_data_impute$spin_rate[which(pitch_data_impute$season == yr & 
                                        is.na(pitch_data_impute$spin_rate))] = predicted_spins
  }
}
# Check for missing values, summary of the model, and multicollinearity
colSums(is.na(pitch_data_impute))
summary(spin_model)
vif(spin_model) # No extreme multicollinearity since nothing is >5

# Copy  dataset again
pitch_data_impute$spin_direction = pitch_data$spin_direction  # reset to original spin_direction

# Loop through each season
for (yr in unique(pitch_data$season)) {
  
  # Subset data for the current season
  season_data = pitch_data %>% filter(season == yr)
  
  # Rows with non-missing spin_direction for model fitting
  train_data = season_data %>% filter(!is.na(spin_direction))
  
  # Rows needing imputation
  missing_data = season_data %>% filter(is.na(spin_direction))
  
  # Only proceed if there are missing rows to impute
  if (nrow(missing_data) > 0) {
    
    # Fit the regression model
    spin_dir_model = lm(spin_direction ~ release_velo + pitch_type + break_x + break_z +
                           release_x + release_z + haa + vaa, data = train_data)
    
    # Predict missing values
    predicted_dirs = predict(spin_dir_model, newdata = missing_data)
    
    # Impute into the main data frame
    pitch_data_impute$spin_direction[which(pitch_data_impute$season == yr & 
                                             is.na(pitch_data_impute$spin_direction))] = predicted_dirs
  }
}

# Check for missing values, summary of the model, and multicollinearity
colSums(is.na(pitch_data_impute))
summary(spin_dir_model)
vif(spin_dir_model) # No extreme multicollinearity since nothing is >5

rm(pitch_data)
pitch_data = pitch_data_impute
rm(pitch_data_impute)
gc()

###########################################################################################################
head(pitch_data)

# Calculate the total number of fastballs/sinkers and the mean
fastball_avgs = pitch_data %>%
  filter(pitch_type %in% fastballs) %>%
  group_by(pitcher_id, pitcher, season) %>%
  summarise(
    fastball_count = n(),
    fastball_avg_velo = mean(release_velo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(fastball_count >= 30)  # Keep only pitchers with 30+ fastballs (sinkers too)

fastball_avgs = fastball_avgs %>%
  left_join(pit_tot, by = c("pitcher_id", "pitcher", "season"))

fastball_avgs = fastball_avgs %>%
  filter(!is.na(total_pitches))

###########################################################################################################
# Fastball Velocity Distribution
ggplot(fastball_avgs, aes(x = fastball_avg_velo)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Fastball Average Velocity",
       x = "Average Fastball Velocity (mph)", y = "Number of Pitcher-Seasons") +
  theme_minimal()

# Fastball Velocity by Season
ggplot(fastball_avgs, aes(x = factor(season), y = fastball_avg_velo)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Fastball Average Velocity by Season",
       x = "Season", y = "Average Fastball Velocity (mph)") +
  theme_minimal()

# Number of Fastballs vs Total Pitches
ggplot(fastball_avgs, aes(x = total_pitches, y = fastball_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Fastball Count vs. Total Pitches",
       x = "Total Pitches in Season", y = "Fastballs Thrown") +
  theme_minimal()

# Top 10 Fastest Average Velocity
fastball_avgs %>%
  arrange(desc(fastball_avg_velo)) %>%
  head(10)

# Top 10 Slowest Average Velocity
fastball_avgs %>%
  arrange(fastball_avg_velo) %>%
  head(10)

fastball_dt = as.data.table(fastball_avgs)

# Summary Statistics for fastballs
fastball_dt[, .(
  total_pitcher_seasons = .N,
  min_fastballs = min(fastball_count),
  median_fastballs = median(fastball_count),
  max_fastballs = max(fastball_count),
  min_total_pitches = min(total_pitches, na.rm = TRUE),
  median_total_pitches = median(total_pitches, na.rm = TRUE),
  max_total_pitches = max(total_pitches, na.rm = TRUE)
)]

# Number of pitcher-seasons by season
fastball_dt[, .(pitcher_seasons = .N), by = season][order(season)]

###########################################################################################################
breaking_avgs = pitch_data %>%
  filter(pitch_type %in% breaking_balls) %>%
  group_by(pitcher_id, pitcher, season, pitch_type) %>%
  summarise(
    breaking_count = n(),
    breaking_avg_velo = mean(release_velo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(breaking_count >= 30)

breaking_avgs = breaking_avgs %>%
  left_join(pit_tot, by = c("pitcher_id", "pitcher", "season"))

breaking_avgs = breaking_avgs %>%
  filter(!is.na(total_pitches))

# Number of Breaking Balls vs Total Pitches
ggplot(breaking_avgs, aes(x = total_pitches, y = breaking_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Breaking Balls Count vs. Total Pitches",
       x = "Total Pitches in Season", y = "Breaking Balls Thrown") +
  theme_minimal()

breaking_avgs = breaking_avgs %>%
  select(1:6)

velocity_gaps = breaking_avgs %>%
  left_join(fastball_avgs, by = c("pitcher_id", "pitcher", "season")) %>%
  mutate(velocity_gap = breaking_avg_velo - fastball_avg_velo)

velocity_gaps = velocity_gaps %>%
  filter(!is.na(fastball_avg_velo))

ggplot(velocity_gaps, aes(x = fastball_avg_velo, y = velocity_gap, color = pitch_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal()

###########################################################################################################
predictors = pitch_data %>%
  filter(pitch_type %in% c(fastballs, breaking_balls)) %>%
  group_by(pitcher_id, pitcher, season, pitch_type) %>%
  reframe(
    spin_rate_25 = quantile(spin_rate, 0.25, na.rm = TRUE),
    mean_spin_rate = mean(spin_rate),
    spin_rate_75 = quantile(spin_rate, 0.75, na.rm = TRUE),
    spin_direction_25 = quantile(spin_direction, 0.25, na.rm = TRUE),
    mean_spin_direction = mean(spin_direction),
    spin_direction_75 = quantile(spin_direction, 0.75, na.rm = TRUE),
    break_x_25 = quantile(break_x, 0.25, na.rm = TRUE),
    mean_break_x = mean(break_x),
    break_x_75 = quantile(break_x, 0.75, na.rm = TRUE),
    break_z_25 = quantile(break_z, 0.25, na.rm = TRUE),
    mean_break_z = mean(break_z),
    break_z_75 = quantile(break_z, 0.75, na.rm = TRUE),
    induced_break_z_25 = quantile(induced_break_z, 0.25, na.rm = TRUE),
    mean_induced_break_z = mean(induced_break_z),
    induced_break_z_75 = quantile(induced_break_z, 0.75, na.rm = TRUE),
    location_x_25 = quantile(location_x, 0.25, na.rm = TRUE),
    mean_location_x = mean(location_x),
    location_x_75 = quantile(location_x, 0.75, na.rm = TRUE),
    location_z_25 = quantile(location_z, 0.25, na.rm = TRUE),
    mean_location_z = mean(location_z),
    location_z_75 = quantile(location_z, 0.75, na.rm = TRUE),
    release_x_25 = quantile(release_x, 0.25, na.rm = TRUE),
    mean_release_x = mean(release_x),
    release_x_75 = quantile(release_x, 0.75, na.rm = TRUE),
    release_z_25 = quantile(release_z, 0.25, na.rm = TRUE),
    mean_release_z = mean(release_z),
    release_z_75 = quantile(release_z, 0.75, na.rm = TRUE),
    extension_25 = quantile(extension, 0.25, na.rm = TRUE),
    mean_extension = mean(extension),
    extension_75 = quantile(extension, 0.75, na.rm = TRUE),
    haa_25 = quantile(haa, 0.25, na.rm = TRUE),
    mean_haa = mean(haa),
    haa_75 = quantile(haa, 0.75, na.rm = TRUE),
    vaa_25 = quantile(vaa, 0.25, na.rm = TRUE),
    mean_vaa = mean(vaa),
    vaa_75 = quantile(vaa, 0.75, na.rm = TRUE),
    hra_25 = quantile(hra, 0.25, na.rm = TRUE),
    mean_hra = mean(hra),
    hra_75 = quantile(hra, 0.75, na.rm = TRUE),
    vra_25 = quantile(vra, 0.25, na.rm = TRUE),
    mean_vra = mean(vra),
    vra_75 = quantile(vra, 0.75, na.rm = TRUE),
    spin_rate_range = spin_rate_75 - spin_rate_25,
    spin_direction_range = spin_direction_75 - spin_direction_25,
    break_x_range = break_x_75 - break_x_25,
    break_z_range = break_z_75 - break_z_25,
    induced_break_z_range = induced_break_z_75 - induced_break_z_25,
    location_x_range = location_x_75 - location_x_25,
    location_z_range = location_z_75 - location_z_25,
    release_x_range = release_x_75 - release_x_25,
    release_z_range = release_z_75 - release_z_25,
    extension_range = extension_75 - extension_25,
    haa_range = haa_75 - haa_25,
    vaa_range = vaa_75 - vaa_25,
    hra_range = hra_75 - hra_25,
    vra_range = vra_75 - vra_25
  )

throws = pitch_data %>%
  select(pitcher_id, pitcher, season, throws) %>%
  unique()

rel_stat = pitch_data %>%
  select(pitcher, season, inning) %>%
  group_by(pitcher, season) %>%
  summarise(
    reliever = ifelse(any(inning <= 5), 0, 1),
    .groups = "drop"
  )

rel_stat$season = relevel(rel_stat$season, ref = "2024")

predictors = predictors %>%
  left_join(throws, by = c("pitcher_id", "pitcher", "season")) %>%
  left_join(rel_stat, by = c("pitcher", "season"))

velocity_gaps_full = velocity_gaps %>%
  left_join(predictors, by = c("pitcher_id", "pitcher", "season", "pitch_type"))

gap_mod = lm(velocity_gap ~ mean_break_z + pitch_type:mean_induced_break_z
             + mean_release_z + fastball_avg_velo + mean_induced_break_z:mean_break_z, 
          data = velocity_gaps_full)
summary(gap_mod)
vif(gap_mod)
AIC(gap_mod)

# Plot the relationship between mean break z and mean induced break z
ggplot(velocity_gaps_full, aes(x = mean_break_z, y = mean_induced_break_z)) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "salmon") +
  labs(
    title = "Relationship between Mean Break Z and Mean Induced Break Z",
    x = "Mean Break Z",
    y = "Mean Induced Break Z"
  ) +
  theme_minimal()

# Predict over the entire dataset
velocity_gaps_full$predicted_gap = predict(gap_mod)

# Calculate residuals
residuals = velocity_gaps_full$velocity_gap - velocity_gaps_full$predicted_gap

# RMSE
rmse = sqrt(mean(residuals^2))
rmse

# MAE
mae = mean(abs(residuals))
mae

par(mfrow = c(2,2))
plot(gap_mod)

ggplot(velocity_gaps_full, aes(x = predicted_gap, y = velocity_gap)) +
  geom_point(alpha = 1.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "salmon") +
  theme_minimal() +
  labs(title = "Predicted vs. Actual Velocity Gap",
       x = "Predicted Gap (mph)", y = "Actual Gap (mph)") + coord_equal()

###########################################################################################################
set.seed(88)
cv_results = train(
  velocity_gap ~ mean_break_z + pitch_type:mean_induced_break_z
  + mean_release_z + fastball_avg_velo + mean_induced_break_z:mean_break_z,
  data = velocity_gaps_full,
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)

cv_results

###########################################################################################################
set.seed(88)

# Create training (80%) and testing (20%) split
train_index = createDataPartition(velocity_gaps_full$velocity_gap, p = 0.8, list = FALSE)

train_data = velocity_gaps_full[train_index, ]
test_data = velocity_gaps_full[-train_index, ]

# Fit the model on training data
gap_mod_train = lm(velocity_gap ~ mean_break_z + pitch_type:mean_induced_break_z
                   + mean_release_z + fastball_avg_velo + mean_induced_break_z:mean_break_z, 
                   data = train_data)
summary(gap_mod_train)

# Predict on test data
test_data$predicted_gap = predict(gap_mod_train, newdata = test_data)

# Calculate residuals
test_residuals = test_data$velocity_gap - test_data$predicted_gap
c(min(test_residuals), max(test_residuals))

# RMSE and MAE on test set
rmse_test = sqrt(mean(test_residuals^2))
mae_test = mean(abs(test_residuals))

rmse_test
mae_test

ggplot(test_data, aes(x = predicted_gap, y = velocity_gap)) +
  geom_point(alpha = 1.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "salmon", linetype = "solid", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Predicted vs. Actual Velocity Gap (Test Set)",
    x = "Predicted Gap (mph)",
    y = "Actual Gap (mph)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_equal()

# Compare all RMSE values
c(rmse, rmse_test, mean(cv_results$resample$RMSE))

# Compare all MAE values
c(mae, mae_test, mean(cv_results$resample$MAE))

vif(gap_mod)
vif(gap_mod_train)
############################################################################################################
# Analyze the greatest errors
gap_results = test_data %>%
  select(pitcher, season, pitch_type, breaking_count, velocity_gap, predicted_gap, pitch_type, 
         fastball_avg_velo, mean_break_z, mean_induced_break_z, mean_location_z, mean_release_z,
         spin_direction_range, mean_break_x) %>% 
  mutate(
    residual = velocity_gap - predicted_gap,
    abs_residual = abs(residual)
  )

# Find the top 50 largest absolute residuals
top_errors = gap_results %>%
  arrange(desc(abs_residual)) %>%
  head(50)
top_errors

curve_avg_velo = pitch_data %>%
  filter(pitch_type == "CB") %>%
  group_by(pitcher_id, pitcher, season) %>%
  summarize(curve_avg_velo = mean(release_velo))

velocity_gaps_full = velocity_gaps_full %>%
  left_join(curve_avg_velo, by = c("pitcher_id", "season"))

velo_long = velocity_gaps_full %>%
  select(fastball_avg_velo, curve_avg_velo) %>%
  pivot_longer(cols = c(fastball_avg_velo, curve_avg_velo),
               names_to = "Pitch_Type",
               values_to = "Velocity")
velo_long = na.omit(velo_long)

ggplot(velo_long, aes(x = Velocity, fill = Pitch_Type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Fastball and Curveball Velocities",
    x = "Velocity (mph)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

velocity_gaps_full = velocity_gaps_full %>%
  rename(pitcher = pitcher.x) %>%
  select(1:69, 71)

velocity_gaps_full = velocity_gaps_full %>% 
  mutate(residual = velocity_gap - predicted_gap,
    abs_residual = abs(residual))
############################################################################################################
library(shiny)
library(DT)

df = velocity_gaps_full

df = df %>%
  select(pitcher, season, pitch_type, breaking_count, fastball_count, total_pitches, velocity_gap, 
         predicted_gap, pitch_type, fastball_avg_velo, breaking_avg_velo, mean_break_x,
         mean_break_z, mean_induced_break_z, mean_location_z, 
         mean_release_z, spin_direction_range, residual, abs_residual) %>%
  mutate(season = as.integer(as.character(season)),
         velocity_gap = round(velocity_gap, 3),
         predicted_gap = round(predicted_gap, 3),
         fastball_avg_velo = round(fastball_avg_velo, 2),
         breaking_avg_velo = round(breaking_avg_velo, 2),
         mean_location_z = round(mean_location_z, 3),
         mean_break_z = round(mean_break_z, 3),
         mean_induced_break_z = round(mean_induced_break_z, 3),
         mean_release_z = round(mean_release_z, 3),
         spin_direction_range = round(spin_direction_range, 3),
         mean_break_x = round(mean_break_x, 3),
         residual = round(residual, 3),
         abs_residual = round(abs_residual, 3),
         fastball_use = round(fastball_count / total_pitches, 2),
         breaking_use = round(breaking_count / total_pitches, 2)) %>%
  rename(
    Pitcher = pitcher,
    Season = season,
    `Pitch Type` = pitch_type,
    `Breaking Ball Count` = breaking_count,
    `Fastball Count` = fastball_count,
    `Total Pitches` = total_pitches,
    `Velocity Gap` = velocity_gap,
    `Predicted Gap` = predicted_gap,
    `Average Fastball Velocity` = fastball_avg_velo,
    `Average Breaking Ball Velocity` = breaking_avg_velo,
    `Mean Break X` = mean_break_x,
    `Mean Break Z` = mean_break_z,
    `Mean Induced Break Z` = mean_induced_break_z,
    `Mean Location Z` = mean_location_z,
    `Mean Release Z` = mean_release_z,
    `Spin Direction Range` = spin_direction_range,
    Residual = residual,
    `Absolute Residual` = abs_residual,
    `Fastball Usage` = fastball_use,
    `Breaking Ball Usage` = breaking_use
  )

ui = fluidPage(
  titlePanel("Overall Velocity Gap Residuals"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Pitcher", "Select Pitcher:",
                  choices = c("All", sort(unique(df$Pitcher))),
                  selected = "All"),
      selectInput("Season", "Select Season:",
                  choices = c("All", sort(unique(df$Season))),
                  selected = "All"),
      checkboxGroupInput("pitch_types", "Select Pitch Types:",
                         choices = sort(unique(df$`Pitch Type`)),
                         selected = unique(df$`Pitch Type`))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("errorTable")),
        tabPanel("Prediction vs Actual", plotOutput("predVsActualPlot"))
      )
    )
  )
)

server = function(input, output) {
  
  filteredData = reactive({
    data = df
    if (input$Pitcher != "All") {
      data = data %>% filter(Pitcher == input$Pitcher)
    }
    if (input$Season != "All") {
      data = data %>% filter(Season == as.numeric(input$Season))
    }
    data = data %>% filter(`Pitch Type` %in% input$pitch_types)
    return(data)
  })
  
  output$errorTable = renderDT({
    datatable(filteredData(),
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  output$predVsActualPlot = renderPlot({
    data = filteredData()
    if (nrow(data) == 0) return(NULL)
    ggplot(data, aes(x = `Predicted Gap`, y = `Velocity Gap`, color = `Pitch Type`)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Predicted vs Actual Velocity Gap",
           x = "Predicted Gap (mph)",
           y = "Actual Gap (mph)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

# Analyze some residuals
summary(df$`Absolute Residual`)

quantile(df$`Absolute Residual`, 0.99)

hist(df$`Absolute Residual`, 
     breaks = 25, 
     main = "Histogram of Absolute Residuals", 
     xlab = "Absolute Residuals", 
     col = "steelblue", 
     border = "white")
###########################################################################################################
library(randomForest)

set.seed(88)

# Remove any non-numeric or ID columns that shouldn't be used as predictors
rf_train = train_data %>% 
  select(-pitcher, -season, -pitch_type, -velocity_gap, -predicted_gap) 

rf_test = test_data %>% 
  select(-pitcher, -season, -pitch_type, -velocity_gap, -predicted_gap)

# Make sure categorical variables are factors
rf_train$throws = as.factor(rf_train$throws)
rf_test$throws = as.factor(rf_test$throws)

rf_train$reliever = as.factor(rf_train$reliever)
rf_test$reliever = as.factor(rf_test$reliever)

# Response variable
y_train = train_data$velocity_gap
y_test = test_data$velocity_gap

# Fit Random Forest
set.seed(88)

rf_model = randomForest(x = rf_train, y = y_train,
                        ntree = 500,
                        mtry = 8,
                        nodesize = 5,
                        importance = TRUE
)

# Predictions
rf_pred_train = predict(rf_model, newdata = rf_train)
rf_pred_test = predict(rf_model, newdata = rf_test)

# RMSE and MAE on Train
rf_rmse_train = sqrt(mean((y_train - rf_pred_train)^2))
rf_mae_train = mean(abs(y_train - rf_pred_train))

# RMSE and MAE on Test
rf_rmse_test = sqrt(mean((y_test - rf_pred_test)^2))
rf_mae_test = mean(abs(y_test - rf_pred_test))

# RMSE
c(rf_rmse_train, rf_rmse_test)

# MAE
c(rf_mae_train, rf_mae_test)

# Variable Importance Plot
varImpPlot(rf_model)
###########################################################################################################
library(gbm)

set.seed(88)

# Drop unnecessary categorical variables
train_data_nocat = train_data %>%
  select(-pitcher, -season, -pitch_type, -predicted_gap, -breaking_avg_velo)

test_data_nocat = test_data %>%
  select(-pitcher, -season, -pitch_type, -predicted_gap, -breaking_avg_velo)

# GBM model with 5-fold cross-validation
gbm_cv_model = train(
  velocity_gap ~ .,
  data = train_data_nocat,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    n.trees = c(500),
    interaction.depth = c(7),
    shrinkage = c(0.05),
    n.minobsinnode = c(5)
  ),
  verbose = FALSE
)

# Best parameters
print(gbm_cv_model$bestTune)

# Predictions
train_preds_gbm = predict(gbm_cv_model, newdata = train_data_nocat)
test_preds_gbm = predict(gbm_cv_model, newdata = test_data_nocat)

# Calculate RMSE and MAE
gbm_rmse_train = sqrt(mean((train_preds_gbm - train_data$velocity_gap)^2))
gbm_mae_train = mean(abs(train_preds_gbm - train_data$velocity_gap))

gbm_rmse_test = sqrt(mean((test_preds_gbm - test_data$velocity_gap)^2))
gbm_mae_test = mean(abs(test_preds_gbm - test_data$velocity_gap))

# RMSE
c(gbm_rmse_train, gbm_rmse_test)

#MAE
c(gbm_mae_train, gbm_mae_test)

# Variable Importance
varImp(gbm_cv_model)
