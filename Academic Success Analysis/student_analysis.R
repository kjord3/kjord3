# How do factors like parental education, lunch type, or test prep affect student scores?
# Identify which factors have the strongest influence on G3 (final grade)
# Compare performance across different demographic groups



# Load necessary libraries
library(tidyverse)
library(caret)
library(GGally)
library(car)
library(stats)

# Load in the data
student_performance = read.table("student-por.csv",sep=";",header=TRUE)

# Find the structure and dims of the student performance df
dim(student_performance) # 1000 rows and 8 columns
str(student_performance)

student_performance_copy = student_performance

student_performance = student_performance %>%
  filter(age < 18)

library(janitor)
# Clean names for convenience
student_performance <- student_performance %>% clean_names()

# Quick overview
glimpse(student_performance)
summary(student_performance)


# Visualize average grades by parental education
student_performance %>%
  pivot_longer(cols = c(medu, fedu), names_to = "parent", values_to = "education_level") %>%
  ggplot(aes(x = factor(education_level), y = g3, fill = parent)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Student Grades by Parental Education Level",
    x = "Parental Education Level (1 = lowest, 4 = highest)",
    y = "Final Grade"
  )

# Correlation between parental education and final grade
cor.test(student_performance$medu, student_performance$g3)
cor.test(student_performance$fedu, student_performance$g3)

# Group summaries
student_performance %>%
  group_by(medu) %>%
  summarise(mean_g3 = mean(g3), sd_g3 = sd(g3), count = n())

# Relationship between studytime and final grade
student_performance %>%
  ggplot(aes(x = factor(studytime), y = g3, fill = factor(studytime))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Final Grade by Weekly Study Time",
       x = "Study Time (1 = <2h, 2 = 2–5h, 3 = 5–10h, 4 = >10h)",
       y = "Final Grade")

# Correlation between absences and final grade
cor.test(student_performance$absences, student_performance$g3)

# Scatter plot for absences vs final grade
ggplot(student_performance, aes(x = absences, y = g3)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "Relationship Between Absences and Final Grade",
       x = "Number of Absences", y = "Final Grade")

# Compare grades based on school and family support
student_performance %>%
  pivot_longer(cols = c(schoolsup, famsup), names_to = "support_type", values_to = "support") %>%
  ggplot(aes(x = support, y = g3, fill = support)) +
  geom_boxplot() +
  facet_wrap(~support_type) +
  theme_minimal() +
  labs(title = "Effect of School and Family Support on Final Grade",
       x = "Support Received", y = "Final Grade")

# Mean comparison
student_performance %>%
  group_by(schoolsup, famsup) %>%
  summarise(mean_g3 = mean(g3))


library(ggpubr)
ggboxplot(student_performance, x = "schoolsup", y = "g3",
          color = "schoolsup", palette = "jco",
          title = "Impact of School Support on Final Grade (G3)",
          ylab = "Final Grade (G3)", xlab = "Extra School Support")

# Compare family support
ggboxplot(student_performance, x = "famsup", y = "g3",
          color = "famsup", palette = "jco",
          title = "Family Support vs Final Grades",
          ylab = "Final Grade (G3)", xlab = "Family Support")


# Correlation matrix for lifestyle factors vs final grade
student_performance %>%
  select(freetime, goout, dalc, walc, g3) %>%
  ggpairs(title = "Lifestyle and Academic Performance")

# Boxplot for weekend alcohol consumption
student_performance %>%
  ggplot(aes(x = factor(walc), y = g3, fill = factor(walc))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Weekend Alcohol Use vs Final Grade",
       x = "Weekend Alcohol Consumption (1 = low, 5 = high)",
       y = "Final Grade")

# Correlation
cor.test(student_performance$walc, student_performance$g3)
cor.test(student_performance$goout, student_performance$g3)

# Scatter plot with regression line
ggplot(student_performance, aes(x = health, y = g3)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Health vs Final Grade", x = "Self-Reported Health (1–5)", y = "Final Grade")

# Correlation test
cor.test(student_performance$health, student_performance$g3)

# Gender comparison
ggplot(student_performance, aes(x = sex, y = g3, fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Final Grade by Gender", x = "Gender", y = "Final Grade")

# Urban vs Rural
ggplot(student_performance, aes(x = address, y = g3, fill = address)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Urban vs Rural Student Performance", x = "Address Type", y = "Final Grade")

# Summary stats
student_performance %>%
  group_by(sex, address) %>%
  summarise(mean_g3 = mean(g3))

student_performance %>%
  group_by(address) %>%
  summarise(mean_g3 = mean(g3))

# Correlation matrix for numeric predictors
student_performance %>%
  select(medu, fedu, studytime, absences, freetime, goout, dalc, walc, health, g3) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

# Visual correlation heatmap
library(corrplot)
corrplot(cor(student_performance %>% 
               select(medu, fedu, studytime, absences, freetime, goout, dalc, walc, health, g3)),
         method = "color", addCoef.col = "black", tl.col = "black", number.cex = 0.6)
