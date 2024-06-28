library(tidyverse)
library(Lahman)
library(caret)
library(pROC)
library(outliers)
library(faraway)
library(lmtest)

names = People %>%
  mutate(name = paste(nameFirst, nameLast)) %>%
  select(playerID, name)

hof = HallOfFame %>%
  filter(inducted == 'Y' & category == 'Player') %>%
  select(playerID, inducted)

position = Fielding %>%
  group_by(playerID, POS) %>%
  mutate(G = sum(G)) %>%
  group_by(playerID) %>%
  slice(which.max(G)) %>%
  ungroup() %>%
  select(playerID, POS)

bat = Batting %>%
  left_join(hof) %>%
  left_join(names) %>%
  left_join(position) %>%
  group_by(playerID) %>%
  replace_na(list(HBP = 0, SF = 0, SB = 0, CS = 0, BB = 0, SO = 0, IBB = 0, SH = 0,
                  GIDP = 0, RBI = 0, inducted = 'N', G = 0, AB = 0, R = 0, H = 0,
                  X2B = 0, X3B = 0, HR = 0)) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), 
         X3B = sum(X3B), HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS),
         BB = sum(BB), SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH),
         SF = sum(SF), GIDP = sum(GIDP), 
         OBP = round((H + BB + HBP) / (AB + BB + HBP + SF), 3), 
         SLG = round(((H-X2B-X3B-HR) + 2*X2B + 3*X3B + 4*HR) / AB, 3),
         K_per = round(SO / AB, 2), BB_per = round(BB / AB, 2), OPS = OBP + SLG) %>%
  replace_na(list(OBP = 0, K_per = 0, BB_per = 0, SLG = 0, OPS = 0)) %>%
  filter(AB > 0 & POS != 'P') %>%
  select(playerID, name, POS, 6:22, 26:30, inducted) %>%
  unique()
bat = na.omit(bat)

bat$inducted = as.factor(bat$inducted)
bat$POS = as.factor(bat$POS)

model = glm(inducted ~ .-playerID-name, bat, family = binomial)
summary(model)
AIC(model)

step = step(model, direction = 'both')
summary(step)
AIC(step)

anova(model, step)
n = nrow(bat)
p = length(coef(model)) - 1

lev_mod = influence(model)$hat
lev_mod[lev_mod > 2*p/n]
plot(lev_mod)
abline(h = 2*p/n, col = 'red')
halfnorm(lev_mod, 2, labs=row.names(bat), ylab = "leverage")

stu.bat <- rstudent(model)
plot(stu.bat)
abline(h = qt(0.05/(2*n),n-1-p), col = 'red')
abline(h = qt(1-0.05/(2*n),n-1-p), col = 'red')
qt(0.05/(2*n),n-1-p)

sort(abs(stu.bat), decreasing = TRUE)[1:5]

cookbat <- cooks.distance(model)
halfnorm(cookbat, 4, ylab = "Cook's distances")
par(mfrow = c(2,2))
plot(model)

bptest(model)

qqnorm(residuals(model),ylab="Residuals")
qqline(residuals(model))
shapiro.test(residuals(model))


predicted_probabilities1 = predict(model, type = 'response')
predicted_probabilities2 = predict(step, type = 'response')

predicted_classes1 = ifelse(predicted_probabilities1 > 0.5, 1, 0)
yes1 = table(predicted_classes1)
yes1
predicted_classes2 = ifelse(predicted_probabilities2 > 0.5, 1, 0)
yes2 = table(predicted_classes2)
yes2
actual = hof %>%
  select(inducted)
actual = table(actual)
actual

confusion_matrix1 = table(Predicted = predicted_classes1, Actual = bat$inducted)
confusion_matrix2 = table(Predicted = predicted_classes2, Actual = bat$inducted)
print(confusion_matrix1)
print(confusion_matrix2)

accuracy1 = sum(diag(confusion_matrix1)) / sum(confusion_matrix1)
accuracy2 = sum(diag(confusion_matrix2)) / sum(confusion_matrix2)
print(paste('Accuracy:', round(accuracy1, 3)))
print(paste('Accuracy:', round(accuracy2, 3)))

set.seed(123)
cv_results = train(inducted ~ . - playerID - name, data = bat, method = 'glm', family = 'binomial', trControl = trainControl(method = 'cv', number = 10))
print(cv_results)
print(paste('Cross-validated Accuracy:', round(cv_results$results$Accuracy, 3)))

# ROC Curve
roc_curve = roc(bat$inducted, predicted_probabilities2)
plot(roc_curve)
print(paste('AUC:', auc(roc_curve)))

# Predict induction for a specific player
player = bat %>%
  filter(name == 'Barry Bonds')
new_predictions1 = predict(model, player, type = 'response')
new_predictions2 = predict(step, player, type = 'response')

print(paste('Full Model Prediction:', round(new_predictions1, 3)))
print(paste('Reduced Model Prediction:', round(new_predictions2, 3)))
