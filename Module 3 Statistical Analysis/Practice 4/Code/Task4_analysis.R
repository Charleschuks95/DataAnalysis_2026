#--------------------start-------------------------------
# Get current working directory
getwd()

#----------------read dataset--------------------------
install.packages("wPerm")
library(wPerm)

data <- read.csv("C:\\Users\\Charl\\Documents\\Data Analysis\\Practice 3\\data_for_analysis.csv")

data <- data[!is.na(data$outcome), ]

data$outcome <- as.factor(data$outcome)
summary(data)

# testing for normality of distribution
shapiro.test(data$lipids1)
shapiro.test(data$lipids2)

hist(data$lipids1)  
qqnorm(data$lipids1)

# Spearman's correlation test
spearman_result <- cor.test(data$lipids1, data$lipids2, method="spearman")
print(spearman_result)

# data.frame for result
results <- data.frame(
  variable = character(),
  spearman_corr = numeric(),
  s_p_value = numeric(),
  stringsAsFactors = FALSE
)

# variables for analysis
target_vars <- c("lipids2", "lipids3", "lipids4")

# main permutation loop
for (var in target_vars) {
  perm_spearman <- perm.relation(
    x = data$lipids1, 
    y = data[[var]],
    method = "spearman",
    R = 10000
  )
  
  results <- rbind(results, data.frame(
    variable = var,
    spearman_corr = perm_spearman$Observed,
    s_p_value =  perm_spearman$p.value
  ))
}

# output result
print(results)

#------visualization of significant results of correlation analysis---------
data <- data[order(data$lipids2),]
plot(data$lipids2, data$lipids1)
lines(data$lipids2, data$lipids1, col = "blue")
abline(lm(data$lipids1 ~ data$lipids2), col="red")

#_____________regression analysis________________ 
df <- data
df <- df[order(df$lipids1),]

#linear regression
model_linear <- lm(lipids1 ~ lipids2, data=df)
summary(model_linear)

#second degree polynomial
model_2 <- lm(lipids1 ~ poly(lipids2, 2), data=df)
summary(model_2)

#third degree polynomial
model_3 <- lm(lipids1 ~ poly(lipids2, 3), data=df)
summary(model_3)

#exponential dependence
model_exp <- lm(log(lipids1) ~ lipids2, data=df)
summary(model_exp)

# log dependence (as in teacher's code)
model_log <- lm(exp(lipids1) ~ lipids2, data=df)
summary(model_log)

#comparison of models
rezult <- data.frame(model=c("model_linear", "model_2", "model_3", "model_exp", "model_log"), 
                     BIC_value=c(BIC(model_linear), BIC(model_2), BIC(model_3), BIC(model_exp), BIC(model_log)))
rezult <- rezult[order(rezult$BIC_value),]
rezult

# __________building graphs______________
plot(df$lipids2, df$lipids1)
lines(df$lipids2, fitted(model_linear), col="blue")

# Logistic regression
# Dependent variable: outcome 
sum(is.na(data$outcome))  # now should be 0
data <- data[!is.na(data$outcome), ]  # (already done, but safe)

# Simple model with one predictor
model_logit_1 <- glm(outcome ~ lipids1, data = data, family = binomial)
summary(model_logit_1)

# Multi-predictor model
model_logit_2 <- glm(outcome ~ lipids1 + lipids2, data = data, family = binomial)
summary(model_logit_2)

# Model with all variables lipids 
model_logit_all <- glm(outcome ~ lipids1 + lipids2 + lipids3 + lipids4, 
                       data = data, family = binomial)
summary(model_logit_all)

# Predicting probabilities for new data (example)
data$pred_prob <- predict(model_logit_2, type = "response")

# Classification by threshold 0.5
data$pred_class <- ifelse(data$pred_prob > 0.5, 1, 0)

# confusion matrix
table(Actual = data$outcome, Predicted = data$pred_class)

# Model Quality Assessment: ROC Curve and AUC 
if (!require(pROC)) install.packages("pROC")
library(pROC)
roc_curve <- roc(data$outcome, data$pred_prob)
plot(roc_curve, main = "ROC-Curve")
auc(roc_curve)

# Stepwise variable selection (AIC)
step_model <- step(model_logit_all, direction = "both")
summary(step_model)

# Coefficient interpretation 
exp(cbind(OR = coef(model_logit_2), confint(model_logit_2)))