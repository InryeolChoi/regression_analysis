rm(list=ls())

graph_new = function() {
  os = Sys.info()["sysname"]
  if (os == 'Windows') {
    windows()
  } else if (os == 'Darwin') {
    quartz()
  } else if (os == 'Linux') {
    x11()
  }
}

# 필요한 패키지 설치 ----
install.packages(c("car", "leaps", "MASS", "ggplot2", "caret"))
library(car)
library(leaps)
library(MASS)
library(ggplot2)
library(caret)

# 데이터 불러오기: mtcars 데이터셋 사용
data(mtcars)

# 데이터 확인
head(mtcars)

# 일부 변수를 선택하여 회귀 모형에 사용
# 예시로: mpg(연비)을 종속 변수로, 나머지를 독립 변수로 설정
mtcars_data = mtcars[, c("mpg", "cyl", "disp", "hp", "wt")]

# 1. 기본 회귀모형 적합
model_basic = lm(mpg ~ ., data = mtcars_data)
summary(model_basic)

# 2. Mallows' Cp를 이용한 최적 모형 선택
model_cp = regsubsets(mpg ~ ., data = mtcars_data, nbest = 1, method = "exhaustive")
summary_cp = summary(model_cp)

# Cp, BIC, Adjusted R^2 시각화
plot(summary_cp$cp, type = "l", col = "blue", ylab = "Mallows' Cp", xlab = "Number of Predictors")
abline(h = min(summary_cp$cp) + 4, col = "red", lty = 2)

# 3. Stepwise 방법 (AIC 기준)
model_step_aic = stepAIC(model_basic, direction = "both", trace = FALSE)
summary(model_step_aic)

# 4. Stepwise 방법 (BIC 기준)
model_step_bic = stepAIC(model_basic, direction = "both", trace = FALSE, k = log(nrow(mtcars_data)))
summary(model_step_bic)

# 5. 교차검증 (Cross-Validation) 사용
train_control = trainControl(method = "cv", number = 10)
model_cv = train(mpg ~ ., data = mtcars_data, method = "lm", trControl = train_control)
print(model_cv)

# 6. 결과 비교 시각화
# 예시: R^2, Adj. R^2 비교
model_list = list(Basic = model_basic, AIC_Stepwise = model_step_aic, BIC_Stepwise = model_step_bic)

rsq_vals = sapply(model_list, function(mod) summary(mod)$r.squared)
adj_rsq_vals = sapply(model_list, function(mod) summary(mod)$adj.r.squared)

rsq_df = data.frame(Model = names(rsq_vals), R_Squared = rsq_vals, Adj_R_Squared = adj_rsq_vals)

ggplot(rsq_df, aes(x = Model)) +
  geom_bar(aes(y = R_Squared, fill = "R^2"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Adj_R_Squared, fill = "Adj. R^2"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("R^2" = "blue", "Adj. R^2" = "red")) +
  theme_minimal() +
  ylab("R-Squared / Adjusted R-Squared") +
  ggtitle("Model Comparison: R^2 and Adjusted R^2")

# 7. 최종 모형 선택
final_model = model_step_bic
summary(final_model)
