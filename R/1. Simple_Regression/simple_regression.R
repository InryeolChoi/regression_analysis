# 데이터 로드 및 전처리
data <- read.table("../dataset/P031.txt", header = TRUE)
names(data) <- c("Minutes", "Units")

# 데이터 확인
print(head(data))

# 시각화
library(ggplot2)
ggplot(data, aes(x = Minutes, y = Units)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Scatter plot with regression line") +
  xlab("Minutes") +
  ylab("Units")

# 단순회귀분석
model <- lm(Units ~ Minutes, data = data)
summary(model)

# 회귀계수 추출
beta_0 <- coef(model)[1]
beta_1 <- coef(model)[2]

# 예측값 계산
predictions <- predict(model, newdata = data)

# SSE, SSR, SST 계산
SSE <- sum((data$Units - predictions) ^ 2)
SSR <- sum((predictions - mean(data$Units)) ^ 2)
SST <- SSE + SSR

# R-squared 확인
R_squared <- summary(model)$r.squared

# 결과 출력
cat("beta_0 (Intercept): ", beta_0, "\n")
cat("beta_1 (Slope): ", beta_1, "\n")
cat("SSE: ", SSE, "\n")
cat("SSR: ", SSR, "\n")
cat("SST: ", SST, "\n")
cat("R-squared: ", R_squared, "\n")

# 가설 검정 (t-test)
t_test <- summary(model)$coefficients
cat("t-value for beta_1: ", t_test[2, "t value"], "\n")
cat("p-value for beta_1: ", t_test[2, "Pr(>|t|)"], "\n")

# 잔차와 잔차제곱합 시각화
residuals <- data$Units - predictions

# Residuals plot
ggplot(data, aes(x = Minutes, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  ggtitle("Residuals plot") +
  xlab("Minutes") +
  ylab("Residuals")

# Residuals squared plot
ggplot(data, aes(x = Minutes, y = residuals^2)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Squared residuals plot") +
  xlab("Minutes") +
  ylab("Squared Residuals")