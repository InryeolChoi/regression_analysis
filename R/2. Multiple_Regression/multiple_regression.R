rm(list=ls())

# 데이터 로드 및 전처리
data <- read.table("../dataset/P060.txt", header = TRUE)
names(data) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6")

# 데이터 확인
print(head(data))

# 시각화: 각 독립변수와 종속변수 간의 관계를 산점도로 시각화
library(ggplot2)
library(gridExtra)

plots <- list()
for (i in 2:ncol(data)) {
  p <- ggplot(data, aes_string(x = names(data)[i], y = "Y")) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    ggtitle(paste("Scatter plot of", names(data)[i], "and Y")) +
    xlab(names(data)[i]) +
    ylab("Y")
  plots[[i-1]] <- p
}

do.call(grid.arrange, c(plots, ncol = 2))

# 다중회귀분석
model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = data)
summary(model)

# 회귀계수 추출
coefficients <- coef(model)

# 예측값 계산
predictions <- predict(model, newdata = data)

# SSE, SSR, SST 계산
SSE <- sum((data$Y - predictions) ^ 2)
SSR <- sum((predictions - mean(data$Y)) ^ 2)
SST <- SSE + SSR

# R-squared 확인
R_squared <- summary(model)$r.squared

# 결과 출력
cat("회귀계수 (Coefficients):\n", coefficients, "\n")
cat("SSE: ", SSE, "\n")
cat("SSR: ", SSR, "\n")
cat("SST: ", SST, "\n")
cat("R-squared: ", R_squared, "\n")

# 가설 검정 (t-test)
t_test <- summary(model)$coefficients
cat("t-values for coefficients:\n", t_test[, "t value"], "\n")
cat("p-values for coefficients:\n", t_test[, "Pr(>|t|)"], "\n")

# 잔차와 잔차제곱합 시각화
residuals <- data$Y - predictions

# Residuals plot
ggplot(data, aes(x = predictions, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  ggtitle("Residuals plot") +
  xlab("Predicted Y") +
  ylab("Residuals")

# Residuals squared plot
ggplot(data, aes(x = predictions, y = residuals^2)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Squared residuals plot") +
  xlab("Predicted Y") +
  ylab("Squared Residuals")