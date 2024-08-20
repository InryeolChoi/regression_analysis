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

# 필요한 패키지 로드
library(ggplot2)
library(MASS)
library(car)
library(glmnet)
library(stats)

if (!require("pls")) install.packages("pls", dependencies = TRUE)
library(pls)


# 다중공선성 문제를 가진 데이터 생성 및 해결
set.seed(202)
n <- 100
x <- seq(1, n)
y <- 5 + 2 * x + rnorm(n, mean = 0, sd = x * 0.1)  # 비등분산성 데이터

x1 <- rnorm(n)
x2 <- 2 * x1 + rnorm(n)  # x2는 x1의 선형 조합
y <- 25 + 6 * x1 + 4 * x2 + rnorm(n)
data_multicol <- data.frame(x1 = x1, x2 = x2, y = y)

# EDA를 통해 다중공선성 찾기
cor(data_multicol$x1, data_multicol$x2)  # 상관계수 확인

# Tolerance 및 VIF 계산
vif_model <- lm(y ~ x1 + x2, data = data_multicol)
vif(vif_model)

# 변수 제거를 통해 해결
reduced_model <- lm(y ~ x1, data = data_multicol)

# 주성분 회귀를 통한 다중공선성 해결
pcr_model <- pcr(y ~ x1 + x2, data = data_multicol, scale = TRUE, validation = "CV")

# 주성분 회귀 결과 시각화
plot(pcr_model, main = "Principal Component Regression")

# 벌점화 회귀를 통한 다중공선성 해결
# 릿지 회귀
ridge_model <- glmnet(as.matrix(data_multicol[, c("x1", "x2")]), data_multicol$y, alpha = 0)

# 라쏘 회귀
lasso_model <- glmnet(as.matrix(data_multicol[, c("x1", "x2")]), data_multicol$y, alpha = 1)

# 벌점화 회귀 결과 시각화
graph_new()
plot(ridge_model, main = "Ridge Regression")

graph_new()
plot(lasso_model, main = "Lasso Regression")
