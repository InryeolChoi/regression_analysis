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

if (!require("nlme")) install.packages("nlme", dependencies = TRUE)
library(nlme)
if (!require("AER")) install.packages("AER", dependencies = TRUE)
library(AER)


# 1. 등분산성이 깨진 데이터 생성 및 로그변환
set.seed(123)
n <- 100
x <- seq(1, n)
y <- 5 + 2 * x + rnorm(n, mean = 0, sd = x * 0.1)  # 비등분산성 데이터

# 데이터 프레임 생성
data <- data.frame(x = x, y = y)

# 원본 데이터 시각화
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Original Data with Heteroscedasticity") +
  theme_minimal()

# 로그 변환
data$log_y <- log(data$y + 1)  # log 변환 (y + 1을 추가하여 0이 되는 값 방지)

# 로그 변환 후 데이터 시각화
ggplot(data, aes(x = x, y = log_y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Log Transformed Data") +
  theme_minimal()

# 2. 비정상적인 분산을 가진 데이터 생성 및 WLS 해결
set.seed(456)
x_wls <- seq(1, n)
y_wls <- 10 + 3 * x_wls + rnorm(n, mean = 0, sd = x_wls * 0.2)  # 비정상적인 분산 데이터

# 데이터 프레임 생성
data_wls <- data.frame(x = x_wls, y = y_wls)

# 가중치 추정 (여기서는 단순히 x를 사용하여 가중치 예측)
weights <- 1 / (x_wls * 0.2 + 1)  # 가중치 (단순화된 예)

# WLS 모델 적합
wls_model <- lm(y ~ x, data = data_wls, weights = weights)

# WLS 결과 시각화
ggplot(data_wls, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  ggtitle("WLS Model") +
  theme_minimal()

# 3. 비정상적인 분산을 가진 데이터 생성 및 GLS 해결
set.seed(789)
x_gls <- seq(1, n)
y_gls <- 15 + 4 * x_gls + rnorm(n, mean = 0, sd = x_gls * 0.3)  # 비정상적인 분산 데이터

# 데이터 프레임 생성
data_gls <- data.frame(x = x_gls, y = y_gls)

# GLS 모델 적합 (비정상적인 분산을 위해 AR1 구조를 가정)
gls_model <- gls(y ~ x, data = data_gls, correlation = corAR1(form = ~ x))

# GLS 결과 시각화
ggplot(data_gls, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  ggtitle("GLS Model") +
  theme_minimal()

# 4. 비정상적인 분산을 가진 데이터 생성 및 도구변수를 사용한 WLS 해결
set.seed(101)
x_iv <- seq(1, n)
y_iv <- 20 + 5 * x_iv + rnorm(n, mean = 0, sd = x_iv * 0.4)  # 비정상적인 분산 데이터

# 데이터 프레임 생성
data_iv <- data.frame(x = x_iv, y = y_iv)

# 도구 변수 (단순화된 예로 x의 제곱을 도구변수로 사용)
data_iv$instrument <- x_iv^2

# 도구변수를 사용하여 가중치 추정
iv_model <- ivreg(y ~ x | instrument, data = data_iv)

# 예측된 가중치
predicted_weights <- predict(iv_model, newdata = data_iv)

# WLS 모델 적합
wls_iv_model <- lm(y ~ x, data = data_iv, weights = predicted_weights)

# WLS with IV 결과 시각화
ggplot(data_iv, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  ggtitle("WLS with Instrumental Variables") +
  theme_minimal()

