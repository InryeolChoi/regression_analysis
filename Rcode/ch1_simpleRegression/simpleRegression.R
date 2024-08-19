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

# 유명한 데이터셋 'mtcars' 가져오기
data(mtcars)

# 데이터에 대한 EDA (Exploratory Data Analysis)
summary(mtcars)
pairs(mtcars)

# 단순회귀 모형의 추정: mpg (연비) ~ hp (마력)
model <- lm(mpg ~ hp, data = mtcars)
summary(model)

# 회귀계수 추출
beta_0 <- coef(model)[1] # 절편 (Intercept)
beta_1 <- coef(model)[2] # 기울기 (Slope)

# 패키지를 사용하지 않고 OLS 구현하기
manual_ols <- function(x, y) {
  n <- length(y)
  x_bar <- mean(x)
  y_bar <- mean(y)
  
  # 기울기(beta_1) 계산
  beta_1_manual <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
  
  # 절편(beta_0) 계산
  beta_0_manual <- y_bar - beta_1_manual * x_bar
  
  # R^2 값 계산
  y_pred <- beta_0_manual + beta_1_manual * x
  sse <- sum((y - y_pred)^2)  # Sum of Squared Errors (SSE)
  ssr <- sum((y_pred - y_bar)^2)  # Sum of Squared Regression (SSR)
  sst <- sum((y - y_bar)^2)  # Total Sum of Squares (SST)
  r_squared <- ssr / sst
  
  # 회귀계수, SSE, SSR, SST, R^2 값을 리스트로 반환
  return(list(beta_0 = beta_0_manual, beta_1 = beta_1_manual, SSE = sse, SSR = ssr, SST = sst, R_squared = r_squared))
}

# OLS 함수 사용하여 결과 계산
x <- mtcars$hp
y <- mtcars$mpg
manual_results <- manual_ols(x, y)
manual_results

# 패키지를 사용한 R^2 값 도출
summary(model)$r.squared

# confint()를 사용한 구간추정
confint(model)

# 단순회귀 모형의 검정 (t검정)
summary(model)$coefficients[, "t value"]

# 회귀계수, SSE, SSR, SST, R^2 사이의 관계 출력
cat("Beta_0 (절편):", manual_results$beta_0, "\n")
cat("Beta_1 (기울기):", manual_results$beta_1, "\n")
cat("SSE (오차제곱합):", manual_results$SSE, "\n")
cat("SSR (회귀제곱합):", manual_results$SSR, "\n")
cat("SST (총제곱합):", manual_results$SST, "\n")
cat("R^2 (결정계수):", manual_results$R_squared, "\n")