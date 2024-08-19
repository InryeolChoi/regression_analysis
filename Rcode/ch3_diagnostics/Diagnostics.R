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
library(car)
library(ggfortify)

# 1. 단순회귀의 모형진단
data("Boston", package = "MASS")  # Boston housing data

# 단순 회귀모형 생성
simple_model <- lm(medv ~ lstat, data = Boston)
summary(simple_model)

# 2. R^2 구하고 anova를 통한 적합도 검정
cat("R-squared:", summary(simple_model)$r.squared, "\n")
anova(simple_model)

# 3. 잔차 분석
# 잔차 추출
residuals <- residuals(simple_model)
fitted_values <- fitted(simple_model)

# 잔차의 정규성 검정
qqnorm(residuals)
qqline(residuals, col = "red")

# 잔차의 등분산성 검정
plot(fitted_values, residuals)
abline(h = 0, col = "red")

# 4. 다중회귀의 모형진단
data("swiss")

# 다중 회귀모형 생성
multiple_model <- lm(Fertility ~ Agriculture + Examination + Education + Catholic, data = swiss)
summary(multiple_model)

# 5. 잔차 분석
# 일반적인 잔차, 내적 표준화잔차, 외적 표준화잔차 추출
standardized_residuals <- rstandard(multiple_model)
studentized_residuals <- rstudent(multiple_model)
hat_values <- hatvalues(multiple_model)

# 잔차의 정규성, 등분산성, 독립성, 선형성 검정
par(mfrow = c(2, 2))

# 일반적인 잔차
plot(fitted(multiple_model), residuals(multiple_model), main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# 내적 표준화잔차
plot(fitted(multiple_model), standardized_residuals, main = "Standardized Residuals vs Fitted")
abline(h = 0, col = "red")

# 외적 표준화잔차
plot(fitted(multiple_model), studentized_residuals, main = "Studentized Residuals vs Fitted")
abline(h = 0, col = "red")

# 레버리지
plot(hat_values, studentized_residuals, main = "Leverage vs Studentized Residuals")
abline(h = 0, col = "red")

# 6. dffits, dfbetas, leverage, cook's distance, bonferroni correction
dffits_values <- dffits(multiple_model)
dfbetas_values <- dfbetas(multiple_model)

# Cook's Distance 계산
cooks_d <- cooks.distance(multiple_model)

# Cook's Distance 시각화
plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4 / n, col = "red", lty = 2)
text(x = which(cooks_d > 4 / n), y = cooks_d[cooks_d > 4 / n], 
     labels = names(cooks_d[cooks_d > 4 / n]), pos = 4, cex = 0.7)

# Bonferroni correction
alpha <- 0.05
n <- length(swiss$Fertility)
p <- length(coef(multiple_model))
bonferroni_threshold <- alpha / (n * p)

cat("Bonferroni Correction Threshold:", bonferroni_threshold, "\n")

# 7. 추가 변수 플롯 및 C-R 플롯
# 추가 변수 플롯
avPlots(multiple_model)

# C-R 플롯
crPlots(multiple_model)