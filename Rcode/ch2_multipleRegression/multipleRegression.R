# 4.7 ----
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

# 데이터셋 로드
data(mtcars)
df <- mtcars

# EDA: pairs()와 cor()를 사용하여 탐색적 데이터 분석 진행
pairs(df, main = "Pairs Plot of mtcars Dataset")
cor_matrix <- cor(df)
print(cor_matrix)

# 다중 회귀 분석
model <- lm(mpg ~ ., data = df)

# 회귀계수 추정
summary(model)

# 분산 추정
sigma_squared <- summary(model)$sigma^2
print(paste("Estimated variance (sigma^2):", sigma_squared))

# 결정계수와 수정된 결정계수
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared

print(paste("R-squared:", r_squared))
print(paste("Adjusted R-squared:", adj_r_squared))

# 전체 모델에 대한 F 검정
f_test <- summary(model)$fstatistic
f_value <- f_test[1]
f_df1 <- f_test[2]
f_df2 <- f_test[3]
p_value <- pf(f_value, f_df1, f_df2, lower.tail = FALSE)

print(paste("F-statistic:", f_value))
print(paste("Degrees of freedom 1:", f_df1))
print(paste("Degrees of freedom 2:", f_df2))
print(paste("p-value:", p_value))

# Nested Model 예제
nested_model <- lm(mpg ~ wt + hp, data = df)
anova(nested_model, model)

# H0: beta0 + beta1 = 0 검정
# 이를 위해, 모든 변수들 중 특정 변수를 제외한 모델을 생성
reduced_model <- lm(mpg ~ wt, data = df)
joint_test <- anova(reduced_model, model)

print(joint_test)

# Generalized Linear Model (GLM) 사용
glm_model <- glm(mpg ~ ., data = df)

# GLM 모델 요약
summary(glm_model)

# 신뢰구간 구하기
conf_intervals <- confint(model)
print(conf_intervals)