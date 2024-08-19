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
library(lme4)
library(afex)
library(dplyr)

# 1. 데이터셋 로드 및 변환 ----
data("mtcars")
df <- mtcars

# 연속형 변수 Y와 이산형 변수 X 생성
# 연속형 변수: mpg (miles per gallon)
# 이산형 변수: cyl (number of cylinders) -> factor로 변환
df$cyl <- as.factor(df$cyl)

# 2. 계획행렬 분석 ----
X <- model.matrix(~ cyl, data=df)  # 계획행렬 생성
print(head(X))  # 계획행렬 출력

# 3. 더미변수 생성 및 reference coding, cellmean coding ----
# Reference Coding
df$cyl_ref <- relevel(df$cyl, ref="4")  # Reference Level 설정

# Cellmean Coding
df$cyl_cellmean <- model.matrix(~ cyl - 1, data=df)  # Cellmean coding

# 4. 이산형 변수의 효과 시각화 ----
# cyl이 4인 경우와 6인 경우의 mpg 차이 시각화
ggplot(df, aes(x=cyl, y=mpg, fill=cyl)) +
  geom_boxplot() +
  labs(title="Effect of Number of Cylinders on MPG", x="Number of Cylinders", y="Miles Per Gallon") +
  theme_minimal()

# 5. 상호작용 변수 추가 및 모델 검정 ----
# 상호작용 변수를 추가하여 모델 생성
df$cyl_num <- as.numeric(df$cyl)  # 이산형 변수를 숫자형으로 변환

model_interaction <- lm(mpg ~ cyl * cyl_num, data=df)
summary(model_interaction)

# 상호작용 시각화
ggplot(df, aes(x=cyl_num, y=mpg, color=cyl)) +
  geom_point() +
  geom_smooth(method="lm", aes(group=cyl), se=FALSE) +
  labs(title="Interaction Effect of Cylinders and Numeric Cylinder Number on MPG",
       x="Numeric Cylinder Number",
       y="Miles Per Gallon") +
  theme_minimal()

# 6. 일원 분산분석 (ANOVA) ----
anova_oneway <- aov(mpg ~ cyl, data=df)
summary(anova_oneway)

# 일원 분산분석 시각화
ggplot(df, aes(x=cyl, y=mpg, fill=cyl)) +
  geom_boxplot() +
  labs(title="One-Way ANOVA: Effect of Number of Cylinders on MPG", x="Number of Cylinders", y="Miles Per Gallon") +
  theme_minimal()

# 7. 이원 분산분석 (ANOVA) ----
# 예를 들어, 추가적으로 gear라는 변수를 생성하여 이원 분산분석을 수행
df$gear <- as.factor(sample(c(3, 4, 5), nrow(df), replace=TRUE))
anova_twoway <- aov(mpg ~ cyl * gear, data=df)
summary(anova_twoway)

# 이원 분산분석 시각화
ggplot(df, aes(x=cyl, y=mpg, color=gear)) +
  geom_boxplot() +
  labs(title="Two-Way ANOVA: Effect of Cylinders and Gear on MPG", x="Number of Cylinders", y="Miles Per Gallon") +
  theme_minimal()

# 8. 공분산분석 (ANCOVA) ----
# 연속형 공변수 추가
anova_ancova <- aov(mpg ~ cyl + hp, data=df)  # hp는 자동차의 horsepower
summary(anova_ancova)

# 공분산분석 시각화
ggplot(df, aes(x=hp, y=mpg, color=cyl)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="ANCOVA: Effect of Cylinders and Horsepower on MPG",
       x="Horsepower",
       y="Miles Per Gallon") +
  theme_minimal()

# 9. 선형혼합모형 ----
# 데이터 생성
set.seed(123)
n_groups <- 10  # 그룹의 수
n_per_group <- 20  # 그룹당 데이터 포인트 수

# 그룹과 서브그룹 생성
group <- rep(1:n_groups, each = n_per_group)
subgroup <- rep(1:2, times = n_groups * (n_per_group / 2))
id <- factor(paste(group, 1:(n_groups * n_per_group), sep = "_"))

# 종속 변수 생성
response <- rnorm(n_groups * n_per_group, mean = 50, sd = 10) +
  as.numeric(group) * 0.5 + as.numeric(subgroup) * 2 +
  rnorm(n_groups * n_per_group, mean = 0, sd = 2)

df <- data.frame(id = id, group = as.factor(group), subgroup = as.factor(subgroup), response = response)

# 선형 혼합 모형 적합
model_lmm <- lmer(response ~ subgroup + (1 | group), data = df)
summary(model_lmm)

# 모델 단일성 검사
if (isSingular(model_lmm)) {
  warning("The model has a singular fit. Consider simplifying the model.")
}

# 시각화

# 그룹별 평균 반응 값 시각화
ggplot(df, aes(x = subgroup, y = response, color = group)) +
  geom_point() +
  geom_jitter(width = 0.2, height = 0) +
  labs(title = "Response vs. Subgroup by Group", x = "Subgroup", y = "Response") +
  theme_minimal()

# 그룹별 평균을 바탕으로 시각화
group_means <- df %>%
  group_by(group, subgroup) %>%
  summarise(mean_response = mean(response), .groups = 'drop')

ggplot(group_means, aes(x = subgroup, y = mean_response, color = group, group = group)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Mean Response by Subgroup and Group", x = "Subgroup", y = "Mean Response") +
  theme_minimal()
