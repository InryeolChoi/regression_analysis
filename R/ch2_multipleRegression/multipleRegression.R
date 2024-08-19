# 4.7 ----
rm(list=ls())
data = read.table("../../dataset/P088.txt", header=T, sep='\t')
str(data)
head(data)

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

## (b) ----
cor(data[, 2:8])
graph_new()
pairs(data[, 2:8])

## (e) ----
fit = lm(formula = Sales ~ Age + HS + Income + Black + Female + Price,
         data = data)
summary(fit)

## 4.11 ----
rm(list=ls())
fit_H0 = lm(formula = Sales ~ Age + HS + Income + Black + Female + Price, data = data)
fit_H1 = lm(formula)

