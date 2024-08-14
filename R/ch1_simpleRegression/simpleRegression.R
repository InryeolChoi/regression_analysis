# 2.12
rm(list=ls())
data = read.table("../../dataset/P054.txt", header=T, sep='\t')
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

# (a) ----
graph_new()
par(mfrow = c(1, 1))
with(data, plot(Daily, Sunday))

# (b) ----
fit = lm(Sunday ~ Daily, data)
abline(fit, col='blue')
summary(fit)

# (c) ----
confint(fit, level = 0.95)

# (d) ----
summary(fit)

# (e) ----
fitSum = summary(fit)
fitSum$r.squared

# (f) ----
x0 = data.frame(Daily=500)
predict(fit, x0, interval = 'confidence', level = 0.95)

# (e) ----
predict(fit, x0, interval = 'prediction', level = 0.95)

# (h) ----
x1 = data.frame(Daily=200)
predict(fit, x1, interval = 'prediction', level = 0.95)

