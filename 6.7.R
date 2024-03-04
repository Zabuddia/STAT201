Tts <- function(Xbar, mu, sigma, n) {
  (Xbar - mu) / (sigma / sqrt(n))
}

#Exercise 4
data <- read.csv("C:/Users/fifea/Downloads/6.7_4.csv")
t.test(data$Control, data$Treatment, conf.level=0.98)

#Exercise 12
data <- read.csv("C:/Users/fifea/Downloads/5.6_12.csv")
t.test(data$X61.degrees, data$X60.degrees, conf.level=0.95)

#Exercise 2
data <- read.csv("C:/Users/fifea/Downloads/6.7_2.csv")
t.test(data$Oval, data$Disk, conf.level=0.95)

#Exercise 10
data <- read.csv("C:/Users/fifea/Downloads/5.6_10.csv")
t.test(data$Grade.5, data$Grade.2, conf.level=0.98)

#Exercise 14
data <- read.csv("C:/Users/fifea/Downloads/6.7_14.csv")
t.test(data$Control, data$Treatment, alternative = "greater")
t.test(data$Treatment, data$Control, alternative = "less")

#Exercise 17
mean_x <- 1.755
mean_y <- 3.239
n_x <- 6
n_y <- 13
StDev_x <- 0.482
SE_y <- 0.094
StDev_y <- SE_y * sqrt(n_y)
SE_x <- StDev_x / sqrt(n_x)
estimate <- mean_x - mean_y
tts <- estimate / sqrt(((StDev_x^2)/n_x) + ((StDev_y^2)/n_y))
print(paste("Question 16: ", round(SE_x, 3)))
print(paste("Question 17: ", round(StDev_y, 3)))
print(paste("Question 18: ", round(estimate, 3)))
print(paste("Question 19: ", round(tts, 3)))