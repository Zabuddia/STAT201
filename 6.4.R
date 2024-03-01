Tts <- function(Xbar, mu, sigma, n) {
  (Xbar - mu) / (sigma / sqrt(n))
}

CISample <- function(n, p, mean, st_dev, question_num) {
  t <- qt((1 - p)/2, n - 1, lower.tail=FALSE)
  ll <- mean - (t * (st_dev/sqrt(n)))
  ul <- mean + (t * (st_dev/sqrt(n)))
  print(paste("Question ", question_num, ": ", round(c(ll,ul), 3)))
}

#Exercise 8
water_data <- c(11.4, 13.9, 11.2, 14.5, 15.2, 8.1, 12.4, 8.6, 10.5, 17.1, 9.8, 15.9)
xbar <- mean(water_data)
mu <- 15
sigma <- sd(water_data)
n <- 12
p <- 1 - 0.05
df <- n - 1
tts <- Tts(xbar, mu, sigma, n)
print(paste("Question 4: ", round(tts, 3)))
critical_value <- qt(p, df)
print(paste("Question 5: ", round(critical_value, 3)))
p_value <- pt(tts, df)
print(paste("Quesiton 6: ", round(p_value, 3)))

#Exercise 12
n <- 5
df <- n-1
mean <- 5.92563
st_dev <- 0.15755
mu <- 6.1
tts <- Tts(mean, mu, st_dev, n)
p_value <- pt(tts, df)
print(paste("Question 9: ", round(p_value, 3)))
CISample(n, 0.99, mean, st_dev, "10&11")

#Exercise 4
n <- 10
mean <- 23.2
st_dev <- 0.2
mu <- 23
p <- 1 - 0.025
df <- n-1
tts <- Tts(mean, mu, st_dev, n)
print(paste("Question 13: ", round(tts, 3)))
critical_value <- qt(p, df)
print(paste("Question 14: ", round(critical_value, 3)))
p_value <- 2 * pt(tts, df, lower.tail = FALSE)
print(paste("Quesiton 15: ", round(p_value, 4)))

#Exercise 13
n <- 11
df <- n-1
mean <- 13.2874
P <- 0.171
mu <- 16
tts <- qt(1-(P/2), df, lower.tail = FALSE)
SEmean <- 1.8389
st_dev <- SEmean * sqrt(n)
#You can also do this:
#st_dev <- uniroot(function(s) Tts(mean, mu, s, n) - tts, lower = 0.0001, upper = 50)$root
print(paste("Question 17: ", round(st_dev, 3)))
CISample(n, 0.95, mean, st_dev, "18&19")
print(paste("Question 20: ", round(tts, 3)))
t <- (mean - mu) / (SEmean)
print(paste("Question 20: ", round(t, 3)))