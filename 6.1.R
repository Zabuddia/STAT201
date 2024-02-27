Zts <- function(Xbar, mu, sigma, n) {
  (Xbar - mu) / (sigma / sqrt(n))
}

CISample <- function(n, p, mean, st_dev, question_num) {
t <- qt((1 - p)/2, n - 1, lower.tail=FALSE)
ll <- mean - (t * (st_dev/sqrt(n)))
ul <- mean + (t * (st_dev/sqrt(n)))
print(paste("Question ", question_num, ": ", round(c(ll,ul), 2)))
}

#Exercise 2
n <- 65
m <- 0.595
s <- 0.05
mu <- 0.6
zts <- Zts(m, mu, s, n)
p_value <- pnorm(zts)
print(paste("Question 1: ", round(zts, 2)))
print(paste("Question 2: ", round(p_value, 3)))
print(paste("Question 3: ", round(p_value*100, 3)))

#Exercise 6
n <- 87
m <- 15.2
s <- 1.8
mu <- 15
zts <- Zts(m, mu, s, n)
p_value <- 2 * (1 - pnorm(abs(zts))) # two-tailed test
print(paste("Question 5: ", round(zts, 2)))
print(paste("Question 6: ", round(p_value, 3)))

#Exercise 16
n <- 145
m <- 73.2461
s <- 2.3634
p <- 0.99
p_value <- 0.196
zts <- Zts(m, 73.6, s, n)
new_p_value <- pnorm(zts)
print(paste("Question 10: ", round(new_p_value, 3)))
CISample(n, p, m, s, "11&12")

#One-Vs-Two-Tailed Tests
zts <- 2.24
p_value <- 1 - pnorm(zts)
print(paste("Question 13: ", round(p_value, 3)))
print(paste("Question 14: ", zts))
print(paste("Question 15: ", round(p_value*2, 3)))