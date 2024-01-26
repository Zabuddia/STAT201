#Exercise 14
f <- function(x) {
  x/250
}

result <- integrate(f, lower = 25, upper = 30)
print(paste("Answer to question 1: ", result$value))

fmean <- function(x) {
  x*(x/250)
}

mean <- integrate(fmean, lower = 20, upper = 30)
print(paste("Answer to question 2: ", mean$value))

fvariance <- function(x) {
  (x^2)*(x/250)
}

variance_integral <- integrate(fvariance, lower = 20, upper = 30)

variance <- variance_integral$value - (mean$value^2)

print(paste("Answer to question 3: ", variance))

st_dev <- sqrt(variance)

print(paste("Answer to question 4: ", st_dev))

result <- integrate(f, lower = 28, upper = 30)
print(paste("Answer to question 5: ", result$value))

#Exercise 15
f <- function(x) {
  0.1*exp(-0.1*x)
}

result <- integrate(f, lower = 0, upper = 12)
print(paste("Answer to question 6: ", result$value))

area_to_x <- function(x) {
  integrate(f, 0, x)$value
}

median <- uniroot(function(x) area_to_x(x) - 0.5, lower = 0, upper = 1000)$root
print(paste("Answer to question 7: ", median))

eightieth_percentile <- uniroot(function(x) area_to_x(x) - 0.8, lower = 0, upper = 1000)$root
print(paste("Answer to question 8: ", eightieth_percentile))

#Exercise 21
f <- function(x) {
  12*((x^2)-(x^3))
}

result <- integrate(f, lower = 0.2, upper = 1)

print(paste("Answer to question 9: ", result$value))

fmean <- function(x) {
  x*(12*((x^2)-(x^3)))
}

mean <- integrate(fmean, lower = 0, upper = 1)

print(paste("Answer to question 10: ", mean$value))

fvariance <- function(x) {
  (x^2)*(12*((x^2)-(x^3)))
}

variance_integral <- integrate(fvariance, lower = 0, upper = 1)

variance <- variance_integral$value - (mean$value^2)

print(paste("Answer to question 11: ", variance))

st_dev <- sqrt(variance)

print(paste("Answer to question 12: ", st_dev))

result2 <- integrate(f, lower = 0, upper = 0.8)

print(paste("Answer to question 13: ", result2$value))

#Exercise 24
f1 <- function(x, c) {
  c/(x^3)
}
f2 <- function(c) {
  integrate(f1, lower = 1, upper = Inf, c=c)$value
}

see <- uniroot(function(c) f2(c) - 1, lower = 0, upper = 100)$root

print(paste("Answer to question 14: ", see))

#Supplementary Exercise
f1 <-function(x, k) {
  k*(1-(x^2))
}
f2 <- function(k) {
  integrate(f1, lower = -1, upper = 1, k=k)$value
}

kay <- uniroot(function(k) f2(k) - 1, lower = 0, upper = 100)$root

print(paste("Anser to question 15: ", kay))