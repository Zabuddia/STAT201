#Exercise 2
values <- c(0, 1, 2, 3, 4)
probabilities <- c(0.4, 0.3, 0.15, 0.1, 0.05)

plot(values, probabilities, type="h", lwd=2, col="blue", main="Probability Mass Function", xlab="Values", ylab="Probability")

mean <- sum(values * probabilities)
variance <- sum(((values - mean) ^ 2) * probabilities)
stddev <- sqrt(variance)

cat("Mean:", mean, "\n")
cat("Variance:", variance, "\n")
cat("Standard Deviation:", stddev, "\n")

#Exercise 5
values <- c(0, 1, 2, 3, 4)
probabilities <- c(0.4, 0.3, 0.15, 0.1, 0.05)

plot(values, probabilities, type="h", lwd=2, col="blue", main="Probability Mass Function", xlab="Values", ylab="Probability")

mean <- sum(values * probabilities)
variance <- sum(((values - mean) ^ 2) * probabilities)
stddev <- sqrt(variance)

cat("Mean:", mean, "\n")
cat("Variance:", variance, "\n")
cat("Standard Deviation:", stddev, "\n")

#Exercise 7
C <- (1/(1+2+3+4+5))
2*C
values <- c(1, 2, 3, 4, 5)
probabilities <- c(1*C, 2*C, 3*C, 4*C, 5*C)

plot(values, probabilities, type="h", lwd=2, col="blue", main="Probability Mass Function", xlab="Values", ylab="Probability")

mean <- sum(values * probabilities)
variance <- sum(((values - mean) ^ 2) * probabilities)
stddev <- sqrt(variance)

cat("Mean:", mean, "\n")
cat("Variance:", variance, "\n")
cat("Standard Deviation:", stddev, "\n")