#Exercise 1
sigmax <- 0.3
sigmay <- 0.2

answer <- 4 * sigmax
cat("Question 1: ", answer)

answer <- sqrt(sigmax^2 + 2^2 * sigmay^2)
cat("Question 2: ", answer)

answer <- sqrt(2^2 * sigmax^2 + 3^2 * sigmay^2)
cat("Question 3: ", answer)

#Exercise 2
uncertainty <- 1.5
target_uncertainty <- 0.5
f <- function(n) {
  uncertainty / sqrt(n)
}
answer <- uniroot(function(n) f(n) - target_uncertainty, lower = 0, upper = 50)$root
test <- 9
test_answer <- uncertainty / sqrt(test)
cat("Test answer question 4: ", test_answer)
cat("Question 4: ", answer)

#Exercise 4
radius <- 5
height <- 6
uncertainty_height <- 0.02
volume <- (pi * radius^2 * height) / 3
cat("Question 5: ", volume)

uncertainty <- volume <- (pi * radius^2 * uncertainty_height) / 3
cat("Question 6: ", uncertainty)

#Exercise 5
mean_1774to1884 <- 4.93
uncertainty_1774to1884 <- 0.23
mean_1885to1984 <- 3.92
uncertainty_1885to1984 <- 0.19

mean_difference <- mean_1774to1884 - mean_1885to1984
cat("Question 7: ", mean_difference)

uncertainty <- sqrt((uncertainty_1774to1884) ^ 2 + (uncertainty_1885to1984) ^ 2)
cat("Question 8: ", uncertainty)

#Exercise 14
bias <- 2
uncertainty <- 3
cat("Question 9: ", uncertainty)
cat("Question 10: ", bias)
cat("Question 11: ", uncertainty / sqrt(4))
cat("Question 12: ", bias)
cat("Question 13: ", uncertainty / sqrt(400))
cat("Question 14: ", bias)

#Exercise 15
average <- 87
st_dev <- 2
uncertainty <- st_dev
target_uncertainty <- 0.4

cat("Question 17: ", average)
cat("Question 18: ", uncertainty / sqrt(8))
cat("Question 19: ", uncertainty / sqrt(16))

answer <- uniroot(function(n) f(n) - target_uncertainty, lower = 0, upper = 50)$root
cat("Question 20: ", answer)