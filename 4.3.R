#Exercise 2
lambda <- 6

x <- 8
answer <- dpois(x, lambda)
cat("Question 1: ", answer)

x <- 2
answer <- dpois(x, lambda)
cat("Question 2: ", answer)

x <- 3
answer <- ppois(x-1, lambda)
cat("Question 3: ", answer)

x <- 1
answer <-1 - ppois(x, lambda)
cat("Question 4: ", answer)

cat("Question 5: ", lambda)

cat("Question 6: ", sqrt(lambda))

#Exercise 4
lambda <- 6

x <- 7
answer <- dpois(x, lambda)
cat("Question 7: ", answer)

x <- 3
answer <- ppois(x, lambda)
cat("Question 8: ", answer)

x <- 2
y <- 7
answerx <- 1 - ppois(x, lambda)
answery <- 1 - ppois(y - 1, lambda)
answer <- answerx - answery
cat("Question 9: ", answer)

cat("Question 10: ", lambda)

cat("Question 11: ", sqrt(lambda))

#Exercise 7
lambda <- 4
x <- 5
answer <- dpois(x, lambda)
cat("Question 12: ", answer)

lambda <- 4 * 1.5
x <- 9
answer <- dpois(x, lambda)
cat("Question 13: ", answer)

lambda <- 4 * 0.5
x <- 3
answer <- ppois(x - 1, lambda)
cat("Question 14: ", answer)

#Exercise 14
prob <- 0.1353
x <- 0
f <- function(m) {
  dpois(x, m)
}
mean <- uniroot(function(m) f(m) - prob,lower = 0, upper = 50)$root
cat("Question 15: ", mean)

checkAnswer <- dpois(x, mean)
cat("Check answer to 15: ", checkAnswer)

#Exercise 8
lambda <- 4
x <- 3
answer <- dpois(x, lambda)
cat("Question 16: ", answer)

lambda <- 12
x <- 8
answer <- dpois(x, lambda)
cat("Question 17: ", answer)

lambda <- 8
x <- 3
answer <- 1 - ppois(x, lambda)
cat("Question 18: ", answer)