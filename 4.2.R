#Exercise 2
n <- 9
p <- 0.4

x <- 6
#Either of these works
problem1 <- pbinom(x, n, p, lower.tail=FALSE)
problem1 <- 1 - pbinom(x, n, p)
cat("The answer to question 1: ", problem1)

x <- 2
#Either of these works
problem2 <- pbinom(x - 1, n, p, lower.tail=FALSE)
problem2 <- 1 - pbinom(x - 1, n, p)
cat("The answer to question 2: ", problem2)

x <- 2
y <- 5
problem3x <- 1 - pbinom(x - 1, n, p)
problem3y <- 1 - pbinom(y - 1, n, p)
problem3 <- problem3x - problem3y
cat("The answer to question 3: ", problem3)

problem4x <- 1 - pbinom(x, n, p)
problem4y <- 1 - pbinom(y, n, p)
problem4 <- problem4x - problem4y
cat("The answer to question 4: ", problem4)

x <- 0
problem5 <- dbinom(x, n, p)
cat("The answer to question 5: ", problem5)

x <- 7
problem6 <- dbinom(x, n, p)
cat("The answer to question 6: ", problem6)

mean <- n * p
cat("The answer to question 7: ", mean)

variance <- n * p * (1 - p)
cat("The answer to question 8: ", variance)

#Exercise 8
n <- 20
p <- 0.2

x <- 4
problem9 <- dbinom(4, n, p)
cat("The answer to question 9: ", problem9)

x <- 3
problem10 <- pbinom(x - 1, n, p)
cat("The answer to question 10: ", problem10)

x <- 0
problem11 <- dbinom(0, n, p)
cat("The answer to question 11: ", problem11)

mean <- n * p
cat("The answer to question 12: ", mean)

st_dev <- sqrt(n * p * (1 - p))
cat("The answer to question 13: ", st_dev)

#Exercise 17
n <- 5
p <- 0.9
x <- 3

problem14 <- 1 - pbinom(x - 1, n, p)
cat("The answer to question 14: ", problem14)

f <- function(en) {
  # Round en to the nearest integer
  int_en <- round(en)
  1 - pbinom(x - 1, int_en, p)
}

problem15 <- uniroot(function(en) f(en) - 0.9, lower=0, upper=100)$root
cat("The answer to question 15: ", rount(problem15))

#Exercise 18
n <- 6
pRainy <- 0.7
pNonRainy <- 0.9
x <- 4

problem16 <- 1 - pbinom(x - 1, n, pRainy)
cat("The answer to question 16: ", problem16)

problem17 <- 1 - pbinom(x - 1, n, pNonRainy)
cat("The answer to question 17: ", problem17)

rain <- 0.2
problem18 <- (rain * problem16) + ((1 - rain) * problem17)
cat("The answer to question 18: ", problem18)