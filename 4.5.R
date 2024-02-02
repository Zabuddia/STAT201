#Exercise 2
mean <- 0
variance <- 1
st_dev <- 1

q <- 0.56
answer <- pnorm(q, mean, st_dev)
cat("Question 1: ", answer)

q <- -2.93
r <- -2.06
answer <- pnorm(r, mean, st_dev) - pnorm(q, mean, st_dev)
cat("Question 2: ", answer)

q <- -1.08
r <- 0.7
answer <- pnorm(r, mean, st_dev) - pnorm(q, mean, st_dev)
cat("Question 3: ", answer)

q <- 0.96
answer <- 1 - pnorm(q, mean, st_dev)
cat("Question 4: ", answer)

#Exercise 3
mean <- 0
variance <- 1
st_dev <- 1

p <- 1 - 0.1587
answer <- qnorm(p, mean, st_dev)
cat("Question 5: ", answer)

p <- 0.5 - 0.4772
answer <- qnorm(p, mean, st_dev)
cat("Question 6: ", answer)

p <- 0.5 + (0.8664/2)
answer <- qnorm(p, mean, st_dev)
cat("Question 7: ", answer)

p <- 0.5 + 0.2967
answer <- qnorm(p, mean, st_dev)
cat("Question 8: ", answer)

#Exercise 4
mean <- 2
variance <- 9
st_dev <- 3

q <- 2
answer <- pnorm(q, mean, st_dev)
cat("Question 9: ", answer)

q <- 1
r <- 7
answer <- pnorm(r, mean, st_dev) - pnorm(q, mean, st_dev)
cat("Question 10: ", answer)

q <- -2.5
r <- -1
answer <- pnorm(r, mean, st_dev) - pnorm(q, mean, st_dev)
cat("Question 11: ", answer)

p <- 1 - 0.3264
answer <- qnorm(p, mean, st_dev)
cat("Question 12: ", answer)

pb <- 0.5 + (0.8384/2)
pa <- 1 - pb
answera <- qnorm(pa, mean, st_dev)
answerb <- qnorm(pb, mean, st_dev)
cat("Question 13: ", answera)
cat("Question 14: ", answerb)

#Exercise 8
mean <- 4.1
st_dev <- 0.6

q <- 3.7
r <- 4.4
answer <- pnorm(r, mean, st_dev) - pnorm(q, mean, st_dev)
cat("Question 15: ", answer)

q <- 4.4
answer <- 1 - pnorm(q, mean, st_dev)
cat("Question 16: ", answer)

p <- .80
answer <- qnorm(p, mean, st_dev)
cat("Question 17: ", answer)

q <- 4.5
answer <- 1 - pnorm(q, mean, st_dev)
cat("Question 18: ", answer)

#Supplementary Exercise
z1 <- qnorm(0.1)
z2 <- qnorm(0.95)

A <- matrix(c(1, z1, 1, z2), nrow = 2, byrow = TRUE)
b <- c(18.3, 19.76)

solution <- solve(A, b)

mu <- solution[1]
sigma <- solution[2]

cat("Question 19: ", mu)
cat("Question 20: ", sigma)