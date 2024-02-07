#Questions not form the textbook
mean <- 25
variance <- (0.2^2)*5
st_dev <- sqrt(variance)
q <- 24
answer <- pnorm(q, mean, st_dev)
cat("Question 6: ", answer)

mean <- 30
variance <- 4/100
st_dev <- sqrt(variance)
q <- 30.2
answer <- pnorm(q, mean, st_dev, lower.tail = FALSE)
cat("Question 7: ", answer)

#Exercise 2
num_pages <- 250
mean <- 0.08
total_mean <- mean * num_pages
st_dev <- 0.01
variance <- st_dev ^ 2
total_variance <- variance * num_pages
total_st_dev <- sqrt(total_variance)
q <- 20.2
answer <- pnorm(q, total_mean, total_st_dev, lower.tail = FALSE)
cat("Question 8: ", answer)

p <- 0.1
answer <- qnorm(p, total_mean, total_st_dev)
cat("Question 9: ", answer)

#Exercise 5
mean <- 15
st_dev <- 5
variance <- st_dev ^ 2
sampleSize <- 60
sample_mean <- mean
sample_variance <- variance / sampleSize
sample_st_dev <- sqrt(sample_variance)
q <- 14
answer <- pnorm(q, sample_mean, sample_st_dev)
cat("Question 10: ", answer)

p <- 0.7
answer <- qnorm(p, sample_mean, sample_st_dev)
cat("Question 11: ", answer)

p1 <- 0.125
p2 <- 0.875
answer1 <- qnorm(p1, sample_mean, sample_st_dev)
answer2 <- qnorm(p2, sample_mean, sample_st_dev)
cat("Question 12: ", answer1)
cat("Question 13: ", answer2)

#Exercise 6
mean <- 1.3
st_dev <- 0.1
variance <- st_dev ^ 2
sample_size <- 200
sample_mean <- mean
sample_variance <- variance / sample_size
sample_st_dev <- sqrt(sample_variance)
q <- 1.305
answer <- pnorm(q, sample_mean, sample_st_dev, lower.tail = FALSE)
cat("Question 14: ", answer)

p <- 0.25
answer <- qnorm(p, sample_mean, sample_st_dev)
cat("Question 15: ", answer)

#Exercise 7
mean <- 4
st_dev <- 2
variance <- st_dev ^ 2
sample_size <- 50
sample_mean <- mean
sample_variance <- variance / sample_size
sample_st_dev <- sqrt(sample_variance)
total_mean <- sample_mean * sample_size
total_variance <- sample_variance * sample_size
total_st_dev <- 2 * sqrt(50)
q <- 180
answer <- pnorm(q, total_mean, total_st_dev)
cat("Question 16: ", answer)

p <- .30
answer <- qnorm(p, total_mean, total_st_dev)
cat("Question 17: ", answer)

p1 <- 0.1515
p2 <- 0.8485
answer1 <- qnorm(p1, total_mean, total_st_dev)
answer2 <- qnorm(p2, total_mean, total_st_dev)
cat("Question 18: ", answer1)
cat("Question 19: ", answer2)