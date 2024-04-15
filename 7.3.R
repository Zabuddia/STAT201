#Exercise 2
B0 <- 0.77289
B1 <- -0.27985

sB0 <- 0.14534
sB1 <- 0.079258

t5_01 <- 3.365

B0Interval <- t5_01 * sB0
B1Interval <- t5_01 * sB1

lowerB0 <- B0 - B0Interval
upperB0 <- B0 + B0Interval

lowerB1 <- B1 - B1Interval
upperB1 <- B1 + B1Interval

print(paste("Question 2: ", round(lowerB1, 3)))
print(paste("Question 3: ", round(upperB1, 3)))
print(paste("Question 4: ", round(lowerB0, 3)))
print(paste("Question 5: ", round(upperB0, 3)))

ts <- (B1 - (-0.6)) / sB1
df <- 5
p_value <- 2 * pt(abs(ts), df, lower.tail = FALSE)

print(paste("Question 6: ", round(ts, 3)))
print(paste("Question 7: ", round(p_value, 3)))

#Exercise 3
slope <- -0.7524
intercept <- 88.761

answer <- intercept + slope * 50

print(paste("Question 12: ", round(answer, 3)))

R2 <- 0.22
R <- sqrt(R2)

print(paste("Question 13: ", round(R, 3)))

#15-21
slope <- 0.72868
intercept <- -0.23429

answer <- intercept + slope * 2.5
print(paste("Question 15: ", round(answer, 3)))

ts <- ((-0.23429) - 0) / 0.23996
df <- 11
p_value <- 2 * pt(abs(ts), df, lower.tail = FALSE)

print(paste("Question 16: ", round(ts, 3)))
print(paste("Question 17: ", round(p_value, 3)))

ts <- (0.72868 - 1) / 0.06353
df <- 11
p_value <- 2 * pt(abs(ts), df, lower.tail = FALSE)

print(paste("Question 19: ", round(ts, 3)))
print(paste("Question 20: ", round(p_value, 3)))