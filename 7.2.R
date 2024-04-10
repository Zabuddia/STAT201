#Exercise 2
ksi <- function(hardness) {
  y <- -196.32 + 2.42 * hardness
  return(y)
}

hardness1 <- 102.7
answer <- ksi(hardness1)

print(paste("Question 1: ", round(answer, 3)))

answer <- 3 * 2.42

print(paste("Question 2: ", answer))

#Supplementary1
pH <- c(4.6, 4.8, 5.2, 5.4, 5.6, 5.8, 6)
Yield <- c(1056, 1833, 1629, 1852, 1783, 2647, 2131)

model <- lm(Yield ~ pH)

coefficients(model)

R2 <- summary(model)$r.squared

print(paste("Question 7: ", round(R2, 3)))

#Supplementary2
Diameter <- c(4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6)
Strength <- c(51, 54, 69, 81, 75, 79, 89, 101, 98, 102)

model <- lm(Strength ~ Diameter)

plot(Diameter, Strength)

coefficients(model)

residuals(model)

intercept <- -67.69091
slope <- 28.93939

answer <- intercept + slope * 4.8

print(paste("Question 12: ", round(answer, 3)))

answer <- 0.3 * 28.939

print(paste("Question 14: ", answer))

answer <- intercept + slope * 5.5

print(paste("Question 15: ", round(answer, 3)))

strength <- function(diameter) {
  y <- intercept + slope * diameter
  return(y)
}

strength1 <- 95

diameter1 <- uniroot(function(x) strength(x) - strength1, lower = 0, upper = 10)$root

print(paste("Question 16: ", round(diameter1, 3)))

R2 <- summary(model)$r.squared

print(paste("Question 17: ", round(R2, 3)))

#Exercise 4
ErrorSOS <- 33.9
TotalSOS <- 181.2
RegressionSOS <- TotalSOS - ErrorSOS

R2 <- RegressionSOS / TotalSOS

print(paste("Question 18: ", round(R2, 3)))

#Exercise 8
r <- 0.7
sx <- 2
sy <- 100
x_bar <- 5
y_bar <- 1350

slope <- r * (sy / sx)
intercept <- y_bar - (slope * x_bar)

print(paste("Question 19: ", round(intercept, 3)))
print(paste("Question 20: ", round(slope, 3)))