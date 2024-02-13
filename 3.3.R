library(Deriv)

#Exercise 1
X <- 2
original_uncertainty <- 0.3
f <- function(x) x^3
f_prime <- Deriv(f, "x")
uncertainty <- abs(f_prime(X)) * original_uncertainty
cat("Question 1: ", uncertainty)

f <- function(x) sqrt(2*x)
f_prime <- Deriv(f, "x")
uncertainty <- abs(f_prime(X)) * original_uncertainty
cat("Question 2: ", uncertainty)

f <- function(x) 3/x
f_prime <- Deriv(f, "x")
uncertainty <- abs(f_prime(X)) * original_uncertainty
cat("Question 3: ", uncertainty)

f <- function(x) log(x)
f_prime <- Deriv(f, "x")
uncertainty <- abs(f_prime(X)) * original_uncertainty
cat("Question 4: ", uncertainty)

f <- function(x) exp(x)
f_prime <- Deriv(f, "x")
uncertainty <- abs(f_prime(X)) * original_uncertainty
cat("Question 5: ", uncertainty)

#Exercise 2
X <- 3
uncertainty_x <- 0.1
#Solve for Y
y <- function(x) 1 / x
Y <- y(X)
f_prime <- Deriv(y, "x")
uncertainty_y <- abs(f_prime(X)) * uncertainty_x
cat("Question 6: ", Y)
cat("Question 7: ", uncertainty_y)

#Solve for Y
y <- function(x) 2 * x
Y <- y(X)
f_prime <- Deriv(y, "x")
uncertainty_y <- abs(f_prime(X)) * uncertainty_x
cat("Question 8: ", Y)
cat("Question 9: ", uncertainty_y)

#Solve for Y
y <- function(x) 9 / x
Y <- y(X)
f_prime <- Deriv(y, "x")
uncertainty_y <- abs(f_prime(X)) * uncertainty_x
cat("Question 10: ", Y)
cat("Question 11: ", uncertainty_y)

#Solve for Y
y <- function(x) 4 / sqrt(x)
Y <- y(X)
f_prime <- Deriv(y, "x")
uncertainty_y <- abs(f_prime(X)) * uncertainty_x
cat("Question 12: ", Y)
cat("Question 13: ", uncertainty_y)

#Exercise 4
T <- 300
uncertainty_T <- 0.4
v <- function(t) 20.04 * sqrt(t)
V <- v(T)
v_prime <- Deriv(v, "t")
uncertainty_V <- abs(v_prime(T)) * uncertainty_T
cat("Question 14: ", V)
cat("Question 15: ", uncertainty_V)

#Exercise 11
y <- 20.9
sigma_y <- 0.4
answer <- (sigma_y / y) * 100
cat("Question 16: ", answer)

y <- 15.1
sigma_y <- 0.8
answer <- (sigma_y / y) * 100
cat("Question 17: ", answer)

#Exercise 12
y <- 48.41
relative_uncertainty <- 0.3/100
absolute_uncertainty <- relative_uncertainty * y
cat("Question 18: ", absolute_uncertainty)

y <- 991.7
relative_uncertainty <- 0.6/100
absolute_uncertainty <- relative_uncertainty * y
cat("Question 19: ", absolute_uncertainty)

#Exercise 14
Tee <- 298.4
uncertainty_T <- 0.2
v <- function(t) 20.04 * sqrt(t)
V <- v(Tee)
v_prime <- Deriv(v, "t")
uncertainty_V <- abs(v_prime(Tee)) * uncertainty_T
relative_uncertainty_V <- uncertainty_V / V
cat("Question 20: ", V)
cat("Question 21: ", relative_uncertainty_V * 100)

#Exercise 15
g <- 9.8
L <- 0.855
uncertainty_L <- 0.005
t <- function(l) 2 * pi * sqrt(l/g)
Tee <- t(L)
t_prime <- Deriv(t, "l")
uncertainty_T <- abs(t_prime(L)) * uncertainty_L
relative_uncertainty_T <- uncertainty_T / Tee
cat("Question 22: ", Tee)
cat("Question 23: ", relative_uncertainty_T * 100)