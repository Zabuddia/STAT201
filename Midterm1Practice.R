#38
penguins <- read.csv("C:/Users/fifea/Downloads/penguins.csv")
hist(penguins$flipper_length_mm)

#39
mean(penguins$bill_length_mm, na.rm = TRUE)

#40
median(penguins$bill_length_mm, na.rm = TRUE)

#41
sd(penguins$bill_length_mm, na.rm = TRUE)

#42
IQR(penguins$bill_length_mm, na.rm = TRUE)

#43
hist(penguins$body_mass_g)

#44
set.seed(201)
hist(rbeta(1000, 2, 7)*100, main='', xlab='x')

#45
lambda <- 8
ppois(10, lambda)

#46
lambda <- 2
ppois(0, lambda)

#47
lambda <- 80
ppois(100, lambda, lower.tail = FALSE)

#48
mean <- 192
st_dev <- 6.5
pnorm(200, mean, st_dev, lower.tail = FALSE)

#49
pnorm(200, mean, st_dev) - pnorm(180, mean, st_dev)

#50
qnorm(0.1, mean, st_dev)

#51
qnorm(0.9, mean, st_dev)

#52
n <- 50
p <- 0.05
pbinom(0, n, p)

#53
pbinom(5, n, p, lower.tail = FALSE)

#54
pbinom(5, n, p) - pbinom(1, n, p)
