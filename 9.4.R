#Exercise 8
data <- read.csv("C:/Users/fifea/Downloads/9-4-8.csv")
fit <- lm(Cost~Machine+factor(Job), data = data)
anova(fit)