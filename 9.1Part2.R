#Exercise 2
data <- read.csv("C:/Users/fifea/Downloads/9-1-2.csv")

Control <- data$Control
Deficient <- data$Deficient
Slow_release <- data$Slow.release

dataset0 <- data.frame(Control, Deficient, Slow_release)
dataset <- stack(dataset0)
names(dataset) <- c("TreeHeight", "FertilizerType")

fit1 <- lm(TreeHeight ~ FertilizerType, data = dataset)
anova(fit1)

#Another way to do it:
#long_data <- pivot_longer(data, cols = c(Control, Deficient, 'Slow.release'), names_to = "FertilizerType", values_to = "TreeHeight")
#fit1 <- lm(TreeHeight ~ FertilizerType, data=long_data)
#anova(fit1)

fit2 <- lm(TreeHeight ~ FertilizerType - 1, data = dataset)
confint(fit2)

#Exercise 6
data <- read.csv("C:/Users/fifea/Downloads/9-1-6.csv")

ZeroToEleven <- data$X0.to.11
TwelveToTwentyfour <- data$X12.to.24
TwentyfiveToFourtyfive <- data$X25.to.45
FourtyfiveAndUp <- data$X45.

dataset0 <- data.frame(ZeroToEleven, TwelveToTwentyfour, TwentyfiveToFourtyfive, FourtyfiveAndUp)
dataset <- stack(dataset0)
names(dataset) <- c("CarbonRatio", "AgeGroup")

fit1 <- lm(CarbonRatio ~ AgeGroup, data = dataset)
anova(fit1)

df1 <- 3
df2 <- 45
critical_value <- qf(0.95, df1, df2)
print(paste("Question 22: ", round(critical_value, 3)))

#Exercise 10
a <- 176.482 / 3
print(paste("Question 24: ", round(a, 3)))
b <- 19 - 3
print(paste("Question 25: ", b))
c <- 235.958 - 176.482
print(paste("Question 26: ", c))
d <- c / b
print(paste("Question 27: ", d))
e <- a / d
print(paste("Quesiton 28: ", round(e, 3)))