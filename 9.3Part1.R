#Exercise 12 part one
data <- read.csv("C:/Users/fifea/Downloads/9-3-12.csv")
mean_sorption_by_concentration <- aggregate(Sorption ~ Concentration, data = data, FUN = mean)
print(mean_sorption_by_concentration)

mean_sorption_by_concentration_and_ratio <- aggregate(Sorption ~ Concentration + Ratio, data = data, FUN = mean)
print(mean_sorption_by_concentration_and_ratio)

#Exercise 12 part two
Ratio <- factor(data$Ratio)
Concentration <- factor(data$Concentration)
Sorption <- data$Sorption

dataset <- data.frame(Sorption, Concentration, Ratio)
summary(dataset)

tapply(dataset$Sorption, list(dataset$Concentration, dataset$Ratio), mean)
tapply(dataset$Sorption, list(dataset$Concentration), mean)
tapply(dataset$Sorption, list(dataset$Ratio), mean)

tapply(dataset$Sorption, list(dataset$Concentration, dataset$Ratio), var)
tapply(dataset$Sorption, list(dataset$Concentration), var)
tapply(dataset$Sorption, list(dataset$Ratio), var)

fitl <- lm(Sorption ~ Concentration * Ratio, data = dataset)
anova(fitl)

#anova_model <- aov(Sorption ~ Concentration * Ratio, data = data)
#summary(anova_model)
#plot(anova_model)