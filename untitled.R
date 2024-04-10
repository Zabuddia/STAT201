#2-6
lap_time <- c(56, 40, 42, 65, 48, 59, 57, 63, 59, 49, 62, 50, 48, 52, 61, 54, 60)

t.test(lap_time, mu = 59, alternative = "less")

qt(0.1, df = 17 - 1, lower.tail = TRUE)

t.test(lap_time, mu = 59)

#7-14
Oil <- read.csv("C:/Users/fifea/Downloads/FishOil.csv")

t.test(Oil$FishOil, Oil$StandardOil, alternative = "greater")

t.test(Oil$FishOil, Oil$StandardOil, conf.level = 0.98)

var.test(Oil$FishOil, Oil$StandardOil)

#15-17
Diet <- read.csv("C:/Users/fifea/Downloads/Diet.csv")

t.test(Diet$Before, Diet$After, paired = TRUE, alternative = "greater")

#18-29
Productivity <- read.csv("C:/Users/fifea/Downloads/ProductivityImprovement.csv")

fit <- lm(Productivity$Improvement ~ factor(Productivity$Level))
anova(fit)

fit <- lm(Productivity$Improvement ~ factor(Productivity$Level) - 1)
confint(fit)

#31-42
Sap <- read.csv("C:/Users/fifea/Downloads/SapProduction1.csv")

mean(Sap$Sap.Production)

tapply(Sap$Sap.Production, Sap$Fertilizer, mean)

tapply(Sap$Sap.Production, Sap$Zone, mean)

tapply(Sap$Sap.Production, list(Sap$Fertilizer, Sap$Zone), mean)

fit <- lm(Sap$Sap.Production ~ factor(Sap$Fertilizer) * factor(Sap$Zone))
anova(fit)