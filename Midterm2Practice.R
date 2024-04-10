#33-38
Tts <- function(Xbar, Ybar, sdX, sdY, nX, nY) {
  (Xbar - Ybar) / sqrt((sdX^2 / nX)+(sdY^2 / nY))
}

Df <- function(sdX, sdY, nX, nY) {
  ((sdX^2/nX) + (sdY^2/nY))^2 / (((sdX^2/nX)^2 / (nX-1)) + ((sdY^2/nY)^2 / (nY-1)))
}

Drugs <- read.csv("C:/Users/fifea/Downloads/DrugEfficacy.csv")

mean_a <- mean(Drugs$Reduction[Drugs$Drug == 'A'], na.rm = TRUE)
mean_b <- mean(Drugs$Reduction[Drugs$Drug == 'B'], na.rm = TRUE)

st_dev_a <- sd(Drugs$Reduction[Drugs$Drug == 'A'], na.rm = TRUE)
st_dev_b <- sd(Drugs$Reduction[Drugs$Drug == 'B'], na.rm = TRUE)

n_a <- 16
n_b <- 15

print(round(mean_a, 3))
print(round(mean_b, 3))

tts <- t.test(Drugs$Reduction[Drugs$Drug == 'A'], Drugs$Reduction[Drugs$Drug == 'B'])
#tts <- Tts(mean_a, mean_b, st_dev_a, st_dev_b, n_a, n_b)

df <- Df(st_dev_a, st_dev_b, n_a, n_b)

print(round(tts$statistic, 4))

#If tts is positive, do lower.tail = TRUE
p_value <- 2 * pt(tts$statistic, df)

print(p_value)

t_result <- t.test(Drugs$Reduction[Drugs$Drug == 'A'], Drugs$Reduction[Drugs$Drug == 'B'], conf.level = 0.96, var.equal = TRUE)

print(t_result$conf.int)

#39-47
TtsMatched <- function(Xbar, Ybar, sdX, sdY, n) {
  (Xbar - Ybar) / sqrt((sdX^2 / n) + (sdY^2 / n)) 
}

cross <- c(23.5, 12, 21, 22, 19.125, 21.5, 22.125, 20.375, 18.25, 21.625, 
           23.25, 21, 22.125, 23, 12)
self <- c(17.375, 20.375, 20, 20, 18.375, 18.625, 18.625, 15.25, 16.5, 
          18, 16.25, 18, 12.75, 15.5, 18)

mean_x <- mean(cross)
mean_y <- mean(self)
st_dev_x <- sd(cross)
st_dev_y <- sd(self)
n <- 15

df_matched <- n - 1
print(df_matched)

ttsm <- t.test(cross, self, paired = TRUE)
print(round(ttsm$statistic, 3))

alpha <- 0.05
criticalValue <- qt(1 - alpha, df_matched)
print(round(criticalValue, 3))

p_value_matched <- pt(ttsm$statistic, df, lower.tail = FALSE)
print(p_value_matched)

ttsmc <- t.test(cross, self, paired = TRUE, conf.level = .90)

print(ttsmc$conf.int)

#48-56
Offer <- read.csv("C:/Users/fifea/Downloads/CashOffer.csv")

fit1 <- lm(Offer$Cash ~ Offer$Age, data = Offer)
anova(fit1)

data <- data.frame(
  Elderly = c(23, 20, 25, 21, 22, 23, 21, 20, 19, 20, 22, 21),
  Middle = c(28, 27, 27, 29, 26, 29, 27, 30, 28, 27, 26, 29),
  Young = c(23, 25, 21, 22, 21, 22, 20, 23, 19, 22, 19, 21)
)

# Elderly
ci_elderly <- t.test(data$Elderly, conf.level = 0.95)$conf.int

# Middle
ci_middle <- t.test(data$Middle, conf.level = 0.95)$conf.int

# Young
ci_young <- t.test(data$Young, conf.level = 0.95)$conf.int

# Print the confidence intervals
print(ci_elderly)
print(ci_middle)
print(ci_young)

#57-64
Drive <- read.csv("C:/Users/fifea/Downloads/DiskDrive.csv")

mean(Drive$Minutes)

tapply(Drive$Minutes, list(Drive$Technician), mean)

#Do the average time for technician 2 minus the average time for everyone

tapply(Drive$Minutes, list(Drive$Make), mean)

tapply(Drive$Minutes, list(Drive$Make, Drive$Technician), mean)

fitl <- lm(Drive$Minutes ~ factor(Drive$Make) * factor(Drive$Technician), data = dataset)
anova(fitl)