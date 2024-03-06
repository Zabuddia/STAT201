#Exercise 2
data <- read.csv("C:/Users/fifea/Downloads/5-7-2.csv")
t.test(data$Difference, conf.level=0.95)
#t.test(data$Before, data$After, paired=TRUE, conf.level=0.95)

#Exercise 6
data <- read.csv("C:/Users/fifea/Downloads/5-7-6.csv")
t.test(data$Hot, data$Cold, paired = TRUE, conf.level = 0.98)

#Supplementary
data <- read.csv("C:/Users/fifea/Downloads/6-8-supplementary.csv")
t.test(data$Difference, alternative = "greater")
t.test(data$Ausculatory, data$Oscillatory, paired = TRUE, alternative = "greater")

#Exercise 4
data <- read.csv("C:/Users/fifea/Downloads/6-8-4.csv")
t.test(data$X40.degrees, data$X80.degrees, paired = TRUE)
df <- 9
critical_value <- qt(0.05/2, df, lower.tail = FALSE)
print(paste("Question 14: ", round(critical_value, 3)))

#Exercise 10
data <- read.csv("C:/Users/fifea/Downloads/6-8-10.csv")
t.test(data$Before, data$After, paired = TRUE, mu = 75, alternative = "greater")