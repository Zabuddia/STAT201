#Exercise 9
answer <- qf(0.95, df1 = 7, df2 = 20)
print(paste("Question 1: ", round(answer, 3)))

#Exercise 10
answer <- qf(0.99, df1 = 2, df2 = 5)
print(paste("Question 2: ", round(answer, 3)))

#Exercise 11
fts <- 7.46
df1 <- 5
df2 <- 7
answer <- pf(7.46, df1, df2, lower.tail = FALSE)
print(paste("Question 3: ", round(answer, 3)))
answer <- pf(7.46, df1, df2, lower.tail = FALSE) * 2
print(paste("Question 4: ", round(answer, 3)))

#Exercise 12a
data <- read.csv("C:/Users/fifea/Downloads/6-11-12.csv")
var.test(data$Day.2, data$Day.1, alternative = "greater", conf.level = 0.95)
answer <- qf(0.05, 12, 12, lower.tail = FALSE)
print(paste("Question 7: ", round(answer, 3)))

#Exercise 13
data <- read.csv("C:/Users/fifea/Downloads/6-11-13.csv")
var.test(data$Brand.B, data$Brand.A)

#Exercise 12b
data <- read.csv("C:/Users/fifea/Downloads/6-11-12.csv")
var.test(data$Day.3, data$Day.2, alternative = "greater")
