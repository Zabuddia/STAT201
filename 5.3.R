CISample <- function(n, p, mean, st_dev, question_num) {
  t <- qt((1 - p)/2, n - 1, lower.tail=FALSE)
  ll <- mean - (t * (st_dev/sqrt(n)))
  ul <- mean + (t * (st_dev/sqrt(n)))
  cat("Question ", question_num, ": ", c(ll,ul))
}

FindT <- function(p, n) {
  t <- qt((1 - p)/2, n - 1, lower.tail=FALSE)
  return (t)
}

ValidDataSet <- function(data_set) {
  mean <- mean(data_set)
  st_dev <- sd(data_set)
  First_Q <- quantile(data_set, probs = 0.25)
  Third_Q <- quantile(data_set, probs = 0.75)
  IQR <- Third_Q - First_Q
  lower_bound <- First_Q - 1.5*IQR
  upper_bound <- Third_Q + 1.5*IQR
  if (any(data_set < lower_bound | data_set > upper_bound)) {
    return("Not valid")
  } else {
    return("Valid")
  }
}

CIfunction<-function(xbar,sd,n,level,dist, question_num){
  
  if (dist=="t") {
    
    multi<-qt((1-level)/2,n-1,lower.tail=F)
    
  } else if 
  
  (dist=="z") {
    
    multi<-qnorm((1-level)/2,lower.tail=F)
    
  } else {multi<-c()
  
  }
  
  ll<-xbar-multi*sd/sqrt(n)
  
  ul<-xbar+multi*sd/sqrt(n)
  
  cat("Question ", question_num, ": ", c(ll,ul))
  
}

#Exercise 5
#I have 3 different ways to do it
n <- 5
p <- 0.95
list <- c(3.32, 2.53, 3.45, 2.38, 3.01)
m <- mean(list)
s <- sd(list)
t <- qt((1-p)/2, n - 1)
ll <- m - (t * (s/sqrt(n)))
ul <- m + (t * (s/sqrt(n)))
cat("Question 1: ", ll)
cat("Question 2: ", ul)
CISample(n, p, m, s, "1&2")
CIfunction(m, s, n, p, "t", "1&2")

p <- 0.98
CISample(n, p, m, s, "3&4")

#Exercise 1
p <- 0.9
n <- 12
cat("Question 5: ", FindT(p, n))
p <- 0.95
n <- 7
cat("Question 6: ", FindT(p, n))
p <- 0.99
n <- 3
cat("Question 7: ", FindT(p, n))
p <- 0.95
n <- 29
cat("Question 8: ", FindT(p, n))

#Exercise 7, 8, 9
dataSet_1 <- c(204.9, 206.1, 203.9, 207.0, 203.5, 206.3, 203.5, 206.7, 205.8)
dataSet_2 <- c(1.317, 1.318, 1.301, 1.307, 1.374, 1.323)
dataSet_3 <- c(0.16, 0.73, 0.58, 0.87, -0.46, 1.97, 0.72, 1.42, 1.49, 0.41)
cat("Dataset #1: ", ValidDataSet(dataSet_1))
cat("Dataset #2: ", ValidDataSet(dataSet_2))
cat("Dataset #3: ", ValidDataSet(dataSet_3))

#Exercise 14
n <- 10
mean <- 6.59635
st_dev <- 0.11213
p <- 0.99
cat("Question 11: ", n-1)
CISample(n, p, mean, st_dev, "12&13")

#Exercise 10
n <- 15
p <- 0.99
mean <- 13
st_dev <- 2
CISample(n, p, mean, st_dev, "14&15")

#Exercise 12
n <- 15
p <- 0.95
mean <- 2.64
st_dev <- 1.02
CISample(n, p, mean, st_dev, "16&17")