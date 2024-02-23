#Function the teacher gave us
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
CIfunction_2<-function(xbar,sd,n,level,dist, question_num){
  
  if (dist=="t") {
    
    multi<-qt((1-level)/2,n-1,lower.tail=F)
    
  } else if 
  
  (dist=="z") {
    
    multi<-qnorm((1-level)/2,lower.tail=F)
    
  } else {multi<-c()
  
  }
  
  ll<-xbar-multi*sd/sqrt(n)
  
  ul<-xbar+multi*sd/sqrt(n)
  
  cat("Question ", question_num, ": ", (ul - ll) / 2)
  
}
CLfunction<-function(mean, sd, n, ll, ul, question_num) {
  ql <- (mean - ll) / (sd / sqrt(n))
  qu <- (ul - mean) / (sd / sqrt(n))
  if (ql != qu) {
    print("Faulty bounds")
  } else {
    p <- pnorm(ql, lower.tail = FALSE)
    answer <- 1 - (p * 2)
    cat("Question ", question_num, ": ", answer)
  }
}

CLfunction_2<-function(mean, sd, n, uncertainty) {
  q <- uncertainty / (sd / sqrt(n))
  p <- pnorm(q, lower.tail=FALSE)
  answer <- 1 - (p * 2)
  return(answer)
}

#Exercise 1
p <- 1 - 0.95
answer <- qnorm(p/2, lower.tail = FALSE)
cat("Question 1: ", answer)

p <- 1 - 0.98
answer <- qnorm(p/2, lower.tail = FALSE)
cat("Question 2: ", answer)

p <- 1 - 0.99
answer <- qnorm(p/2, lower.tail = FALSE)
cat("Question 3: ", answer)

p <- 1 - 0.80
answer <- qnorm(p/2, lower.tail = FALSE)
cat("Question 4: ", answer)

#Exercise 2
q <- 2.17
p <- pnorm(q, lower.tail = FALSE)
answer <- 1 - (p * 2)
cat("Question 5: ", answer)

q <- 3.28
p <- pnorm(q, lower.tail = FALSE)
answer <- 1 - (p * 2)
cat("Question 6: ", answer)

#Exercise 8
mean <- 348.2
st_dev <- 5.1
n <- 68
sample_st_dev <- st_dev / sqrt(n)

p <- 1 - 0.90
q <- qnorm(p/2, lower.tail = FALSE)
ll <- mean - q * sample_st_dev
ul <- mean + q * sample_st_dev
cat("Question 7: ", ll)
cat("Quesiton 8: ", ul)

#Using the function makes it easier. The code above does the same thing
CIfunction(mean, st_dev, n, 0.90, "z", "7&8")

CIfunction(mean, st_dev, n, 0.95, "z", "9&10")

#I made my own function for this and defined it above
CLfunction(mean, st_dev, n, 347.5, 348.9, "11")

uncertainty <- 0.8
#I made another function for these questions and it is defined above
answer <- uniroot(function(N) CLfunction_2(mean, st_dev, N, uncertainty) - 0.9, lower = 0, upper = 1000)$root
cat("Question 12: ", answer)

answer <- uniroot(function(N) CLfunction_2(mean, st_dev, N, uncertainty) - 0.95, lower = 0, upper = 1000)$root
cat("Question 13: ", answer)

CIfunction_2(mean, st_dev, 110, 0.90, "z", "12check")
CIfunction_2(mean, st_dev, 157, 0.95, "z", "13check")

#Exercise 9
n <- 80
mean <- 1.56
sd <- 0.1

CIfunction(mean, sd, n, 0.95, "z", "14&15")

uncertainty <- 0.01
answer <- uniroot(function(N) CLfunction_2(mean, sd, N, uncertainty) - 0.98, lower = 0, upper = 10000)$root
cat("Question 16: ", answer)
CIfunction_2(mean, sd, 542, 0.98, "z", "16check")

#Exercise 10
n <- 60
mean <- 85
sd <- 2

CIfunction(mean, sd, n, 0.995, "z", "17&18")

uncertainty <- 0.35
answer <- uniroot(function(N) CLfunction_2(mean, sd, N, uncertainty) - 0.995, lower = 0, upper = 10000)$root
cat("Question 19: ", answer)
CIfunction_2(mean, sd, 258, 0.995, "z", "19check")

answer <- uniroot(function(N) CLfunction_2(mean, sd, N, uncertainty) - 0.95, lower = 0, upper = 10000)$root
cat("Question 20: ", answer)
CIfunction_2(mean, sd, 126, 0.95, "z", "20check")