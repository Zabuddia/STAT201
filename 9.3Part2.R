#Exercise 4
I <- 4
J <- 3
K <- 5
dfA <- I - 1
dfCP <- J - 1
dfACP <- (I - 1) * (J - 1)
dfE <- I * J * (K - 1)
dfT <- I * J * K - 1
SSA <- 155.7
SSCP <- 287.9
SSACP <- 156.7
SST <- 997.3
SSE <- SST - SSA - SSCP - SSACP
MSA <- SSA / dfA
MSCP <- SSCP / dfCP
MSACP <- SSACP / dfACP
MSE <- SSE / dfE
FA <- MSA / MSE
FCP <- MSCP / MSE
FACP <- MSACP / MSE
PA <- pf(FA, dfA, dfE, lower.tail = FALSE)
PCP <- pf(FCP, dfCP, dfE, lower.tail = FALSE)
PACP <- pf(FACP, dfACP, dfE, lower.tail = FALSE)
a <- dfACP
b <- dfE
c <- SSE
d <- MSA
e <- MSE
f <- FCP
g <- PACP
cat(a, "\n", b, "\n", c, "\n", d, "\n", e, "\n", f, "\n", g)

#Exercise 14
Ex9_3_14 <- read.table("C:/Users/fifea/Downloads/9-3-14.csv", header = TRUE, sep = ",", 
                       na.strings = "NA", dec = ".", strip.white = TRUE)

with(Ex9_3_14, (tapply(Hardness, list(A, B), mean, na.rm = TRUE)))

fit<-lm(Hardness~factor(A)*factor(B),data=Ex9_3_14)
anova(fit)

with(Ex9_3_14, (tapply(Hardness, list(A), mean, na.rm = TRUE)))
with(Ex9_3_14, (tapply(Hardness, list(B), mean, na.rm = TRUE)))

I <- 3
J <- 3
K <- 6
dfE <- 45
MSE <- 6116
tE <- qt(0.025, dfE, lower.tail = FALSE)
XbarA <- 777.1667
XbarB <- 695.3333
CIA <- tE * sqrt(MSE / (J * K))
CIB <- tE * sqrt(MSE / (I * K))
CIA_lower <- XbarA - CIA
CIA_upper <- XbarA + CIA
CIB_lower <- XbarB - CIB
CIB_upper <- XbarB + CIB

print(paste("Questions 16-17: ", CIA_lower, ", ", CIA_upper))
print(paste("Questions 18-19: ", CIB_lower, ", ", CIB_upper))