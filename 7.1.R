#Compute the Correlation Coefficient

xA <- c(10.2, 8.4, 12.9, 10.2, 13, 5.9, 8.9, 10.7, 10.9, 8.2)
yA <- c(20.2, 14.9, 24.6, 14.1, 17.3, 8.4, 13.6, 14.1, 16.5, 10.5)

xB <- c(9.4,	10.2,	9.1,	9.2,	9.5,	9.1, 9.6,	9.9,	10.5, 10.4)
yB <- c(19.0,	20.8,	22.4,	21.8,	20.0,	21.6,	19.6,	19.8,	21.4,	19.8)

xC <- c(16.3,	13.4,	9.2,	16.3,	19.8,	9.4,	14.2,	17.1,	17.4,	13.1)
yC <- c(15.0,	13.4,	16.1,	12.7,	17.1,	7.6,	12.2,	12.7,	14.8,	9.5)

corrA <- cor(xA, yA)
corrB <- cor(xB, yB)
corrC <- cor(xC, yC)

print(paste("Quesiton 10: ", round(corrA, 3)))
print(paste("Quesiton 11: ", round(corrB, 3)))
print(paste("Quesiton 12: ", round(corrC, 3)))

plot(xA, yA, main="Dataset A Scatterplot", xlab="xA", ylab="yA", pch=19)
plot(xB, yB, main="Dataset B Scatterplot", xlab="xB", ylab="yB", pch=19)
plot(xC, yC, main="Dataset C Scatterplot", xlab="xC", ylab="yC", pch=19)
