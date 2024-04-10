#Exercise 4

Ex9_5_4 <- read.csv("C:/Users/fifea/Downloads/9-5-4.csv")

# You need to make sure that the types of variables 
# A, B and C are factors. If not, then you need to 
# coerce the type of each to be factor. To find 
# out the type of a varaible, use the "class()" function.

class(Ex9_5_4$A)
class(Ex9_5_4$B)
class(Ex9_5_4$C)

# The function "factor()" was used to coerce
# the typle of a variable to be factor.

Ex9_5_4$A<-factor(Ex9_5_4$A)
Ex9_5_4$B<-factor(Ex9_5_4$B)
Ex9_5_4$C<-factor(Ex9_5_4$C)

# Assign the fitted model to "fit.1". (you can name 
# the model whichever the way you like.)

fit.1<-lm(Yield~(A+B+C)^3, data=Ex9_5_4)

# Obtain the ANOVA table.  

anova(fit.1)

# To obtain treatment means, level means or grand mean, you may do the following, 
# replace "mean" by "sd" will get you standard deviations instead. 

with(Ex9_5_4, (tapply(Yield, list(A, B, C), mean, na.rm = TRUE)))
with(Ex9_5_4, (tapply(Yield, list(A, B), mean, na.rm = TRUE)))
with(Ex9_5_4, (tapply(Yield, list(A, C), mean, na.rm = TRUE)))
with(Ex9_5_4, (tapply(Yield, list(B, C), mean, na.rm = TRUE)))
with(Ex9_5_4, (tapply(Yield, list(A), mean, na.rm = TRUE)))
with(Ex9_5_4, (tapply(Yield, list(B), mean, na.rm = TRUE)))
with(Ex9_5_4, (tapply(Yield, list(C), mean, na.rm = TRUE)))

mean(Ex9_5_4$Yield)


# If you would ever consider to obtain interaction plots, 
# you may try to exlore more about the funtion "interaction.plot()".

mean_b_negative <- 0.51875
mean_b_positive <- 0.32
main_effect_b <- mean_b_positive - mean_b_negative

print(paste("Question 1: ", main_effect_b))

both_pos <- 0.3925
both_neg <- 0.4225
mean_same <- both_pos + both_neg
a_pos_c_neg <- 0.4625
c_pos_a_neg <- 0.4
mean_diff <- a_pos_c_neg + c_pos_a_neg
interaction_effect_a_c <- (mean_same - mean_diff) / 2

print(paste("Quesiton 2: ", interaction_effect_a_c))



#Another way to do it
Dataset<-read.csv("C:/Users/fifea/Downloads/9-5-4.csv")
Dataset

# Just want to be safe if you would like to # 
Dataset$A<-factor(Dataset$A)
Dataset$B<-factor(Dataset$B)
Dataset$C<-factor(Dataset$C)

# fit the model #

fit<-lm(Yield~A*B*C,data=Dataset,model=TRUE)
anova(fit)

# Find the contrast #
lsm = emmeans(fit, ~A*B*C)
lsm

Contrasts = list(A    = c(1,-1,1,-1,1,-1,1,-1),
                 B  = c(1,1,-1,-1,1,1,-1,-1),
                 C      = c(1,1,1,1,-1,-1,-1,-1),
                 InterAB        = c(1,-1,-1,1,1,-1,-1,1),
                 InterAC        = c(1,-1,1,-1,-1,1,-1,1),
                 InterBC        = c(1,1,-1,-1,-1,-1,1,1),
                 InterABC       = c(1,-1,-1,1,-1,1,1,-1))

contrast(lsm, Contrasts)

# Find all the effects #

a<-as.data.frame(contrast(lsm, Contrasts))
a$estimate/4

# Or use the following codes to find all effects #

#Find the main effect# 

effect = list(PlusvsMinus    = c(1,-1))

lsm1 = emmeans(fit, ~A)
contrast(lsm1, effect)  

lsm2 = emmeans(fit, ~B)
contrast(lsm2, effect)  

lsm3 = emmeans(fit, ~C)
contrast(lsm3, effect) 

#Find the 2-way interacion effect# 

effect2way= list(PlusvsMinus    = c(0.5,-0.5,-0.5,0.5))

lsm4 = emmeans(fit, ~A*B)
contrast(lsm4, effect2way) 

lsm5 = emmeans(fit, ~A*C)
contrast(lsm5, effect2way) 

lsm6 = emmeans(fit, ~B*C)
contrast(lsm6, effect2way) 

#Find the 3-way interacion effect# 

effect3way= list(PlusvsMinus    = c(0.25,-0.25,-0.25,0.25,-0.25,0.25,0.25,-0.25))

contrast(lsm, effect3way) 

#Exercise 12
Dataset<-read.csv("C:/Users/fifea/Downloads/9-S-12.csv")
Dataset

# Just want to be safe if you would like to # 
Dataset$A<-factor(Dataset$A)
Dataset$B<-factor(Dataset$B)
Dataset$C<-factor(Dataset$C)

# fit the model #

fit<-lm(Thickness~A*B*C,data=Dataset,model=TRUE)
anova(fit)

# Find the contrast #
lsm = emmeans(fit, ~A*B*C)
lsm

Contrasts = list(A    = c(1,-1,1,-1,1,-1,1,-1),
                 B  = c(1,1,-1,-1,1,1,-1,-1),
                 C      = c(1,1,1,1,-1,-1,-1,-1),
                 InterAB        = c(1,-1,-1,1,1,-1,-1,1),
                 InterAC        = c(1,-1,1,-1,-1,1,-1,1),
                 InterBC        = c(1,1,-1,-1,-1,-1,1,1),
                 InterABC       = c(1,-1,-1,1,-1,1,1,-1))

contrast(lsm, Contrasts)

# Find all the effects #

a<-as.data.frame(contrast(lsm, Contrasts))
a$estimate/4

# Or use the following codes to find all effects #

#Find the main effect# 

effect = list(PlusvsMinus    = c(1,-1))

lsm1 = emmeans(fit, ~A)
contrast(lsm1, effect)  

lsm2 = emmeans(fit, ~B)
contrast(lsm2, effect)  

lsm3 = emmeans(fit, ~C)
contrast(lsm3, effect) 

#Find the 2-way interacion effect# 

effect2way= list(PlusvsMinus    = c(0.5,-0.5,-0.5,0.5))

lsm4 = emmeans(fit, ~A*B)
contrast(lsm4, effect2way) 

lsm5 = emmeans(fit, ~A*C)
contrast(lsm5, effect2way) 

lsm6 = emmeans(fit, ~B*C)
contrast(lsm6, effect2way) 

#Find the 3-way interacion effect# 

effect3way= list(PlusvsMinus    = c(0.25,-0.25,-0.25,0.25,-0.25,0.25,0.25,-0.25))

contrast(lsm, effect3way) 

joint_tests(lsm)