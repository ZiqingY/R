### Chapter 12: Resampling statistics and bootstrapping ###
# see definitions in code list

install.packages(c("coin", "lmPerm"))
library(coin)
library(lmPerm)

## Permutation tests with the coin package
# t-test vs. one-way permutation test for the hypothetical data: latter test is not significant
score <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65) 
treatment <- factor(c(rep("A",5), rep("B",5))) 
mydata <- data.frame(treatment, score) 
t.test(score~treatment, data=mydata, var.equal=TRUE)

oneway_test(score~treatment, data=mydata, distribution="exact")

# Wilcoxon–Mann–Whitney U test
library(MASS)
UScrime$So <- factor(UScrime$So)
wilcox_test(Prob ~ So, data=UScrime, distribution="exact")

# a k-sample test: 9,999 permutations of the data
library(multcomp) 
set.seed(1234) 
oneway_test(response~trt, data=cholesterol, 
            distribution=approximate(nresample=9999))

# Independence between paregoric or numeric variables
library(coin) 
library(vcd) 
Arthritis <- transform(Arthritis, Improved=as.factor(as.numeric(Improved))) 
set.seed(1234) 
chisq_test(Treatment~Improved, data=Arthritis, 
             distribution=approximate(nresample=9999))

library(MASS) 
wilcoxsign_test(U1~U2, data=UScrime, distribution="exact")


## Permutation tests with the lmPerm package
# tests for simple regression and polynomial regression
install.packages("lmPerm")
library(lmPerm)
set.seed(1234) 
fit <- lmp(weight~height, data=women, perm="Prob")
summary(fit)

set.seed(1234) 
fit1 <- lmp(weight~height + I(height^2), data=women, perm="Prob")
summary(fit1)

# test for multiple regression
set.seed(1234)
states <- as.data.frame(state.x77) 
fit2 <- lmp(Murder~Population + Illiteracy+Income+Frost, 
             data=states, perm="Prob") 
summary(fit2)

# One-way ANOVA and ANCOVA
library(multcomp)
set.seed(1234) 
fit3 <- aovp(response~trt, data=cholesterol, perm="Prob")
anova(fit3)

set.seed(1234) 
fit4 <- aovp(weight ~ gesttime + dose, data=litter, perm="Prob") 
anova(fit4)

# Two-way ANOVA
set.seed(1234)
fit5 <- aovp(len~supp*dose, data=ToothGrowth, perm="Prob")
anova(fit5)


# Bootstrapping with the boot package: single or multiple statistics
install.packages("boot")
