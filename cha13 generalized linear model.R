### Chapter13: generalized linear models ###
# Generalized linear models extend the linear-model framework to include dependent variables that are decidedly non-normal.

# 1. logistic regression; assume Y ~ binomial
# The regression coefficients give the change in log(odds) in the response for a unit change in the predictor variable
library(AER)
data(Affairs, package="AER")
# assign factors to affairs variable
Affairs$ynaffair <- ifelse(Affairs$affairs > 0, 1, 0)
Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels=c(0,1), 
                           labels=c("No","Yes"))
table(Affairs$ynaffair)
# regression on all variables
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation +rating, 
                data=Affairs, family=binomial())
summary(fit.full)
# regression on partly variables
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                     rating, data=Affairs, family=binomial())
summary(fit.reduced)
# test fitness of two models
anova(fit.reduced, fit.full, test="Chisq")
# test for overdispersion of response variable; if p < 0.05 then overdispersion
fit <- glm(ynaffair ~ age + yearsmarried + religiousness + 
             rating, family = binomial(), data = Affairs)
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                rating, family = quasibinomial(), data = Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual, 
       fit$df.residual, lower = F)


# 2. Poisson regression: assume Y ~ Poisson
data(epilepsy, package="robustbase")
fit1 <- glm(Ysum ~ Base + Age + Trt, data=epilepsy, family=poisson())
summary(fit1)
coef(fit1)
exp(coef(fit1)) # to demonstrate scale effect
# test for overdispersion of response variable again
library(qcc) 
qcc.overdispersion.test(epilepsy$Ysum, type="poisson")
