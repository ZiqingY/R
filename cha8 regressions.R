# simple regression
women
fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height, women$weight, xlab="Height (in inches)", ylab="Weight (in pounds)")
abline(fit)


# polynomial regression
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
plot(women$height, women$weight, xlab="Height (in inches)", ylab="Weight (in pounds)")
lines(women$height, fitted(fit2))


# multicovariate regression
states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", "Income", "Frost")])
states
cor(states)
install.packages("car")
library(car)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2), main="Scatter Plot Matrix")

fit3 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit3)


# multicovariate regression with interactive terms: 
fit4 <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit4)
# mpg_hat = 49.81 - 0.12hp - 8.22wt + 0.03*hp*wt
# interaction plot for hp*wt: hp covariate on wt_hat when wt=2.2, 3.2, 4.2
install.packages("effects")
library(effects)
plot(effect("hp:wt", fit4,, list(wt=c(2.2, 3.2, 4.2))), multiline=TRUE)


# confidence intervals for covariates
fit_ci <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
confint(fit_ci)


# visualizations of OLS regression assumptions  
fit <- lm(weight ~ height, data=women)
par(mfrow = c(2,2))
plot(fit)


# package 'car': test OLS 
# assumption1: NORMALITY
install.packages("car")
library(car)
fit_5 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
par(pin = c(5,4))
qqPlot(fit_5, labels=row.names(states), id=list(method="identify"), 
       simulate=TRUE, main="Q-Q Plot")
# assumption2: INDEPENDENCE OF ERRORS
durbinWatsonTest(fit_5)
# assumption3: LINEARITY
crPlots(fit_5)
# assumption4: HOMOSCEDASTICITY
ncvTest(fit_5)
spreadLevelPlot(fit_5)
# create a function to plot distribution of errors
residplot <- function(fit_5, nbreaks=10) {
              z <-rstudent(fit_5)
              hist(z, breaks=nbreaks, freq=FALSE,
                   xlab='Studentized Residual',
                   main="Distribution of Errors")
              rug(jitter(z), col="brown")
              curve(dnorm(x, mean=mean(z), sd=sd(z)),
                    add=TRUE, col="blue", lwd=2)
              lines(density(z)$x, density(z)$y,
                    col="red", lwd=2, lty=2)
              legend("topright",
                     legend = c("Normal Curve", "Kernel Density Curve"),
                     lty=1:2, col=c("blue", "red"), cex=.7)
}
residplot(fit_5)


# aggregate tests on linear model assumptions
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit_5)
summary(gvmodel)

# multi-colinearity
vif(fit_5)
sqrt(vif(fit_5)) > 2


# Testing abnormal values 
# 1. Outliers:  aren’t predicted well by the model
library(car)
outlierTest(fit_5)

# 2. High-leverage points: they have an unusual combination of predictor values
hat.plot <- function(fit) {
              p <- length(coefficients(fit))
              n <- length(fitted(fit))
              plot(hatvalues(fit), main="Index Plot of Hat Values")
              abline(h=c(2,3)*p/n, col="red", lty=2)
              identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit_5)

# 3. Influential observations: have a disproportionate impact on the values of the model parameters
cutoff <- 4/(nrow(states)-length(fit_5$coefficients-2))
plot(fit_5, which=4, cook, levels=cutoff)
abline(h=cutoff, lty=2, col="red")

# 4. Aggregating all above observations
influencePlot(fit_5, id.mehod='identify', main="Income Plot", sub="Circle size is proportional to Cook's distance")


# regressing covariates to covariates
avPlots(fit_5, ask=FALSE, id.method="identify")


# show how normalized variables can improve estimation
summary(powerTransform(states$Murder))
boxTidwell(Murder~Population+Illiteracy, data=states)


# 2 ways to compare regression models
fit_6 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit_5, fit_6)

AIC(fit1,fit2)


# selecting variables: stepwise method, all-subsets regression
install.packages("MASS")
library(MASS)
step(fit_5, direction="backward")

install.packages("leaps")
library(leaps)
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")

subsets(leaps, statistic="cp", main="Cp Plot for All Subsets Regression")
abline(1, 1, lty=2, col="red")        # the closer to red line, the better model


# Cross Validation
install.packages("bootstrap")
library(bootstrap)

shrinkage <- function(fit, k=10) {
  require(bootstrap)
  
  theta.fit <- function(x, y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}
shrinkage(fit_5)


# relative importance of covariates
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)


# relative weight method for relative importance of covariates
relweights <-
  function(fit,...){                         
    R <- cor(fit$model)   
    nvar <- ncol(R)          
    rxx <- R[2:nvar, 2:nvar] 
    rxy <- R[2:nvar, 1]      
    svd <- eigen(rxx)        
    evec <- svd$vectors                           
    ev <- svd$values         
    delta <- diag(sqrt(ev))  
    lambda <- evec %*% delta %*% t(evec)        
    lambdasq <- lambda ^ 2   
    beta <- solve(lambda) %*% rxy           
    rsquare <- colSums(beta ^ 2)                   
    rawwgt <- lambdasq %*% beta ^ 2    
    import <- (rawwgt / rsquare) * 100 
    lbls <- names(fit$model[2:nvar])   
    rownames(import) <- lbls
    colnames(import) <- "Weights"
    barplot(t(import),names.arg=lbls,
            ylab="% of R-Square",
            xlab="Predictor Variables",
            main="Relative Importance of Predictor Variables", 
            sub=paste("R-Square=", round(rsquare, digits=3)),
            ...)  
    return(import)
}
relweights(fit_5, col="blue")
