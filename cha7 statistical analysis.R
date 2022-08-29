# clear environment
rm(list = ls())


# various methods to statistically summarize the table
myvars <- c("mpg", "hp", "wt")
head(mtcars[myvars])         # select first 6 rows of the dataframe
summary(mtcars[myvars])
mtcars


# select certain statistics as output using sapply()
mystats <- function(x, na.omit=FALSE){
              if (na.omit)
                  x <- x[!is.na(x)]
              m <- mean(x)
              n <- length(x)
              s <- sd(x)
              skew <- sum((x-m)^3/s^3)/n
              kurt <- sum((x-m)^4/s^4)/n - 3
              return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
sapply(mtcars[myvars], mystats)


# using packages
install.packages("Hmisc")
library(Hmisc)
describe(mtcars[myvars])

install.packages("pastecs")
library(pastecs)
stat.desc(mtcars[myvars])

install.packages("psych")
library(psych)
describe(mtcars[myvars])        # 'psych' package also has describe()


# aggregate() selects statistics by columns
aggregate(mtcars[myvars], by=list(am=mtcars$am), mean)
aggregate(mtcars[myvars], by=list(am=mtcars$am), sd)


# select certain statistics by columns
dstats <- function(x)sapply(x, mystats)
by(mtcars[myvars], mtcars$a, dstats)


# other packages for statistics by group: 
# doBy: summaryBy(formula, data=dataframe, FUN=function)
# psych: describeBy()
install.packages('doBy')
library(doBy)
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats)
library(psych)
describeBy(mtcars[myvars], list(am=mtcars$am))


# frequency table: different dimensions; using package gmodels
library(vcd)
head(Arthritis)
mytable <- with(Arthritis, table(Improved))
prop.table(mytable)
prop.table(mytable)*100

mytable_dim2 <-xtabs(~Treatment + Improved, data=Arthritis)
margin.table(mytable_dim2, 1)
prop.table(mytable_dim2, 1)
margin.table(mytable_dim2, 2)
prop.table(mytable_dim2, 2)
prop.table(mytable_dim2)
addmargins(mytable_dim2)
addmargins(mytable_dim2,2)
addmargins(prop.table(mytable_dim2, 1) ,2)

mytable_dim3 <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable_dim3
ftable(mytable_dim3, c(1,2))

install.packages('gmodels')
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)


# Independece Test: chisq.test()  fisher.test()  mantelhaen.test()
# input is 'table', its dimension is 2*2


# Dependence Valuation: assocstats(table)


# Correlation calculation: cor(x, use= , method= )
states <- state.x77[, 1:6]
states
cor(states, method="spearman")

x <- states[, c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[, c("Life Exp", "Murder")]
cor(x, y)


# partial correlation: pcor(u,s)
install.packages("ggm")
library(ggm)
states
colnames(states)
pcor(c(5, 6, 2, 3, 1), cov(states))


# Other correlation caculation methods: e.g. hetcor() from 'polycor' package


# significance level calculation, H0 is cor=0: cor.test(x, y, alternative= , method= )
cor.test(states[, 3], states[, 5])

library(psych)
corr.test(states, use="complete")


# t-test with H0 being 'x1=x2 by y'
library(MASS)
UScrime
t.test(Prob ~ So, data=UScrime)


# t-test with H0 being 'x-y ~ Normal'
sapply(UScrime[c("U1", "U2")], function(x)(c(mean=mean(x), sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))


# Non-parametric test: Mann-Whitney U Test for H0 'x and y follows same distribution'
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data = UScrime)

with(UScrime, wilcox.test(U1, U2, paired=TRUE))


# multiple arries comparision with H0 'arries are same as one another'
kruskal.test(Illiteracy ~ state.region, data=states)

source("http://www.statmethods.net/RiA/wmc.txt")
wmc(Illiteracy ~ state.region, data=states, method='holm')
