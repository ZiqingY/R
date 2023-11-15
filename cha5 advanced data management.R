### chapter 5: advanced data management ###

# 1. Maths, statistical and prob functions (see doc for code list)

# standardize data: newdata <- scale(mydata)

# Normal Distributions
library(ggplot2)
x <- seq(from = -3, to = 3, by = 0.1) 
y = dnorm(x)
nd <- data.frame(x = x, y = y)
ggplot(nd, aes(x, y)) + 
  geom_line() + 
  labs(x = "Normal Deviate", 
       y = "Density") + 
  scale_x_continuous(breaks = seq(-3, 3, 1))

pnorm(1.96)                 # normal cdf at x=1.96

qnorm(.9, mean=500, sd=100) # 90th percentile of a normal distribution 
                            # with a mean of 500 and a standard deviation of 100
rnorm(50, mean=50, sd=10)   # 50 random normal deviates with a mean of 50 and
                            # a standard deviation of 10

# set random seed from a uniform distribution [0,1]
runif(5)
set.seed(123456)
runif(5)

# multivariate normal distribution
install.packages("MultiRNG")
library(MultiRNG)
options(digits=3)  # 3 decimals
set.seed(1234)
# specify mean
mean <- c(230.7, 146.7, 3.6)
# specify cov matrix
sigma <- matrix(c(15360.8, 6721.2, -47.1, 
                  6721.2, 4700.9, -16.5,-47.1, -16.5, 0.3), nrow=3, ncol=3)
# generate data
mydata <- draw.d.variate.normal(500, 3, mean, sigma)
mydata <- as.data.frame(mydata)
# view results
dim(mydata)
names(mydata) <- c("y","x1","x2")
head(mydata, n=5)


# An example for the use of escape characters in printing
name <- "Bob" 
cat( "Hello", name, "\b.\n", "Isn\'t R", "\t", "GREAT?\n")


## To solve a problem:  combine subject test scores into a single performance indicator for each student
options(digits=2)
# input data using vectors into data frame
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose", 
               "David Jones", "Janice Markhammer", "Cheryl Cushing", 
               "Reuven Ytzrhak", "Greg Knox", "Joel England", 
               "Mary Rayburn") 
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522) 
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86) 
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

roster1 <- data.frame(Student, Math, Science, English, 
                       stringsAsFactors=FALSE)

# generate the single indicator to the data frame
z <- scale(roster[,2:4])
score <- apply(z, 1, mean)
roster2 <- cbind(roster1, score)

# marking students with ranks
qt <- quantile(score, c(.8,.6,.4,.2))
roster2$grade <- NA
roster2$grade[score >= y[1]] <- "A"
roster2$grade[score < y[1] & score >= y[2]] <- "B"
roster2$grade[score < y[2] & score >= y[3]] <- "C"
roster2$grade[score < y[3] & score >= y[4]] <- "D"
roster2$grade[score < y[4]] <- "F"

# split names of the data frame
name <- strsplit((roster2$Student), " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)
roster3 <- cbind(Firstname,Lastname, roster2[,-1])
roster3 <- roster3[order(Lastname,Firstname),]
roster3


## 2.Repetition and looping, conditional execution (executed by each line) 
# for
for (i in 1:10) print("Hello")

# while
j <- 10 
while (j > 0) {print("Hello"); j <- j - 1}

# if else
grade <- c("year1", "year2", "year3")

if (is.character(grade)) 
  grade <- as.factor(grade)

if (!is.factor(grade)) 
  grade <- as.factor(grade) else print("Grade already is a factor")

# ifelse
ifelse(score > 0.5, print("Passed"), print("Failed")) 
outcome <- ifelse (score > 0.5, "Passed", "Failed")

# switch()
feelings <- c("sad", "afraid", "happy", "angry") 
for (i in feelings) 
  print( 
    switch(i, 
           happy = "I am glad you are happy", 
           afraid = "There is nothing to fear", 
           sad = "Cheer up", 
           angry = "Calm down now"
           )
    )


### 3. User-written functions...


### 4. Reshaping data
# transpose
cars <- mtcars[1:5,1:4]
cars
t(cars)
# use gather() and spread() to transfer between long and wide
library(tidyr)
data_wide <- data.frame(ID = c("AU", "CN", "PRK"), 
                        Country = c("Australia", "China", "North Korea"), 
                        LExp1990 = c(76.9, 69.3, 69.9), 
                        LExp2000 = c(79.6, 72.0, 65.3), 
                        LExp2010 = c(82.0, 75.2, 69.6))
data_long <- gather(data_wide, key="Variable", value="Life_Exp", 
                    c(LExp1990, LExp2000, LExp2010))
data_wide1 <- spread(data_long, key=Variable, value=Life_Exp)


### 5. Aggregating data: summrize by certain columns
# create 2 groups: by cylinders and gear
aggdata1 <-aggregate(mtcars, 
                    by=list(mtcars$cyl,mtcars$gear), 
                    FUN=mean, na.rm=TRUE)
# name the 2 groups created, and drop redundant columns
aggdata2 <-aggregate(mtcars[-c(2, 10)], 
                    by=list(Cylinders=mtcars$cyl, Gears=mtcars$gear), 
                    FUN=mean, na.rm=TRUE)
