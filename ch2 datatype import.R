### chapter 2: Data Types in R and importing ###

# 1. Vectors
a <- c("k", "j", "h", "a", "c", "m")
a[c(1, 3, 5)]


# 2. Matrix: a two-dimensional array
x <- matrix(1:10, nrow=2)
cells <- c(1, 26, 24, 68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")

y <- matrix(cells, nrow=2, ncol=2, byrow = TRUE, 
            dimnames=(list(rnames, cnames)))


# 3. Array: multi-dimensional


# 4. DataFrame: multi-dimensional, multi-data-type components
patientID <- c(1,2,3,4)
View(x1)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status <- c("Poor", "Imporved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
# selecting data
patientdata[1:2]
patientdata[c("diabetes", "status")]
patientdata$age
table(patientdata$diabetes, patientdata$status)
# describe data frame
str(patientdata)
summary(patientdata)
# with() refers to a data frame
with(mtcars, { 
  summary(mpg) 
  plot(mpg, disp) 
  plot(mpg, wt) 
})


# 5. list: an ordered collection of objects
g <- 'my first list'
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist1 <- list(title=g, ages=h, ID=list(j, k))
mylist1[["ID"]]

mylist2 <- list(title=g, ages=h, ID=j, k)
mylist2[["ID"]]
mylist2[4]


# 6. Tibbles: data frames that have specialized behaviors
library(tibble)
mtcars <- as_tibble(mtcars)
mtcars


# 7. Some examples of inputting data
setwd("D:/學習/R/R语言实战")
grades <- read.table("studentgrade.csv", header=TURE, row.names="StudentID", sep=",")

install.packages("RODBC")
library(RODBC)
myconn <-odbcConnect("mydsn", uid="Rob", pwd="aardvark") 
crimedat <- sqlFetch(myconn, Crime) 
pundat <- sqlQuery(myconn, "select * from Punishment") 
close(myconn)























