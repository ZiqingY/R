# add new columns to a data.frame
mydata <- data.frame(x1=c(2, 2, 6, 4),
                     x2=c(3, 4, 2, 8))
mydata <- transform(mydata, 
                    sumx=x1+x2,
                    meanx=(x1+x2)/2)


# create data.frame 'leadership' and organize NA, create new columns
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/14","10/28/14", "10/01/14", "10/12/14", "05/01/14")
country <- c("US","US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 3, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors = FALSE)

leadership$age[leadership$age==99] <-NA
leadership <- within(leadership, {
                     agecat <- NA
                     agecat[age>75]              <- "Elder"
                     agecat[age >=55 & age <=75] <- "Middle Aged"
                     agecat[age < 55]            <- "Young"})


# rename columns
fix(leadership)

names(leadership)[2] <- "testDate"
names(leadership)[6:10] <- c("item1","item2","item3","item4","item5")

install.packages("plyr")
library(plyr)
leadership <- rename(leadership,
                     c(managerID="managerID", TestDate="TestDate"))


# recognize and deal with NA; distinguish NA and NaN
is.na(leadership[,6:10])

x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4]
z <- sum(x)
y
z

y1 <- sum(x, na.rm=TRUE)
y1

newleadership <- na.omit(leadership)
newleadership


# date values(default: yyyy-mm-dd)
mydates <- as.Date(c('2007-06-22', '2004-02-13'))
mydate <- as.Date(c('01/05/1965', '08/16/1975'), "%m/%d/%Y")

leadership$TestDate <- as.Date(leadership$TestDate, "%m/%d/%Y")

Sys.Date()
date()

today <- Sys.Date()
format(today, format="%B %d %Y")
format(today, format="%A")

days=mydate[1]-mydates[1]
days

difftime(mydate[1], mydates[1], units="weeks")

strtoday <- as.character(today)
strtoday


# determine variable type
a <- c(1, 2, 3)
is.numeric(a)
is.vector(a)
is.character(a)


# order data in data.frame
newdata <- leadership[order(leadership$age),]
newdata <- leadership[order(leadership$gender, -leadership$age),]


# add column:                                   total <- merge(datadrameA, dataframeB, by"column")
# direct merge:                                 total <- cbind(A, B)
# add rows(dataframes must have same columns):  total <- rbind(dataframeA, dataframeB)


# extract subsets of the data set
sub1 <- leadership[, c(6:10)]

col11 <- c("item1", "item2", 'item3')
sub11 <- leadership[col11]

col12 <- paste("item", 1:4, sep="")
col12
sub12 <- leadership[col12]
sub12

col2 <- names(leadership) %in% c("item3", "item4")
sub2 <- leadership[!col2]
sub2

leadership$item3 <- NULL
leadership


# select data
sub3 <- leadership[1:3,]
sub3

sub4 <- leadership[leadership$gender=='M' & leadership$age>30,]
sub4


sub5 <- subset(leadership, age>=35 | age<24, select=c(gender, age, item1))
sub6 <- subset(leadership, age>=35 | age<24, select=gender : item5)
sub5
sub6


# Use SQL to operate dataframe
install.packages("sqldf")
library(sqldf)
sub_sql <- sqldf("select * from leadership where gender='M'")
sub_sql
