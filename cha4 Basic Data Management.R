### Chapter 4: basic data management ###

# create dataframe with aggregate functions and transform() 
mydata <- data.frame(x1=c(2, 2, 6, 4),
                     x2=c(3, 4, 2, 8))
mydata <- transform(mydata, 
                    sumx=x1+x2,
                    meanx=(x1+x2)/2)
mydata


# assign NA to dataframe; create new columns using logical operators
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
leadership
leadership$age[leadership$age==99] <-NA
leadership <- within(leadership, {
                     agecat <- NA
                     agecat[age>75]              <- "Elder"
                     agecat[age >=55 & age <=75] <- "Middle Aged"
                     agecat[age < 55]            <- "Young"})


# rename columns: interactive editor, names(), rename()
fix(leadership) 

names(leadership)[2] <- "testDate"
names(leadership)[6:10] <- c("item1","item2","item3","item4","item5")

install.packages("plyr")
library(plyr)
leadership <- rename(leadership,
                     c(manager="managerID", Date="TestDate"))


# recognize and deal with NA; NA in calculation; distinguish NA from NaN
is.na(leadership[, 6:10])

x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4]
z <- sum(x)
y
z
y1 <- sum(x, na.rm=TRUE) # ignores NA
y1

newleadership <- na.omit(leadership) # removes all NA rows (listwise deletion)
newleadership


# date values(default: yyyy-mm-dd)
mydates <- as.Date(c('2007-06-22', '2004-02-13'))
mydate <- as.Date(c('01/05/1965', '08/16/1975'), "%m/%d/%Y")  # transform to default

myformat <- "%m/%d/%y"
leadership$TestDate <- as.Date(leadership$TestDate, myformat) # transform to default in a df

today <- Sys.Date() 
format(today, format="%B %d %Y")
format(today, format="%A")
date()

days=mydate[1]-mydates[1] # date calculation
days

difftime(mydate[1], mydates[1], units="weeks") # date calculation function

strtoday <- as.character(today) # transform date variable into character
strtoday


# recognize variable type: TRUE or FALSE
a <- c(1, 2, 3)
is.numeric(a)
is.vector(a)
is.character(a)


# order data in data.frame
newdata <- leadership[order(leadership$age),]
newdata
newnewdata <- leadership[order(leadership$gender, -leadership$age),]
newnewdata

# add column:                                   total <- merge(dataframeA, dataframeB, by="column")
# direct horizontal concatenation:              total <- cbind(A, B)
# vertical join (df must have same columns):    total <- rbind(dataframeA, dataframeB)


# extract subsets of the data set
sub1 <- leadership[, c(6:10)]
sub1

col11 <- c("item1", "item2", 'item3')
sub11 <- leadership[col11]
sub11

col12 <- paste("item", 1:4, sep="") # use paste() to create vector
col12
sub12 <- leadership[col12]
sub12


# Dropping variables
col2 <- names(leadership) %in% c("item3", "item4") # judge if column names contain item3 or item4
col2
sub2 <- leadership[!col2] # Note: the not (!) operator reverses the logical values
sub2
leadership1 <- leadership
leadership1$item3 <- NULL #drops item3 column in this table
leadership1


# select data; select with conditions; use subset() to select;
sub3 <- leadership[1:3,]
sub3

sub4 <- leadership[leadership$gender=='M' & leadership$age>30,]
sub4

startdate <- as.Date("2014-05-01")
enddate <- as.Date("2014-10-12") 
newdata <- leadership[which(leadership$TestDate >= startdate & leadership$TestDate <= enddate),]
newdata

sub5 <- subset(leadership, age>=35 | age<24, select=c(gender, age, item1))
sub6 <- subset(leadership, age>=35 | age<24, select=gender : item5)
sub5
sub6


# random samples in dataframe
mysample <- leadership[sample(1:nrow(leadership), 3, replace=FALSE),]
mysample


# Use dplyr package to manipulate dataframes (see doc for function explanations)
library(dplyr)
leadership2 <- leadership
leadership2 <- mutate(leadership2,
                     total_score = q1 + q2 + q3 + q4 + q5,
                     mean_score = total_score / 5)                # Create two summary variables 
leadership2$gender <- recode(leadership2$gender,
                            "M" = "male", "F" = "female")         # Recode M and F to male and female 
leadership2 <- rename(leadership2, ID = "manager", sex = "gender")# Rename the manager and gender variables  
leadership2 <- arrange(leadership2, sex, total_score)             # Sort the data by sex and then total score within sex
leadership2_ratings <- select(leadership2, ID, mean_score)        # Create a new data frame containing the rating variables  
leadership2_men_high <- filter(leadership2,
                              sex == "male" & total_score > 10)   # Create a new data frame containing males with total scores above 10


# pipe statements (see Google notes)


# Use SQL to operate dataframe
install.packages("sqldf")
library(sqldf)
sub_sql <- sqldf("select * from leadership where gender='M'")
sub_sql
