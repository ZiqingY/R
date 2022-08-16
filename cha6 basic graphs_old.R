# load data from vcd package and graphqlize
install.packages("vcd")
library(vcd)
counts <- table(Arthritis$Improved)
counts

barplot(counts, main="Simple Bar Plot",
        xlab="Improvement", 
        ylab = "Frequencey")
barplot(counts,
        main="Horizontal Bar Plot",
        xlab="Frequency",
        ylab="Improvement",
        horiz=TRUE)

plot(Arthritis$Improved,
     main="Simple Bar Plot",
     xlab="Improved",
     ylab="Frenquency")
plot(Arthritis$Improved,
     main="Horizontal Bar Plot",
     xlab="Frequency",
     ylab="Improved",
     horiz=TRUE)                   # plot factor variables can use plt()

counts2 <- table(Arthritis$Improved, Arthritis$Treatment)
counts2

barplot(counts2,
        main="Stacked Bar Plot",
        xlab="Treatment",
        ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts))                # piled up hist
barplot(counts2,
        main="Grouped Bar Plot",
        xlab="Treatment",
        ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts), beside=TRUE)   # grouped hist


# summary statistic included into graphqlizing
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
means <- means[order(means$x), ]
barplot(means$x, names.arg=means$Group.1)
title("Mean Illiteracy Rate")


# reshape/orgnize plots by par() 
par(mar=c(5, 10, 4, 2))
par=(las=3)
counts3 <- table(Arthritis$Improved)
barplot(counts,
        main = "Treatment Outcome",
        horiz = TRUE,
        cex.names = 0.8,
        names.arg = c("No Improvement", "Some Improvement", "Marked Improvement"))


# spinogram
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = "Spinogram Example")
detach(Arthritis)


# pie gram
par(mfrow=c(2,2))
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Austrilia", "Germany", "France")
pie(slices, 
    labels = lbls,
    main = "Simple Pie Chart")

pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, " ", pct, "%", sep="")
pie(slices, 
    labels = lbls2, 
    col=rainbow(length(lbls2)),
    main="Pie Chart with Percentages")

install.packages("plotrix")
library(plotrix)
pie3D(slices,
      labels=lbls,
      explode=0.1,
      main="3D Pie Chart")
mytable <- table(state.region)

lbls3 <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, 
    labels = lbls3,
    main="Pie Chart from a Table\n (with sample sizes)")


# fan plot
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Austrilia", "Germany", "France")
fan.plot(slices, labels = lbls, main="Fan Plot")


# histgram: hist(x)
par(mfrow=c(2,2))
hist(mtcars$mpg)

hist(mtcars$mpg,
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main="Colored histogram with 12 bins")

hist(mtcars$mpg,
     freq=FALSE,
     breaks = 12,
     col="blue",
     xlab = "Miles Per Gallon",
     main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col="blue", lwd=2)

x <- mtcars$mpg
h <- hist(x,
          breaks=12,
          col="green",
          xlab="Miles Per Gallon",
          main="Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
box()


# kernel density
par(mfrow=c(2,1))
d <- density(mtcars$mpg)

plot(d)

d1 <- density(mtcars$mpg)
plot(d1, main="Kernel density of Miles Per Gallon")
polygon(d, col='red', border = "blue")
rug(mtcars$mpg, col="blue")


# kernel density for comparison
install.packages("sm")
library(sm)
attach(mtcars)
cyl.f <- factor(cyl, levels=c(4,6,8),
               labels = c("4 cylinder", "6 cylinder", "8 cylinder"))

sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")

colfill <- c(2:(1+length(levels(cl.f))))
legend(locator(1), levels(cyl.f), fill=colfill)
detach(mtcars)


# Box plot
par(mfrow=c(1,1))
boxplot(mtcars$mpg, main="Box plot", ylab="Miles per Gallon")
boxplot(mpg~cyl, data=mtcars,
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")


# Dot chart
dotchart(mtcars$mpg,
         labels=row.names(mtcars),
         cex=0.7,
         main="Gas Mileage for Car Models",
         xlab="Miles Per Gallon") 
