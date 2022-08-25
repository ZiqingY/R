### Chapter 11: intermediate graphs ###

# 1. scatter points
library(ggplot2)
# plot scatter and fit lines according to cyl variable
ggplot(mtcars, 
       aes(x=wt, y=mpg, 
           color=factor(cyl), 
           shape=factor(cyl))) + 
  geom_point(size=2) + 
  geom_smooth(method="lm", se=FALSE) + 
  geom_smooth(method="loess", se=FALSE, linetype="dashed") + 
  labs(title = "Scatter Plot of MPG vs. Weight", 
       subtitle = "By Number of Cylinders", 
       x = "Car Weight (lbs/1000)", 
       y = "Miles Per Gallon", 
       color = "Number of \nCylinders", 
       shape = "Number of \nCylinders") + 
  theme_bw()

# plot matrices: kernel density curve for each variable, scatter plots, correlations
library(GGally) 
ggpairs(mtcars[c("mpg","disp","drat", "wt")])

# customize plot matrices: histgrams on diagonal, point & fit for lower, correlations for upper
diagplots <- function(data, mapping) {
  ggplot(data = data, mapping = mapping) + 
  geom_histogram(fill="lightblue", color="black") 
}
lowerplots <- function(data, mapping) {
  ggplot(data = data, mapping = mapping) + 
    geom_point(color="darkgrey") + 
    geom_smooth(method = "lm", color = "steelblue", se=FALSE) + 
    geom_smooth(method="loess", color="red", se=FALSE, linetype="dashed") 
} 
upperplots <- function(data, mapping) {
  ggally_cor(data=data, mapping=mapping, 
             displayGrid=FALSE, size=3.5, color="black") 
} 
mytheme <- theme(strip.background = element_blank(),
                 panel.grid = element_blank(), 
                 panel.background = element_blank(), 
                 panel.border = element_rect(color="grey20", fill=NA)) 
ggpairs(mtcars,
        columns=c("mpg","disp", "drat", "wt"), 
        columnLabels=c("MPG", "Displacement", 
                       "R Axel Ratio", "Weight"), 
        title = "Scatterplot Matrix with Linear and Loess Fits", 
        lower = list(continuous = lowerplots), 
        diag = list(continuous = diagplots), 
        upper = list(continuous = upperplots)) + 
        mytheme

# high density scatter plots using color
set.seed(1234) 
n <- 10000 
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2) 
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2) 
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata) 
names(mydata) <- c("x", "y")
with(mydata, 
     smoothScatter(x, y, 
                   main="Scatter Plot Colored by Smoothed Densities"))
# hexagonal cells with different colors 
library(hexbin)
ggplot(mydata, aes(x=x, y=y)) + 
  geom_hex(bins=50) + 
  scale_fill_continuous(trans = 'reverse') + 
  ggtitle("Scatter Plot with 10,000 Observations")

# 3D scatter plots
library(scatterplot3d) 
with(mtcars, 
     scatterplot3d(wt, disp, mpg, 
                   main="Basic 3D Scatter Plot"))
# customized
with(mtcars, 
     scatterplot3d(wt, disp, mpg, 
                   pch=16, 
                   highlight.3d=TRUE, 
                   type="h", 
                   main="3D Scatter Plot with Vertical Lines"))
# regression plane on top of scatter points
s3d <-with(mtcars, 
           scatterplot3d(wt, disp, mpg, 
                         pch=16, 
                         highlight.3d=TRUE, 
                         type="h", 
                         main="3D Scatter Plot with Vertical Lines and Regression Plane")) 
fit <- lm(mpg ~ wt+disp, data=mtcars)
s3d$plane3d(fit)
# spin 3D plot like a cube
install.packages("rgl")
library(rgl) 
with(mtcars, 
     plot3d(wt, disp, mpg, col="red", size=5))

# bubble plots: size of bubble for value of the third variable
ggplot(mtcars, 
       aes(x = wt, y = mpg, size = disp)) + 
  geom_point() + 
  labs(title="Bubble Plot with point size proportional to displacement", 
       x="Weight of Car (lbs/1000)", 
       y="Miles Per Gallon")
# customized
ggplot(mtcars, 
       aes(x = wt, y = mpg, size = disp, fill=factor(cyl))) + 
  geom_point(alpha = .5, 
             color = "black", 
             shape = 21) + 
  scale_size_continuous(range = c(1, 10)) + 
  labs(title = "Auto mileage by weight and horsepower", 
       subtitle = "Motor Trend US Magazine (1973-74 models)", 
       x = "Weight (1000 lbs)", 
       y = "Miles/(US) gallon", 
       size = "Engine\ndisplacement", 
       fill = "Cylinders") + 
  theme_minimal()

# corrgram() for correlation and mosaic plots for categoric data
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.cor, 
         main="Corrgram of mtcars data using shading and coefficients")

library(vcd) 
mosaic(Titanic, shade=TRUE, legend=TRUE)






















