### Chapter 6 Basic graphs ###

# simple bar chart
install.packages("vcd")
library(vcd)
library(ggplot2)
ggplot(Arthritis, aes(x=Improved)) + geom_bar() + 
  labs(title="Simple Bar chart", x="Improvement",y="Frequency")

# horizontal bar chart
ggplot(Arthritis, aes(x=Improved)) + geom_bar() + 
  labs(title="Horizontal Bar chart",
       x="Improvement", 
       y="Frequency") +
  coord_flip()

# stacked bar chart
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) + 
  geom_bar(position = "stack") + 
  labs(title="Stacked Bar chart", 
       x="Treatment", 
       y="Frequency")

# grouped bar chart
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "dodge") +
  labs(title="Grouped Bar chart",
       x="Treatment", 
       y="Frequency")

# filled bar chart
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "fill") +
  labs(title="Stacked Bar chart",
       x="Treatment",
       y="Frequency")

# Mean/standard error bar charts
states <- data.frame(state.region, state.x77)
library(dplyr)
plotdata <- states %>% #1 
  group_by(state.region) %>% 
  summarize(n=n(), 
            mean = mean(Illiteracy), 
            se = sd(Illiteracy)/sqrt(n))
plotdata
ggplot(plotdata, aes(x=reorder(state.region, mean), y=mean)) +
  geom_bar(stat="identity", fill="skyblue") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  labs(x="Region", 
       y="", 
       title = "Mean Illiteracy Rate", 
       subtitle = "with standard error bars")

# Tweaking bar charts: manually fill colors categorically into bars
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) + 
  geom_bar(position = "stack", color="black") + 
  scale_fill_manual(values=c("red", "grey", "gold") + 
                      labs(title="Stacked Bar chart", x="Treatment", y="Frequency")

# Adding labels to bar chart: horizontal and vertical
ggplot(mpg, aes(x=model)) + 
  geom_bar() + 
  labs(title="Car models in the mpg dataset", 
       y="Frequency", x="") + 
  coord_flip()
ggplot(mpg, aes(x=model)) + 
  geom_bar() + 
  labs(title="Model names in the mpg dataset", 
       y="Frequency", x="") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))

# create pie charts using ggpie package
library(devtools)
devtools::install_github("rkabacoff/ggpie")
library(ggpie)                    
ggpie(mpg, class)
ggpie(mpg, class, legend=FALSE, offset=1.3, title="Automobiles by Car Class")
ggpie(mpg, class, year, legend=FALSE, offset=1.3, title="Car Class by Year")

# Tree maps: for categorical variable
library(dplyr)
install.packages("treemapify")
ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)

# Histograms with density function
cars2008 <- mpg[mpg$year == 2008, ]
ggplot(cars2008, aes(x=hwy, y=..density..)) +
  geom_histogram(bins=20, color="white", fill="steelblue") + 
  scale_y_continuous(labels=scales::percent) +
  geom_density(color="red", size=1) +
  labs(title="Histogram with density curve", 
       y="Percent" ,
       x="City Miles Per Gallon")

# Kernal Density plots
ggplot(cars2008, aes(x=cty)) +
  geom_density(fill="grey", bw=1) +
  labs(title="Kernel density plot with bw=1")

# Box plots
ggplot(mtcars, aes(x="", y=mpg)) + 
  geom_boxplot() + 
  labs(y = "Miles Per Gallon", x="", title="Box Plot"))

# Violin plots

# Dot plots
ggplot(data, aes(x=contvar, y=catvar)) + geom_point()
