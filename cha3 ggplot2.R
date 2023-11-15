### Chapter 3: getting started with graphs using ggplot2 ###

## ggplot2 package: see Google week4 notes for more applications
library(ggplot2)
library(mosaicData)
CPS85 <- CPS85[CPS85$wage < 40, ]

# basic scatterpoint plot
ggplot(data = CPS85, mapping = aes(x = exper, y = wage)) + geom_point()

# scatter point with colors, shapes, best-fit lines, transparency, no confidence interval
ggplot(data = CPS85,
       mapping = aes(x = exper, y = wage,
                     color = sex, shape = sex, linetype = sex)) +
  geom_point(alpha = .7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5)

install.packages("scales")
# adjusting for x-axis scale and y-axis scale and colors, continue on above +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  scale_color_manual(values = c("indianred3", "cornflowerblue"))

# facets, label and theme
ggplot(data = CPS85,
       mapping = aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender") +
  theme_minimal()

# graphs can also be assigned to objects
