### Chapter 9: ANOVA Model for including factors into predictions ###

# 1. One-way ANOVA; summarize results and plot with confidence intervals
library(dplyr)
data(cholesterol, package="multcomp")
plotdata <- cholesterol %>%
  group_by(trt) %>% 
  summarize(n = n(), 
            mean = mean(response), 
            sd = sd(response), 
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
fit <- aov(response ~ trt, data=cholesterol)
summary(fit)

library(ggplot2)
ggplot(plotdata, aes(x = trt, y = mean, group = 1)) + 
  geom_point(size = 3, color="red") + 
  geom_line(linetype="dashed", color="darkgrey") + 
  geom_errorbar(aes(ymin = mean - ci, 
                    ymax = mean + ci), 
                width = .1) + 
  theme_bw() + 
  labs(x="Treatment", 
       y="Response", 
       title="Mean Plot with 95% Confidence Interval")

# use TukeyHSD() for Multiple comparisons: to compare treatments in pairs
pairwise <- TukeyHSD(fit)
pairwise
plotdata <- as.data.frame(pairwise[[1]])
plotdata$conditions <- row.names(plotdata)
ggplot(data=plotdata, aes(x=conditions, y=diff)) + 
  geom_point(size=3, color="red") + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width=.2)) + 
  geom_hline(yintercept=0, color="red", linetype="dashed") + 
  labs(y="Difference in mean levels", x="", title="95% family-wise confidence level") + 
  theme_bw() + 
  coord_flip()

# glht() function for more comprehensive comparisons: from letters below x-axis to tell significance
library(multcomp)
summary(tuk)
tuk <- glht(fit, linfct=mcp(trt="Tukey"))
labels1 <- cld(tuk, level=.05)$mcletters$Letters
labels2 <- paste(names(labels1), "\n", labels1)
ggplot(data=fit$model, aes(x=trt, y=response)) + 
  scale_x_discrete(breaks=names(labels1), labels=labels2) + 
  geom_boxplot(fill="lightgrey") + 
  theme_bw() + 
  labs(x="Treatment", 
       title="Distribution of Response Scores by Treatment", 
       subtitle="Groups without overlapping letters differ signifcantly (p < .05)")

# assessing test assumptions: normality and homoscedasticity
library(car) 
fit <- aov(response ~ trt, data=cholesterol) 
qqPlot(fit, simulate=TRUE, main="Q-Q Plot")

bartlett.test(response ~ trt, data=cholesterol)


# 2. One-way ANCOVA
litter
litter %>% 
  group_by(dose) %>% 
  summarise(n=n(), mean=mean(gesttime), sd=sd(gesttime))
fit2 <- aov(weight ~ gesttime + dose, data=litter)
summary(fit2)
library(effects)
effect("dose", fit2)  # partialing out the effects of other covariates
library(multcomp) 
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1)) 
summary(glht(fit2, linfct=mcp(dose=contrast)))  # comparing does=0 and dose!=0 effects

# assessing test assumptions: normality, homoscedasticity and homogeneity of regression slopes
library(multcomp) 
fit2 <- aov(weight ~ gesttime*dose, data=litter) 
summary(fit2)

# Visualizing the results: relationship between the dependent variable, the covariate, and the factor
pred <- predict(fit2)
library(ggplot2) 
ggplot(data = cbind(litter, pred), 
       aes(gesttime, weight)) + geom_point() + 
  facet_wrap(~ dose, nrow=1) + geom_line(aes(y=pred)) + 
  labs(title="ANCOVA for weight by gesttime and dose") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), 
        legend.position="none")


# 3. Two-way factorial ANOVA: interactive term
ToothGrowth <- ToothGrowth

ToothGrowth$dose <- factor(ToothGrowth$dose)
stats <- ToothGrowth %>% 
  group_by(supp, dose) %>% 
  summarise(n=n(), mean=mean(len), sd=sd(len), 
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
fit3 <- aov(len ~ supp*dose, data=ToothGrowth)
summary(fit3)

pd <- position_dodge(0.2) 
ggplot(data=stats, 
       aes(x = dose, y = mean, 
           group=supp, 
           color=supp, 
           linetype=supp)) + 
  geom_point(size = 2, 
             position=pd) + 
  geom_line(position=pd) + 
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = .1, 
                position=pd) + 
  theme_bw() + 
  scale_color_manual(values=c("blue", "red")) + 
  labs(x="Dose", 
       y="Mean Length", 
       title="Mean Plot with 95% Confidence Interval")


# 4. Repeated measures ANOVA: subjects are measured more than once
CO2 <- CO2
stats <- CO2 %>% 
  group_by(conc, Type) %>% 
  summarise(mean_conc = mean(uptake))
stats

CO2$conc <- factor(CO2$conc) 
w1b1 <- subset(CO2, Treatment=='chilled') 
fit4 <- aov(uptake ~ conc*Type + Error(Plant/(conc)), w1b1) 
summary(fit4)

ggplot(data=stats, aes(x=conc, y=mean_conc, group=Type, color=Type, linetype=Type)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  theme_bw() + theme(legend.position="top") + 
  labs(x="Concentration", y="Mean Uptake", 
       title="Interaction Plot for Plant Type and Concentration")

ggplot(data=CO2, aes(x=conc, y=uptake, fill=Type)) + 
  geom_boxplot() + 
  theme_bw() + theme(legend.position="top") + 
  scale_fill_manual(values=c("gold", "green"))+ 
  labs(x="Concentration", y="Uptake", 
       title="Chilled Quebec and Mississippi Plants")


# 5. Multivariate analysis of variance (MANOVA): more than one dependent (outcome) variable
data(UScereal, package="MASS") 
shelf <- factor(UScereal$shelf) 
shelf <- factor(shelf) 
y <- cbind(UScereal$calories, UScereal$fat, UScereal$sugars) 
colnames(y) <- c("calories", "fat", "sugars") 

aggregate(y, by=list(shelf=shelf), FUN=mean)

fit5 <- manova(y ~ shelf)
summary(fit5)

# assessing multivariate normality and homoscedasticity
enter <- colMeans(y) 
n <- nrow(y) 
p <- ncol(y) 
cov <- cov(y) 
d <- mahalanobis(y,center,cov) 
coord <- qqplot(qchisq(ppoints(n),df=p), 
                  d, main="Q-Q Plot Assessing Multivariate Normality", 
                  ylab="Mahalanobis D2") 
abline(a=0,b=1) 
identify(coord$x, coord$y, labels=row.names(UScereal)) # if normal, then points should be close to line


# 6. ANOVA and Regression: note that lm() will automatically regress to k-1 factor levels
fit.aov <- aov(response ~ trt, data=cholesterol) 
summary(fit.aov)
fit.lm <- lm(response ~ trt, data=cholesterol) 
summary(fit.lm)
