### Chapter 10 Power analysis for sample size or confidence ###

# 'Sample size' refers to the number of observations in each condition/group of the experimental design.
# The 'significance level' (also referred to as alpha) is defined as the probability of making a Type I error
# 'Power' can be thought of as the probability of finding an effect that is there (defined as one minus the probability of making a Type II error).
# 'Effect size' is the magnitude of the effect under the alternate or research hypothesis

install.packages("pwr")
library(pwr)

# 1. different tests
# t-test: 2-tailed independent sample t test to compare 2 groups, with some test settings
pwr.t.test(d=0.8, sig.level=0.05, power=0.9, type="two.sample", 
           alternative="two.sided")
pwr.t.test(n=20, d=0.5, sig.level=0.1, type="two.sample", 
           alternative="two.sided")

# Anova Test example: sample size needed
pwr.anova.test(k=5, f=.25, sig.level=.05, power=.8)

# correlation test
pwr.r.test(r=.25, sig.level=.05, power=.90, alternative = "greater")

# linear model test
pwr.f2.test(u=3, f2=0.0769, sig.level=0.05, power=0.90)

# ratio test
pwr.2p.test(h=ES.h(.65, .6), sig.level=.05, power=.9,
            alternative="greater")

# Kai-squared test
prob <- matrix(c(.42, .28, .03, .07, .10, .10), byrow=TRUE, nrow=3)
ES.w2(prob)
pwr.chisq.test(w=.1853, df=2, sig.level=.05, power = .9)


# 2. example of creating power analysis plots: for different effect levels etc. of correlation tests
r <- seq(0.1, 0.5, 0.01) 
p <- seq(0.4, 0.9, 0.1) 
df <- expand.grid(r, p) 
colnames(df) <- c("r", "p") 

for (i in 1:nrow(df)){ 
  result <- pwr.r.test(r = df$r[i], 
                       sig.level = .05, power = df$p[i], 
                       alternative = "two.sided") 
  df$n[i] <- ceiling(result$n) 
}

library(ggplot2)
ggplot(data=df, 
       aes(x=r, y=n, color=factor(p))) + 
  geom_line(size=1) + 
  theme_bw() + 
  labs(title="Sample Size Estimation for Correlation Studies", 
       subtitle="Sig=0.05 (Two-tailed)", 
       x="Correlation Coefficient (r)", 
       y="Samsple Size (n)", 
       color="Power")
