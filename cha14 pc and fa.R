### Chapter 14: Principle components and factor analysis ###

# 1. Principle components: replace a large number of correlated variables with a smaller number of uncorrelated variables
data(USJudgeRatings)
# Example 1: single pc
# The Kaiser–Harris criterion: retain components with eigenvalues > 1, using scree test.
library(psych) 
fa.parallel(USJudgeRatings[,-1], fa="pc", n.iter=100, 
            show.legend=FALSE, main="Scree plot with parallel analysis") 
abline(h=1)
# extracting principle components
pc <- principal(USJudgeRatings[,-1], nfactors=1)
pc

# Example 2: more pcs
fa.parallel(Harman23.cor$cov, n.obs=302, fa="pc", n.iter=100, 
            show.legend=FALSE, main="Scree plot with parallel analysis") 
abline(h=1)
pc1 <- principal(Harman23.cor$cov, nfactors=2, rotate="none")
pc1

# Rotating principal components
rc <- principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
rc

# Obtaining principal components scores
pc2 <- principal(USJudgeRatings[,-1], nfactors=1, score=TRUE)
head(pc2$scores)
cor(USJudgeRatings$CONT, pc$score)

round(unclass(rc$weights), 2)
PCs1 = 0.28*height + 0.30*arm.span + 0.30*forearm + 0.29*lower.leg - 
  0.06*weight - 0.08*bitro.diameter - 0.10*chest.girth - 
  0.04*chest.width 
PCs2 = -0.05*height - 0.08*arm.span - 0.09*forearm - 0.06*lower.leg + 
  0.33*weight + 0.32*bitro.diameter + 0.34*chest.girth + 
  0.27*chest.width


# 2. Exploratory factor analysis:  to explain the correlations among a set of observed variables by uncovering 
#    a smaller set of more fundamental unobserved variables underlying the data.
options(digits=2) 
covariances <- ability.cov$cov 
correlations <- cov2cor(covariances) 
correlations  # a correlation coefficient matrix
# how many factors to extract
fa.parallel(correlations, n.obs=112, fa="both", n.iter=100, 
              main="Scree plots with parallel analysis")    # 'both' shows pc and fa
abline(h=c(0, 1))
# extracting common factors
fa <- fa(correlations, nfactors=2, rotate="none", fm="pa")
fa
# Rotating factors
library(GPArotation)
fa.promax <- fa(correlations, nfactors=2, rotate="promax", fm="pa") 
fa.promax
# plot to see variables' loading on factors
factor.plot(fa.promax, labels=rownames(fa.promax$loadings))
fa.diagram(fa.promax, simple=FALSE)
# Factor scores
fa.promax$weights
