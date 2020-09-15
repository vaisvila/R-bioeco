#### Intro to multivariate methods
#### 20 March 2019
#### ferrenbe@nmsu.edu

# The dependent variables should be normally distributed
# The R function mshapiro.test( )within the 'mvnormtest' package will perform
# the Shapiro-Wilk test for multivariate normality
# Homogeneity of variances across the range of predictors is also assumed

# Use the iris data set
Main <- iris

#Define individual variables
sepl <- Main$Sepal.Length
petl <- Main$Petal.Length
sepw <- Main$Sepal.Width
petw <- Main$Petal.Width

# Load the 'mvnormtest' package to run a multivariate Shapiro Wilk test
##install.packages("mvnormtest")
library(mvnormtest)
mshapiro.test(cbind(sepl, petl, sepw, petw))

# Load the 'heplots' package to run the Box test--not a trustworthy test
##install.packages("heplots")
library(heplots)
boxM(cbind(sepl, petl, sepw, petw), Main$Species)

# MANOVA test
man.mod <- manova(cbind(Sepal.Length, Petal.Length, Sepal.Width, Petal.Width) ~ Species, data = iris)
summary(man.mod, test = "Pillai") # Can also be "Wilks", "Hotelling", and "Roy"

# Look to see which differ
summary.aov(man.mod)

#### Where do we go if assumptions are not met?
#### There are forms of MANOVA that don't assume equal variances the 'WRS2' package
#### And a permutational verions; PERMANOVA, commonly from the 'vegan' package

# Within 'vegan' the 'adonis()' function is the call for a PERMANOVA
# adonis() can be used in any scenario where a perumutation anova is desired, not just for community data
# Nesting terms is possible in an adonis() call
library(vegan)
perm.mod <- adonis(Main[,1:4] ~ Species, data = Main)
perm.mod

# Use the package 'RVAideMemoire' to perform pairwise post-hoc tests
##install.packages("RVAideMemoire")
library(RVAideMemoire)
pairwise.perm.manova(Main[,1:4], Main$Species, test = c("Pillai", "Wilks",
                                          "Hotelling-Lawley", "Roy", "Spherical"), nperm = 999, 
                     progress = TRUE, p.method = "fdr")

# 'betadisper()' is a multivariate analogue of Levene's test for homogeneity of variances
# Before we run the test, we need to make a distance matrix from our data
# Here we've used 'dist()' which gives us Euclidean distances in our matrix
# If we wanted Bray-Curtis/Sorenson distances, we use 'vegdist()'
# We can also call 'betadiver()' and get one of 24 indicies of beta diversity

# Make the distance matrix
dist.matrix <- dist(Main[,1:4])

# Call the betadisper mod
disp.mod <- betadisper(dist.matrix, Main$Species, type = "centroid")

# Plot the model
plot(disp.mod)
boxplot(disp.mod)

# Call the test to get an F and P value
permutest(disp.mod)

#### Can you run a PERMANOVA on the species data from 'dune' using "Management" from 'dune.env'
#### as the grouping variable?
library(vegan)
Dune <- data(dune.env)

perm.mod <- adonis(dune ~ Management, data = dune.env)
perm.mod

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("mixOmics", version = "3.8")
install.packages("RVAideMemoire")
library(RVAideMemoire)
pairwise.perm.manova(dune, Main$Species, test = c("Pillai", "Wilks",
                                                        "Hotelling-Lawley", "Roy", "Spherical"), nperm = 999, 
                     progress = TRUE, p.method = "fdr")
