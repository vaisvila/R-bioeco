# Continuing with linear models
# ANOVA, comparing the lm() and the glm() functions
# ferrenbe@nmsu.edu
# 28 February 2019
setwd("~/Dropbox/R Biology")
# For a tutorial, we'll use the 'mtcars' data again
names(mtcars)

# Let's start with an ANOVA using the number of cylinders in a motor as a categorical variable
# Remember that R sees numbers as non-character values even if they are names or groups
is.numeric(mtcars$cyl)

# Make a new variable called 'motor' by turning 'cyl' into a category
mtcars$motor <- as.character(mtcars$cyl)
is.character(mtcars$motor)

# Quick check to see if 'mpg' is normally distributed
hist(mtcars$mpg)
shapiro.test(mtcars$mpg) #recall that small p-values indicate non-normal in this test

library(ggpubr)
ggqqplot(mtcars$mpg)

# An ANOVA of mpg x cyl
anova.1 <- aov(mpg ~ motor, data = mtcars)
summary(anova.1)

anova.2 <- lm(mpg ~ motor, data = mtcars)
anova(anova.2) 
# How does the 'aov' compare to the 'lm'?
##### As we learned before, linear models are a family so 
# the 'lm()' function can call an ANOVA, regression, correlation, or ANCOVA

# View the boxplot
boxplot(mpg ~ motor, data = mtcars) 

# Is the data heteroscedastic?
# Install.packages("onewaytests") so we can use a Welch's ANOVA
install.packages("onewaytests")
library(onewaytests)

# Perform a Welch's ANOVA--a version that handles heteroscedasticity
welch.test(mpg ~ motor, mtcars, rate = 0.0, alpha = 0.05, na.rm = TRUE, verbose = TRUE)

# A linear multiple regression
lm.1 <- lm(mpg ~  wt + am + disp, data = mtcars)
summary(lm.1)

# Further check of the assumptions
library(gvlma)
par(mar=c(1,1,1,1))
plot(lm.1)
plot(gvlma(lm.1))

# While we don't seem to need one, let's still 
# make a glm using the 'glm()' function
glm.1 <- glm(mpg ~ wt + am + disp, data = mtcars)
summary(glm.1) 
# How do the results of the lm and glm compare?

##### By default, a glm considers the data to be of the family 
##### "Gaussian" which is the same as an lm. 
##### You need to specify either a different family or link 
##### function as relevant to your data

#####Let's move on to the pine defenses data set
# Load the data set; we don't need the "Sites" data this time
Main <- read.csv("Resin_defense_wide.csv", header = TRUE)
names(Main)

# 'dplyr' has a 'rename()' function that we haven't used yet
# Let's use it to make our variable names shorter
library(tidyverse)

Main <- Main %>%
  rename(Spp = Spp., RDarea = RD_area_5yr, RDsize = Mean_RD_size_5yr) %>%
  glimpse()

# A quick check of the RDarea distribution
hist(Main$RDarea)

library(ggpubr)
ggqqplot(Main$RDarea)

# Now we make our ANOVA model for RDarea ~ Spp
anova.spp <- aov(RDarea ~ Spp, data = Main)
anova(anova.spp)

# Let's perform a Bartlett test--this is part of 'onewaytests'
homog.test(RDarea ~ Spp, Main, method = "Bartlett")

# We can use 'gvlma' if we call our ANOVA as an 'lm()'
lm.spp <- lm(RDarea ~ Spp, data = Main)
library(gvlma)
gvlma(lm.spp)

# It looks like we don't have normality but we do have homoscedasticity
# Let's consider some transformations

# log
hist(log(Main$RDarea))
lm.spp.2 <- lm(log(RDarea) ~ Spp, data = Main)
gvlma(lm.spp.2)
ggqqplot(log(Main$RDarea))

hist(sqrt(Main$RDarea))
lm.spp.3 <- lm(sqrt(RDarea) ~ Spp, data = Main)
gvlma(lm.spp.3)
ggqqplot(sqrt(Main$RDarea))

shapiro.test(Main$RDarea)
shapiro.test(log(Main$RDarea))
shapiro.test(sqrt(Main$RDarea))

library(fitdistrplus)
descdist(Main$RDarea, discrete = FALSE, boot = 500)
descdist(log(Main$RDarea), discrete = FALSE, boot = 500)
descdist(sqrt(Main$RDarea), discrete = FALSE, boot = 500)

# Use the 'multcomp' package to get compact lettering
boxplot(sqrt(RDarea) ~ Spp, data = Main)

##install.packages("multcomp")
library(multcomp)
tuk <- glht(lm.spp.3, linfct = mcp(Spp = "Tukey"))
cld(tuk)

# Let's illustrate a t-test in R
# First we need to reduce our data set to only 2 of the 3 pine species
# Here we get rid of the "PIFL"
Main.2 <- Main %>%
  filter(Spp != "PIFL") %>%
  glimpse()

boxplot(RDarea ~ Spp, data = Main.2)
hist(Main.2$RDarea)
hist(log(Main.2$RDarea))
hist(sqrt(Main.2$RDarea))

ggqqplot(sqrt(Main.2$RDarea))
t.test.mod <- lm(RDarea ~ Spp, data = Main.2)
gvlma(t.test.mod)

# Now perform the independent t-test
t.test(RDarea ~ Spp, data = Main.2)
boxplot(sqrt(RDarea) ~ Spp, data = Main.2)

hist(Main$RD1)
glm.poisson <- glm(RD1 ~ RG1, family = poisson(link = "log"), data = Main)
summary(glm.poisson)

plot(predict(glm.poisson, type = "response"),
     residuals(glm.poisson, type = "deviance"))

deviance(glm.poisson)/df.residual(glm.poisson)
# If this value = 1 or is close to 1, the model is not overdispersed
# We might have issues with our model and need to consider zero-inflation

#install.packages("pscl")
library(pscl)
zi.poisson <- zeroinfl(RD1 ~ RG1, data = Main)
summary(zi.poisson)

# Compare the two models
vuong(glm.poisson, zi.poisson)

library(ggplot2)
ggplot(Main, aes(x = RG1, y = RD1)) +
  geom_point() +
  geom_smooth(method = lm)
