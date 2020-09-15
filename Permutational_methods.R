#### Permutations, resampling, and bootstrapping
#### 12 March 2019
#### ferrenbe@nmsu.edu
setwd("~/Dropbox/R Biology")
install.packages(c("coin", "lmPerm", "boot"), dependencies = TRUE)
library(coin) # Conditional Inference Procedures in a Permutation Test Framework
library(lmPerm)
library(tidyverse)

#### First an example with data on mosquito attraction and beer consumption
beer <- c(27, 19, 20, 20, 23, 17, 21, 24, 31, 26, 28, 20, 27, 19, 25, 31, 24, 28, 24, 29, 21, 21, 18, 27, 20)
water <- c(21, 19, 13, 22, 15, 22, 15, 22, 20, 12, 24, 24, 21, 19, 18, 16, 23, 20)

ds <- data.frame(y = c(beer, water), 
                x = c(rep("beer", length(beer)), rep("water", length(water))))

#### Calculate the observed differences in the means of these two groups
# 'rep()' is a command for replicating values in a vector or data frame
alldata <- c(beer, water)
labels <- c(rep("beer", length(beer)), rep("water", length(water)))
obsdiff <- mean(alldata[labels=="beer"]) - mean(alldata[labels=="water"])
obsdiff

#### Calculate the resample differences in the means of these two groups
# Repeat this step a few times to see how the resampling affects the difference among the means
# 'sample()' takes a random sample
resample_labels <- sample(labels)
resample_diff <- mean(alldata[resample_labels=="beer"]) - 
  mean(alldata[resample_labels=="water"])
resample_diff

#### To use this method in an analysis we need to repeat it hundreds or thousands of times
# We can't do that by hand so let's use the function 'replicate()'
resamp_means <- function(data, labs){
  resample_labels <- sample(labs)
  resample_diff <- mean(data[resample_labels=="beer"]) - 
    mean(data[resample_labels=="water"])
  return(resample_diff)
}
nulldist <- replicate(9999,resamp_means(alldata,labels))

#### Plot the histogram showing distribution of differences 
hist(nulldist, col="cyan")
abline(v = obsdiff, col = "red")

#### Calculate a P value 
# This value is obtained by counting the proportion of statistics 
# (including the actual observed difference) that are greater than or equal to the observed statistic
# In other words, a randomized distribution of statistics is tested against the observed
# In a very simple sense, this is a null model approach and is a basic Bayesian technique
alldiffs <- c(obsdiff,nulldist)
p <- sum(abs(alldiffs >= obsdiff)/ 10000)
p

##### We can use permutation approaches within almost all GLMs and nonparametric tests
# Let's use the iris data again
Main <- iris %>%
  filter(Species != "setosa") %>%
  glimpse()

# Two and K-sample permutation test for 3 or more groups
library(coin)
oneway_test(Sepal.Length ~ Species, data = iris, distribution = approximate(nresample = 999))

# Two and K-sample permutation test for 2 groups
oneway_test(Sepal.Length ~ Species, data = Main, distribution = approximate(nresample = 999))

#Wilcoxon rank-sum test: equivalent to other versions when set to "exact"
wilcox_test(Sepal.Length ~ Species, data = Main, distribution = "exact")

# Spearman's test for correlations/regressions: the null hyp. is that the variables are independent
spearman_test(Sepal.Length ~ Sepal.Width, data = Main, distribution = approximate(nresample = 999))

# Check with a lm() call to see if the above test seems to be correct
plot(Sepal.Length ~ Sepal.Width, data = Main)
abline(lm(Sepal.Length ~ Sepal.Width, data = Main), col="red") # regression line (y~x) 
test.mod <- lm(Sepal.Length ~ Sepal.Width, data = Main)
summary(test.mod)

# the coin package has options for Chi-square and paired tests, blocked designs, etc.
# see the following for the many options
vignette("coin")

#### We can perform permutation tests with linear models in the lmPerm package
library(lmPerm)

setwd("~/Dropbox/R Biology")

Main.2 <- read.csv("Resin_defense_wide.csv", header = TRUE, stringsAsFactors=FALSE)

Site <- readxl::read_xlsx("Resin_defense_sites.xlsx")

# Join the two data sets
Main.2 <- inner_join(Main.2, Site, by = "Site")

# Rename and mutate some variables
Main.2 <- Main.2 %>%
  rename(Spp = Spp., RDarea = RD_area_5yr, Elev = Elev_m_asl) %>%
  mutate(DBH = rad_mm*2, Res_ducts = rowSums(select(.,RD1:RD10)), Rad_grow = rowSums(select(.,RG1:RG10))) %>%
  glimpse()

# Permutation based multiple regression
perm.mod.1 <- lmp(RDarea ~ Rad_grow + Elev + Age + DBH, data = Main.2, perm = "Prob")
summary(perm.mod.1)

perm.mod.2 <- lm(RDarea ~ Rad_grow + Elev + Age + DBH, data = Main.2)
summary(perm.mod.2)

perm.aov.1 <- aovp(RDarea ~ Spp, data = Main.2, perm = "Prob")
anova(perm.aov.1)

perm.aov.2 <- aov(RDarea ~ Spp, data = Main.2)
anova(perm.aov.2)

#### Use bootstrapping to get more robust models
# Bootstrapping is a method that resamples you data over and over
# It is not the same as simulating data as in Monte Carlo methods
# Bootstrapping samples with replacment; jackknifing resamples by leaving one observation out each time


library(boot) # Very powerful package; the workhorse function is 'boot()'
?boot

# Make a fucntion that calls for a model R2 value
rsq <- function(formula, data, indicies) {
  d <- data[indicies,] 
  fit <- lm(formula, data = d)
  return(summary(fit)$r.square)
}

# Bootstrap a model to get permutation of the R2
boot.mod.1 <- boot(data = Main.2, statistic = rsq, R = 999, 
                   formula = RDarea ~ Rad_grow + Elev + Age + DBH)

print(boot.mod.1) # Get your bootstrapped model results

boot.ci(boot.mod.1, conf = 0.95, type = "all") # Get confidence intervales at a specified level; default = 95%

plot(boot.mod.1) # Plot a histogram showing bootstrapped value distribution and qqplot

#### Monte Carlo Methods
# Introduce a little randomness
#To use Monte Carlo methods, you need to be able to replicate some random process many times. 
#There are two main ways this is commonly done: either withreplicate() or with for() loops.

# How can we get repeatable results with randomness involved?
# If you use the 'set.seed()' function and start with the same numbers, you can recreate your results
set.seed(1234)

# Say we have a vector x, which represents 30 observations of fish length (mm)
# 'rnorm()' generates nromal random vairates
x = rnorm(30, 500, 30)

# We can randomly sample from a distribution by making this function
means <- replicate(n = 1000, expr = {
  x_i <- sample(x, length(x), replace = T)
  mean(x_i)
})

# Check that it's working by first making a standard error function
se = function(x) sd(x)/sqrt(length(x))

# Check your random samples against the specified values
mean(means); mean(x) # We've not yet done this yet, so let's call two function on one line
sd(means); se(x)

#########################################################
# An example for fish length and maturity
# Say you have a fitted model from which you want to propagate the uncertainty 
# in some derived quantity. Consider the case of the von Bertalanffy growth model. 
# This is a non-linear model used to predict the size of an organism (weight or length) based on its age

dat <- read.csv("fish_growth.csv", header = TRUE)
plot(length ~ age, data = dat, pch = 16, col = "grey")

# Suppose you would like to obtain the probability that an average-sized fish of each age 
# is sexually mature. You know that fish of this species mature at approximately 450 mm, 
# and you simply need to determine the fraction of all fish at each age that are greater than 450 mm. 
# However, you don't have any observations for some ages (e.g., age 8), so you cannot simply calculate
# this fraction based on your raw data. You need to fit the von Bertalanffy growth model, 
# then carry the statistical uncertainty from the fitted model forward to the predicted length-at-age. 
#This would be difficult to obtain using only the coefficient estimates and their standard errors, 
# because of the non-linear relationship between the x and y variables.

# Enter the bootstrap, which is a Monte Carlo analysis using an observed data set and a model. 
#The steps for a bootstrap analysis are:
# 1 Resample from the original data (with replacement)
# 2 Fit a model of interest
# 3 Derive some quantity of interest from the fitted model
# 4 Repeat steps 1 - 3 many times
# 5 Summarize the randomized quantities from step 4

#STEP 1
# Randomize the data
randomize = function(dat) {
  # number of observed pairs
  n = nrow(dat)
  # sample the rows to determine which will be kept
  keep = sample(x = 1:n, size = n, replace = T)
  # retreive these rows from the data
  dat[keep,]
}

#STEP 2
# The prespecified growth model
fit_vonB = function(dat) {
  nls(length ~ linf * (1 - exp(-k * (age - t0))),
      data = dat,
      start = c(linf = 600, k = 0.3, t0 = -0.2)
  )
}

#STEP 3
# create a vector of ages
ages <- min(dat$age):max(dat$age)
pred_vonB <- function(fit) {
  # extract the coefficients
  ests <- coef(fit)
  # predict length-at-age
  ests["linf"] * (1 - exp(-ests["k"] * (ages - ests["t0"])))
}

# STEP 4
#Now, use these three functions to perform one iteration:
pred_vonB(fit = fit_vonB(dat = randomize(dat = dat)))

#You can wrap this inside of a replicate() call to perform step 4 above:
set.seed(2)
out <- replicate(n = 100, expr = {
  pred_vonB(fit = fit_vonB(dat = randomize(dat = dat)))
})

dim(out) # rows are ages, columns are bootstrapped iterations

# STEP 5
# Summarize the random lengths at each age
summ = apply(out, 1, function(x) c(mean = mean(x), quantile(x, c(0.025, 0.975))))

#Plot the data, the summarized ranges of mean lengths, 
# and the length at which all fish are assumed to be mature (450 mm)
plot(length ~ age, data = dat, col = "grey", pch = 16,
     ylim = c(0, max(dat$length, summ["97.5%",])),
     ylab = "Length (mm)", xlab = "Age (years)")
lines(summ["mean",] ~ ages, lwd = 2)
lines(summ["2.5%",] ~ ages, col = "grey")
lines(summ["97.5%",] ~ ages, col = "grey")
abline(h = 450, col = "blue")

# Obtain the fraction of iterations that resulted in the mean length-at-age 
#being greater than 450 mm. This is interpreted as the probability that the 
#average-sized fish of each age is mature
p_mat = apply(out, 1, function(x) mean(x > 450))
plot(p_mat ~ ages, type = "b", pch = 17,
     xlab = "Age (years)", ylab = "Probability of Average Fish Mature")

##### Another permutation resample example with easier code
dat.2 <- read.csv("ponds.csv")
plot(chl.a ~ treatment, data = dat.2)

Dobs <- mean(dat.2$chl.a[dat.2$treatment == "Add"]) - mean(dat.2$chl.a[dat.2$treatment == "Control"])
Dobs

# x is the group: Add or Control
# y is chl.a
perm = function(x, y) {
  # turn x to a character, easier to deal with
  x = as.character(x)
  # shuffle the x values:
  x_shuff = sample(x)
  # calculate the mean of each group:
  x_bar_add = mean(y[x_shuff == "Add"])
  x_bar_ctl = mean(y[x_shuff == "Control"])
  # calculate the difference:
  x_bar_add - x_bar_ctl
}

# Use the function just once
perm(x = dat.2$treatment, y = dat.2$chl.a)

# Use the function repeatedly
Dnull <- replicate(n = 5000, expr = perm(x = dat.2$treatment, y = dat.2$chl.a))

# Plot the distribution of the null test statistic and 
# draw a line where the originally-observed difference falls:
hist(Dnull)
abline(v = Dobs, col = "blue", lwd = 3, lty = 2)

# Generate a 2 tailed P value
mean(abs(Dnull) >= Dobs)

