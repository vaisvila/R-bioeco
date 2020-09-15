# Assessing generalized linear model fits
# 6 March 2019
# ferrenbe@nmsu.edu
# Thanks to David Lillis for this example

# Let's use the 'mtcars' data again
# The variable 'vs' is a binary value for "V" or "Strait" engine type (1 = yes, v-engine, 0 = no)
# The variable 'am' is a binary value for automatic transmission (yes/no)
?mtcars
head(mtcars)

# Let's call a glm with vs as a function of weight and displacement
# This model will be of the binomial family
# Once you call 'summary()' the coefficients will be in logits
# The z value is the Wald statistic that tests the hypothesis that the estimate is zero
# The null hypothesis is that the estimate has a normal distribution with mean zero and 
#standard deviation of 1. The quoted p-value, P(>|z|), gives the tail area 
# in a two-tailed test
glm.1 <- glm(vs ~ wt + disp, data=mtcars, family=binomial)
summary(glm.1)

# Remember that your model is a predicted relationship based on observed values
# That means we can already use our model to make predictions
# Here we can pick a 'wt' and 'disp' value and ask what the probability of the car being a V or S engine is
# The 'predict()' function works with any model and is in Base R
# It can be applied to entire vectors (e.g., a data column)
new_data.1 <- data.frame(wt = 1.9, disp = 220)
predict(glm.1, new_data.1, type = "response")

new_data.2 <- data.frame(wt = 2.3, disp = 180)
predict(glm.1, new_data.2, type = "response")

######################INTERPRET YOUR MODEL OUTPUT#############################################
# What are the effects of 'wt' and 'disp' ? One has a positive and the other a negative effect
# There is no R-squared for models that use Maximum Likelihood estimation-multilevel models
# Our output has two forms of deviance the null deviance and the residual deviance. 
# The null deviance shows how well the response variable is predicted by a model that includes only the intercept (grand mean)
# Deviance is a measure of goodness of fit of a generalized linear model
# As the value increases, the model fit gets worse (low value = better model, just like with AIC & BIC)
# Fisher scoring interactions tells us that the model converged

# We can also compute a pseudo R2
# For a summary of various pseudo R2 methods see: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
#install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(glm.1)

# Another approach for binary data is to use the 'Hosmer-Lemeshow Goodness of Fit' test
#install.packages("ResourceSelection", dependencies = TRUE)
# This will test for differences between the fitted model and the observed data
# A small p-value indicates a model that differs from the observed values
library(ResourceSelection)
hoslem.test(mtcars$vs, fitted(glm.1))

# Let's look at plots which can help us interpret our model
glm.2 <- glm(vs ~ wt, family = binomial, data = mtcars)
summary(glm.2)
PseudoR2(glm.2)

range(mtcars$wt)

x.weight <- seq(0, 6, 0.01)
y.weight <- predict(glm.2, list(wt=x.weight), type = "response")
plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "Weight (1000 lbs)", ylab = "Probability of v- or s-engine")
lines(x.weight, y.weight)

glm.3 <- glm(vs ~ disp, family = binomial, data = mtcars)
summary(glm.3)
PseudoR2(glm.3) # This result suggests that we only really need 'disp' to get a decent glm

range(mtcars$disp)
x.disp <- seq(70, 500, 10)
y.disp <- predict(glm.3, list(disp=x.disp), type = "response")
plot(mtcars$disp, mtcars$vs, pch = 16, xlab = "DISPLACEMENT (cubic inches)", ylab = "Probability of v- or s-engine")
lines(x.disp, y.disp)


#####Let's move on to the pine defenses data set
# Load the data set; we don't need the "Sites" data this time
setwd("~/Dropbox/R Biology")
Main <- read.csv("Resin_defense_wide.csv", header = TRUE, stringsAsFactors = FALSE)
names(Main)

library(tidyverse)

Main <- Main %>%
  select(Spp = Spp., RDarea = RD_area_5yr, RDsize = Mean_RD_size_5yr, everything()) %>%
  filter(Spp != "PIFL") %>%
  mutate(Species = ifelse(Spp == "PIPO", "1", "0")) %>%
  glimpse()

str(Main)
Main$Species <- as.numeric(Main$Species)
glm.4 <- glm(Species ~ RDarea + Age + rad_mm, family = binomial, data = Main)
summary(glm.4)
PseudoR2(glm.4)

Main$predicted <- predict(glm.4, type = "response")
table(Main$Species, Main$predicted > 0.5)
table(Main$Species, Main$predicted > 0.7)

#install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(Main$predicted, Main$Species)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#### Another example using the mistletoe infection data
Main.2 <- read.csv("Mistletoe_data_day2.csv", header = TRUE, stringsAsFactors = FALSE)
names(Main.2)

# Cleaning up the data and making a binary variable for mistletoe infections
Main.2 <- Main.2 %>%
  select(Spp = Spp., Elev = M.asl, RDarea = Res_duct_area, RDsize = Res_duct_mean_size, everything()) %>%
  mutate(Infected = ifelse(Mt_rating > 0, 1, 0)) %>%
  glimpse()

# Specify a glm and get the summary
glm.5 <- glm(Infected ~ Age + RGR10, family = binomial, data = Main.2)
summary(glm.5)
PseudoR2(glm.5)   

#Use the glm to make a prediction value
Main.2$predicted <- predict(glm.5, type = "response")

# Plot an ROC curve with thresholds
ROCRpred.2 <- prediction(Main.2$predicted, Main.2$Infected)
ROCRperf.2 <- performance(ROCRpred.2, "tpr", "fpr")
plot(ROCRperf.2, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Make confusion matricies with different threshold cut-offs
table(Main.2$Infected, Main.2$predicted > 0.1)
table(Main.2$Infected, Main.2$predicted > 0.8)

#### Another option for ROCs and AUCs
#install.packages("pROC")
library(pROC)

roc(Infected ~ Age, Main.2) # Will give us the AUC automatically
plot.roc(Main.2$Infected, Main.2$Age) # sensitivity = correct 'yes' calls; specificity = correct 'no' calls

# Compare two ROCs
roc.1 <- roc(Infected ~ Age, Main.2)
roc.2 <- roc(Infected ~ RGR10, Main.2)
roc.test(roc.1, roc.2, method = "bootstrap")

# View the two GLMs
glm.6 <- glm(Infected ~ Age, family = binomial, data = Main.2)
summary(glm.6)
PseudoR2(glm.6)

glm.7 <- glm(Infected ~ RGR10, family = binomial, data = Main.2)
summary(glm.7)
PseudoR2(glm.7)

# Plot a logistic curve with histogram for Infection ~ RGR10
logi.hist.plot(Main.2$RGR10, Main.2$Infected, type = "hist", boxp = FALSE, rug = FALSE, logi.mod = 2,
               xlabel = "Relative growth rate (10 yr)")


#### Continue with this online tutorial: https://wilkelab.org/classes/SDS348/2015_spring_worksheets/class11_solutions.html


######################FOR POISSON GLM########################################################
# Deviance here is labelled as the 'residual deviance' and is = 21.4
# There are 32 observations and the model has 3 parameters so d.f. = 29
# To calculate the p-value for the deviance goodness of fit test we simply calculate the probability 
# to the right of the deviance value for the chi-squared distribution on 29 degrees of freedom
Main.3 <- read.csv("Resin_defense_wide.csv", header = TRUE)
names(Main.3)

library(fitdistrplus)
descdist(Main.3$RD1)
hist(Main.3$RD1, breaks = 25)

# Specify a poisson glm
glm.poisson <- glm(RD1 ~ RG1, family = poisson(link = "log"), data = Main.3)
summary(glm.poisson)

glm.quasipoisson <- glm(RD1 ~ RG1, quasipoisson, data = Main.3)
summary(glm.quasipoisson)

# Goodness of fit test
# The null hypothesis is that our model is correctly specified
# We reject if P is small
pchisq(glm.poisson$deviance, df=glm.poisson$df.residual, lower.tail=FALSE)
pchisq(glm.quasipoisson$deviance, df=glm.quasipoisson$df.residual, lower.tail=FALSE)


library(pscl)
glm.zip <- zeroinfl(RD1 ~ RG1 + Age + rad_mm + Spp., data = Main.3)
summary(glm.zip)

vuong(glm.poisson, glm.zip) # Says zero inflated is a better model

# There's a missing value somewhere in our data set so let's make a new data frame omitting it
Main.4 <- na.omit(Main.3)

# Specify two ZI poisson models to compare
glm.zip.2 <- zeroinfl(RD1 ~ RG1, data = Main.4)
summary(glm.zip.2)

glm.zip.3 <- zeroinfl(RD1 ~ RG1 + Spp. + Age, data = Main.4)
summary(glm.zip.3)

# Compare these ZI models
vuong(glm.zip.2, glm.zip.3)





# make a reduced iris data set that only contains virginica and versicolor species
iris.small <- filter(iris, Species %in% c("virginica", "versicolor"))

# logistic regression
glm.out <- glm(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length,
               data = iris.small,
               family = binomial) 
# family = binomial required for logistic regression
summary(glm.out)

glm.out <- glm(Species ~ Sepal.Width + Petal.Width + Petal.Length,
               data = iris.small,
               family = binomial)
summary(glm.out)

lr_data <- data.frame(predictor=glm.out$linear.predictors, prob=glm.out$fitted.values, Species=iris.small$Species)
ggplot(lr_data, aes(x=predictor, y=prob, color=Species)) + geom_point()

ggplot(lr_data, aes(x=predictor, fill=Species)) + geom_density(alpha=.5)

plant1 <- data.frame(Sepal.Length=6.4, Sepal.Width=2.8, Petal.Length=4.6, Petal.Width=1.8)
plant2 <- data.frame(Sepal.Length=6.3, Sepal.Width=2.5, Petal.Length=4.1, Petal.Width=1.7)
plant3 <- data.frame(Sepal.Length=6.7, Sepal.Width=3.3, Petal.Length=5.2, Petal.Width=2.3)

predict(glm.out, plant1, type="response")

predict(glm.out, plant2, type="response")

cutoff <- 0
virg_true <- sum(lr_data$predictor > cutoff & lr_data$Species=="virginica") 
virg_false <- sum(lr_data$predictor <= cutoff & lr_data$Species=="virginica") 
virg_true

virg_false

vers_true <- sum(lr_data$predictor <= cutoff & lr_data$Species=="versicolor") 
vers_false <- sum(lr_data$predictor > cutoff & lr_data$Species=="versicolor") 
vers_true
vers_false

tp <- virg_true/(virg_true + virg_false)
tn <- vers_true/(vers_true + vers_false)
tp
tn
