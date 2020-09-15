#For the examples that follow, we will be using the Orthodont data 
# from the nlme package. The  Orthodont data frame has 108 rows 
# and 4 columns of the change in an orthdontic measurement over 
#time for several young subjects. 

library(nlme)
?Orthodont

# Set up the data as an object and change subject ID to a factor
data1 <- as.data.frame(nlme::Orthodont)
data1$Subject <- factor(data1$Subject, ordered = FALSE)
head(data1)

# load libraries
library(lattice)
library(ggplot2)
library(dplyr)

# Take an intial look at the data with a line graph
data1 %>% 
  ggplot(aes(x=age, y=distance, group=Subject, color=Subject, linetype=Sex)) + 
  geom_line(size=1) + theme_classic()

# Start by fitting a model with no quantifiable predictors, aka "a null model" or "intercept only model"
# We need to decide if REML will be T vs F.
# REML = restricted maximum liklihood
# (From Bolker and Fox) It's generally good to use REML, if it is available, when you are interested 
# in the magnitude of the random effects variances, 
# but never when you are comparing models with different fixed effects 
# via hypothesis tests or information-theoretic criteria such as AIC.
# in simple cases (balanced/nested/etc.) REML can be proven to provide unbiased estimates of variance 
# components (but not unbiased estimates of e.g. standard deviation or log standard deviation)
# you cannot compare models that differ in fixed effects if they are fitted by REML rather than ML

# For small sample sizes REML is preferred. However, likelihood ratio tests for REML require 
# exactly the same fixed effects specification in both models. So, to compare models with different 
# fixed effects (a common scenario) with an LR test, ML must be used.
# REML takes account of the number of (fixed effects) parameters estimated, losing 1 df for each. 
# This is achieved by applying ML to the least squares residuals, which are independent of the fixed effects.

library(lme4)
mod1 <- lmer(distance ~ (1|Subject), data=data1, REML=FALSE)
summary(mod1)

# Save predicted values and make a graph
# This graph will show only the intercept
data1 %>% 
  mutate(pred_dist = fitted(mod1)) %>% 
  ggplot(aes(x=age, y=pred_dist, group=Subject, color=Subject)) + theme_classic() +
  geom_line(size=1) 

# Random intercept model with patient age included
mod2 <- lmer(distance ~ age + (1|Subject), data=data1, REML=F)
summary(mod2)

# Save predicted values and make a graph with random intercepts and an age effect
# In this figure, all slopes will be the same
data1 %>% 
  mutate(pred_dist = fitted(mod2)) %>% 
  ggplot(aes(x=age, y=pred_dist, group=Subject, color=Subject)) + theme_classic() +
  geom_line(size=1) 

# Random intercept and random slope model
mod3 <- lmer(distance ~ age + (1|Subject) + (0+age|Subject), data=data1, REML=F)
summary(mod3)

# Save predicted values and make yet another graph with random intercepts
# but this time with unique slopes for each group
data1 %>% 
  mutate(pred_dist = fitted(mod3)) %>% 
  ggplot(aes(x=age, y=pred_dist, group=Subject, color=Subject)) + theme_classic() +
  geom_line(size=1) 

# Random intercept and random slope (correlated)
# This model extends the previous one by allowing random intercept
# and random slope to be correlated
mod4 <- lmer(distance ~ age + (1+age|Subject), data=data1, REML=F)
summary(mod4)

# save predicted values and make the graph
data1 %>% 
  mutate(pred_dist = fitted(mod4)) %>% 
  ggplot(aes(x=age, y=pred_dist, group=Subject, color=Subject)) + theme_classic() +
  geom_line(size=1) 

# Compare the models
anova(mod1, mod2, mod3, mod4)

# Use the MuMIn package to get pseudo R2 values 
# The output will show marginal and conditional R2 values
# Marginal = fixed effects only, Conditional = full model
##install.packages("MuMIn")
library(MuMIn)
library(arm) # You don't need arm for this but it works well for Bayesian methods: https://it.unt.edu/sites/default/files/bayesglm_jds_jan2011.pdf
r.squaredGLMM(mod1)
r.squaredGLMM(mod2)
r.squaredGLMM(mod3)
r.squaredGLMM(mod4)

# MuMIn also has a model building framework.
# We can use the 'dredge' function in MuMIn to get a best fit model
# We need to change the default R setting for handling na values
options(na.action=na.fail)

# A quick example with the 'mtcars' data
lm.1 <- lm(mpg ~ wt + vs + cyl, data = mtcars)
models<-dredge(lm.1) # default is ranked by AICc--WARNING, WARNING--THIS CAN TAKE A LONG TIME
models
importance(models)

# MuMIn will also compute model averages
mod.avg <- model.avg(models, subset = delta < 6, revised.var = TRUE)
mod.avg

# Now back to our orthodont models; pick one and dredge it
re.mods <- dredge(mod2, rank = BIC)
re.mods
importance(re.mods)

# Reset R to the default option for na.action
options(na.action = na.omit)

##############################################################
## Read data directly from website 
trout<-read.csv("http://sites.google.com/site/rforfishandwildlifegrads/home/week-8/Westslope.csv?attredirects=0&d=1") 

# Lets see what's in the data set
head(trout) 

# WSHD = watershed 
# SOIL_PROD = % of watershed with productive soils
# GRADIENT = % grade of stream bed
# WIDTH = mean width of the stream in meters
# PRESENCE = lamprey present or absent

## fit logistic regression with random effect output to model1 
model1 <-glmer(PRESENCE ~ SOIL_PROD + GRADIENT + WIDTH + (1|WSHD),data = trout, family = binomial) 
summary(model1) 

# fit remaining candidate models 
model2 <-glmer(PRESENCE ~ GRADIENT + WIDTH + (1|WSHD),data = trout, family = binomial) 
model3 <-glmer(PRESENCE ~ SOIL_PROD + (1|WSHD),data = trout, family = binomial) 
model4 <-glmer(PRESENCE ~ SOIL_PROD + WIDTH + (1 + SOIL_PROD|WSHD),data = trout, family = binomial) 
model5 <-glmer(PRESENCE ~ SOIL_PROD + WIDTH + (1 |WSHD),data = trout, family = binomial) 

# conduct model selection using BIC and the 'model.sel()' function from 'MuMIn'
my.models<-model.sel(model1,model2,model3,model4,model5,rank=BIC) 
my.models 

# calculate importance weights 
importance(my.models) 

## yes dredge works here too 
options(na.action = na.fail)
dredge(model1, rank = BIC)

r.squaredGLMM(model1) #The theoretical is based on the specified distribution
# the delta method for variance approximation uses a first-order Taylor series expansion
# This is only an outcome when the model is binomial in type
# The theoretical value is recommended for a log link type model
r.squaredGLMM(model2)
