# Linear Models Continued
# 27 February 2019

setwd("~/Dropbox/R Biology")

# Load the tidyverse
library(tidyverse)

# Load the data sets
Main <- read.csv("Resin_defense_wide.csv", header = TRUE, stringsAsFactors=FALSE)
names(Main)

Site <- read.csv("Resin_defense_sites.csv", header = TRUE, fileEncoding="latin1")
names(Site)

# Join the two data sets
Main <- inner_join(Main, Site, by = "Site")

# Create three new variables and rename one
Main <- Main %>%
  mutate(Res_ducts = rowSums(select(.,RD1:RD10)), Rad_grow = rowSums(select(.,RG1:RG10))) %>%
  mutate(DBH = rad_mm*2) %>%
  mutate(Spp = Spp.) %>%
  glimpse()

# Get a summary of the data
# Take note, remove the first na.rm = TRUE statement below and see what happens
# You have to place the na.rm = TRUE within the function call in dplyr
Summary.tab <- Main %>%
  group_by(Spp) %>%
  summarize(
    meanRD = mean(Res_ducts, na.rm = TRUE),
    meanRG = mean(Rad_grow),
    meanDBH = mean(DBH, na.rm = TRUE),
    meanAge = mean(Age, na.rm = TRUE),
    rangeElev = max(Elev_m_asl) - min(Elev_m_asl)
  )
Summary.tab


# Plot the relationship among the total radial growth and total resin ducts by species
plot1 <- ggplot(Main, aes(x = Rad_grow, y = Res_ducts, color = Spp)) + 
  geom_point(size = 1, shape = 19) +
  geom_smooth (method = lm, se = FALSE) +
  scale_y_continuous(name = "Resin ducts per annual ring") +
  scale_x_continuous(name = "Annual ring radial growth (mm)") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        legend.position = "right")
plot1

# Examine all colleciton sites with facet wrap
plot2 <- ggplot(Main, aes(Rad_grow, Res_ducts)) + geom_point(shape = 1)+ geom_smooth(method="lm") +
  facet_wrap(~Site, scales = "free")
plot2

####### Make some linear models showing how resin duct total responds to growth, age, DBH, and elevation
#Load packages for testing model assumptions
library(gvlma)
library(fitdistrplus)
library(ggpubr)
library(e1071)
library(lmtest)

is.na(Main$Res_ducts)
Main.2 <- na.omit(Main)
plotdist(Main.2$Res_ducts, histo = TRUE, demp = TRUE)

#Density plot to check is response is normally distributed
plot(density(Main.2$Res_ducts), main="Density Plot: Res_ducts", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(Main.2$Res_ducts), col="red")

#qq plot
ggqqplot(Main$Res_ducts)

#Compare to theoretical distributions
descdist(Main.2$Res_ducts, discrete=FALSE, boot=500)

#Fit to a lognormal distribution and view
fit <- fitdist(Main.2$Res_ducts, "lnorm")
plot(fit)

######## Not looking good in terms of normality but a log fit seems much better
######## Let's use a log transformation on the 'Res_duct' variable and reconsider
Main.2$log_RD <- log(Main.2$Res_ducts)

plot(density(Main.2$log_RD), main="Density Plot: Res_ducts", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(Main.2$log_RD), col="red")

#qq plot
ggqqplot(Main.2$log_RD)

######### Looks better now so let's make our models

# Model 1--does not include the species information so this a multiple regression
mod.1 <- lm(log_RD ~ Rad_grow + Age + Elev_m_asl + DBH, data = Main.2)
summary(mod.1)
anova(mod.1) #calling this function will give you an ANOVA type table for summary
AIC(mod.1) #this is how you can get an AIC score
BIC(mod.1) #this is how you can get a BIC score

# Now we can apply some other assumptions tests
gvlma.lm(mod.1)
shapiro.test(Main.2$log_RD) #does this test reflect reality?
bptest(mod.1, studentize = TRUE) #Who do we trust?

par(mfrow=c(2,2)) #So we can see multiple graphs at once
plot(mod.1)

# Model 2--includes species as a term so this becomes an ANCOVA
mod.2 <- lm(log_RD ~ Rad_grow + Age + Elev_m_asl + DBH + Spp, data = Main.2)
summary(mod.2)
anova(mod.2)
AIC(mod.2)
BIC(mod.2)

gvlma.lm(mod.2)
bptest(mod.2, studentize = TRUE) #Who do we trust?

par(mfrow=c(2,2)) #So we can see multiple graphs at once
plot(mod.2)

mean(mod.2$residuals)
bgtest(mod.2)


##### Now we're at a decision point: do we keep species in the model or make individual models for each?
##### The answer depends on your research question. My question is about what controls defenses across 
##### environmental gradients so I plan to break the species apart

# Using 'dplyr' and 'broom' you can make a model for each species without a loop
# With species removed, these return to being multiple regressions
# call your modelling 'loop'
library(broom)
mod.3 <- Main %>%
  group_by(Spp) %>%
  do(mod = lm(Res_ducts ~ Rad_grow + Age + Elev_m_asl + DBH, data = .))

# Get the summary information and coefficents using functions from 'broom'
mod.summary = glance(mod.3, mod)
mod.summary
mod.coef = tidy(mod.3, mod)
mod.coef

