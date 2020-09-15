#####Rcode for mistletoe infection effects on P. contorta growth & defense
#Script by Scott Ferrenberg
#ferrenbe@nmsu.edu
#Version 1, 23 Jan 2019

#What is your working directory
getwd()
setwd("~/Dropbox/R Biology")

####Load the data
Main <- read.csv("Mistletoe_data_day2.csv")
structure(Main)
summary(Main)

#Is something numeric or a character variable?
#R will see anything that is a number as a numeric value even if it's an ID
#How do we check and change this?
is.numeric(Main$ID)
Tree.ID <- as.factor(Main$ID)
is.numeric(Tree.ID)

####calculate relative cross sectional growth rates and add them to the data frame
Main$RCAGR5 <- Main$RGR5/Main$DBH

#Transform some variables and add them to the data frame
Main$Res.duct.area <- log(Main$Res_duct_area)

#Maybe you want Z-scores
Main$Z_Res.duct.area <- scale(Main$Res_duct_area, center = TRUE, scale = TRUE)

names(Main)

#Get basic summary info
max(Main$Res_duct_area)

####Assumptions check
#Test to make sure metrics are normally distributed 
#Shapiro-Wilk's method
#A small P-value suggest departure from normality
attach(Main)
shapiro.test(Res_duct_area)
shapiro.test(log(Res_duct_area))
detach(Main)

#Install ggpubr
##install.packages("ggpubr", repos="http://cran.us.r-project.org")

#Check the density plot and Q-Q plots# 
library(ggpubr)

#Resin duct area 5 yr
#Make a density plot
ggdensity(Main$Res_duct_area, main = "density", xlab = "Response")

#How does it compare to a histogram?
hist(Main$Res_duct_area)

#Make a qqplot
ggqqplot(Main$Res_duct_area)

#Relative cross sectional growth
ggdensity(log(Main$RCAGR5), main = "density", xlab = "Response")
ggqqplot(log(Main$RCAGR5))

####Linear models
mod1 <- lm(Main$Res_duct_area ~ log(Age) + Mt_rating + RCAGR5, data = Main)
summary(mod1)

mod2 <- lm(Main$Res_duct_area ~ RCAGR5, data = Main)
summary(mod2)

attach(Main)
plot(Res_duct_area ~ RCAGR5, xlab = "Relative cross-sectional growth rate", 
     ylab = "Resin duct area (-5yr)", pch = 19, cex.lab=1.5, cex.axis=1.5)
abline(lm(Res_duct_area ~ RCAGR5), col = "red", lwd = 2)
lines(lowess(Res_duct_area ~ RCAGR5), col= "blue", lwd = 2, lty = "dashed")

#Save the graph to your working directory as a high res png file
#Adjust the width and height of the file to match your target size
dev.copy(png,"Res.duct.area_Rel.growth.rate.png",width=20,height=20,units="cm",res=300)
dev.off()
detach(Main)

#Let's make a categorical, bianary variable for mistletoe infection
#If a tree has any infection = 1, if zero infection = 0
#We will use the ifelse() function
Mt.binom <- ifelse(Main$Mt_rating > 0, 1, 0)
Mt.binom

#See this website for more ways to use 'ifelse()'
#http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual4.html

#During an earlier session, assume you learned that mistletoe infection is related to tree age
#You could perform a bionomial regression--good idea
#Or you could try a linear discriminant analysis--let's do that one because it's more fun
library(MASS)
DA.mod <- lda(Mt.binom ~ log(Age), data=Main)
predict.mod <- predict(DA.mod, Main)
table(Mt.binom, predict.mod$class)
#17/21 correctly categorized = 0.8095 correctly classified with just tree age