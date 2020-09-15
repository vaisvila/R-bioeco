####Introduction to linear models
#25 Feb 2019
#ferrenbe@nmsu.edu
#Thank you to Selva Prabhakaran for this tutorial

#View the 'cars' data from Base R
head(cars)
?cars

#Now we fit the linear model
lm.1 <- lm(dist ~ speed, data=cars)
print(lm.1) #This output gives us an intercept and a B coefficent that tells us "dist = -17.579 + 3.932 * speed" 
summary(lm.1) #This output gives us the comprehensive view of the outcome

#Make a scatterplot to view the relationship of speed to stopping distance
attach(cars)
plot(speed, dist, xlab = "Driving speed (mph)", ylab = "Stopping distance (ft)")
abline(lm(dist ~ speed))
detach(cars)  

#Let's take a look for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

#Check to see if the mean of residuals is zero
mean(lm.1$residuals)

#Autocorrelation in the residuals?
acf(lm.1$residuals) #autocorrelation plot

##install.packages("lmtest")
library(lmtest) #you can use a specific test statistic called "Durbin-Watson test"
dwtest(lm.1)

#Are the X and residuals correlated?
cor.test(cars$speed, lm.1$residuals)

################Check for homoscedasticity##########
par(mfrow=c(2,2)) #So we can see multiple graphs at once
plot(lm.1)

library(lmtest) #We can use a specific test, in this case the Breusch-Pagan test
bptest(lm.1) #A small p-value indicates an issue with heteroscedasticity

###################Normality########################
#Test to make sure metrics are normally distributed 
#Shapiro-Wilk's method
#A small P-value suggest departure from normality
shapiro.test(cars$dist)

#Let's look at a qqplot more specifically for normality of residuals
ggqqplot(cars$dist)

#Density plot to check is response is normally distributed
##install.packages("e1071")
library(e1071) #This is a great package for testing assumptions
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

#Check many assumptions at once using the 'glvma' package
#Won't work with some version of R
install.packages("gvlma")
library(gvlma)

gvlma(lm.1)
gvlma.1 <- gvlma(lm.1)
plot(gvlma.1)

#################Multicollinearity##################
#We need a new data set so let's use 'mtcars'
names(mtcars)

#specify a new linear model
lm.2 <- lm(mpg ~ ., data = mtcars)
summary(lm.2) #compare the R2 to the adj R2

#load the 'car' library and complete a VIF (variance inflation factor) check
#VIF = 1/(1-R2)
#If the value exceeds 2 we need to consider multicollinearity as a possible issue
##install.packages("car")
library(car)
vif(lm.2)

##install.packages("corrplot")
library(corrplot)
corrplot(cor(mtcars[,-1]))

library(MASS)
step.lm.2 <- stepAIC(lm.2, direction = "both")
step.lm.2$anova

best.lm.1 <- lm(mpg ~ wt + qsec + am, data = mtcars)
summary(best.lm.1) #compare R2 to adj R2
