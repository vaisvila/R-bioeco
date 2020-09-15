####BIOL550
####Spring 2019
####Intro to R, day 1

#Let's install a package
#install.packages("car", dependencies = TRUE)

#Now we load the package
library(car)

#R has a large number of built in data sets
#Let's call upon one called "mtcars"
names(mtcars) #get the names of the variables in the data set--useful for when you're making models and ensuring spelling is correct
head(mtcars) #see the top few rows of data--useful when you want to see if the data are integers, categories, etc.
dim(mtcars) #get the dimension in rows x cols--useful for when you're subsetting a data frame

#If you want to see a larger portion or all of the data, an alternative is to call for the data structure
structure(mtcars)

#With 'car' loaded and the data from 'mtcars' we can make a simple linear model
#Start with a regression of 'mpg' to 'wt'
lm(mpg~wt, data = mtcars)

#We need more information from our model
#If we make the model an R object we can get a lot more info by using 'summary()'
lmfit.1 <- lm(mpg~wt, data = mtcars)
summary(lmfit.1)

#A simple scatterplot would be helpful since this a linear regression
scatterplot(mpg ~ wt, data=mtcars)

#'mpg' is strongly related to 'wt', but what about other variables?
#Let's try the number of cylinders in the motor or 'cyl'
lmfit.2 <- lm(mpg ~ cyl, data = mtcars)
summary(lmfit.2)
scatterplot(mpg ~ cyl, data = mtcars) #now we have 3 categories of engine types, is a scatterplot the best?

#Let's make a box and whisker plot instead to show the categories of engines
boxplot(mpg~cyl, data = mtcars) #works but could use some labels

#Add axis labels
boxplot(mpg~cyl, data = mtcars, xlab = "Number of cylinders", ylab = "Miles per gallon")

#Add colors for the boxes to show categories
boxplot(mpg~cyl, data=mtcars, col = (c("green", "gold", "red")), xlab = "Number of cylinders", ylab = "Miles per gallon")

#It looks like 'cyl' and 'wt' are similarly good predictors of 'mpg'
#Are these 2 varibles correlated?
lmfit.3 <- lm(wt ~ cyl, data = mtcars)
summary(lmfit.3)
scatterplot(wt ~ cyl, data = mtcars)

#What if you don't want to type "data = mtcars" every time you do something?
#You can attach the data temporarily
#Caution: if you don't detach the data you can accidentally call on it when you mean something else
#When you're done use "dettach(mtcars)"
attach(mtcars)
lmfit.4 <- lm(mpg ~ wt)
summary(lmfit.4)
scatterplot(mpg ~ wt)
detach(mtcars)

#####Try these activities############################
#Find and fix the mistakes in the next 2 lines of code
lmfit.4 <- lm(mpg ~ disp, data = mtcars)
summary(lmfit.4)

#Once you've fixed the above code, make a scatterplot and add axis labels

#Finish this code to make a linear model that combines 2 or more predictors of mpg and get a summary
lmfit.5 <- lm(mpg ~ disp+wt, data = mtcars)
summary(lmfit.5)


