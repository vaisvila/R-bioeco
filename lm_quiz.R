############## Linear Model Quiz #################################################
# COMPLETE THE FOLLOWING 6 CHALLENGES

############## 1. Check to see if 'mpg' in the 'mtcars' data is normal########
library(tidyverse)
library(ggpubr)
head(mtcars)
shapiro.test(mtcars$mpg)
ggqqplot(mtcars$mpg)

# Make numbered categories into character variables for tests
# Note the names of these new variables
mtcars$motor <- as.character(mtcars$vs)
mtcars$trans <- as.character(mtcars$am)
is.numeric(mtcars$motor)
is.numeric(mtcars$trans)

############## 2. Make a one-way ANOVA testing the influence of 'motor' on 'mpg' ############
mtcars.1anova <- aov(mpg ~ motor, data = mtcars)
summary(mtcars.aov)

############## 3. Are your data heteroscedastic? #####################
bartlett.test(mpg ~ motor, data = mtcars)

############## 4. Make a two-way ANOVA testing the influence of 'motor' and 'trans' on 'mpg' ############
mtcars.2anova <- aov(mpg ~ motor+trans, data = mtcars)
summary(mtcars.aov)

############## 5. Make boxplots for your two ANOVA models from above ####################
ggboxplot(mtcars, x = "motor", y = "mpg",
          ylab = "MPG", xlab = "Motor")

ggboxplot(mtcars, x = "motor", y = "mpg", color = "trans",
          palette = c("hot pink", "chartreuse"))
############## 6. Perform an ANCOVA that uses 'motor' and 'wt' to predict 'mpg'########################
mtcars.ancova <- aov(mpg ~ motor+wt, data = mtcars)
summary(mtcars.aov)

