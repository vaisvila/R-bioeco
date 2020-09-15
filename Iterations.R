library(tidyverse)

data(iris) ##load data
str(iris)
head(iris)

#### When you have multiple Left Hand Side variables (LHS)
# with the same Right Hand Side (RHS) you can loop through analyses using several approaches

# Loop option 1
loop.lm.1 <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris)
summary(loop.lm.1)

# Loop option 2
# paste has 3 arguments. 
# paste (..., sep = " ", collapse = NULL)
# The '...' is the stuff you want to paste together
# sep and collapse get it done
# paste0 is short for 'paste(x, sep="")' so it's a simplier format
loop.lm.2 <- as.formula(paste0("cbind(", paste(names(iris[,-5]),
                                               collapse = ","), ") ~ Species"))
fit <- lm(loop.lm.2, data = iris)
summary(fit)

# Loop option 3
# sprintf() 
string_formula <- sprintf("cbind(%s) ~ .", toString(names(iris)[1:4])) # the % means "some element goes here
loop.lm.3 <- lm(string_formula, data = iris)
summary(loop.lm.3)

# Loop option 4
# using 'broom' package
library(broom)
loop.lm.4 <- iris %>%
  group_by(Species) %>%
  do(mod = lm(Sepal.Length ~ Sepal.Width, data = .))

glance(loop.lm.4, mod)
tidy(loop.lm.4, mod)

# Option using the apply family
dvList <- names(iris)[1:4]

model <- lapply(dvList, function(x) {
  lm(substitute(i~Species, list(i = as.name(x))), data = iris)})

lapply(model, summary)

#### What if our data is in different frames?
# Here is an example to extract slopes
dfs <- split(iris, iris$Species)

fit.mods <- lapply(dfs, function(x) {
  lm(formula = Sepal.Length ~ Species, data = iris)
})

slopes <- sapply(fit.mods, function(x) x$coefficients)
slopes

