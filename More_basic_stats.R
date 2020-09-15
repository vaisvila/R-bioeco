# More common statistics
# Two way ANOVA
# Chi-square tests
# Nonparametric tests

# REFRESHER 1: CALL A LIST AND GET SUMMARY STATS
mt <- mtcars[c("mpg", "hp", "wt", "am")]
head(mt)
summary(mt)

# REFRESHER 3: DEFINE A FUNCTION
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}

# REFRESHER 4: USE sapply TO APPLY A FUNCTION TO A LIST WITHIN AN OBJECT
myvars <- c("mpg", "hp", "wt")
sapply(mtcars[myvars], mystats)

#### Now let's learn to make nested models
#install.packages("vegan")
library(vegan) # Vegan is the package most often used for community ecology
data(dune)     # The dune and dune.env data sets are part of Vegan
data(dune.env)
?dune.env      # Have a look at what's in the seconary data matrix

Shannon <- diversity(dune) # Calculate Shannon Diversity on the plant data
Manure <- as.character(dune.env$Manure) # Make the ordinal levels into a character variable
is.numeric(Manure)

hist(Shannon)
shapiro.test(Shannon) # Remember, this test is biased in favor of normality at low sample sizes

attach(dune.env)
nested_anova <- lm(Shannon ~ Management + Management:Manure) # Manure addition is nested into the management type
anova(nested_anova)
detach(dune.env)

boxplot(Shannon ~ Management * Manure, data = dune.env) # Our example is not the best because there is no replication in some groups

plot(nested_anova)

################################ Contigency Tables and Chi-squares#####################
#install.packages("vcd")
library(vcd)
head(Arthritis)

# One way table
mytable <- with(Arthritis, table(Improved))
mytable  # frequencies
prop.table(mytable) # proportions
prop.table(mytable)*100 # percentages

# Two way table
mytable.2 <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable.2 # frequencies
margin.table(mytable.2,1) #row sums
margin.table(mytable.2, 2) # column sums
prop.table(mytable.2) # cell proportions
prop.table(mytable.2, 1) # row proportions
prop.table(mytable.2, 2) # column proportions
addmargins(mytable.2) # add row and column sums to table

# More complex tables
addmargins(prop.table(mytable.2))
addmargins(prop.table(mytable.2, 1), 2)
addmargins(prop.table(mytable.2, 2), 1)

# Two way table using CrossTable
#install.packages("gmodels")
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

# Three way table
mytable.3 <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable.3
ftable(mytable.3) 
margin.table(mytable.3, 1)
margin.table(mytable.3, 2)
margin.table(mytable.3, 2)
margin.table(mytable.3, c(1,3))
ftable(prop.table(mytable.3, c(1,2)))
ftable(addmargins(prop.table(mytable.3, c(1, 2)), 3))

# Chi-square test of independence
# Determines whether there is a significant difference between 
# the expected frequencies and the observed frequencies in one or more categories
library(vcd)
mytable.4 <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable.4)

mytable.5 <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable.5)

# Fisher's exact test
# Tests the association between two categorical variables 
# when you have small cell sizes (expected values less than 5)
mytable.6 <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable.6)

# Mantel-Haenszel test:
# Perform a Cochran-Mantel-Haenszel ?? chi test of the null hypothesis 
# that two nominal variables are conditionally independent.
mytable.7 <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable.7)

# Measures of association for a two-way table use 'assocstats()'
# Phi correlation is a post-test to give this additional information.
# Phi varies between -1 and 1. Close to 0 it shows little association between variables. 
# Close to 1, it indicates a strong positive association. 
# Close to -1 it shows a strong negative correlation.
# Remember that Phi is only of use in 2x2 tables. Where tables are larger, use Cramer's V
# If C is near zero (or equal to zero) you can conclude that your variables are independent 
# C can only take on positive values.
# Cramer's V is a post-test to give this additional information.
# Cramer's V varies between 0 and 1. Close to 0 it shows little association between variables. 
# Close to 1, it indicates a strong association.
library(vcd)
mytable.8 <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable.8)

art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
art

par(mar=c(2.1,2.1,2.1,2.1))
mosaic(art, gp = shading_max, split_vertical = TRUE)

########################## Non-parametric Tests ##########################################
# Wilcoxon two group comparison--aka, Mann-Whitney U Test
# Nonparametric version of t-tests
# We will illustrate our nonparametric tests with the 'iris' data in Base R
names(iris)
str(iris)

# Kolmogorov-Smirnov Test that samples are from the same distribution
# Learn to love this test: it does not assume normality and variance is not an issue
ks.test(iris[iris$Species == "setosa",]$Sepal.Length, iris[iris$Species == "versicolor",]$Sepal.Length)
ks.test(iris$Sepal.Length, iris$Sepal.Width)

# Make 2 random samples
x <- sample(x = 1:100, size  = 30)
y <- sample(x = 1:100, size  = 30)

x # Have a look and you'll find they are indeed different samples
y

ks.test(x, y) # We'll find > 95% of the time that these two groups are not different via the K-S test

# Drop 1 species so we can do some pairwise tests
library(tidyverse)
iris.1 <- iris %>%
  filter(Species != "setosa") %>%
  glimpse()

# A Wilcox test of Sepal Length in the two remaining iris species
wilcox.test(Sepal.Length ~ Species, data = iris.1)

# Drop another species to leave only one
iris.2 <- iris.1 %>%
  filter(Species == "versicolor") %>%
  glimpse()

# Compare the Petal and Sepal Lengths in an individual flower using a paired Wilcox test
sapply(iris.2[c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")], median)
with(iris.2, wilcox.test(Petal.Length, Sepal.Length, paired=TRUE))

# Go back to the 'iris' data with all 3 species
# Test for homogeneity of variances with a Bartlett Test
library(stats)
bartlett.test(Sepal.Length ~ Species, data = iris) #suggets we violate a key assumption of ANOVA
ggplot(iris, aes(x=Sepal.Length, color=Species)) +
  geom_histogram(fill="white")

#install.packages("FSA")
library(FSA)
hist(Sepal.Length ~ Species, data = iris)

# Compare Sepal Length among the 3 species
# Kruskal Wallis test--nonparametric version of ANOVA
kruskal.test(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Length ~ Species, data = iris)

#install.packages("FSA")
library(FSA)
dunnTest(Sepal.Length ~ Species, data = iris)

# Non-parametric tests still assume equal variances; do we have that in our data?
iris.lm <- lm(Sepal.Length ~ Species, data = iris)
bartlett.test(Sepal.Length ~ Species, data = iris)
gvlma::gvlma(iris.lm)

# This is great time to use the Welch's F-test
library(onewaytests)
welch.test(Sepal.Length ~ Species, data = iris)

# Pairwise Games-Howell test--a posthoc test suited to unequal variances
# Tukeys, pairwise t, etc., all assume equal variances so we follow Welch's with Games-Howell
# You can try the package 'userfriendlyscience' but it works with a limited suite of R versions
install.packages("userfriendlyscience")
library(userfriendlyscience)

# To run the following function without the 'userfriendlyscience' package, we can
# can first run the function below to make our own version of the Games-Howell Test

posthoc.tgh(y = iris$Sepal.Length, x = iris$Species, method = "games-howell")
posthoc.tgh(y = iris$Sepal.Length, x = iris$Species, method = "tukey")

#### The following is the Games-Howell & Tukey Function
#### This is the code behind the test so to speak

posthoc.tgh <- function(y, x, method=c("games-howell", "tukey"), digits=2) {
  method <- tolower(method);
  tryCatch(method <- match.arg(method), error=function(err) {
    stop("Argument for 'method' not valid!");
  });
  
  res <- list(input = list(x=x, y=y, method=method, digits=digits));
  
  res$intermediate <- list(x = factor(x[complete.cases(x,y)]),
                           y = y[complete.cases(x,y)]);
  res$intermediate$n <- tapply(y, x, length);
  res$intermediate$groups <- length(res$intermediate$n);
  res$intermediate$df <- sum(res$intermediate$n) - res$intermediate$groups;
  res$intermediate$means <- tapply(y, x, mean);
  res$intermediate$variances <- tapply(y, x, var);
  
  res$intermediate$pairNames <- combn(levels(res$intermediate$x),
                                      2, paste0, collapse=":");
  
  res$intermediate$descriptives <- cbind(res$intermediate$n,
                                         res$intermediate$means,
                                         res$intermediate$variances);
  rownames(res$intermediate$descriptives) <- levels(res$intermediate$x);
  colnames(res$intermediate$descriptives) <- c('n', 'means', 'variances');
  
  ### Start on Tukey
  res$intermediate$errorVariance <-
    sum((res$intermediate$n-1) * res$intermediate$variances) /
    res$intermediate$df;
  res$intermediate$t <- combn(res$intermediate$groups, 2, function(ij) {
    abs(diff(res$intermediate$means[ij]))/
      sqrt(res$intermediate$errorVariance*sum(1/res$intermediate$n[ij]));
  } );
  res$intermediate$p.tukey <- ptukey(res$intermediate$t*sqrt(2),
                                     res$intermediate$groups,
                                     res$intermediate$df,
                                     lower.tail=FALSE);
  res$output <- list();
  res$output$tukey <- cbind(res$intermediate$t,
                            res$intermediate$df,
                            res$intermediate$p.tukey)                                     
  rownames(res$output$tukey) <- res$intermediate$pairNames;
  colnames(res$output$tukey) <- c('t', 'df', 'p');
  
  ### Start on Games-Howell
  res$intermediate$df.corrected <- combn(res$intermediate$groups, 2, function(ij) {               
    sum(res$intermediate$variances[ij] /
          res$intermediate$n[ij])^2 / 
      sum((res$intermediate$variances[ij] /
             res$intermediate$n[ij])^2 / 
            (res$intermediate$n[ij]-1));
  } );
  res$intermediate$t.corrected <- combn(res$intermediate$groups, 2, function(ij) {               
    abs(diff(res$intermediate$means[ij]))/
      sqrt(sum(res$intermediate$variances[ij] /
                 res$intermediate$n[ij]));
  } );    
  res$intermediate$p.gameshowell <- ptukey(res$intermediate$t.corrected*sqrt(2),
                                           res$intermediate$groups,
                                           res$intermediate$df.corrected,
                                           lower.tail=FALSE)  
  res$output$games.howell <- cbind(res$intermediate$t.corrected,
                                   res$intermediate$df.corrected,
                                   res$intermediate$p.gameshowell);
  rownames(res$output$games.howell) <- res$intermediate$pairNames;
  colnames(res$output$games.howell) <- c('t', 'df', 'p');
  
  ### Set class and return object
  class(res) <- 'posthocTukeyGamesHowell';
  return(res);
  
}

print.posthocTukeyGamesHowell <- function(x, digits=x$input$digits, ...) {
  print(x$intermediate$descriptives, digits=digits);
  cat('\n');
  if (x$input$method == 'tukey') {
    print(x$output$tukey);
  }
  else if (x$input$method == 'games-howell') {
    print(x$output$games.howell, digits=digits);
  }
}

