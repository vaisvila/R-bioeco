library(tidyverse)
setwd("~/Dropbox/R Biology")

# Load the main data file
Main <- read.csv("Creosote_seed_weights.csv", header=T)
names(Main)

Main$seed[is.na(Main$seed)] <- 0 # Fill NAs with zeros
Main$shrub <- as.character(Main$shrub) # Make shrub # a character variable
str(Main)

Main <- Main %>%
  mutate(binary = if_else(seed == 0, 0, 1)) %>% # Create binary variable for seed production
  glimpse()

# Make a proportional value of seed weight to mericarp weight
Main$prop.seed <- Main$seed / Main$mericarp

#### Let's assume we need to calculate a percentage value 
# and want to add the "%" sign for a table to our output
#### The long way--do this over and over for any variable of interest
# changing the variable name in the code

percent <- round(Main$prop.seed * 100, digits = 1)
result <- paste(percent, "%", sep = " ")
print(result)

#### The easily repeated way--Make a function and use it forever
# by calling the function for the varible name

calc_percent <- function(x){
  percent <- round(x * 100, digits = 1)
  result <- paste(percent, "%", sep = " ")
                  return(result)
}

# Example use of your function
calc_percent(Main$prop.seed)

#### Another example: perhaps you commonly use a specific transformation
# or index that would require another package be loaded but the package
# creates conflicts with other packages that are in use

hist(Main$prop.seed) # Are the proportions of mericarp weight made up by seeds normal?

#### A common transformation for variables on a 0 to 1.0 scale is the "logit"
# Logit is available in several packages but not base R, so let's make our own function
# We can make a transformation to logit for our proportional weight

logit_trans <- function(x){
  log(x) - log(1-x)
}

# Apply the transformation to the proportional weights and recheck the distribution

Main$logit_seed <- logit_trans(Main$prop.seed)

hist(Main$logit_seed) # Now it's better

#### How many mericarps have no seeds? Let's find the zeros

ZerosPerCol <- function(x) {
  D <- (x == 0)
  colSums(D)
}

ZerosPerCol(Main)

#### I ran into a frustration yesterday while working with a student on data
# We knew there were NAs in the data set and could find them in a list view
# but wanted a summary by each column--so I wrote this function for later

# A function to sum for NAs by column
NAsPerCol <- function(x) {
  D <- is.na(x)
  colSums(D)
}

NAsPerCol(Main) # Apply the function to the data object

#### How do we combine these functions into one?
ColumnInfo <- function(X1, Choice1) {
  if (Choice1 == "Zeros") { D = (X1==0)  }
  if (Choice1 == "NAs") { D <- is.na(X1) }
  colSums(D, na.rm = TRUE)
}

ColumnInfo(Main, "Zeros") # Here we need to specify which choice we want

#### Let's add a default choice to the function
ColumnInfo <- function(X1, Choice1 = "Zeros") {
  if (Choice1 == "Zeros") { D = (X1==0)  }
  if (Choice1 == "NAs") { D <- is.na(X1) }
  colSums(D, na.rm = TRUE)
}

ColumnInfo(Main) # defaults to zeros
ColumnInfo(Main, "NAs") # specified as NAs

#### Existing R functions have error messages to help you debug your code
# Make an intentional mistake and mis-spell "NAs" as "nas"

ColumnInfo(Main, "nas") # You get gibberish

#### Fix this in your function
ColumnInfo <- function(X1, Choice1 = "Zeros") {
  if (Choice1 == "Zeros") { D = (X1==0)  }
  if (Choice1 == "NAs") { D <- is.na(X1) }
  if (Choice1 != "Zeros" & Choice1 != "NAs") {
    print("Error in specified choice text") } else {
      colSums(D, na.rm = TRUE) }
}

# Make the same mistake again
ColumnInfo(Main, "nas")

#### YOUR TURN: WRITE A FUNCTION THAT CONVERTS mg TO ounces AND RUN IT ON THE SEED WEIGHTS
# mg * 0.000035274 = oz
# verify by plotting the seed weight in mg against seed weight in oz

mg_to_oz <- function(x){
  oz <- round(x * 0.000035274, digits = 5)
  return(oz)
}

Main$seed_wt_oz <- mg_to_oz(Main$seed)

library(ggplot2)

ggplot(data = Main, aes(x = seed, y = seed_wt_oz)) +
geom_point()

#### YOUR TURN: WRITE A FUNCTION THAT CONVERTS FAHRENHEIT INTO CELSIUS
# (F - 32) * (5/9)
# verify that it works by checking the freezing and boiling points

F_to_C <- function(x){
  C <- round((x-32) * (5/9), digits = 2)
  return(C)
}

F_to_C(32)
F_to_C(212)

