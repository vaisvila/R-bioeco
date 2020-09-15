rm(list = ls())
setwd("~/Dropbox/R Biology/datasets-master")


# Always takes and gives a data frame
library(dplyr)


comp <- read.csv("compensation.csv")
glimpse(comp)

# Gets summary statistics
summary(comp)

names(comp)

# Selecting column(s)
select(comp, Fruit)
select(comp, -Root)


# Grabs row(s)
slice(comp, 2)
slice(comp, 2:10)
slice(comp, c(2, 3, 10))


# Subsetting
filter(comp, Fruit > 80)
filter(comp, Fruit > 80 | Fruit < 20)
lo_hi_fruit <- filter(comp, Fruit > 80 | Fruit < 20)
lo_hi_fruit


head(comp)

# Creating a new colum logFruit
compensation <- mutate(comp, logFruit = log(Fruit))

head(compensation)

# Sorting
arrange(compensation, Fruit)

# Taking the mean of Fruit while grouping by compensation and Grazing
mean.fruit <- summarise(
  group_by(compensation, Grazing), meanFruit = mean(Fruit))

compensation %>%
  group_by(Grazing) %>% 
    summarise(meanFruit = mean(Fruit))

compensation %>%
  group_by(Grazing) %>% 
    summarise(
      meanFruit = mean(Fruit), 
      sdFruit = sd(Fruit))


