setwd("~/Dropbox/R Biology/datasets-master")

nasty.format <- read.csv("nasty format.csv")

str(nasty.format)

##install.packages("dplyr")
 library(dplyr)

# Only keeping rows that have an entry in the bottle variable
nasty.format <- filter(nasty.format, Bottle != "")

glimpse(nasty.format)

##install.packages("tidyr")
library(tidyr)

# Making a date and abundance column from the data in columns 4-11
tidy_data <- gather(nasty.format, Date, Abundance, 4:11)

glimpse(tidy_data)

##install.packages("stringr")
library(stringr)

# Getting rid of the x in the Date column
tidy_data <- mutate(tidy_data, Date = substr(Date, 2, 20))

##install.packages("lubridate")
library(lubridate)

unique(tidy_data$Date)

# Transforming into nice Date format for Date column
tidy_data <- mutate(tidy_data, Date = dmy(Date))

glimpse(tidy_data)

library(ggplot2)

ggplot(data = tidy_data, aes(x=Date, y=Abundance)) +
  geom_point() +
  facet_wrap(~Bottle)



