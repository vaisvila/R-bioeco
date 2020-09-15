#BIOL550 Day 4
#Continuing with data operations and wrangling
#30 January 2019
#A few refreshers
#R has many built-in data sets for helping you to learn how to use it
#You can see them with 'data()'
data()
?trees


#On this list there is a data set called 'trees'
#Have a look
trees


#Get a summary of the trees data
summary(trees)


#Add a variable to trees using the $ approach to extracting or adding an element
trees$vol.ht.ratio <- trees$Volume/trees$Height


#Did it add the variable?
names(trees)


#What's the mean and standard error, etc., of this new variable?
mean(trees$vol.ht.ratio)
min(trees$vol.ht.ratio)
max(trees$vol.ht.ratio)


#Let's image a scenario where we want to define a function and apply it to our data
#An example of this might be a case where a new biodiversity index is published in a journal
#and you want to apply it to your data set
#For simplicity, we're going to make a function that calculates the standard error in this example
#but anything is possible
#To identify a function we use 'function(x)' and the curly brackets {}
#{} are for identifying conditional elements and you will see them in loop operations as well


#Identify your function, in this case a calculation of standard error
#RStudio automatially formats your code to approximate a universal standard, hence the indents
#hanging brackets
SE <- function(x) {
  n <- length(x)
  sd(x)/sqrt(n)
}


#Now call your function on height and our ratio
SE(trees$Height)
SE(trees$vol.ht.ratio)


#If you're wondering, yes, you can combine the 'function(x)' approach with 
#the 'apply' family of functions to save time
#Here we're calling for our function to be applied to all of the columns in the "trees" data set
apply(trees, 2, SE)


#If we make this an object, we can use it again later in a summary table
summary.SE <- apply(trees, 2, SE)
summary.SE


#Let's get more summary info for our table
summary.mean <- apply(trees, 2, mean)
summary.med <- apply(trees, 2, median)
summary.min <- apply(trees, 2, min)
summary.max <- apply(trees, 2, max)


#Let's use r bind to put these together into one data frame
summary.table <- rbind(summary.SE, summary.mean, summary.med, summary.min, summary.max)
summary.table


#This table is useful but maybe you want to change the row and column names
#We can change these with 'rownames()' and 'colnames()'
rownames(summary.table) <- c("Std. error", "Mean", "Median", "Min value", "Max value")
colnames(summary.table) <- c("DBH (in.)", "Height (ft.)", "Vol (cubic ft.)", "Vol:Ht")
summary.table


#If this was all you needed and want a table, you can use 'write.table()' to save it
write.table(summary.table, "trees_summary_table.csv", sep = ",")


#Let's do some work in 'dplyr'
#dplyr has 5 basic verbs: filter, select, arrange, mutate, summarise
#dplyr is naturally paired with 'tidyr' which has a number of additional functions for preparing and altering data
library(dplyr)
library(tidyr)


#the functions 'glimpse()' and 'tbl_df()' are useful for having a look at your data
glimpse(summary.table)
tbl_df(summary.table)
summary.table


#Let's convert the 'summary.table' into a data frame and add the rownames as a column in 'dplyr'
#dplyr and tidyr are growing in popularity because they allow us to use piping which is %>%
#Piping is a sort of pass through command that says "use the result from the left in the operation on the right"
#You can make a long series of pipes reducing the need to recall an object or use loops


#A quick example of using a pipe based on the table we made above
#This bit of code calls the object once and then makes it a data frame and appends each row number as a column in the data sheet
summary.table2 <- summary.table %>% 
  as_data_frame %>%
tibble::rownames_to_column(var = "Measure")
summary.table2


summary.table2$Measure <- c("Std.error", "Mean", "Median", "Min value", "Max value")
summary.table2


#####################################################
#Now another, more complicated example
#Credit for data and the idea behind this exercise goes to: Dr. No?mie Becker, Dr. Benedikt Holtmann, Prof. Dirk Metzler
#Load 3 data sets


rm(list = ls())
setwd("~/Downloads")


Fish_survey_long <- read.csv("Fish_survey_long.csv", header = TRUE, stringsAsFactors=FALSE)
Water_data <- read.csv("Water_data.csv", header = TRUE,stringsAsFactors=FALSE)
GPS_location <- read.csv("GPS_data.csv", header = TRUE, stringsAsFactors=FALSE)


#Get a sense of these data
dim(Fish_survey_long)
dim(Water_data)
dim(GPS_location)

#These data sets are all of different lengths and widths so we couldn't use a simple 'cbind' or 'rbind' approach
#tidyr is what we need unless we want to use the 'reshape2' package
#Use a 'tidyr' join command Join the Fish and Water data
Fish_and_Water <- inner_join(Fish_survey_long, Water_data,
                             by = c("Site", "Month"))

#Take a look at your work
str(Fish_and_Water)
head(Fish_and_Water)

#Add the location information to the data set
Fish_survey_combined <- inner_join(Fish_and_Water, GPS_location,
                                   by = c("Site", "Transect"))

#Check your work
str(Fish_survey_combined)
head(Fish_survey_combined)

####################################################
#Another example, this time using bird behavior data
Bird_Behaviour<- read.csv("Bird_Behaviour.csv", header = TRUE, stringsAsFactors=FALSE)

#Get an overview
str(Bird_Behaviour)


#Three ways adding a new variable (log of FID)
#Using $
Bird_Behaviour$log_FID1 <- log(Bird_Behaviour$FID)
 
 
#Using [ ] - operator
Bird_Behaviour[ , "log_FID2"] <- log(Bird_Behaviour$FID)


#Using mutate() from dplyr package
Bird_Behaviour <- mutate(Bird_Behaviour, log_FID3 = log(FID))



#You can verify that all 3 worked using a plethora of functions
head(Bird_Behaviour) #Header + top 5 or 6 rows of data
str(Bird_Behaviour) #Detailed structure information
names(Bird_Behaviour) #Just the column names
glimpse(Bird_Behaviour) #a diplyr function that shows you a more detailed version of str
Bird_Behaviour #The longest output; shows you the entire object


#Split one column into two using separate() from dplyr package
#'remove = TRUE' removes the original column or columns and leaves the new ones
Bird_Behaviour <- separate(Bird_Behaviour, Species,
                           c("Genus","Species"), sep="_", remove=TRUE)


#See what happened
head(Bird_Behaviour)



#Let's pretend the genus and species names were originally separated in our data
#Combine these two columns using unite() from .dyr package
Bird_Behaviour <- unite(Bird_Behaviour, "Genus_Species",
                        c(Genus, Species), sep="_", remove=TRUE)


#See what happened
head(Bird_Behaviour)


#Re-split the Genus_species column for use below
Bird_Behaviour <- separate(Bird_Behaviour, Genus_Species,
                           c("Genus","Species"), sep="_", remove=TRUE)
Bird_Behaviour


#Now let's subset some data! YEAH!
#Subsetting using [ ] - operator--4 examples
Bird_Behaviour[ , 1:4] # selects the first 4 columns
Bird_Behaviour[c(2,3), ] # selects rows 2 and 3
Bird_Behaviour[1:3, 1:4] # selects the rows 1 to 3 and columns 1 to 4
Bird_Behaviour[c(1:3, 6), c(1:4, 8)] # selects the rows 1 to 3 and 6, and the columns 1 to 4 and 8


#Subsetting using [ ] and $ operator--2 examples
Bird_Behaviour[Bird_Behaviour$Sex == "male", ] # selects all rows with males
Bird_Behaviour[Bird_Behaviour$Year == "2013", ] # selects all rows from 2013

#Subsetting using 'subset()'--3 examples increasing in complexity
subset(Bird_Behaviour, FID < 10) #1 selects all rows with FID smaller than 10m
subset(Bird_Behaviour, FID < 10 & Sex == "male") #2 selects all rows for males with FID smaller than 10m


#3 Selects all rows that have a value of FID greater than 10 or less than 15; We keep only the IND, Sex and Year column
subset(Bird_Behaviour, FID > 10 & FID < 15, select = c(Ind, Sex, Year)) 


#Subsetting with 'dplyr' functions
#Using by rows using slice() and filter()--2 examples
Bird_Behaviour.slice <- slice(Bird_Behaviour, 3:5) #selects rows 3-5
Bird_Behaviour.filter <- filter(Bird_Behaviour, FID < 5) #selects rows that meet certain criteria


#Subsetting by columns using select()--2 examples
Bird_Behaviour_col <- select(Bird_Behaviour, Ind, Sex, Fledglings) #selects the columns Ind, Sex, and Fledglings
Bird_Behaviour_reduced <- select(Bird_Behaviour, -Disturbance) #excludes the variable disturbance


#Other useful functions in 'dplyr'
#Taking a random sample of rows using 'sample_frac()' and 'sample_n()'
Bird_Behaviour.50 <- sample_frac(Bird_Behaviour, size = 0.5, replace=FALSE) # takes randomly 50% of the rows
Bird_Behaviour_50Rows <- sample_n(Bird_Behaviour, 50, replace=FALSE) # takes randomly 50 rows


#Get the overall mean for FID using summarise() and mean()
summarise(Bird_Behaviour, mean.FID=mean(FID))


#The whole enchilada--run this as a block of code
summarise(Bird_Behaviour,
          mean.FID=mean(FID), # mean
          min.FID=min(FID), # minimum
          max.FID=max(FID), # maximum
          med.FID=median(FID), # median
          sd.FID=sd(FID), # standard devia+on
          var.FID=var(FID), # variance
          n.FID=n()) # sample size


#Get summaries for each species
#Before you calculate summaries, you have to apply the 'group_by()' function
Bird_Behaviour_by_Species <- group_by(Bird_Behaviour, Species)


Summary.species<- summarise(Bird_Behaviour_by_Species,
                            mean.FID=mean(FID), # mean
                            min.FID=min(FID), # minimum
                            max.FID=max(FID), # maximum
                            med.FID=median(FID), # median
                            sd.FID=sd(FID), # standard devia+on
                            var.FID=var(FID), # variance
                            n.FID=n()) # sample size
as.data.frame(Summary.species)
