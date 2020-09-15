library(tidyverse)

Bird_Behaviour<- read.csv("Bird_Behaviour.csv", header = TRUE, stringsAsFactors=FALSE)
names(Bird_Behaviour)

Bird_Behaviour %>%
  select(Species, Sex, FID) %>%
  glimpse()

#A few neat options
#Selecting based on partial matches
Bird_Behaviour %>%
  select(contains("FI")) %>%
  glimpse()

Bird_Behaviour %>%
  select(contains("FI"), ends_with("s")) %>%
  glimpse()

#Selecting based on regex
Bird_Behaviour %>%
  select(matches("i+es")) %>%
  glimpse()

Bird_Behaviour %>%
  select(matches("e.+i")) %>%
  glimpse()

#Selecting by data type
Bird_Behaviour %>%
  select_if(is.numeric) %>%
  glimpse()

Bird_Behaviour %>%
  select_if(is.character) %>%
  glimpse()

#Select using common logical expressions
#Selecting by data type
#Notice the '~' used below. We use this to turn a non-funciton into a function
Bird_Behaviour %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm = TRUE) > 1) %>%
  glimpse()

#Selecting by the number of distinct values in a column
Bird_Behaviour %>%
  select_if(~n_distinct(.) < 10) %>%
  glimpse()

#Maybe you want to reorder your data columns
#You can specify the order manually or pick a few columns and use 'everything()'
#Here we apply our use of 'separate' from last time and then select by our two new columns 'Genus' and 'Species'
#and the existing column 'Year'
#everything can be used to bring all the other columns along without specifying their order
Bird_reorder <- Bird_Behaviour %>%
  separate(Species, c("Genus","Species"), sep="_", remove=TRUE) %>%
  select(Genus, Species, Year, everything()) %>%
  glimpse()

#Maybe you want to rename your data columns
#There are two ways to do this: select or rename
#The select option is great if you want a subset
#The rename option is what you want if you just want new column names
#Use 'new_name = old_name' as in the example below
Bird_rename.1 <- Bird_reorder %>%
  select(Gen = Genus, Spp = Species, Yr = Year, everything()) %>%
  glimpse()

#You can make new columns with the mutate() function
#The options inside mutate are numerous and can be a long string of operations or logical statements
#Anything inside the mutate can either be a new column or can replace the current column.
#Here we make a ratio of FID to disturbance and make it a new column
Bird_Behaviour %>%
  select(Species, Disturbance, FID) %>%
  mutate(FID_Disturbance = FID/Disturbance) %>%
  glimpse()

#Another example making two new columns
Bird_Behaviour.2 <- Bird_Behaviour %>%
  select(Species, Disturbance, FID) %>%
  mutate(FID_vs_avg = FID - mean(FID), FID_vs_max = FID - max(FID)) %>%
  glimpse()

#Let's deal a bit more with summarizing
#dplyr has a 'count()' function
Bird_Behaviour %>%
  count(Species, sort = TRUE)

#Time to start challenging yourself by linking dplyr functions with pipes
#Note the new function 'group_by()' 
Bird_Behaviour %>%
  select(Species:Fledglings) %>%
  group_by(Species, Sex) %>%
  summarize(avg_FID = mean(FID))%>%
  arrange(desc(avg_FID)) %>%
  write.csv("Bird_FID_x_Spp.csv")

###################################################
#It's time to do some work on your own
#Using the the data files provided you will do the following tasks
#1 unite the two data sets so that location and elevation are included with the tree data
#2 create two new variables: resin duct total and radial growth total 
#3 get summaries of these values for each species x site
#4 plot the relationship among the total radial growth and total resin ducts
#5 make a linear model showing how resin duct area responds to tree radial growth, age, and elevation

