#More data visualisation and graphics
#Thank you to Benedikt Holtmann for this exercise data and idea

library(tidyverse)
setwd("~/Dropbox/R Biology")

#Load pine defense data
Defense <- read.csv(file = "Resin_defense_wide.csv", header = TRUE)

#Make a histogram showing tree age
hist(Defense$Age)

#Make a new data column that turns tree radius (rad_mm) in tree diameter (DBH)
#Then make a histogram of the DBH
Defense$DBH <- Defense$rad_mm*2
hist(Defense$DBH)

#Add colors and change number of bins in your histogram of DBH
#by adapting the example code
hist(Defense$Age, col="grey", breaks= 20)

#Want to compare two or more graphs in your viewer?
#Change the viewer settings to a specified number of rows and columns
#Then run the code for each of the next two graphs
#Your viewer will stay this way until you reset it to (1,1)
par(mfrow=c(1, 2))

#Add a density curve
#Note the new code 'na.rm = TRUE'--this means "remove the NAs"
#To see other available colours than "grey" and "blue"
colors()

hist(Defense$Age, col="grey", breaks= 50, freq = FALSE)
  lines(density(Defense$Age, na.rm = TRUE), col="blue", lwd=2)

#A histogram for age that includes only trees with DBH > 200
hist(Defense[Defense$DBH >= 200,]$Age, col="plum", breaks = 20)

###########Make a histogram for DBH of trees <300 and >100#############
par(mfrow=c(1, 1))
hist(Defense[Defense$DBH < 300 & Defense$DBH > 100, ]$Age)

###########Try other subsetting or filtering methods###################

subs <- filter(Defense, DBH < 300 & DBH > 100)
hist(subs$Age)

#Scatterplots--two examples to call a Base R scatterplot
###########Try adding axis labels#######################
plot(Defense$Age, Defense$DBH, xlim = c(10,500), pch=21, cex = 0.5, col="blue", xlab = "Length (mm)")

plot(DBH ~ Age, data = Defense, xlim = c(10,500), pch=17, cex = 1.5, col="orange")

###########Make these in ggplot2#######################
library(ggplot2)
library(ggpubr)

ggplot(Defense, aes(x = Age, y = DBH)) +
  geom_point(color = "blue", size = 0.5, shape = 21) + 
  theme_bw() +
  xlim(c(10, 500))
  
ggplot(Defense, aes(x = Age, y = DBH)) +
  geom_point(color = "orange", size = 1.5, shape = 17) + 
  theme_bw() +
  xlim(c(10, 500))
  
###########Add a unique line for each of the three species##########

ggplot(Defense, aes(x = Age, y = DBH)) +
  geom_line(data = Defense[Defense$Spp. == 'PICO', ], color = "red") +
  geom_line(data = Defense[Defense$Spp. == 'PIFL', ], color = "black") +
  geom_line(data = Defense[Defense$Spp. == 'PIPO', ], color = "purple") +
  theme_bw() +
  xlim(c(10, 500))

ggplot(Defense, aes(x = Age, y = DBH, color = Spp.)) +
  geom_point(size = 0.5, shape = 21) +
  geom_smooth(method = lm) +
  theme_bw() +
  xlim(c(10, 500))
  
#You can use ?points to get a list of symbols
?points

#Boxplots
#Make 3 groups of tree ages and then a boxplot of DBH x Age group
Defense$age_group <- cut(Defense$Age, 3, labels = c(1:3))

#See the means of each group
tapply(Defense$Age, Defense$age_group, mean, na.rm = TRUE)
tapply(Defense$DBH, Defense$age_group, mean, na.rm = TRUE)

boxplot(DBH ~ age_group, data = Defense, notch = FALSE,
        names = c("Young", "Middle aged", "Old"),
        xlab = 'Age group', ylab = 'Tree age (yrs)',
        col = c("orange", "darkorange"), 
        border = "brown",
        ylim = c(0,500))                          

#Want to compare your data to a randomized normal distribution?
#Prepare a data vector is normally distributed
age <- Defense$Age
dbh <- Defense$DBH

#Generate the normal distribution with the same mean and sd
#The number (200) is the number of simulations of the data used to get the distribution
age_norm <- rnorm(200,mean=mean(age, na.rm=TRUE), sd=sd(age, na.rm=TRUE))
dbh_norm <- rnorm(200,mean=mean(dbh, na.rm=TRUE), sd=sd(dbh, na.rm=TRUE))

#Now plot the two sets with a gap in between
#This graph can be horizontal if you set 'horizontal = TRUE'
boxplot(age, age_norm, dbh, dbh_norm,
        main = "Multiple boxplots for comparision",
        at = c(1,2,4,5),
        names = c("Age", "Normal", "DBH", "Normal"),
        las = 2,
        col = c(rgb(0.1,0.1,0.7,0.5) , rgb(0.8,0.1,0.3,0.6)),
        border = "black",
        horizontal = FALSE,
        notch = TRUE)

#Check the density plot and Q-Q plots
#First load 'ggpubr'
library(ggpubr)

#Make a density plot
ggdensity(Defense$Age, main = "density", xlab = "Response")
ggqqplot(Defense$Age)

ggdensity(Defense$DBH, main = "density", xlab = "Response")
ggqqplot(Defense$DBH)

#Multiple grouping variables on x-axis
#Group the radial growth and resin duct values into one summed column each
Defense <- Defense %>%
  mutate(Rad_grow = rowSums(select(.,RG1:RG10)), Res_ducts = rowSums(select(.,RD1:RD10)))

#Make three growth groups and get their mean growth rates
Defense$grow_grp <- cut(Defense$Rad_grow, 3, labels = c(1:3))
tapply(Defense$Rad_grow, Defense$grow_grp, mean)

#Rename your grouping variables in your data frame
Defense <- Defense %>%
  mutate(growth_rate = ifelse(grow_grp == 1, "Slow",
                                             ifelse(grow_grp == 2, "Medium", "Fast"))) %>%
  mutate(age_level = ifelse(age_group ==1, "Juvenile",
                            ifelse(age_group == 2, "Young", "Old"))) %>%
  unite("Age_x_growth_group", c("growth_rate", "age_level")) %>%
  glimpse()

#Make the boxplot
par(mfrow=c(1, 1))
boxplot(DBH ~ Age_x_growth_group, data = Defense, 
        xlab = 'Growth group', ylab = 'Radial growth (mm)',   
        col = c("red", "blue"))                                       

#######################################################################################
# Prepare the space for the graphs
par(mfrow=c(1, 3))
hist(Defense$Age, col="grey",breaks= 50, freq = FALSE,
     xlab = "Tree age (yrs)", ylab = "Density")
plot(Defense$Age, Defense$DBH, xlim = c(0,500), pch=15, col="blue",
     xlab = "Tree age (yrs)", ylab = "Tree DBH (mm)")
boxplot(DBH ~ age_group, data = Defense, notch = FALSE,
        names = c("Young", "Middle aged", "Old"),
        xlab = 'Age group', ylab = 'Tree age (yrs)',
        col = c("red", "blue"),                    
        ylim = c(0,500)) 


#Plot directly into a file()
pdf("Figure1.pdf", width= 6.5, height = 3) # width and height are in inches unless you specify units 
par(mfrow=c(1, 3))
hist(Defense$Age, col="grey",breaks= 50, freq = FALSE, main = "",
     xlab = "Tree age (yrs)", ylab = "Density")
plot(Defense$Age, Defense$DBH, xlim = c(0,500), pch=15, col="blue",
     xlab = "Tree age (yrs)", ylab = "Tree DBH (mm)")
boxplot(DBH ~ age_group, data = Defense, notch = FALSE,
        names = c("Young", "Middle aged", "Old"),
        xlab = 'Age group', ylab = 'Tree age (yrs)',
        col = c("red", "blue"),                    
        ylim = c(0,500)) 
dev.off()  # shuts down current device
