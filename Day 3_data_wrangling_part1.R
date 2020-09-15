####Learn about R opperators
#Make a simple data vector
x <- c(1:10)
x > 7 #greater than
x == 7 #equal to
x <= 4 #less than or equal to
x != 4 #not equal to

#A list of T and F is find, but if you want values, use a set of brackets
x[(x==7)]
x[(x<=4)]

#do a few more with brackets to reinforce the idea


####Now let's cover data frames
#An R data frame follows these 4 rules:
#1.The column names should be non-empty.
#2.The row names should be unique but rows can be empty (blanks in a .csv file are fine)
#3.The data stored in a data frame can be of numeric, factor or character type.
#4.Each column should contain the same number of rows.

####Learn about data frames and 'rbind' and 'cbind'
#Make two vectors and combine them with cbind in a data.frame
#Here's an example using sports trophies--thanks to J. Lander for this example
sport <- c("Hockey", "Baseball", "Football")
league <- c("NHL", "MLB", "NFL")
trophy <- c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy")
trophies1 <- cbind(sport, league, trophy)

#Make another data.frame using data.frame()
trophies2 <- data.frame(sport=c("Basketball", "Golf"),
                        league=c("NBA", "PGA"),
                        trophy=c("Larry O'Brien Championship Trophy",
                                 "Wanamaker Trophy"),
                        stringsAsFactors=FALSE)

#Combine them into one data.frame with rbind
Sports.trophies <- rbind(trophies1, trophies2)
print(Sports.trophies)

#You create a data frame with 'data.frame()' as shown in the example above
#Another example
class.grades <- data.frame(
  student.id = c(1:5),
  student.name = c("Mickey", "Donald", "Mini", "Pluto", "Goofy"),
  grade = c(88.2, 92.8, 99.1, 75.2, 65.1),
  attendance = c(8, 9, 10, 6, 6),
  stringsAsFactors = FALSE)

#Now you can extract specific columns and see a summary table
result <- data.frame(class.grades$student.name, class.grades$grade)
print(result)

##############Make a table for attendance###################
student_attendance <- data.frame(class.grades$student.id, class.grades$student.name, class.grades$attendance)
print(student_attendance)



#Let's imagine that you forgot to include information about the students' majors
#Add a column to the data frame for this new variable
class.grades$major <- c("Mammology", "Ornithology", "Genetics", "Astronomy", "Undeclared")
print(class.grades)

###############Add a column for 'species" identifiation of each character############

class.grades$species <- c("")


#How do we add rows?
#Assume we have a new student and make a data frame just for him/them
##################How and why does the object defined below###############
##################compare to the class.grades object from above?#############
class.grades2 <- data.frame(
  student.id = 6,
  student.name = "Scrooge McDuck",
  grade = 100,
  attendance = 1,
  major = "Finance",
  species = "Irish duck",
  stringsAsFactors = FALSE)

#Bind the two data frames together
class.grades.master <- rbind(class.grades, class.grades2)
print(class.grades.master)

#Remember that object names are transferable 
#That means you can rename or give 2, 3, etc. names to an object
#This can be great if you want to shorten your names before a long section of code
#Both objects remain in the R memory
cgm <- class.grades.master
summary(cgm$grade)
summary(class.grades.master$grade) #gives us the same result because objects are equal

#################make a histogram showing the grades#####################

hist(cgm$grade)

#################make a scatterplot showing 'grades ~ attendance'#############

plot(cgm$attendance)

#Now consider a situation where you need to do a repeated or mass action
#The 'apply' family of functions is very useful in this situation
#An example with our class of characters is that we want to know the class-level mean scores on each exam and attendance
class.data <- data.frame(
  student.name = c("Mickey", "Donald", "Mini", "Pluto", "Goofy"),
  grade.1 = c(88.2, 92.8, 99.1, 75.2, 65.1),
  grade.2 = c(91.0, 88.1, 87.2, 82.1, 75.2),
  grade.3 = c(78.2, 92.8, 89.1, 85.2, 85.1),
  attendance = c(8, 9, 10, 7, 8),
  stringsAsFactors = FALSE)
str(class.data)
class.data
summary.1 <- apply(class.data[,-1], 2, mean) #The bracketed part defines the data matrix we want to work with
summary.1 #Let's see our class means for tests 1 to 3 and attendance--looks like you're an easy grader

###############There are many ways to define the data matrix################
###############Redo the 'apply' command from above using a different approaches for defining the d.f. in the brackets#########
summary.1 <- apply(class.data[, c(-1, -2)], 2, mean)

#Now let's get student averages instead of class test averages
#We need to remove the attendance values this time too
Final.avg <- apply(class.data[,2:4], 1, mean) #What differs here compared to line 91 and why?
Final.avg

#'apply' works for many funcitons--you can get a sum, median, mix, max,...
#'apply' is a command for applying a function to all selected rows or columns, or both at the same time
#You will find many uses for this command

#################How would you add these average back to your data frame?#############
#################The first example in the script has the answer################
#################YOU NEED TO DO THIS STEP BEFORE THE CODE THAT FOLLOWS WILL WORK#################
#################KEEP THE NAME AS 'Final.grades' OR YOU WILL HAVE TO CHANGE THE SUBSEQUENT CODE#############

Final.grades <- cbind(Final.avg, class.data)

#In addition to 'apply' there are 'lapply' and 'sapply'
#The 'l' in lapply() stands for list. 
#The difference between lapply() and apply() is the output return. 
#The output of lapply() is a list. 
#lapply() can be used for other objects like data frames and lists.
students <- lapply(Final.grades[,1], tolower) #This example uses lapply to change all student names to lowercase
students

#You can 'unlist' to turn the list into a vector for further use
students.lower <- unlist(lapply(Final.grades[,1], tolower))
students.lower
Final.grades <- cbind(Final.grades, students.lower)
Final.grades

#Another example, you messed up and missed adding 2 points to each exam during the semester
#You can call mathematical functions on rows
Final.grades.2 <- unlist(lapply(Final.grades[3:5], function(x) x+2))
Final.grades.2

#You won't often use the 'lapply' function because there is a wrapper version called 'sapply'
#sapply returns a vector or matrix instead of a list and that's usually what we want
#So lapply and sapply due roughly the same thing, but return different object classes
Final.grades.3 <- sapply(Final.grades[3:5], function(x) x+2)
Final.grades.3

#There's another member of the 'apply family' known as 'tapply'
#tapply is very useful and lets you break a vector into pieces and then apply a function to each piece
#Here we're going to go back to the 'mtcars' dataset from day 1 because it will better illustrate the function
str(mtcars$cyl)

#Here we set the cyl value to be a factor instead of a continous variable
levels(as.factor(mtcars$cyl))

#Get the mean mpg for each type of motor (4, 6, and 8 cylinder)
tapply(mtcars$mpg, mtcars$cyl, mean)

#Let's make car weights (wt) into three levels using the 'cut()' function
#This function as used below will make a specified number of even ranged--not sized--groups across our variable
#You can specify where the breaks occur in a continuous variable using: c(0,#,#...) where '#' are the upper limits of the intervals
#Or you can use the 'seq()' function where you specify a sequence with a star value #1, an end value #2, and an interval for breaks #3
# seq(#1, #2, #3)
wt.grps <- cut(mtcars$wt, 3, labels = c("small", "Mid", "Large"))
wt.grps

#Use tapply to get the mean weight of each of the groups you made
tapply(mtcars$mpg, wt.grps, mean)




