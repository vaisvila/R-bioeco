#BIOL 550
#An intro graphics and ggplot2
rm(list = ls())
#Let's start by making a scatterplot in Base R
#Scatterplot with individual vectors from the mtcars data
plot(mtcars$wt, mtcars$mpg)

#Make this plot better
fit <- lm(mtcars$mpg ~ mtcars$wt)
plot(mtcars$wt, mtcars$mpg, col="blue", axes = FALSE, ann = FALSE)
lines(mtcars$wt, fitted(fit), col="blue")
title(main = "Autos", col.main = "blue", font.main = 12)
title(xlab = "Auto weight", col.lab = rgb(0,0,0))
title(ylab = "Miles per gallon", col.lab = rgb(0,0,0))
box()

#Another approach that's a little easier
attach(mtcars)
plot(mpg ~ wt, xlab = "Auto weight", 
     ylab = "Miles per gallon", pch = 19, cex.lab=1.25, cex.axis=1.25)
abline(lm(mpg ~ wt), col = "blue", lwd = 2)
lines(lowess(mpg ~ wt), col= "red", lwd = 2, lty = "dashed")
detach(mtcars)

# 1. ggplot differs from Base R graphics in that it requires a data frame or data object and
#won't take a simple vector as an argument

# 2. As a result, the most basic part of ggplot2 is calling the 'ggplot()' function to 
#specify the df or object

# 3. A common example in tutorials uses the built-in data called 'diamonds' or the 'mtcars' data
#here is an example with the diamonds data to walk through the basic code needed for a ggplot graph

library(ggplot2)
names(diamonds)
####THE BASE
#this will call the data and won't return an error but you will get a blank plot screen
ggplot(diamonds)

#'aes' is short for 'aesthetic'; this example defines the x axis
ggplot(diamonds, aes(x = carat))

#now you have both an x and y axis defined and a base layer created
ggplot(diamonds, aes(x = carat, y = price)) 

#'color =' will use a default color scheme to give each carat group a distinct color
#you can also add 'size =' to the list to specify sizes that change based on a variable in your df
ggplot(diamonds, aes(x = carat, y = price, color = cut)) 

#If you want the color or size to be fixed, you can also specify them outside of the 'aes()' call
ggplot(diamonds, aes(x = carat, y = price), color = "dodgerblue")

####THE LAYERS
#ggplot uses a language of layers where each layer adds or removes something
#Here is an example adding scatterplot geom (layer1) and smoothing geom (layer2)
#The order of the layers doesn't matter and you can see from the next three groups of code
#that layers are independent

#1
ggplot(diamonds, aes(x = carat, y = price, color = cut)) + 
  geom_point() 

#2
ggplot(diamonds, aes(x = carat, y = price, color = cut)) + 
  geom_smooth()

#3
ggplot(diamonds, aes(x = carat, y = price, color = cut)) + 
  geom_point()+ 
  geom_smooth()

#There are two options to adding layers and setting colors and sizes
#The two options shown here will give you the same plot
#Option 1 is the same as example #3 from above
ggplot(diamonds, aes(x = carat, y = price, color = cut)) + 
  geom_point()+ 
  geom_smooth()

#Option 2--note where color is now called in the layers, not the base
ggplot(diamonds) + 
  geom_point(aes(x = carat, y = price, color = cut)) + 
  geom_smooth(aes(x = carat, y = price, color = cut))

#Why have two options?
#Perhaps you want to change the point colors for cut of the diamond 
#but have only one fitted line for all the diamonds--we simply delete the 'color =' from the geom_smooth
#having the color calls in the layers allows us more flexibility
ggplot(diamonds) + 
  geom_point(aes(x = carat, y = price, color = cut)) + 
  geom_smooth(aes(x = carat, y = price))

#What if you want to change the size and/or shape of the points?
ggplot(diamonds) + 
  geom_point(aes(x = carat, y = price, color = cut, size = 2, shape = color)) + 
  geom_smooth(aes(x = carat, y = price))

#####THE LABELS
#Labels are another layer
#In the simplest form, we add a title and x/y labels
#Changing the font, size, and color is done with another layer called 'theme()'
#1
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() +
  geom_smooth() +
  labs(title="Scatterplot", x="Carat", y="Price")

#####THE THEME
#Adjusting the size of labels is done using the theme() function 
#plot.title=, axis.text.x= and axis.text.y=
#They need to be specified inside the element_text()
#If you want to remove them, set it to '=element_blank()'
#You change the legend title with either 'scale_color_discrete' for characters
#or 'scale_color_continuous' for a continuous variable
ggplot(diamonds) + 
  geom_point(aes(x = carat, y = price, color = cut)) + 
  geom_smooth(aes(x = carat, y = price, color = cut)) +
  labs(title="Scatterplot", x="Carat", y="Price") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=25),
        axis.title.y=element_text(size=25)) + 
  scale_color_discrete(name="Cut of diamonds")

#####THE OBJECT
#Like everything in R, you can make a ggplot into an object using <-
#Doing so allows you to combine it with other objects or call more functions on it
plot.1 <- ggplot(diamonds) + 
  geom_point(aes(x = carat, y = price, color = cut)) + 
  geom_smooth(aes(x = carat, y = price, color = cut)) +
  labs(title="Scatterplot", x="Carat", y="Price") +
  theme(plot.title = element_blank(),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) + 
  scale_color_discrete(name="Cut of diamonds")

plot.1 + facet_wrap(~cut, ncol = 3)

#Get goofy with themes
theme_spooky = function(base_size = 10, base_family = "Chiller") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "purple", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "purple", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "orange", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "orange", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = " gray10"),  
      legend.key = element_rect(color = "white",  fill = " gray10"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "none",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = " gray10", color  =  NA),  
      #panel.border = element_rect(fill = NA, color = "white"),  
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = " gray10", fill = " gray10"),  
      plot.title = element_text(size = base_size*1.2, color = "orange",hjust=0,lineheight=1.25,
                                margin=margin(2,2,2,2)),  
      plot.subtitle = element_text(size = base_size*1, color = "white",hjust=0,  margin=margin(2,2,2,2)),  
      plot.caption = element_text(size = base_size*0.8, color = "orange",hjust=0),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

plot.2 <- ggplot(diamonds) + 
  geom_point(aes(x = carat, y = price), color = "purple", alpha = 1, shape = 21, fill = "orange") + 
  geom_smooth(aes(x = carat, y = price, color = "purple")) +
  labs(title="Scatterplot", x="Carat", y="Price") +
  theme_spooky(base_size = 20) + 
  labs(title="Make your own theme!", x="Carat", y="Price", caption = "Gettin' Spooky")
plot.2

#####A CHALLENGE
#Change the line colors in your facet plots so they can be seen over the points
#Change the points to open circles
#Scale your point sizes to another variable in the data set--see my mistletoe infection on pine example
#Make a scatterplot showing the relationship of catfish weight in march to weight in april
#Can you add lines for each species*sex combination?