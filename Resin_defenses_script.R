#Day 5_resin defenses example solutions

#It's time to do some work on your own
#Using the the data files provided you will do the following tasks
#1 unite the two data sets so that location and elevation are included with the tree data
#2 create two new variables: resin duct total and radial growth total 
#3 get summaries of these values for each species x site
#4 plot the relationship among the total radial growth and total resin ducts
#5 make a linear model showing how resin duct area responds to tree radial growth, age, and elevation

###################
#Load the tidyverse
library(tidyverse)

#Load the data sets
Main <- read.csv("Resin_defense_wide.csv", header = TRUE, stringsAsFactors=FALSE)
names(Main)

Site <- read.csv("Resin_defense_sites.csv", header = TRUE)
names(Site)

###### 1 Join the two data sets
Main.2 <- inner_join(Main, Site, by = "Site")

###### 2 Create the new variables
#The long way
Main.4 <- Main.2 %>%
  mutate(Res_ducts = RD1 + RD2 + RD3 + RD4 + RD5, Rad_grow = RG1 + RG2 + RG3 + RG4 + RG5) %>%
  glimpse()

#The best way?
Main.5 <- Main.2 %>%
  mutate(Res_ducts = rowSums(select(.,RD1:RD10)), Rad_grow = rowSums(select(.,RG1:RG10))) %>%
  glimpse()

###### 3 get summaries of these new values for each species x site
Summary.1 <- Main.5 %>%
  group_by(Spp., Site) %>%
  summarize_all(funs(mean))

Summary.2 <- Main.5 %>%
  group_by(Spp., Site) %>%
  summarize(mean_res_ducts = mean(Res_ducts), mean_rad_grow = mean(Rad_grow)) %>%
  glimpse()

####### 4 plot the relationship among the total radial growth and total resin ducts
plot1 <- ggplot(Main.5, aes(x = Rad_grow, y = Res_ducts, color = Site)) + 
  geom_point(size = 1, shape = 19) +
  geom_smooth (method = lm, se = FALSE) +
  scale_y_continuous(name = "Resin ducts per annual ring") +
  scale_x_continuous(name = "Annual ring radial growth (mm)") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        legend.position = "right")
plot1

#By species
plot2 <- ggplot(Main.5, aes(x = Rad_grow, y = Res_ducts, color = Spp.)) + 
  geom_point(size = 1, shape = 19) +
  geom_smooth (method = lm, se = FALSE) +
  scale_y_continuous(name = "Resin ducts per annual ring") +
  scale_x_continuous(name = "Annual ring radial growth (mm)") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        legend.position = "right")
plot2

#Examine all sites with facet wrap
plot3 <- ggplot(Main.5, aes(Rad_grow, Res_ducts)) + geom_point(shape = 1)+ geom_smooth(method="lm") +
  facet_wrap(~Site, scales = "free")
plot3

#Examine all trees with facet wrap
plot4 <- ggplot(Main.5, aes(Rad_grow, Res_ducts)) + geom_point(shape = 1)+ geom_smooth(method="lm") +
  facet_wrap(~Spp., scales = "free")
plot4

######## 5 make a linear model showing how resin duct total responds to growth, age, and elevation
#All species together
mod.1 <- lm(Res_ducts ~ Rad_grow + Age + Elev_m_asl, data = Main.5)
summary(mod.1)

#Using dply to do them all without a loop
#We need the 'broom' package which is for the tidyverse but not yet bundled into the group
#call your linear model 'loop'
library(broom)
mod.2 <- Main.5 %>%
  group_by(Spp.) %>%
  do(mod = lm(Res_ducts ~ Rad_grow + Age + Elev_m_asl, data = .))

#Get the summary information and coefficents using functions from 'broom'
mod.summary = glance(mod.2, mod)
mod.summary
mod.coef = tidy(mod.2, mod)
mod.coef