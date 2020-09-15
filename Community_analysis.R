##### Intro to ordination methods
##### 3/27/2019
##### ferrenbe@nmsu.edu

#Load libraries
library(vegan)

#Load data sets; Main = biotic, Env = environmental
Main <- read.csv("Bacteria_community.csv")
names(Main)
dim(Main)

Env <- read.csv("Soil_environment.csv")
names(Env)
dim(Env)

##### ALPHA DIVERSITY METRICS ####################

# Calculate Shannon diversity
# The default diversity metric in Vegan's 'diversity()' is Shannon
H <- diversity(Main[, -1], index = "shannon")
summary(H)

#calculate Simpson's D
D <- diversity(Main[, -1], index = "simpson")
summary(D)

#calculate inverse Simpson
invsimpson <- diversity(Main[, -1], index = "invsimpson")
summary(invsimpson)

#calculate Fisher's alpha
fish.a <- fisher.alpha(Main[, -1])
summary(fish.a)

#calculate Pielou's evenness (J)
J <- H/log(specnumber(Main[, -1]))
summary(J)

#### SPECIES DIVERSIT ESTIMATORS #################
#calculate diversity estimators
specpool(Main[, -1])

##### Create species accumulation curves
spa <- specaccum(Main[, -1]) # This method uses the samples to estimate occurrence
plot(spa, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

spi<-specaccum(Main[, -1], method="rarefaction", permutations = 100, gamma = "jack1") # This method uses the individuals not the samples
plot(spi, ci.type="poly", col="red", lwd=2, ci.lty=0, ci.col="lightblue")

plot(spa, add=TRUE, col=4)

##### MAKE YOUR MATRIX A DISTANCE MATRIX ###########
# Transform the data into a Bray-Curtis distance matrix
# The "binary=FALSE" means you want to retain the counts or cover of individuals
# TRUE would give the result for presence-absence (Sorenson's index)
bc<-vegdist(Main[, -1], method="bray", binary=FALSE)

##### MAKE A SCREE PLOT--THIS WILL TAKE SOME TIME ########
# In this part, we define a function NMDS.scree() that automatically 
# performs aN NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(bc)

##### MAKE AN NMDS PLOT AND STRESSPLOT ####################
##### Use a nonmetric multidimensional scaling, NMDS, to ordinate and visualize the data
#'metaMDS()' uses Bray-Curtis distances by default which is what you want for NMDS 
# 'vegan' applies automatic transformations when needed
NMDS <- metaMDS(bc, k = 2, trymax = 100, trace = FALSE)
NMDS

#View the stress plot
stressplot(NMDS)

#Add convex hulls to outline 'treatment groups'
treat=c(rep("Unburned_Fall",22),rep("Burned_Fall",19), rep("Unburned_Winter",23), rep("Burned_Winter", 21))
ordiplot(NMDS,type="n", air = 1)
colors=c(rep("blue",22),rep("red",19), rep("green",23), rep("orange", 21))
ordiplot(NMDS,type="n")
for(i in unique(treat)) {
ordihull(NMDS$point[grep(i,treat),],draw="polygon",
   groups=treat[treat==i],col=colors[grep(i,treat)],label=T) } 

#Add environmental data as vectors and get stats
fit <- envfit(NMDS, Env[, 4:8], perm = 999, na.rm = TRUE)
scores(fit, "vectors")
plot(fit, p.max = 0.05, col = "black")

#Display the 'species' weights and label the samples if desired
orditorp(NMDS,display="species",col="black",air=1)
orditorp(NMDS,display="sites",col=c(rep("green",5),
  rep("blue",5)),air=1,cex=1.25)

##### Save your plot as a high res image file
#Save the graph to your working directory as a high res png file
#Adjust the width and height of the file to match your target size
dev.copy(png,"NMDS.png",width=15,height=7.5,units="cm",res=300)
dev.off()

########## Perform a PERMANOVA on the community data #############
library(tidyverse)
Env <- Env %>%
  unite(Type, Treatment, Month, sep = "_") %>%
  glimpse()

# PERMANOVA model 1
per.mod.1 <- adonis(bc ~ Env$Type)
per.mod.1

# Complete a posthoc comparison on "Type"
# Use the package 'RVAideMemoire' to perform pairwise post-hoc tests
library(RVAideMemoire)
pairwise.perm.manova(bc, Env$Type, test = c("Pillai"), nperm = 999, progress = TRUE, p.method = "fdr")

homog.test <- betadisper(bc, Env$Type, type = "centroid")
plot(homog.test)
boxplot(homog.test)
permutest(homog.test)

# We are in violation of the assumptions of a PERMANOVA
sptrans.1 <- decostand(Main[,-1], "max")
dist.matrix.1 <- vegdist(sptrans.1, method="bray", binary=FALSE)
homog.test.1 <- betadisper(dist.matrix.1, Env$Type, type = "centroid")
boxplot(homog.test.1)
permutest(homog.test.1)

sptrans.2 <- wisconsin(Main[,-1])

dist.matrix.2 <- vegdist(sptrans.2, method="bray", binary=FALSE)
homog.test.2 <- betadisper(dist.matrix.2, Env$Type, type = "centroid")
boxplot(homog.test.2)
permutest(homog.test.2)

# double standardization
library(vegan)
stand.dat <- decostand(Main[,-1], method = "range")
dist.matrix.3 <- vegdist(stand.dat, method="bray", binary=FALSE)
homog.test.3 <- betadisper(dist.matrix.3, Env$Type, type = "centroid")
boxplot(homog.test.3)
permutest(homog.test.3)

########## CHALLENGE #1 ###################
# Complete an ordination and biplot (envfit()) using the dune and dune.env data in vegan




########## CHALLENGE #2 ###################
# Complete an NMDS on the BCI data using the 'Habitat' factor from "BCI.env" as the grouping variable
?BCI
?BCI.env

data(BCI)
head(BCI)

data(BCI.env)
head(BCI.env)
str(BCI.env)
