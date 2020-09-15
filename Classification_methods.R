#### Classification Methods
#### BIOL 301
#### ferrenbe@nmsu.edu
#### Kabacoff's 'R in Action' was a valuable source for this code

#### An example with a commonly used dataset
# This data is a set of breast tumor attributes 
# The goal is to determine whether tumors are benign or malignant

# The data are online so we can use a url approach to loading it
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep="")

# The data table
breast <- read.table(url, sep=",", header=FALSE, na.strings="?")
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "maginalAdhesion",
                   "singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "class")

#### The goal is to use the listed attributes to predict "class"
#### We will drop the "ID" variable because it's not needed here
df <- breast[-1]
df$class <- factor(df$class, levels=c(2,4),
                   labels=c("benign", "malignant"))

set.seed(1234) # Make our result repeatable

#### A common aspect of classification methods is model validation
# We will split the data to make a model training set
# and a model validation set

train <- sample(nrow(df), 0.7*nrow(df)) # Make a training data set 
df.train <- df[train,]

df.validate <- df[-train,] # Make a validation data set or out-of-bag sample
table(df.train$class)
table(df.validate$class)

# First thing we can try is logistic regression
fit.logit <- glm(class~., data=df.train, family=binomial)
summary(fit.logit)

prob <- predict(fit.logit, df.validate, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE),
                       labels=c("benign", "malignant"))
logit.perf <- table(df.validate$class, logit.pred,
                      dnn=c("Actual", "Predicted"))
logit.perf

# An alternative to the confusion matrix method from above
library(regclass) # A useful package for testing logistic models
confusion_matrix(fit.logit, DATA = df.validate) 

# What proportion were correctly classified?
(118 + 76)/200 # 0.97 or 97 %

# Not all of the variables were significant at a P < 0.05 level
# We can step-fit a logit model
logit.fit.reduced <- step(fit.logit)
reduced.logit.mod <- glm(class ~ clumpThickness + shapeUniformity + maginalAdhesion +
                           bareNuclei + blandChromatin + normalNucleoli + mitosis,
                         data = df.train, family=binomial())
summary(reduced.logit.mod)
confusion_matrix(reduced.logit.mod, DATA = df.validate)
(118 + 77)/200 # A tiny bit better accuracy

prob <- predict(reduced.logit.mod, df.validate, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE),
                     labels=c("benign", "malignant"))
reduced.logit.perf <- table(df.validate$class, logit.pred,
                    dnn=c("Actual", "Predicted"))

reduced.logit.perf

#### A traditional decision tree example
library(rpart)
set.seed(1234)

# First we grow a tree
dtree <- rpart(class ~ ., data=df.train, method="class",
               parms=list(split="information")) 
dtree$cptable
plotcp(dtree)

# Then we prune the tree
dtree.pruned <- prune(dtree, cp=.0125)


library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104,
      fallen.leaves = TRUE, main="Decision Tree")

dtree.pred <- predict(dtree.pruned, df.validate, type="class") # Classify new data
dtree.perf <- table(df.validate$class, dtree.pred,
                      dnn=c("Actual", "Predicted"))
dtree.perf

#### A conditional inference tree example
#### CI Trees use a signficance test approach instead of a purity approach
library(party)
fit.ctree <- ctree(class~., data=df.train)
plot(fit.ctree, main="Conditional Inference Tree")

ctree.pred <- predict(fit.ctree, df.validate, type="response")
ctree.perf <- table(df.validate$class, ctree.pred,
                      dnn=c("Actual", "Predicted"))
ctree.perf

#### A random forest approach
#### There has been some concern that the 'randomForest' package has
#### calculation errors; consider using the package 'Ranger' instead
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class~., data=df.train,
                             na.action=na.roughfix,
                             importance=TRUE)
fit.forest

# Rank variable importance
importance(fit.forest, type=2) # The larger the mean decrease in Gini, the more important the variable

forest.pred <- predict(fit.forest, df.validate)
forest.perf <- table(df.validate$class, forest.pred,
                       dnn=c("Actual", "Predicted"))
forest.perf
(117 + 79)/200 # 0.98 correctly classified

#### Support Vector Machine
library(e1071)

set.seed(1234)

fit.svm <- svm(class~., data=df.train)
fit.svm

svm.pred <- predict(fit.svm, na.omit(df.validate))

svm.perf <- table(na.omit(df.validate)$class,
                    svm.pred, dnn=c("Actual", "Predicted"))
svm.perf

# Tune the machine; gamma and cost
set.seed(1234)

tuned <- tune.svm(class~., data=df.train,
                    gamma=10^(-6:1),
                    cost=10^(-10:10))
tuned # This will take some time 

fit.svm <- svm(class~., data=df.train, gamma=.01, cost=1)
svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class,
                    svm.pred, dnn=c("Actual", "Predicted"))
svm.perf
(117 + 77)/200 #0.97 correctly classified

#### We need to consider a model's ability to correctly classify
#### As well as how often it's wrong

# A function for performance of a classification model
performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

# Let's apply the function to our models
performance(logit.perf)
performance(reduced.logit.perf)
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf)
performance(svm.perf)
