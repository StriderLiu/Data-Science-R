library(rpart)
# Read Files
data <- read.csv("/Users/vincentliu/Documents/R_Workspace/DataScience/assign5/titanic_train.csv")
str(data)

# Adjust feature format
data <- as.data.frame(lapply(data, function(x) if(is.character(x)|is.factor(x)) gsub("male",0,x) else x))
data <- as.data.frame(lapply(data, function(x) if(is.character(x)|is.factor(x)) gsub("fe0",1,x) else x))
data <- as.data.frame(lapply(data, function(x) if(is.character(x)|is.factor(x)) gsub("S",1,x) else x))
data <- as.data.frame(lapply(data, function(x) if(is.character(x)|is.factor(x)) gsub("C",2,x) else x))
data <- as.data.frame(lapply(data, function(x) if(is.character(x)|is.factor(x)) gsub("Q",3,x) else x))
str(data)

# Split the data into training set and validation set
n <- nrow(data)
training <- data[1:((2/3)*n), ]
testing <- data[((2/3)*n):n, ]

# Decision tree
tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data = training, method = "class")

# Plot the result
plot(tree)
text(tree, pretty=0)
summary(tree)
print(tree)
# Make a better plot
#install.packages("rpart.plot")
library(rpart.plot)
library(RColorBrewer)
rpart.plot(tree, main = "Decision Tree")

# Evaluate model performance with test data
pred_test <- predict(tree, testing, type = "class")
#install.packages("gmodels")
library("gmodels")
tab <- CrossTable(testing$Survived, pred_test,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# Calculate precision rate
precision_rate <- (tab$t[1,1] + tab$t[2, 2]) / (tab$t[1,1] + tab$t[1,2] + tab$t[2,1] + tab$t[2,2])
precision_rate

# Pruning the tree
printcp(tree)
plotcp(tree)
ptree<- prune(tree, 
              cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
rpart.plot(ptree, main = "Pruned Decision Tree")

# Evaluate the new model
pred_test2 <- predict(ptree, testing, type = "class")
tab2 <- CrossTable(testing$Survived, pred_test2,
                  prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                  dnn = c('actual default', 'predicted default'))
precision_rate2 <- (tab2$t[1,1] + tab2$t[2, 2]) / (tab2$t[1,1] + tab2$t[1,2] + tab2$t[2,1] + tab2$t[2,2])
precision_rate2
