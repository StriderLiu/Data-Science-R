#install.packages("tree")
library(tree)
dt <- read.csv("/Users/vincentliu/Documents/R_Workspace/DataScience/assign5/ENB2012_data.csv")
str(dt)

n <- nrow(dt)
training <- dt[1:((2/3)*n), ]
testing <- dt[((2/3)*n):n, ]

tree_model <- tree(log(Y2) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + Y1, data=training)
plot(tree_model)
text(tree_model, cex=.75)