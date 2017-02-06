#Linear Regression
library("MASS")
m <- as.matrix(Boston)
boston <- matrix(m, ncol = ncol(Boston), dimnames = NULL)
target <- boston[,14]
input <- boston[,1:13]

res <- lm(formula = target ~ input)
sum <- summary(res)
coe <- sum$coefficient
avg_err <- sum(abs(res$residuals)/nrow(boston))

# Remove input[3, 7]
improved_res <- lm(formula = target ~ input[,1]+input[,2]+input[,4]+input[,5]+input[,6]+input[,8]+input[,9]+input[,10]+input[,11]+input[,12]+input[,13])
improved_sum <- summary(improved_res)
improved_coe <- improved_sum$coefficient
imp_avg_err <- sum(abs(improved_res$residuals)/nrow(boston))

# k-means
data <- read.csv(file = "./DataScience/assign3/bank-data.csv", head = TRUE, sep = ",")
# use pattern matching to adjust the format of data
test <- as.data.frame(lapply(data,function(x) if(is.character(x)|is.factor(x)) gsub("YES",1,x) else x))
test <- as.data.frame(lapply(test,function(x) if(is.character(x)|is.factor(x)) gsub("NO",0,x) else x))
test <- as.data.frame(lapply(test,function(x) if(is.character(x)|is.factor(x)) gsub("MALE",1,x) else x))
test <- as.data.frame(lapply(test,function(x) if(is.character(x)|is.factor(x)) gsub("FE1",0,x) else x))
test <- as.data.frame(lapply(test,function(x) if(is.character(x)|is.factor(x)) gsub("INNER_CITY",1,x) else x))
test <- as.data.frame(lapply(test,function(x) if(is.character(x)|is.factor(x)) gsub("TOWN",2,x) else x))
test <- as.data.frame(lapply(test,function(x) if(is.character(x)|is.factor(x)) gsub("SUBURBAN",3,x) else x))
test <- as.data.frame(lapply(test,function(x) if(is.character(x)|is.factor(x)) gsub("RURAL",4,x) else x))

data = test[,2:12]
m = as.matrix(data)
data <- matrix(m, ncol = ncol(data), dimnames = NULL)

num_data <- apply(data, 1, as.numeric)
num_data <- t(num_data)

#data normalization
z_scores <- scale(num_data, center = TRUE,scale = TRUE)
install.packages("NbClust")
library(NbClust)
set.seed(Sys.time())
# find the best number of clusters used for kmeans
nc <- NbClust(z_scores, min.nc=2, max.nc=20, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Find the Best Number of Clusters")

k_result <- kmeans(z_scores,iter.max = 15,centers = 2)
