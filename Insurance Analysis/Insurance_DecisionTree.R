library(XLConnect)
library(caret)
library(rpart)
library(party)
library(randomForest)

prud <- read.csv('C:/Users/Abhijeet/Desktop/ADS/Midterm/train.csv', head=TRUE)
View(prud)
summary(prud)
head(prud)

#Function for changing the data types#
change_type <- function(col_name, range, data, col_type){
  for(i in range){
    new_col_name <- paste0(col_name,i)
    if(col_type=="factor"){
      data[, new_col_name] <- as.factor(data[, new_col_name])
    }else if(col_type=="numeric"){
      data[, new_col_name] <- as.numeric(data[, new_col_name])
    }else if(col_type=="integer"){
      data[, new_col_name] <- as.integer(data[, new_col_name])
    }
  }
  return(data)
}



#Product_Info_
prud <- change_type("Product_Info_", c(1,5,6,7), prud, "factor")

#Employment_Info_
prud <- change_type("Employment_Info_", c(3,5), prud, "factor")

#InsuredInfo_
prud <- change_type("InsuredInfo_", c(1,2,4,5,6,7), prud, "factor")

#Insurance_History_
prud <- change_type("Insurance_History_", c(1,2,3,4,7,8,9), prud, "factor")

#Family_Hist_
prud <- change_type("Family_Hist_", 1, prud, "factor")

#Medical_History_
v <- 1:41
prud <- change_type("Medical_History_", v[c(-1,-2,-10,-15,-24,-32)], prud, "factor")
#prud <- change_type("Medical_History_", c(2,10,15,24,32), prud, "factor")


#Medical_Keyword_1-48
prud <- change_type("Medical_Keyword_", 1:48, prud, "factor")


summary(prud)
str(prud)
View(prud)

# Delete columns having NAs more than 1/3 of dataset
v <- c()
prud1 <- prud
for (i in names(prud)){
  count <- sum(is.na(prud[, i]))
  if(count<30000){
    v <- c(v, i)
  }
}
prud1 <- prud1[v]

# Insert the missing values

#Employment_Info_1
m <- mean(prud1$Employment_Info_1[!is.na(prud1$Employment_Info_1)])
prud1$Employment_Info_1[is.na(prud1$Employment_Info_1)] = m


#Employment_Info_4
m <- mean(prud1$Employment_Info_4[!is.na(prud1$Employment_Info_4)])
prud1$Employment_Info_4[is.na(prud1$Employment_Info_4)] = m


#Employment_Info_6
m <- mean(prud1$Employment_Info_6[!is.na(prud1$Employment_Info_6)])
prud1$Employment_Info_6[is.na(prud1$Employment_Info_6)] = m


#Insurance_History_5
m <- mean(prud1$Insurance_History_5[!is.na(prud1$Insurance_History_5)])
prud1$Insurance_History_5[is.na(prud1$Insurance_History_5)] = m


#Family_Hist_2
m <- mean(prud1$Family_Hist_2[!is.na(prud1$Family_Hist_2)])
prud1$Family_Hist_2[is.na(prud1$Family_Hist_2)] = m


#Family_Hist_4
m <- mean(prud1$Family_Hist_4[!is.na(prud1$Family_Hist_4)])
prud1$Family_Hist_4[is.na(prud1$Family_Hist_4)] = m


#Medical_History_1
m <- mean(prud1$Medical_History_1[!is.na(prud1$Medical_History_1)])
prud1$Medical_History_1[is.na(prud1$Medical_History_1)] = m




# convert factorial data

change_factor <- function(col_name, range, df){
  prud2 <- df
  for(i in range){
    new_col_name <- paste0(col_name,i)
    print(new_col_name)
    a <- unique(prud2[,new_col_name])
    b <- as.numeric(a)
    k <- 1
    for(j in b){
      
      col <- paste(new_col_name,k,sep = "_")
      prud2[, col] <-ifelse(as.numeric(prud2[,new_col_name])==j,1,0)
      if(length(b)==2 && k==1){
        prud2[, col] <- NULL
      }
      k <- k+1
    }
    
  prud2[,new_col_name] <-NULL
  }
  return(prud2)
}
prud3 <- change_factor("Product_Info_", c(1,5,6,7), prud1)
prud3 <- change_factor("Employment_Info_", c(3,5), prud3)
prud3 <- change_factor("InsuredInfo_", c(1,2,4,5,6,7), prud3)
prud3 <- change_factor("Insurance_History_", c(1,2,3,4,7,8,9), prud3)
prud3 <- change_factor("Family_Hist_", 1, prud3)
prud3 <- change_factor("Medical_History_", 3:9, prud3)
prud3 <- change_factor("Medical_History_", 11:14, prud3)
prud3 <- change_factor("Medical_History_", 16:23, prud3)
prud3 <- change_factor("Medical_History_", 25:31, prud3)
prud3 <- change_factor("Medical_History_", 33:41, prud3)
prud3 <- change_factor("Medical_Keyword_", 1:48, prud3)

# Convert Product_Info_2 into continuous column
df <- prud3
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A1",1,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A2",2,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A3",3,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A4",4,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A5",5,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A6",6,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A7",7,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("A8",8,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("B1",9,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("B2",10,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("C1",11,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("C2",12,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("C3",13,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("C4",14,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("D1",15,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("D2",16,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("D3",17,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("D4",18,x) else x))
df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub("E1",19,x) else x))
head(df)
df$Product_Info_2 <- as.numeric(df$Product_Info_2)
sapply(df, class)
write.csv(df, "C:/Users/Abhijeet/Desktop/ADS/Midterm/prud_final.csv")

#Divide data into test and train-----------------------------------------------------------------------------------
df <- read.csv('C:/Users/Abhijeet/Desktop/ADS/Midterm/prud_final.csv', head=TRUE)
df$X <- NULL
index <- 1:nrow(df)
testindex <- sample(index, trunc(length(index)*10/100))
test <- df[testindex,]
train <- df[-testindex,]

# ----------Regression Decision Tree --------------------------------------------------
lm1 <- lm(Response ~ ., data=train)
summary(lm1)
lm2 <- lm (Response~Product_Info_4 + Ins_Age + Ht + Wt + BMI + Family_Hist_2 + Family_Hist_4 
           + Medical_History_1 + Employment_Info_3_2 + InsuredInfo_2_2 
           +InsuredInfo_5_2 + InsuredInfo_6_2 + InsuredInfo_7_2 + Insurance_History_7_1 
           + Medical_History_4_2 + Medical_History_7_2 + Medical_History_11_1 
           + Medical_History_11_2 + Medical_History_22_2 + Medical_History_35_1 
           + Medical_History_39_1 + Medical_History_40_1 + Medical_Keyword_2_2 + 
             Medical_Keyword_3_2 + Medical_Keyword_9_2 + Medical_Keyword_15_2 + 
             Medical_Keyword_22_2 + Medical_Keyword_31_2 + Medical_Keyword_33_2	+ 
             Medical_Keyword_34_2	+ Medical_Keyword_38_2 + Medical_Keyword_45_2, data = train)
summary(lm2)

#---------------------------------- RPART-----------------------------------------------
fit <- rpart(Response~Product_Info_4 + Ins_Age + Ht + Wt + BMI + Family_Hist_2 + Family_Hist_4 
             + Medical_History_1 + Employment_Info_3_2 + InsuredInfo_2_2 
             +InsuredInfo_5_2 + InsuredInfo_6_2 + InsuredInfo_7_2 + Insurance_History_7_1 
             + Medical_History_4_2 + Medical_History_7_2 + Medical_History_11_1 
             + Medical_History_11_2 + Medical_History_22_2 + Medical_History_35_1 
             + Medical_History_39_1 + Medical_History_40_1 + Medical_Keyword_2_2 + 
               Medical_Keyword_3_2 + Medical_Keyword_9_2 + Medical_Keyword_15_2 + 
               Medical_Keyword_22_2 + Medical_Keyword_31_2 + Medical_Keyword_33_2	+ 
               Medical_Keyword_34_2	+ Medical_Keyword_38_2 + Medical_Keyword_45_2, method="anova", data=train)
printcp(fit)
plotcp(fit)
summary(fit)

pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE, main="Regression tree using RPART")
text(pfit,use.n=TRUE, all=TRUE, cex=.8)
response_prediction <- predict(pfit, test, method = "anova")

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- response_prediction - test$Response
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE # ERROR = 2.134315

#------------------------- CTREE------------------------------------------------------------------

fit <- ctree(Response~Product_Info_4 + Ins_Age + Ht + Wt + BMI + Family_Hist_2 + Family_Hist_4 
             + Medical_History_1 + Employment_Info_3_2 + InsuredInfo_2_2 
             +InsuredInfo_5_2 + InsuredInfo_6_2 + InsuredInfo_7_2 + Insurance_History_7_1 
             + Medical_History_4_2 + Medical_History_7_2 + Medical_History_11_1 
             + Medical_History_11_2 + Medical_History_22_2 + Medical_History_35_1 
             + Medical_History_39_1 + Medical_History_40_1 + Medical_Keyword_2_2 + 
               Medical_Keyword_3_2 + Medical_Keyword_9_2 + Medical_Keyword_15_2 + 
               Medical_Keyword_22_2 + Medical_Keyword_31_2 + Medical_Keyword_33_2	+ 
               Medical_Keyword_34_2	+ Medical_Keyword_38_2 + Medical_Keyword_45_2, data=train)

printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE, main="Regression tree using CTREE")
text(fit,use.n=TRUE, all=TRUE, cex=.8)
response_prediction <- predict(fit, test, method = "anova")
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- response_prediction - test$Response
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE 

#----------------------------Random Forest-------------------------------------------------------------
testindex <- sample(index, trunc(length(index)*1/100))
train1 <- df[-testindex,]

lm3 <- lm(Response~Product_Info_4 + Ins_Age +  Family_Hist_4 
          + Medical_History_1 + Employment_Info_3_2  
          +InsuredInfo_5_2 + InsuredInfo_6_2 + InsuredInfo_7_2 + Insurance_History_7_1 
          + Medical_History_4_2  
          + Medical_History_39_1 + Medical_History_40_1 + 
            Medical_Keyword_3_2 + Medical_Keyword_15_2, data=train)
summary(lm3)

fit <- randomForest(Response~Product_Info_4 + Ins_Age +  Family_Hist_4 
                    + Medical_History_1 + Employment_Info_3_2  
                    +InsuredInfo_5_2 + InsuredInfo_6_2 + InsuredInfo_7_2 + Insurance_History_7_1 
                    + Medical_History_4_2  
                    + Medical_History_39_1 + Medical_History_40_1 + 
                      Medical_Keyword_3_2 + Medical_Keyword_15_2, data=train1)
printcp(fit)
plotcp(fit)
summary(fit)

response_prediction <- predict(fit, test, method = "anova")
pred <- as.integer(ceiling(response_prediction))
vector_Response <- test$Response
my_solution_y1 <- data.frame(Actual_Response = vector_Response, Predicted_Response = pred)
#write.csv(my_solution_y1, "C:/Users/Abhijeet/Desktop/ADS/Midterm/xyz.csv")
table(vector_Response, pred)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- response_prediction - test$Response
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE 


 
