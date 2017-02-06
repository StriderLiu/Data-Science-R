# First, data preprocessing for training data set
df <- read.csv("/Users/vincentliu/Documents/R_Workspace/DataScience/assign6/train.csv")

# 1.Fill in the missing data with mean
# find all those columns with missing data
df.hasNA <- df[, colSums(is.na(df)) > 0] # 13
#count the ratio of NAs in each columns
rows.num <- nrow(df)
cnt <- sapply(df.hasNA, function(x) sum(is.na(x)))
cnt <- cnt / rows.num
cnt
# Thus, omitt those columns whoes NA rate is larger than 50%
# And we get Employment_Info_1, Employment_Info_4, Employment_Info_6,
# Insurance_History_5, Family_Hist_2, Family_Hist_4, Medical_History_1

# Just IGNORE Family_Hist_3, Family_Hist_5, Medical_History_10,
# Medical_History_15, Medical_History_24, Medical_History_32
# We can fill the missing data in these columns,
# but with whatever value we fill in there will be too many same values in a column,
# which makes the variance of the column very low
# So fill in or not, these columns can't be very useful to us

##Missing values insertion
#Employment_Info_1
m <- mean(df$Employment_Info_1[!is.na(df$Employment_Info_1)])
df$Employment_Info_1[is.na(df$Employment_Info_1)] = m


#Employment_Info_4
m <- mean(df$Employment_Info_4[!is.na(df$Employment_Info_4)])
df$Employment_Info_4[is.na(df$Employment_Info_4)] = m


#Employment_Info_6
m <- mean(df$Employment_Info_6[!is.na(df$Employment_Info_6)])
df$Employment_Info_6[is.na(df$Employment_Info_6)] = m


#Insurance_History_5
m <- mean(df$Insurance_History_5[!is.na(df$Insurance_History_5)])
df$Insurance_History_5[is.na(df$Insurance_History_5)] = m


#Family_Hist_2
m <- mean(df$Family_Hist_2[!is.na(df$Family_Hist_2)])
df$Family_Hist_2[is.na(df$Family_Hist_2)] = m


#Family_Hist_4
m <- mean(df$Family_Hist_4[!is.na(df$Family_Hist_4)])
df$Family_Hist_4[is.na(df$Family_Hist_4)] = m


#Medical_History_1
m <- mean(df$Medical_History_1[!is.na(df$Medical_History_1)])
df$Medical_History_1[is.na(df$Medical_History_1)] = m

library("plyr")

# 2.Transform categorical data into continuous format
# "1-to-N" method

splitCol <- function(df, prefix, seq) {
  for(surfix in seq) { # for each column
    col.name <- paste0(prefix, surfix)
    col <- df[, col.name]
    ori <- unique(col)
    newNames <- c()
    if (length(ori) > 2) {
      # for each possible value in this column
      for (i in 1:length(ori)) {
        val <- strrep('0', length(ori))
        substr(val, i, i) <- '1'
        col[col == ori[i]] <- val
        newNames <- c(newNames, paste0(col.name, "_", as.character(i)))
      }
      # split column and append the new ones to data frame
      list = strsplit(as.character(col), "")
      df.add <- ldply(list)
      colnames(df.add) <- newNames
      
      for (name in newNames) {
        df[, name] <- as.numeric(df.add[, name])
      }
      df[, col.name] <- NULL
    }
    else {
      for (i in 1:length(ori)) {
        col[col == ori[i]] <- (i - 1)
      }
      df[, col.name] <- col
    }
  }
  df # return the data frame
}

df <- splitCol(df, "Product_Info_", c(1, 5, 6, 7))
df <- splitCol(df, "Employment_Info_", c(3, 5))
df <- splitCol(df, "InsuredInfo_", c(1, 2, 4, 5, 6, 7))
df <- splitCol(df, "Insurance_History_", c(1, 2, 3, 4, 7, 8, 9))
df <- splitCol(df, "Family_Hist_", c(1))
df <- splitCol(df, "Medical_History_", c(3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 40, 41))

# 3.Transform Product_Info_2 to numeric data format
vals <- unique(df$Product_Info_2)
i = 1
for (s in vals) {
  df <- as.data.frame(lapply(df,function(x) if(is.character(x)|is.factor(x)) gsub(s,i,x) else x))
  i <- i + 1
}

df$Product_Info_2 <- as.numeric(levels(df$Product_Info_2))[df$Product_Info_2]

# 4.Omitt dummy columns and those who has too much missing data
df <- subset(df, select = -c(Family_Hist_3, Family_Hist_5, Medical_History_10,
                             Medical_History_15, Medical_History_24, Medical_History_32,
                             Medical_Keyword_1, Medical_Keyword_2, Medical_Keyword_3,
                             Medical_Keyword_4, Medical_Keyword_5, Medical_Keyword_6,
                             Medical_Keyword_7, Medical_Keyword_8, Medical_Keyword_9,
                             Medical_Keyword_10, Medical_Keyword_11, Medical_Keyword_12,
                             Medical_Keyword_13, Medical_Keyword_14, Medical_Keyword_15,
                             Medical_Keyword_16, Medical_Keyword_17, Medical_Keyword_18,
                             Medical_Keyword_19, Medical_Keyword_20, Medical_Keyword_21,
                             Medical_Keyword_22, Medical_Keyword_23, Medical_Keyword_24,
                             Medical_Keyword_25, Medical_Keyword_26, Medical_Keyword_27,
                             Medical_Keyword_28, Medical_Keyword_29, Medical_Keyword_30,
                             Medical_Keyword_31, Medical_Keyword_32, Medical_Keyword_33,
                             Medical_Keyword_34, Medical_Keyword_35, Medical_Keyword_36,
                             Medical_Keyword_37, Medical_Keyword_38, Medical_Keyword_39,
                             Medical_Keyword_40, Medical_Keyword_41, Medical_Keyword_42,
                             Medical_Keyword_43, Medical_Keyword_44, Medical_Keyword_45,
                             Medical_Keyword_46, Medical_Keyword_47, Medical_Keyword_48))
# should be False which means no NA exists
unique(apply(df, 2, function(x) any(is.na(x))))

# 5.Run Logistic Regression and perform dimension reduction
loglm <- glm(Response ~ ., data = df)
summary(loglm)
# dimention reduction
# select (Product_Info_1, Product_Info_2, Product_Info_3, Product_Info_4, Product_Info_5, Product_Info_6, Ins_Age, Ht, Wt, BMI,
# Employment_Info_3, Employment_Info_6, InsuredInfo_2, InsuredInfo_5, InsuredInfo_6, InsuredInfo_7, Insurance_History_1,
# Insurance_History_5, Family_Hist_2, Family_Hist_4, Medical_History_1, Medical_History_2, Medical_History_4, Medical_History_22,
# Medical_History_33, Medical_History_38, Insurance_History_4_1, Insurance_History_4_2, Insurance_History_4_3,
# Insurance_History_7_1,Insurance_History_7_2, Insurance_History_7_3, Medical_History_7_1, Medical_History_7_2, Medical_History_7_3,
# Medical_History_11_1, Medical_History_11_2, Medical_History_11_3, Medical_History_14_1, Medical_History_26_1, Medical_History_26_2, Medical_History_26_3
# Medical_History_35_1, Medical_History_35_2, Medical_History_35_3, Medical_History_39_1, Medical_History_39_2, Medical_History_39_3)
df <- subset(df, select = c(Product_Info_1, Product_Info_2, Product_Info_3, Product_Info_4, Product_Info_5, Product_Info_6, Ins_Age, Ht, Wt, BMI,
                            Employment_Info_3, Employment_Info_6, InsuredInfo_2, InsuredInfo_5, InsuredInfo_6, InsuredInfo_7, Insurance_History_1,
                            Insurance_History_5, Family_Hist_2, Family_Hist_4, Medical_History_1, Medical_History_2, Medical_History_4, Medical_History_22,
                            Medical_History_33, Medical_History_38, Insurance_History_4_1, Insurance_History_4_2, Insurance_History_4_3,
                            Insurance_History_7_1,Insurance_History_7_2, Insurance_History_7_3, Medical_History_7_1, Medical_History_7_2, Medical_History_7_3,
                            Medical_History_11_1, Medical_History_11_2, Medical_History_11_3, Medical_History_14_1, Medical_History_14_2, Medical_History_14_3, 
                            Medical_History_26_1, Medical_History_26_2, Medical_History_26_3, Medical_History_35_1, Medical_History_35_2, Medical_History_35_3,
                            Medical_History_39_1, Medical_History_39_2, Medical_History_39_3, Response))

# write.csv(df, "cleanedDF.csv")

# df <- df[1:20000, ]

# 6.Split the data set into training set and cross validation set
index <- 1:nrow(df)
testindex <- sample(index, trunc(length(index)*10/100))
test <- df[testindex,]
train <- df[-testindex,]

# 7.Apply NN:
feats <- names(subset(train, select = -c(Response)))

# Concatenate strings
f <- paste(feats, collapse = ' + ')
f <- paste('Response ~', f)

# Convert to formula
f <- as.formula(f)

# Train model with f
# library(neuralnet)
# nn <- neuralnet(f, train, hidden = 10, stepmax = 1e+06, linear.output = TRUE)

# Compute Predictions off Test Set
# pred.nn.values <- compute(nn, test[, 1:50])

# Check out net.result
# print(head(pred.nn.values$net.result))

# pred.nn.values$net.result <- sapply(pred.nn.values$net.result, round, digits = 0)

# table(test$Response, pred.nn.values$net.result)

# accuracy <- sum(!is.na(match(test$Response, pred.nn.values$net.result))) / length(test$Response)
library(nnet)
nn <- nnet(f, data = train, maxit = 800, size = 20, linout = TRUE)

pred.nn.values <- predict(nn, test[, 1:50])
pred.nn.values <- sapply(pred.nn.values, round, digits = 0)

prediction_boundary <- function(p,upper,lower){
  for(i in c(1:length(p))){
    if(p[i] > upper){
      p[i] = upper
    }
    if(p[i] < lower){
      p[i] = lower
    }
  }
  return(p)
}

pred.nn.values <- prediction_boundary(pred.nn.values, 8, 1)

table(test$Response, pred.nn.values)
mae <- function(y, pred) {
  mean(abs(y - pred))
}
mae(pred.nn.values, test$Response)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)
