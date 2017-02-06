library(plyr)
library(stringr)
library(SnowballC)

# Load files
setwd('/Users/vincentliu/Documents/R_Workspace/DataScience/assign8')
pos <- scan(file.path('goodWords.txt'), what='character', comment.char=';')
neg <- scan(file.path('badWords.txt'), what='character', comment.char=';')
Data <- read.csv('northeastern_class.csv',header = TRUE)

# Extract reviews and stem dictionaries
reviews <- lapply(Data$Body, as.character)
pos <- wordStem(pos)
neg <- wordStem(neg)

# Initialize score matrix 
len <- length(reviews)
pos_score <- matrix(0, len, 1)
neg_score <- matrix(0, len, 1)

# Compare each stemmed review with the stemmed positive/negative dictionary
for (i in c(1 : len)){
reviews[i] <- gsub('[[:punct:]]', '', reviews[i])
reviews[i] <- gsub('[[:cntrl:]]', '', reviews[i])
reviews[i] <- gsub('\\d+', '', reviews[i])

reviews[i] <- tolower(reviews[i])
word.list <- str_split(reviews[i], '\\s+')
words <- wordStem(unlist(word.list))

pos.matches <- match(words, pos)
neg.matches <- match(words, neg)
pos.matches <- !is.na(pos.matches)
neg.matches <- !is.na(neg.matches)

# Compute the percentage of positive and negative words in each review
pos_score[i] <- sum(pos.matches)/(sum(pos.matches)+sum(neg.matches))
neg_score[i] <- sum(neg.matches)/(sum(pos.matches)+sum(neg.matches))
}

# Calculate average sentiments
mean_neg <- mean(neg_score)
mean_pos <- mean(pos_score)
