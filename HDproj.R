## this project is done on Window environment,
## therefore, the current working derectory will change
## depending on the platform that using this script.
rm(list=ls())
setwd("C://CS//Kaggle")
library(arules)
library(tm)
library(randomForest)
train<-read.csv(file ="train.csv", header=TRUE, sep = ",")
##head(train, 3)
test<-read.csv(file ="test.csv", header=TRUE, sep = ",")
##head(test, 3)
attr<-read.csv(file ="attributes.csv", header=TRUE, sep = ",")
##head(attr, 3)
prod_des<-read.csv(file ="product_descriptions.csv", header=TRUE, sep = ",")
##head(prod_des, 3)
train <- merge(train,prod_des, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,prod_des, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

clean_text <- function(text)
{
  gsub(" "," ", text)
  gsub("°", "degrees", text)
  gsub(" v ", "volts", text)
  gsub("^", " ", text)
}
word_match <- function(words,title,desc)
{
  n_title <- 0
  n_desc <- 0
  words <- unlist(strsplit(words," "))
  ## text mining on desc
  pdCorpus <- Corpus(VectorSource(desc))
  # to lower case
  pdCorpus <- tm_map(pdCorpus, content_transformer(tolower))
  # remove punctuation
  pdCorpus <- tm_map(pdCorpus, removePunctuation)
  # remove stopwords
  pdCorpus <- tm_map(pdCorpus, removeWords, stopwords(kind="en"))
  pdCorpus <- tm_map(pdCorpus, stripWhitespace)
  # remove stemming words
  dictCorpus <- pdCorpus
  pdCorpus <- tm_map(pdCorpus, stemDocument)
  #pdCorpus <- tm_map(pdCorpus, stemCompletion, dictionary = dictCorpus)
  # # Building a Document-Term Matrix
  # pdDTm <- TermDocumentMatrix(pdCorpus, control = list(minwordlength=1))
  # pdDTm <- as.matrix(pdDTm)
  # sort by freq
  #v <- sort(rowSums(pdDTm), decreasing = T)
  nwords <- length(words)
  for(i in 1:nwords)
  {
    n_char <- nchar(words[1])
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    #pattern <- words[i]
    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,pdCorpus$content,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_title,nwords,n_desc))
}

train$product_title <- clean_text(train$product_title)
train$product_description <- clean_text(train$product_description)
train$search_term <- clean_text(train$search_term)
test$product_title <- clean_text(test$product_title)
test$product_description <- clean_text(test$product_description)
test$search_term <- clean_text(test$search_term)
cat("Get number of words and word matching title in train\n")
train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
train$nmatch_title <- train_words[,1]
train$nwords <- train_words[,2]
train$nmatch_desc <- train_words[,3]
cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_title <- test_words[,1]
test$nwords <- test_words[,2]
test$nmatch_desc <- test_words[,3]
model1<-randomForest(relevance~nmatch_title+nmatch_desc,
                     data=train,
                     type=regression,
                     importance=T,
                     na.action = na.omit,
                     ntree=501
)
test_result<-as.data.frame(predict(model1, test, type="response"))
test_result<-cbind(test$id,test_result)
colnames(test_result)<- c("id","predict_relevance")