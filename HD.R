rm(list=ls())
setwd("C://CS//Kaggle")
# Load Packages needed
#Needed <- c("tm", "ggplot2", "wordcloud","Snowball",
#            "RWeka", "arulesViz", "RWekajars","randomForest","data.table")
#install.packages(Needed, dependencies=TRUE,repos="https://cloud.r-project.org/")
library(arules)
library(tm)
library(wordcloud)
library(ggplot2)
library(randomForest)
library(data.table)

##read in data
train<-read.csv(file ="train.csv", header=TRUE, sep = ",")
##head(train, 3)
test<-read.csv(file ="test.csv", header=TRUE, sep = ",")
##head(test, 3)
attr<-read.csv(file ="attributes.csv", header=TRUE, sep = ",")
##head(attr, 3)
prod_des<-read.csv(file ="product_descriptions.csv", header=TRUE, sep = ",")
##head(prod_des, 3)


####################################################
##### Combination of dataset #######################
####################################################
attr1<-subset(attr, is.na(attr$product_uid)==F)
attr2<-aggregate(attr1, by=list(attr1$product_uid), paste, collapse = " ")
attr2$attribute <- paste(attr2$name,attr2$value,sep = " ")
attr3<-subset(attr2, select=c(Group.1,attribute))
names(attr3)[names(attr3)=="Group.1"]<-"product_uid"

train1 <- merge(train,attr3, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
train1<-head(train1,10)
train2 <- merge(train1,prod_des, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)


test1 <- merge(test,attr3, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test1<-head(test1,10)
test2 <- merge(test1,prod_des, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

clean_text <- function(text)
{
  gsub(" ","  ", text)
  gsub("°", "degrees", text)
  gsub(" v ", "volts", text)
  gsub("^", " ", text)
}

word_match <- function(words,title,attr,desc)
{
  n_title <- 0
  n_desc <- 0
  n_attr <- 0
  words <- unlist(strsplit(as.character(words)," "))
  title <- unlist(strsplit(as.character(title)," "))
  attr<- unlist(strsplit(as.character(attr)," "))
  desc <- unlist(strsplit(as.character(desc)," "))
  ## text mining on words title desc
  wdCorpus <- Corpus(VectorSource(words))
  ttCorpus <- Corpus(VectorSource(title))
  atCorpus <- Corpus(VectorSource(attr))
  pdCorpus <- Corpus(VectorSource(desc))
  # to lower case
  wdCorpus <- tm_map(wdCorpus,content_transformer(tolower))
  ttCorpus <- tm_map(ttCorpus,content_transformer(tolower))
  atCorpus <- tm_map(atCorpus,content_transformer(tolower))
  pdCorpus <- tm_map(pdCorpus,content_transformer(tolower))
  
  # remove punctuation
  wdCorpus <- tm_map(wdCorpus, removePunctuation)
  ttCorpus <- tm_map(ttCorpus, removePunctuation)
  atCorpus <- tm_map(atCorpus, removePunctuation)
  pdCorpus <- tm_map(pdCorpus, removePunctuation)
  # remove stopwords
  wdCorpus <- tm_map(wdCorpus, removeWords, stopwords(kind="en"))
  wddCorpus <- tm_map(wdCorpus, stripWhitespace)
  ttCorpus <- tm_map(ttCorpus, removeWords, stopwords(kind="en"))
  ttCorpus <- tm_map(ttCorpus, stripWhitespace)
  atCorpus <- tm_map(atCorpus, removeWords, stopwords(kind="en"))
  atCorpus <- tm_map(atCorpus, stripWhitespace)
  pdCorpus <- tm_map(pdCorpus, removeWords, stopwords(kind="en"))
  pdCorpus <- tm_map(pdCorpus, stripWhitespace)
  # remove stemming words
  wddictCorpus <- wdCorpus
  wdCorpus <- tm_map(wdCorpus, stemDocument)
  wdCorpus <- tm_map(wdCorpus, stemCompletion, dictionary = wddictCorpus)
  ttdictCorpus <- ttCorpus
  ttCorpus <- tm_map(ttCorpus, stemDocument)
  ttCorpus <- tm_map(ttCorpus, stemCompletion, dictionary = ttdictCorpus)
  atdictCorpus <- atCorpus
  atCorpus <- tm_map(atCorpus, stemDocument)
  atCorpus <- tm_map(atCorpus, stemCompletion, dictionary = atdictCorpus)
  pddictCorpus <- pdCorpus
  pdCorpus <- tm_map(pdCorpus, stemDocument)
  pdCorpus <- tm_map(pdCorpus, stemCompletion, dictionary = pddictCorpus)
  # # Building a Document-Term Matrix
  # pdDTm <- TermDocumentMatrix(pdCorpus, control = list(minwordlength=1))
  # pdDTm <- as.matrix(pdDTm)
  # sort by freq
  #v <- sort(rowSums(pdDTm), decreasing = T)
  n_words <- length(words)
  for(i in 1:n_words)
  {
    pattern <- paste("(^| )",wdCorpus$content[i],"($| )",sep="")
    #pattern <- words[i]
    n_title <- n_title + grepl(pattern,ttCorpus$content,perl=TRUE,ignore.case=TRUE)
    n_attr <- n_attr + grepl(pattern,atCorpus$content,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,pdCorpus$content,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_words,n_title,n_attr, n_desc))
}

train2$product_title <- clean_text(train2$product_title)
train2$atrribute <- clean_text(train2$attribute)
train2$product_description <- clean_text(train2$product_description)
train2$search_term <- clean_text(train2$search_term)

test2$product_title <- clean_text(test2$product_title)
test2$atrribute <- clean_text(test2$attribute)
test2$product_description <- clean_text(test2$product_description)
test2$search_term <- clean_text(test2$search_term)

cat("Get number of words and word matching title, attribute, product_description in train\n")
train_words <- as.data.frame(t(mapply(word_match,train2$search_term,train2$product_title,train2$attribute, train2$product_description)))
train2$nwords <- train_words[,1]
train2$nmatch_title <- train_words[,2]
train2$nmatch_attr <- train_words[,3]
train2$nmatch_desc <- train_words[,4]

cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test2$search_term,test2$product_title, test2$attribute, test2$product_description)))
test2$nwords <- test_words[,1]
test2$nmatch_title <- test_words[,2]
test2$nmatch_attr <- test_words[,3]
test2$nmatch_desc <- test_words[,4]

model1<-randomForest(relevance~nmatch_title+nmatch_attr+nmatch_desc, 
                     data=train2, 
                     type=regression,
                     importance=T, 
                     na.action = na.omit,
                     ntree=501
)

test_result<-as.data.frame(predict(model1, test2, type="response"))
test_result<-cbind(test2$id,test_result)
colnames(test_result)<- c("id","predict_relevance")

write.csv(test_result,"test_result_submission.csv")
### Display
# model error rate
plot(model1,log="y")
varImpPlot(model1)

