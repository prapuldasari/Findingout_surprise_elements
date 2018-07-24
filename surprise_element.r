install.packages("tm") 
install.packages("magrittr") 
install.packages("factoextra") 
install.packages("skmeans") 
install.packages("wordcloud") 
library(tm) 
library(cluster) 
library(factoextra) 
library(magrittr) 
library(skmeans) 
library(wordcloud) 
 
require("slam") 
 
setwd("C:/Users/Prapul Kumar/Desktop/KDD/Rdata") 
 
text_corpus<-Corpus(DirSource("diabetes")) 
text_corpus <- tm_map(text_corpus, stripWhitespace) 
text_corpus <- tm_map(text_corpus, content_transformer(tolower)) 
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english")) 
 
text_corpus1<-Corpus(DirSource("test")) 
text_corpus1 <- tm_map(text_corpus1, stripWhitespace) 
text_corpus1 <- tm_map(text_corpus1, content_transformer(tolower)) 
text_corpus <- tm_map(text_corpus1, removeWords, stopwords("english")) 
#text_corpus <- tm_map(text_corpus, removePunctuation) 
 
dtm <- DocumentTermMatrix(text_corpus) 
 
 
summary(text_corpus) 
inspect(dtm) 
 
dtm <- weightTfIdf(dtm, normalize = TRUE) 
inspect(dtm) 
 
# 
mfrq_words_per_cluster <- function(clus, dtm, first = 10, unique = TRUE){ 
  if(!any(class(clus) == "skmeans")) return("clus must be an skmeans object") 
   
  dtm <- as.simple_triplet_matrix(dtm) 
  indM <- table(names(clus$cluster), clus$cluster) == 1 # generate bool matrix 
   
  hfun <- function(ind, dtm){ # help function, summing up words 
    if(is.null(dtm[ind, ]))  dtm[ind, ] else  col_sums(dtm[ind, ]) 
  } 
  frqM <- apply(indM, 2, hfun, dtm = dtm) 
   
  if(unique){ 
    # eliminate word which occur in several clusters 
    frqM <- frqM[rowSums(frqM > 0) == 1, ]  
  } 
  # export to list, order and take first x elements  
  res <- lapply(1:ncol(frqM), function(i, mat, first) 
    head(sort(mat[, i], decreasing = TRUE), first), 
    mat = frqM, first = first) 
   
  names(res) <- paste0("CLUSTER_", 1:ncol(frqM)) 
  return(res) 
} 
#we have to delete a empty file to run this (data preprocessing) 
clus <- skmeans(dtm, 5) 
mfrq_words_per_cluster(clus, dtm) 
mfrq_words_per_cluster(clus, dtm, unique = FALSE) 
# 
m3<-as.matrix(dtm) 
df3<-as.data.frame(m3) 
# 
m  <- as.matrix(dtm) 
dataframe <-as.data.frame(m) 
#m <- m[1:2, 1:3] 
distMatrix <- dist(dataframe, method="euclidean") 
flatclust <- pam(distMatrix,k=2,metric = "manhattan",medoids = NULL) 
plot(flatclust, cex=0.9, hang=-1) 
#flatclust1<- as.matrix(flatclust) 
class(flatclust) 
flatclust 
# 
install.packages("dendextend") 
library(dendextend) 
dendoclust <- hclust(distMatrix,method="ward.D") 
dd <- as.dendrogram(dendoclust) 
labels(dd)  
label.dendrogram(dd) 
plot(dendoclust, cex=0.9, hang=-1) 
rect.hclust(dendoclust,k=25) 
install.packages("ggplot2") 
library(ggplot2) 
m<-as.matrix(dtm) 
gc() 
#wordcloud of camel document 
text_corpus<-Corpus(DirSource("test1")) 
text_corpus <- tm_map(text_corpus, stripWhitespace) 
text_corpus <- tm_map(text_corpus, content_transformer(tolower)) 
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english")) 
library(wordcloud) 
wordcloud(text_corpus,min.freq = 1.5) 
m<-as.matrix(dtm) 
memory.limit() 
dtm 
m5<-m[,"camel"] 
m5<-as.matrix(m5) 
m6<-m[,"okra"] 
m6<-as.matrix(m6) 
# Cosine similarity 
dtm <- DocumentTermMatrix(text_corpus) 
m<-t(m) 
ma<-cosine(m) 
ma<-as.matrix(ma)
