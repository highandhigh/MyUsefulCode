#!/usr/bin/env Rscript
# @Author: wuchlong
# @Date:   2015-11-27 11:25:03
# @Last Modified by:   wuchlong
# @Last Modified time: 2016-01-23 10:35:21
##############################################
## readme:本脚本用于爬取东方财富每个股票板块的资讯
## 用于后续的板块关键词提取
##############################################


# http://stock.eastmoney.com/hangye/427.html
# source("D:\\ntusd\\股吧数据分析\\R爬取行业资讯及关键词提取.R",encoding="utf8")
setwd("D:/ntusd/股吧数据分析")
source("sourceDir.R")
sourceDir("D:/ntusd/function")
###资讯爬取
# library(rvest)
# hycode <-  fread("股票行业分类.txt",encoding="UTF-8")
# setnames(hycode,c("code","name"))
# for(i in 1:nrow(hycode)){
# 	url <- paste("http://stock.eastmoney.com/hangye/",hycode[["code"]][i],".html",sep="")
# 	a=url%>%html_session()%>%html_nodes("div.mainCont div.list ul li a")
# 	# a%>%html_text()
# 	urls<- a%>%html_attr("href")

# 	results_dir <- file.path("行业新闻",hycode[["name"]][i])
# 	if(!file_test("-d", results_dir)){
# 		dir.create(results_dir)
# 	}
# 	for(j in 1:length(urls)){
# 		tryCatch({url <- urls[j]
# 			a=url%>%html_session()
# 			title <- a%>%html_nodes("div.newText h1")%>%html_text()%>%repair_encoding()%>%gsub("[^一-龥a-zA-Z0-9]","",.)
# 			boby <- a%>%html_nodes("#ContentBody")%>%html_text()%>%repair_encoding()%>%gsub("[^一-龥a-zA-Z0-9]","",.)
# 			write.table(boby, file = paste(file.path(results_dir, title),".txt", sep = ""), row.names = FALSE, col.names = FALSE, quote = F, fileEncoding = "UTF-8")
# 			cat(i,"###",j,"\n")
# 			}, error=function(e){test_len = 0;} )
# 	}
# }

##关键词提取
library(data.table)
read.all.files <- function(dir.name){
	dir.data <- rbindlist(lapply(list.files(file.path(dir.name),full.names=TRUE),fread,header=FALSE, encoding = "UTF-8"))
	dir.data[,code:=tail(unlist(strsplit(dir.name,"/")),1)]
	setnames(dir.data,1,"content")
	dir.data
}

all.data <- rbindlist(lapply(list.files("公告分类/公告文本",full.names=TRUE),read.all.files ))
all.data[,content:=gsub("[^一-龥]","",content)]

# 复制代码
#处理中文分词,此处用到Rwordseg包
library(Rwordseg)
insertWords(readLines("公告分类/userdict/userdict.txt", encoding = "UTF-8"))
insertWords(readLines("公告分类/userdict/company.txt", encoding = "UTF-8"))
insertWords(readLines("公告分类/userdict/profwords.txt", encoding = "UTF-8"))
insertWords(readLines("公告分类/KeyWords/KeyWords.txt", encoding = "UTF-8"))
sample.words <- lapply(all.data[["content"]], segmentCN)

# sample.words <- lapply(sample.words,reweight)

# 3.    wordcloud展示
#构建语料库
library(tm)
corpus = Corpus(VectorSource(sample.words))
meta(corpus,"cluster") <- all.data[["code"]]


#############   SVM 
#建立文档-词条矩阵
# tm_map(corpus, removePunctuation) 
(sample.dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf))))
# # # # the freq weight

# (sample.dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf), weighting = function(x) weightTfIdf(x, normalize = FALSE),stopwords = TRUE)))
### the if_idf weight

# sample.dtm$dimnames
# train<- scv$text
sample_matrix = as.matrix(sample.dtm)
rownames(sample_matrix) <- all.data[["code"]]
rm(all.data);gc()


Doc_class_index <- rownames(sample_matrix)
Chi_square_Wei <- Chi_square(sample_matrix, Doc_class_index)
rm(sample_matrix);gc()

# Chi_squre_choose <- t(apply(Chi_square_Wei$Chi_freq_matrix, 1, function(x){colnames(Chi_square_Wei$Chi_freq_matrix)[order(x, decreasing = TRUE)][1:10]}))

## print the key words  
## the Chi_square method
cat("****  the KeyWords extracted by Chi_square   ****", "\n")
Chi_square_words <- NULL
for(class_choose in 1:length(unique(Doc_class_index))){
	# most_freq <- order(Chi_square_Wei$Chi_freq_matrix[7, ], decreasing = TRUE)[1:30]
	most_chi <- order(Chi_square_Wei$Chi_square_matrix[class_choose, ], decreasing = TRUE)[1:200]
	most_freq <- which(Chi_square_Wei$Chi_freq_matrix[class_choose, ] > 0)
	Chi_square_words <- cbind.na(Chi_square_words, head(colnames(Chi_square_Wei$Chi_freq_matrix)[intersect(most_chi, most_freq)], 100))
	# cat(paste("Class", class_choose), head(colnames(Chi_square_Wei$Chi_freq_matrix)[intersect( most_chi, most_freq)], 10), "\n")
}
Chi_square_words <- Chi_square_words[, !apply(Chi_square_words, 2, function(x){all(is.na(x))})]
Chi_square_words <- data.frame(Chi_square_words, row.names = paste("Keywords", 1:100))
colnames(Chi_square_words) <- unique(Doc_class_index)
# print(Chi_square_words)
write.table(Chi_square_words, file = "公告分类/行业关键词.txt", row.names = FALSE, col.names = TRUE, sep=",",quote = F, fileEncoding = "UTF-8")


## print the key words  
## the TF-IDF perform well  只是权重相加
cat("****  the KeyWords extracted by TF-IDF   ****", "\n")
TF_IDF_words <- NULL
for(class_choose in 1:length(unique(Doc_class_index))){
	# most_freq <- order(Chi_square_Wei$Chi_freq_matrix[7, ], decreasing = TRUE)[1:30]
	most_freq <- order(Chi_square_Wei$Chi_freq_matrix[class_choose, ], decreasing = TRUE)[1:100]
	TF_IDF_words <- cbind.na(TF_IDF_words, head(colnames(Chi_square_Wei$Chi_freq_matrix)[most_freq], 20))
	# cat(paste("Class", class_choose), TF_IDF_words[class_choose], "\n")
}
TF_IDF_words <- TF_IDF_words[, !apply(TF_IDF_words, 2, function(x){all(is.na(x))})]
TF_IDF_words <- data.frame(TF_IDF_words, row.names = paste("Keywords", 1:20))
colnames(TF_IDF_words) <- unique(Doc_class_index)
print(TF_IDF_words)

library(tm)



##########进行分类
TF_IDF_words = unlist(strsplit(readLines("公告分类/行业关键词.txt",encoding="UTF-8"),","))
key_words="TF_IDF_words"
goal_key_words <- unlist(eval(parse(text=key_words)) )

csv_pre <- rbindlist(lapply(list.files("公告分类/公告文本",full.names=TRUE),read.all.files ))
csv_pre[,content:=gsub("[^一-龥]","",content)]
setnames(csv_pre,c("text","type"))
# words_pre <- lapply(csv_pre$text, Del_Words, Goal_Words = c(mystopwords, 1:9))
words_pre <- lapply(csv_pre$text, segmentCN)
# words_pre <- lapply(words_pre, reweight)


# data<-gsub("\n","",sample.words[14])
pre_corpus = Corpus(VectorSource(words_pre))
meta(pre_corpus,"cluster") <- csv_pre$type

(train.dtm <- DocumentTermMatrix(pre_corpus, control = list(dictionary = as.vector(goal_key_words), wordLengths = c(2, Inf))))
train_matrix = as.matrix(train.dtm)
train_matrix <- train_matrix[, colSums(train_matrix) > 0]
train <- as.data.frame(train_matrix)
train$type <-  csv_pre$type


(pre.dtm <- DocumentTermMatrix(pre_corpus, control = list(dictionary = as.vector(goal_key_words), wordLengths = c(2, Inf))))
# (train.dtm <- DocumentTermMatrix(corpus, control = list( wordLengths = c(2, Inf))))
pre_matrix = as.matrix(pre.dtm)
pre_matrix <- pre_matrix[, colSums(pre_matrix) > 0]
test <- as.data.frame(pre_matrix)
test$type <-  csv_pre$type
library(dr)
library(kernlab)
train_ksvm  <-  ksvm(type~., data = train)
svmCl  <-  predict(train_ksvm, pre_matrix)
(svmTable <-table(SVM = svmCl, sample = test$type))
cat("****  the KeyWords ",key_words,"*******", "\n")
print(calculate_P_R_F(prevalue = svmCl, realvalue = test$type))




