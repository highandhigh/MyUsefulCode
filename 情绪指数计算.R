options(scipen=20)#取消科学计数法
setwd("D:/ntusd/股吧数据分析")
source("sourceDir.R")
sourceDir("function")
limit_num <- seq(0,1000000,1000)
for(limit_i in limit_num){
	con <- dbConnect(MySQL(),user='root',password='tcl_bigdata@321',host='192.168.1.142',dbname='bigdata',port=3306)
	dbSendQuery(con,'set names gbk')
	content  <- dbSendQuery(con, paste('SELECT content FROM tb_dfcf01 limit ',limit_i,',1000;', sep=" "))
	content <- unlist(fetch(content, -1))
	dbDisconnect(con)

	freq_info <- sort(table(unlist(segmentCN(content))),decreasing = TRUE)
	if(limit_i==0){
		freq_table <- data.table(names(freq_info),freq_info)
	}else{
		freq_table <- rbind(freq_table, data.table(names(freq_info),freq_info))
		freq_table <- freq_table[,sum(V2),by=V1]
		setnames(freq_table,c("V1","V2"))
	}
	print(limit_i)
}

write.table(freq_table, file = "sentiwords.txt", row.names = FALSE, col.names = FALSE, quote = F, fileEncoding = "UTF8")
# sentiwords=paste(c("不错","挺好"),collapse="|")
# guba_content=c("不错滴额","难道","就是","可以") 
# grepl(substring,string_vector)

mystopwords <- readLines("D:\\ntusd\\topics\\StopWords.txt",encoding="UTF-8")
Finawords <- readLines("D:\\ntusd\\sentiment\\Finance.txt",encoding="UTF-8")

#  读入停词和金融词汇
level_words <- readLines("D:\\ntusd\\sentiment\\level.txt",encoding="UTF-8")
negative_emo <- readLines("D:\\ntusd\\sentiment\\negative_emo.txt",encoding="UTF-8")
negative_eva <- readLines("D:\\ntusd\\sentiment\\negative_eva.txt",encoding="UTF-8")
negative_add <- readLines("D:\\ntusd\\sentiment\\negative_add.txt",encoding="UTF-8")
positive_add <- readLines("D:\\ntusd\\sentiment\\positive_add.txt",encoding="UTF-8")
positive_emo <- readLines("D:\\ntusd\\sentiment\\positive_emo.txt",encoding="UTF-8")
positive_eva <- readLines("D:\\ntusd\\sentiment\\positive_eva.txt",encoding="UTF-8")
Notword_add <- readLines("D:\\ntusd\\sentiment\\Notword_add.txt",encoding="UTF-8")
# 情感词汇
#  读入停词和金融词汇
write.table(subset(freq_table,V1%chin%c(positive_add, positive_emo, positive_eva)), file = "sentiwords_posi.txt", row.names = FALSE, col.names = FALSE, quote = F, fileEncoding = "UTF8")
write.table(subset(freq_table,V1%chin%c(negative_emo, negative_eva, negative_add)), file = "sentiwords_neg.txt", row.names = FALSE, col.names = FALSE, quote = F, fileEncoding = "UTF8")



# subset(content_senti,pos_neg==0)[1:100]


# con <- dbConnect(MySQL(),user='root',password='tcl_bigdata@321',host='192.168.1.142',dbname='bigdata',port=3306)
# dbSendQuery(con,'set names gbk')
# content  <- dbSendQuery(con, paste('SELECT content FROM tb_dfcf01 limit 100000;', sep=" "))
# content <- unlist(fetch(content, -1))
# dbDisconnect(con)
# write.csv(content, file = "content.csv", row.names = FALSE, col.names = FALSE, quote = F, fileEncoding = "UTF8")
####读取文本 然后 计算情感  写入数据库
neg_words <- read.table(file = "sentiwords_neg.txt",fileEncoding = "UTF8")$V1
posi_words <- read.table(file = "sentiwords_posi.txt",fileEncoding = "UTF8")$V1
neg_substring <- paste(neg_words,collapse="|")
posi_substring <- paste(posi_words,collapse="|")

limit_num <- seq(0,6400000,10000)
for(limit_i in limit_num){
	con <- dbConnect(MySQL(),user='root',password='tcl_bigdata@321',host='192.168.1.142',dbname='gushi',port=3306)
	dbSendQuery(con,'set names gbk')
	content  <- dbSendQuery(con, paste('SELECT post_date,stockCode,content FROM tb_dfcf01 limit ',limit_i,',10000;', sep=" "))
	content <-fetch(content, -1)
	dbDisconnect(con)

	content_senti <- data.table(content)
	string_vector <- content$content
	neg_local <- which(grepl(neg_substring,string_vector))
	posi_local <- setdiff(which(grepl(posi_substring,string_vector)),neg_local)
	content_senti[,pos_neg:=0]
	content_senti[neg_local,pos_neg:=-1]
	content_senti[posi_local,pos_neg:=1]

	con <- dbConnect(MySQL(),user='root',password='tcl_bigdata@321',host='192.168.1.142',dbname='gushi',port=3306)
	dbSendQuery(con,'SET NAMES gbk')
	# dbSendQuery(con,'delete from product_percentile;')
	dbWriteTable(con,"guba_senti", value=subset(content_senti,select=c(post_date,stockCode,pos_neg)),overwrite=FALSE,append=TRUE,row.names=FALSE)
	dbDisconnect(con)
	print(limit_i)
}



# grepl(neg_substring,string_vector)

# grepl(posi_substring,string_vector)

###画图 
library(ggplot2)
con <- dbConnect(MySQL(),user='root', password='tcl_bigdata@321',host='192.168.1.142',dbname='gushi',port=3306)
dbSendQuery(con,'set names gbk')
SQLQuery <- "SELECT * FROM guba_senti WHERE LENGTH(post_date)=10;"
res  <- dbSendQuery(con, SQLQuery)
##### 读取数据并解决乱码问题 
data0 <- fetch(res,-1) 
dbDisconnect(con)

data <- data.table(data0)
senti_vaue <- function(vec){
	# vec <- as.numeric(vec)
	pos_num <- sum(vec=="1")
	neg_num <- sum(vec=="-1")
	med_num <- sum(vec=="-1")
	return((1+pos_num/length(vec))/(1+neg_num/length(vec)))
	# return((1+pos_num)/(1+neg_num))
	# return(pos_num/(pos_num+neg_num))
	# return((pos_num+med_num/3)/(med_num+pos_num+neg_num))
}
data[,Data_month := substr(post_date,1,7)]
data <- data[,list(value=senti_vaue(pos_neg),time=head(post_date,1)),by = Data_month]
data <- subset(data,select=c("time","value"))
# setnames(data,c("time","value"))
data[,time:=as.Date(time)]

##获取每月上证指数
con <- dbConnect(MySQL(),user='root', password='tcl_bigdata@321',host='192.168.1.142',dbname='gushi',port=3306)
dbSendQuery(con,'set names gbk')
SQLQuery <- "SELECT time,value   FROM guba_redu WHERE en='shang_month';"
res  <- dbSendQuery(con, SQLQuery)
##### 读取数据并解决乱码问题 
data_shang <- fetch(res,-1) 
dbDisconnect(con)
data_shang$time <- as.Date(data_shang$time)

#
data_senti <- subset(data,time>"2009-01-01")
data_senti$group <- "情绪指数"
data_senti$value <- data_senti$value*4000
data_shang$group <- "上证指数"

data_ulti <- rbind(data_senti,data_shang)
data_ulti$group <- as.factor(data_ulti$group)
p <- ggplot(subset(data_ulti,time>"2009-01-01"),aes(time,value,colour=group,group = group))
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧情绪指数走势图")


# p <- ggplot(data_senti,aes(time,value),colour=group,group = group)
# p + geom_line(size=1)+ 
# labs(x = NULL, y = "指数", title ="上证指数 VS 股吧情绪指数走势图")











# data[,group="情绪指数"]
cn_en_name <- data.frame(group=c("上证指数--每天", "股吧热度--每天", "上证指数--每周", "股吧热度--每周" ,"上证指数--每月" ,"股吧热度--每月"),
  en = c("shang_day","guba_day","shang_week","guba_week","shang_month","guba_month"))
data <- left_join(data, cn_en_name, by="en")

p <- ggplot(data,aes(time,value))
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")



p <- ggplot(data,aes(time,value,colour=group,group = group))
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")


+geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
ggsave(filename="stock.guba.month.png", plot=stock.guba.month, width=10, height=6)


 data[,sum(pos_neg=="1"),by = post_date]
 data[,sum(pos_neg=="-1"),by = post_date]
 data[,length(pos_neg),by = post_date]
