# 获取股吧数据的历史发帖数量    按天统计
library(RMySQL)
library(dplyr)
library(ggplot2)
con <- dbConnect(MySQL(),user='root', password='tcl_bigdata@321',host='192.168.1.142',dbname='gushi',port=3306)
dbSendQuery(con,'set names gbk')
SQLQuery <- "SELECT *   FROM guba_redu;"
res  <- dbSendQuery(con, SQLQuery)
##### 读取数据并解决乱码问题 
data <- fetch(res,-1) 
dbDisconnect(con)
data$time <- as.Date(data$time)
data <- tbl_df(data)

data <- filter(post_data,post_num < 30000)
#  获取上证指数
library(quantmod)
getSymbols('^SSEC',src='yahoo',from = '1999-06-07',to = '2015-11-08')
close <- (Cl(SSEC))
time1 <- index(close)
value1 <- as.vector(close)
yrng <- range(value1)
xrng <- range(time1)
data_day <- rbind(data.frame(group = "上证指数--每天",time= time1,value= value1),
	data.frame(group = "股吧热度--每天",time= as.Date(post_data$post_date[post_data$post_date%chin%as.character(time1)]),
		value=post_data$post_num[post_data$post_date%chin%as.character(time1)]))

##统计每周平均热度
data_week <- rbind(data.frame(group = "上证指数--每周",time= time1,value= value1),
	data.frame(group = "股吧热度--每周",time= as.Date(post_data$post_date[post_data$post_date%chin%as.character(time1)]),
		value=post_data$post_num[post_data$post_date%chin%as.character(time1)]))
text_week= paste(floor(0:as.numeric(difftime('2015-11-08', '1999-06-07'))/7)+1,"周",sep="")
text_date=as.character(as.Date(0:as.numeric(difftime('2015-11-08','1999-06-07')),origin= '1999-06-07'))
date_week <- data.frame(time=as.Date(text_date),text_week=text_week)
date_week <- tbl_df(date_week)
Data <- data.table(left_join(data_week, date_week, by="time"))
Data_week <- Data[,mean(value),by=.(group ,text_week)]
Data_time <- Data[,tail(time,1),by=.(group ,text_week)]
Data_time <- select(Data_time,text_week,V1)
data_week <- left_join(Data_week, unique(Data_time), by="text_week")
names(data_week)=c("group","text_week","value","time" )


##统计每月平均热度
data_month <- rbind(data.frame(group = "上证指数--每月",time= time1,value= value1),
	data.frame(group = "股吧热度--每月",time= as.Date(post_data$post_date[post_data$post_date%chin%as.character(time1)]),
		value=post_data$post_num[post_data$post_date%chin%as.character(time1)]))
data_month <- data.table(data_month)
data_month[,Data_month := substr( time,1,7)]

Data_month <- data_month[,mean(value),by=.(group ,Data_month)]
Data_time <- data_month[,tail(time,1),by=.(group ,Data_month)]
Data_time <- select(Data_time, Data_month,V1)
data_month <- left_join(Data_month, unique(Data_time), by="Data_month")
names(data_month)=c("group","text_week","value","time" )

data <- rbind(data_day,select(data_week,group,time, value),select(data_month,group,time, value))



timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-09-16','2010-03-31','2015-06-15'))
events <- c('证券法实施','国有股减持','股权分置改革','次贷危机全面爆发','融资融券试点','股市大跌')
data2 <- data.frame(group = "上证指数",timepoint,events,stock=value1[time1 %in% timepoint])


data_plot <- filter(data, group%in%c("上证指数--每天","股吧热度--每天") )
p <- ggplot(data_plot,aes(time,value,colour=group,group = group))
stock.guba.day <- p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
ggsave(filename="stock.guba.day.png", plot=stock.guba.day, width=10, height=6)

data_plot <- filter(data, group%in%c("上证指数--每周","股吧热度--每周") )
p <- ggplot(data_plot,aes(time,value,colour=group,group = group))
stock.guba.week <- p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
ggsave(filename="stock.guba.week.png", plot=stock.guba.week, width=10, height=6)


data_plot <- filter(data, group%in%c("上证指数--每月","股吧热度--每月") )
p <- ggplot(data_plot,aes(time,value,colour=group,group = group))
stock.guba.month <- p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
ggsave(filename="stock.guba.month.png", plot=stock.guba.month, width=10, height=6)




# 插入到数据库
con <- dbConnect(MySQL(),user='root',password='tcl_bigdata@321',host='192.168.1.142',dbname='gushi',port=3306)
dbSendQuery(con,'SET NAMES utf8')
dbSendQuery(con,'delete from guba_redu;')
dbWriteTable(con,"guba_redu", value=select(data,en,time,value),overwrite=TRUE,row.names=FALSE)
dbDisconnect(con)


# 从数据库读取数据
con <- dbConnect(MySQL(),user='root', password='tcl_bigdata@321',host='192.168.1.142',dbname='gushi',port=3306)
dbSendQuery(con,'set names gbk')
SQLQuery <- "SELECT *   FROM guba_redu;"
res  <- dbSendQuery(con, SQLQuery)
##### 读取数据并解决乱码问题 
data <- fetch(res,-1) 
dbDisconnect(con)
data$time <- as.Date(data$time)
data <- tbl_df(data)
cn_en_name <- data.frame(group=c("上证指数--每天", "股吧热度--每天", "上证指数--每周", "股吧热度--每周" ,"上证指数--每月" ,"股吧热度--每月"),
  en = c("shang_day","guba_day","shang_week","guba_week","shang_month","guba_month"))
data <- left_join(data, cn_en_name, by="en")


data <- left_join(data, cn_en_name, by="en")
data_plot <- filter(data, group%in%c("上证指数--每月","股吧热度--每月") )
p <- ggplot(data_plot,aes(time,value,colour=group,group = group))
stock.guba.month <- p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
ggsave(filename="stock.guba.month.png", plot=stock.guba.month, width=10, height=6)


