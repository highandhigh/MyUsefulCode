setwd("D:/ntusd/股吧数据分析")
library(dygraphs)
library(dplyr)
library(data.table)
library(ggplot2)
library(quantmod)
getSymbols('^SSEC',src='yahoo',from = '2015-06-01',to = '2015-11-18')
close <- (Cl(SSEC))
shangzheng <- data.table(time=index(close),value= as.vector(close), group="上证指数")

shoushi <- read.csv("gupiaowatcher1.csv",header=FALSE)
shoushi <- data.table(shoushi)
totalwatcher <- read.csv("totalwatcher.csv",header=FALSE)
totalwatcher <- data.table(totalwatcher)
shoushi <- data.table(left_join(shoushi, totalwatcher, by="V1"))
shoushi[,value:=V2.x/V2.y]
shoushi <- select(shoushi, V1,value)
setnames(shoushi,1,"time")

# holiday <- c('2015-06-22','2015-09-02',as.character(as.Date(0:2,origin= '2015-09-03')),as.character(as.Date(0:6,origin= '2015-10-01')))
holiday <- c('2015-06-22',as.character(as.Date(0:2,origin= '2015-09-03')),as.character(as.Date(0:6,origin= '2015-10-01')))
names(shoushi) <- c("time","value")
# 去除节假日
shoushi <- filter(shoushi,!as.character(time)%chin%holiday)
shoushi[,c("time","value","group"):=list(as.Date(time),value,"收视人数")]
# 同时多增加多列
# 网页图
# require(xts)
# shoushi1 <- filter(shoushi,!weekdays(time)%chin%c("星期六","星期日"))
# stockprices.ts <- xts(shoushi1$value, order.by=as.POSIXct(shoushi1$time))
# dygraph(stockprices.ts)%>%dyRangeSelector()%>%dyLegend(show = "follow")


# 合并上证指数和收视人数
all.data <- rbind(shangzheng,shoushi)
#####两个数据表公共时用   判断是否是星期天
text_week = paste(floor(0:as.numeric(difftime('2015-11-18','2015-06-01'))/7)+1,"周",sep="")
text_date = as.character(as.Date(0:as.numeric(difftime('2015-11-18','2015-06-01')),origin= '2015-06-01'))
date_week <-data.table(time=as.Date(text_date),text_week=text_week)
date_week[,week:=weekdays(time)]
Data_time <- select(filter(date_week,week=="星期三"),text_week,time)
# Data_time <- Data0[,time[3],by=.(text_week)]
# 每周 星期三
#########################
Data0 <- data.table(left_join(date_week, all.data, by="time"))
Data <- filter(Data0, !is.na(value),!week%chin%c("星期六","星期日"))
Data_week <- Data[ ,mean(value),by=.(group,text_week)]
setnames(Data_week,3,c("value"))
# 排序
# arrange(Data_week,group,text_week)
# 每周平均每天收视  -----------------最终数据表
data_week <- left_join(Data_week, Data_time, by="text_week")
#################图1#########################################
mt <- ggplot(data_week, aes(time,value, colour=group)) + geom_line(size=2)
 mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：工作日收视每周内平均每天收视人数\n去除放假时间 09/03-09/04  10/01-10/07")



#################图1#########################################

# stockprices.ts <- xts(data_week$persons, order.by=as.POSIXct(data_week$time))
# dygraph(stockprices.ts)
#####################图2
data_week01 <- copy(data_week)
data_week01[group=="收视人数",value:=value/5]
p <- ggplot(data_week01,aes(time,value,colour=group,group = group))
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")

# geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
# geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
# ggsave(filename="stock.guba.week.png", plot=stock.guba.week, width=10, height=6)
# 画两幅图
p <- ggplot(data =data_week, mapping = aes(time,value))
p <- p + facet_grid(group~., scale="free")
p <- p + layer(data= filter(data_week,group=="上证指数"),  geom = c( "line"), stat = "identity",color="red",size=2)
p <- p + layer(data= filter(data_week,group=="收视人数"), geom = c( "line"), stat = "identity",color="blue",size=2)
p+ labs(x = NULL, title ="财经类节目收视人数 VS 上证指数")
# 画两幅图方法2

mt <- ggplot(data_week, aes(time,value, colour=group)) + geom_line(size=2)
shoushi <-  mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：工作日收视每周内平均每天收视人数")
ggsave(filename="shoushi.png", plot=shoushi, width=10, height=6)


# 去除放假的时间
mt <- ggplot(data_week, aes(time,value, colour=group)) + geom_line(size=2)
shoushi_no_holi <-  mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：工作日收视每周内平均每天收视人数\n去除放假时间 09/03-09/04  10/01-10/07")
ggsave(filename="shoushi_no_holi.png", plot=shoushi_no_holi, width=10, height=6)




# 添加拟合曲线
mt <- ggplot(data_week, aes(time,value, colour=group)) +geom_point(size=2)+ stat_smooth() 
mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：工作日收视每周内平均每天收视人数\n去除放假时间 09/03-09/04  10/01-10/07")


write.table(data_week, "shoushi.txt", row.names = FALSE, col.names = TRUE, sep = " ", quote = F, fileEncoding = "UTF8")
