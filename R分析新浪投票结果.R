#!/usr/bin/env Rscript
# @Author: wuchlong
# @Date:   2015-12-02 14:48:01
# @Last Modified by:   wuchlong
# @Last Modified time: 2015-12-15 09:26:35
##############################################
##readme:
##############################################
setwd("D:/ntusd/股吧数据分析")
source("sourceDir.R")
sourceDir("function")
source("R求移动平均数.R")
source("R求移动累积乘积.R")

library(data.table)
library(reshape2)
library(dplyr)
library(dygraphs)
require(xts)

library(RMySQL)
library(ggplot2)
library(quantmod)
getSymbols('^SSEC',src='yahoo',from = '2009-11-01',to = '2015-12-02')
close <- (Cl(SSEC))
shangzheng <- data.table(time=index(close),value= as.vector(close), group="上证指数")
# write.table(shangzheng, file = '上证指数.txt', row.names = FALSE, col.names = TRUE, sep = " ", quote = F, fileEncoding = "UTF-8",append = TRUE)


vote_data <- fread('vote.txt', encoding='UTF-8',sep='&',header=FALSE)
setnames(vote_data,c('vote_date', 'vote_class', 'vote_per'))
vote_data_up_down <- filter(vote_data,vote_class%chin%c('上涨','下跌','震荡'))
# 注意 dcast and acast 的区别
vote_data_up_down <- data.table(dcast(vote_data_up_down, vote_date~vote_class, value.var="vote_per"))
vote_data_up_down <- filter(vote_data_up_down,as.numeric(gsub('%','',下跌))>0,as.numeric(gsub('%','',上涨))>0,as.numeric(gsub('%','',震荡))>0)
# # write.table(vote_data_up_down, file = '看涨看跌.txt', row.names = FALSE, col.names = TRUE, sep = " ", quote = F, fileEncoding = "UTF-8",append = TRUE)
# # vote_data_up_down[,ratio:= (1/max(0.1,as.numeric(gsub('%','',震荡))/100)-1)*(1+as.numeric(gsub('%','',上涨))/100)/(as.numeric(gsub('%','',下跌))/100+1)]
# vote_data_up_down[,ratio:=log((1/max(0.1,as.numeric(gsub('%','',震荡))/100)-1)*(1+as.numeric(gsub('%','',上涨))/100)/(as.numeric(gsub('%','',下跌))/100+1))]
# # vote_data_up_down[,ratio:=(as.numeric(gsub('%','',上涨))+as.numeric(gsub('%','',下跌)))*as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌))/100]
# vote_data_up_down[,ratio:=as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌))]
# vote_data_up_down[,ratio:=log((as.numeric(gsub('%','',上涨))+as.numeric(gsub('%','',下跌)))/100*as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌)))]
# vote_data_up_down[,ratio:=ratio*(abs(ratio-1)>0.2)]

###### 算法 1 
hist_weight <- function(x,k){
    hist_weight <- NULL
    for(i in 1:length(x)){
        hist_weight[i] <- sum(x[max(i-k,1):i]==x[i])
    }
    hist_weight
}
P_Z_N <- function(v){
    unlist(lapply(v,function(x){
    if(x>0){P_Z_N <- "P"
    }else if(x<0){
        P_Z_N <- "N"
    }else{
        P_Z_N <- "Z"
    }
    }))
}

vote_data_up_down[,ratio:=log(as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌)))]
vote_data_up_down[,ratio:=log((1-1/as.numeric(gsub('%','',震荡)))*as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌)))]
# vote_data_up_down[,ratio:=ifelse(as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌))>0,1,2)*log(as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌)))]
vote_data_up_down[,ratio:=ratio*(abs(ratio-1)>0.3)]
vote_data_up_down[,ratio:=ratio*hist_weight(P_Z_N(ratio),5)]
vote_data_up_down[,ratio0:=hist_weight(P_Z_N(ratio),5)]

#####
# vote_data_up_down[,ratio:=ifelse((as.numeric(gsub('%','',上涨))/as.numeric(gsub('%','',下跌)))>1 ,1,-1)]
# stockprices.ts <- xts(vote_data_up_down[["ratio"]], order.by=as.POSIXct(vote_data_up_down[["vote_date"]]))
# dygraph(stockprices.ts)
# as.numeric(gsub('%','',vote_data_up_down$震荡))

shoushi <- select(vote_data_up_down,vote_date,ratio)
setnames(shoushi,c("time","value"))
shoushi[,c("time","value","group"):=list(as.Date(time),value,"看多指数")]
# 合并上证指数和看多指数
all.data <- rbind(shangzheng,shoushi)

# 添加均值拟合曲线
all.data0 <- copy(all.data)
# all.data0[group=="上证指数",c("value"):=list(movingCumpro(as.numeric(value),5,TRUE))]
# all.data0[group=="看多指数",c("value"):=list(movingCumpro(as.numeric(value),10,FALSE))]
all.data0[group=="看多指数",c("value"):=list(cumsum(as.numeric(value)))]
# all.data0[group=="看多指数",c("value"):=list(cumsum(as.numeric(value)))]
# all.data0 <- copy(all.data)
all.data0[group=="上证指数",c("value"):=list(movingAverage(as.numeric(value),10,FALSE))]
all.data0[group=="看多指数",c("value"):=list(movingAverage(as.numeric(value),10,FALSE))]
####两幅图 
mt0 <- ggplot(all.data0, aes(time,value, colour=group)) + geom_line(size=2)
mt0 + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：")
#####################################


#####两个数据表公共时用   判断是否是星期天
text_week = paste(floor(0:as.numeric(difftime('2015-11-25','2007-06-01'))/7)+1,"周",sep="")
text_date = as.character(as.Date(0:as.numeric(difftime('2015-11-25','2007-06-01')),origin= '2007-06-01'))
date_week <-data.table(time=as.Date(text_date),text_week=text_week)
date_week[,week:=weekdays(time)]
Data_time <- select(filter(date_week,week=="星期三"),text_week,time)
# Data_time <- Data0[,time[3],by=.(text_week)]
# 每周星期三
#########################
Data0 <- data.table(left_join(date_week, all.data, by="time"))
Data <- filter(Data0, !is.na(value),!week%chin%c("星期六","星期日"))
Data_week <- Data[ ,mean(value),by=.(group,text_week)]
# Data_week <- Data[ ,tail(cumprod(value),1),by=.(group,text_week)]
# 累积乘积值
setnames(Data_week,3,c("value"))
# 排序
# arrange(Data_week,group,text_week)
# 每周平均每天收视  -----------------最终数据表
data_week <- left_join(Data_week, Data_time, by="text_week")
#################图1#########################################
mt <- ggplot(data_week, aes(time,value, colour=group)) + geom_line(size=2)
 mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：工作日收视每周内平均每天看多指数\n去除放假时间 09/03-09/04  10/01-10/07")

#################图1####################
# stockprices.ts <- xts(data_week$persons, order.by=as.POSIXct(data_week$time))
# dygraph(stockprices.ts)
#####################图2################
data_week01 <- copy(data_week)
data_week01[group=="看多指数",value:=value/100]
p <- ggplot(data_week01,aes(time,value,colour=group,group = group))
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 博客看多指数")

# geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
# geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
# ggsave(filename="stock.guba.week.png", plot=stock.guba.week, width=10, height=6)
# 画两幅图
p <- ggplot(data =data_week, mapping = aes(time,value))
p <- p + facet_grid(group~., scale="free")
p <- p + layer(data= filter(data_week,group=="上证指数"),  geom = c( "line"), stat = "identity",color="red",size=2)
p <- p + layer(data= filter(data_week,group=="看多指数"), geom = c( "line"), stat = "identity",color="blue",size=2)
p+ labs(x = NULL, title ="财经类节目看多指数 VS 上证指数")
# 画两幅图方法2

mt <- ggplot(data_week, aes(time,value, colour=group)) + geom_line(size=2)
blog_reader <-  
mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：博客只包含徐小明博客")
ggsave(filename="blog_reader.png", plot=blog_reader, width=10, height=6)


# 去除放假的时间
mt <- ggplot(data_week, aes(time,value, colour=group)) + geom_line(size=2)
shoushi_no_holi <-  mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：工作日收视每周内平均每天看多指数\n去除放假时间 09/03-09/04  10/01-10/07")
ggsave(filename="shoushi_no_holi.png", plot=shoushi_no_holi, width=10, height=6)


# 添加拟合曲线
mt <- ggplot(data_week, aes(time,value, colour=group)) +geom_point(size=2)+ stat_smooth() 
mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：")





movingCumpro(as.numeric(all.data0$value),5,FALSE)
# 将阅读量处理后
all.data0[group=="看多指数",value:=value/100]
p <- ggplot(all.data0,aes(time,value,colour=group,group = group))
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 博客看多指数")
# 网页图
all.data01 <- all.data0[group=="看多指数"]
stockprices.ts <- xts(all.data01$value, order.by=as.POSIXct(all.data01$time))
dygraph(stockprices.ts)%>%dyRangeSelector()%>%dyLegend(show = "follow")

all.data01 <- all.data0[group=="上证指数"]
stockprices.ts <- xts(all.data01$value, order.by=as.POSIXct(all.data01$time))
dygraph(stockprices.ts)%>%dyRangeSelector()%>%dyLegend(show = "follow")
####时间滞后 重叠图形
difftime('2015-08-31','2015-06-05')
all.data0 <- copy(all.data)
all.data1 <- all.data[group=="上证指数"]
all.data1[,c("group","time"):=list("上证指数滞后87天",time+87)]
all.data0 <- rbind(all.data0,all.data1)
all.data0[group=="上证指数",c("value"):=list(movingAverage(as.numeric(value),10,TRUE))]
all.data0[group=="看多指数",c("value"):=list(movingAverage(as.numeric(value),10,TRUE))]
all.data0[group=="上证指数滞后87天",c("value"):=list(movingAverage(as.numeric(value),10,TRUE))]

all.data0[group=="看多指数",value:=value/100]

mt0 <- ggplot(all.data0, aes(time,value, colour=group)) + geom_line(size=2)
mt0 + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：博客只包含徐小明博客")

p <- ggplot(all.data0[group%chin%c("上证指数滞后87天","看多指数")],aes(time,value,colour=group,group = group))
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 博客看多指数\n 上证指数平移87天\n各指数10日平均曲线")

