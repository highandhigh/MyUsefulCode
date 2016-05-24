library(rvest)
library("RCurl")
library(XML)
url <- "http://vote.eastmoney.com/vote_look1.asp"
a <- url%>%html_session(encoding="gb2312")

# %>%html_nodes("td")%>%html_table()
# html_text()
# %>%html_nodes("tr")
# html_table()
# temp <- getURL(url)
# temp<-iconv(temp,to="gb2312") #转码
# Encoding(temp) #UTF-8
# k<-htmlParse(temp)#选择UTF-8进行网页的解析
# text_data <- getNodeSet(k,'/tr')
# xmlValue(text_data[[1]])  ###提取正文 

# grep("td",a[[6]])

a <- getURL(url)

# a=iconv(a[[6]][6],to="gb2312") 
b= gregexpr(">[0-9]+<",a)
b <- b[[1]]
c = gsub(">|<","",substring(a, b, b + attr(b, "match.length")-1))[c(2,4,6)]
data.frame(投票时间 = Sys.Date(),趋势=c("上涨","整盘","下跌"), 投票结果=c)
iconv(a[[6]][[6]],to="gb2312")
iconv(a[[6]][[6]],to="UTF-8")