library("RCurl")
# cookie = 'yanbao.txt'   
# curl  =  getCurlHandle ( cookiefile = cookie,
#                          cookiejar = cookie,
#                          useragent = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6"
# )

# # code to do something with web page
# rm(curl)
# gc()
# temp<-getURL("http://www.microbell.com/toplogin.asp",curl=curl)
#  grep("password",temp)


# site <- "http://www.microbell.com/toplogin.asp"
# cookie_1= "yanbao.txt"

# # Should work!
# h = getCurlHandle(cookiejar = "-", cookiefile="yanbao.txt")
# a = postForm(site, par = "yanbao.txt", curl = h, style = "POST")


# myHttpheader<-c(
# "User-Agent" = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)",
# "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
# "Accept-Encoding" = "gzip, deflate, sdch",
# "Accept-Language"= "zh-CN,zh;q=0.8,en;q=0.6",
# "Cache-Control"="max-age=0",
# "Pragma"="no-cache",
# "Connection" = "keep-alive"
#   )
# d2 =debugGatherer()
# cHandle2<- getCurlHandle(httpheader=myHttpheader,followlocation=1,
# debugfunction=d2$update,verbose=TRUE,cookiefile="yanbao.txt")
# # 接着去cos.name的R论坛看看：

# temp<- getURL("http://www.microbell.com/toplogin.asp",
# curl=cHandle2)

# # 验证一下temp里面是不是已经有你的大名了呢？
# grep("yourname",temp)
# readLines("yanbao.txt")
# ##############################################################
# library(RCurl)
# #Set your browsing links 
# loginurl = "http://www.microbell.com/toplogin.asp"
# dataurl  = "http://www.microbell.com/toplogin.asp"
# #Set user account data and agent
# pars=list(
#      'name' = "saiwaiyanyu",
#     'pwd' = "919812"
# )
# agent="Mozilla/5.0" #or whatever 
# #Set RCurl pars
# curl = getCurlHandle()
# curlSetOpt(cookiejar="yanbao.txt",  useragent = agent, followlocation = TRUE, curl=curl)
# #Also if you do not need to read the cookies. 
# #curlSetOpt(  cookiejar="", useragent = agent, followlocation = TRUE, curl=curl)
# #Post login form
# html=postForm(loginurl, .params = pars, curl=curl)
# #Go wherever you want
# html=getURL(dataurl, curl=curl)
# #Start parsing your page
# grep("login", html)
# #... .... ...
# rm(curl)
# gc()
# #############################################################
# cookie = 'yanbao.txt'   
# curl  =  getCurlHandle ( cookiefile = cookie,
#                          cookiejar = cookie,
#                          useragent = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6"
# )
# html=postForm(loginurl, .params = pars, curl=curl)
# #Go wherever you want
# html=getURL(dataurl, curl=curl)
# #Start parsing your page
# grep("login", html)
# ##############################################
# library(RCurl) #调用curl
# curl <- getCurlHandle() #虚拟一个浏览器
# curlSetOpt(cookiejar=tempfile(), curl=curl) #生成cookie
# getURL("http://www.microbell.com/toplogin.asp", curl=curl) #填充cookie
# response <- postForm("http://www.microbell.com/toplogin.asp", style="HTTPPOST", data=data, file=fileUpload(filename="tobeuploadedfile.txt", contentType="text/plain"), curl=curl) 
# #提交表单，表单对应的网址写在第一个参数中，具体帮助请使用?postForm查看。


# myHttpheader<- c(
# "User-Agent"="Mozilla/5.0(Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6)",
# "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
# "Accept-Language"="en-us",
# "Connection"="keep-alive",
# "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
# )
# d =debugGatherer()
# temp<- getURL('http://cos.name/cn/',httpheader=myHttpheader,debugfunction=d$update,verbose= TRUE)


# cHandle<- getCurlHandle(httpheader = myHttpheader)
# 在getURL中可以如下应用：
# d =debugGatherer()
# temp <- getURL('http://cos.name/cn/',.opts = list(debugfunction=d$update,verbose = TRUE), curl=cHandle)
# myPost<- function(x){
#  post <-scan(x,what="character",quiet=TRUE,sep="\n")
#  abcd=strsplit(post,"&")[[1]]
#  abc=gsub("(^.*)(=)(.*$)","\\3",abcd)
#  abcnames=gsub("(^.*)(=)(.*$)","\\1",abcd)
#  names(abc)=abcnames
#  return(abc)
# }
# postinfo<- myPost("clipboard")
# temp<- postForm('http://cos.name/cn/',.params=postinfo,
#  .opts=list(cookiefile=""),curl=cHandle,style="post")
# getCurlInfo(cHandle)[["cookielist"]]
# temp<- getURL('http://cos.name/cn/',curl=cHandle,.encoding="gbk")



# #设置背包
# d = debugGatherer()
# cHandle2 <- getCurlHandle(httpheader=myHttpheader,followlocation=1,
#                           debugfunction=d$update,verbose=TRUE)
# #若无意外此时手工拼：
# temp<-getURL(url,curl=cHandle2)
# #会返回对应搜索页html。
# url <- "http://www.microbell.com/result.asp?lm=0&area=DocTitle&timess=13&key=&page=1"
# a=url%>%html_session%>%html_nodes("td") %>%html_text()%>%.[1:10]
# repair_encoding(a)


# wp<-getURL(url,.encoding="gb2312") #用网页本身的编码
# wp2=iconv(wp,"gb2312","UTF-8") #转码
# Encoding(wp2) #UTF-8
# doc <- htmlParse(wp2,asText=T,encoding="UTF-8") #选择UTF-8进行网页的解析
# #查看doc的内容时显示有乱码，但没关系，table的解析结果没有乱码
# tables <-readHTMLTable(doc,header=F)


# getNodeSet(doc,'//div[@class="indexSidebarLeftnew"]')


# url="http://www.microbell.com/result.asp?lm=0&area=DocTitle&timess=13&key=&page=1"


# library("RCurl")
# myHttpheader<- c(
# "Accept" = "text/html, application/xhtml+xml, */*",
# "Accept-Language" ="zh-CN",
# "User-Agent" = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)",
# "Accept-Encoding"= "gzip, deflate",
# "Connection" = "Keep-Alive"
# )

# d = debugGatherer()
# cat(d$value()[2])
# cHandle <- getCurlHandle(httpheader=myHttpheader,followlocation=1,
#              debugfunction=d$update,verbose=TRUE,
#              cookiefile="yanbao.txt")
# temp <- getURL("http://www.microbell.com/result.asp?lm=0&area=DocTitle&timess=13&key=&page=1",
# curl=cHandle,.encoding="gb2312")
# grep("tinydust",temp)
# grep("登录",temp)


# # library(RCurl)
# # curl = getCurlHandle()
# # curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
# # html <- getURL('http://simba.isr.umich.edu/u/Login.aspx', curl = curl)

# # viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
# # params <- list(
# #     'ctl00$ContentPlaceHolder3$Login1$UserName'    = '<USERNAME>',
# #     'ctl00$ContentPlaceHolder3$Login1$Password'    = '<PASSWORD>',
# #     'ctl00$ContentPlaceHolder3$Login1$LoginButton' = 'Log In',
# #     '__VIEWSTATE'                                  = viewstate
# #     )
# # html = postForm('http://simba.isr.umich.edu/u/Login.aspx', .params = params, curl = curl)
# # grepl('Logout', html)

# library(RCurl)
# curl = getCurlHandle()
# curlSetOpt(cookiejar = 'yanbao.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
# html <- getURL("http://www.microbell.com/toplogin.asp", curl = curl)

# viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
# params <- list(
#    'name' = "saiwaiyanyu",
#     'pwd' = "919812"
#     )
# html = postForm("http://www.microbell.com/toplogin.asp", .params = params, curl = curl)

# html = getURL("http://www.microbell.com/toplogin.asp", curl=curl)

# grepl('退出登陆', html)
# ########################################################

library(RCurl)
library(rvest)
library(XML)
myHttpheader<- c(
"User-Agent"="Mozilla/5.0(Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6)",
"Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
"Accept-Language"="en-us",
"Connection"="keep-alive",
"Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)


d2 =debugGatherer()
cHandle2<- getCurlHandle(httpheader=myHttpheader,followlocation=1,
 debugfunction=d2$update,verbose=TRUE,
 cookiefile="cookies.txt")

temp<- getURL('www.microbell.com/toplogin.asp',curl=cHandle2,.encoding="GB2312")
cat(temp)
grep("saiwaiyanyu",temp)
cat(getURL('www.microbell.com/toplogin.asp'))
# url <- "http://www.microbell.com/docdetail_1708149.html"
##主页面
url<- "http://www.microbell.com/result.asp?lm=0&area=DocTitle&timess=36&key=&dtype=&page=20000"
# url <- "http://www.microbell.com/result.asp?lm=0&area=DocTitle&timess=12&key=&dtype=&page=2"
temp <- getURL(url,curl=cHandle2,.encoding="GB2312")
grep("股票名称",temp)
grep("下一页",temp)
cat(temp)


temp1<-iconv(temp,"gb2312","UTF-8") #转码
Encoding(temp1) #UTF-8
k<-htmlParse(temp1,asText=T,encoding="UTF-8")#选择UTF-8进行网页的解析
#查看doc的内容时显示有乱码，但没关系，table的解析结果没有乱码
tables<-readHTMLTable(k,header=F)
tables
getNodeSet(k,'//div[@class="classbaogao_sousuo_new"]//a')
###########################################################################################
###子页面
url2 <- "http://www.microbell.com/docdetail_620601.html" ##此链接 出现错误
url2 <- "http://www.microbell.com/docdetail_620599.html"
temp <- getURL(url2,curl=cHandle2,.encoding="GB2312")

temp<-iconv(temp,"gb2312","UTF-8") #转码
Encoding(temp) #UTF-8
k<-htmlParse(temp,asText=T,encoding="UTF-8")#选择UTF-8进行网页的解析
text_data <- getNodeSet(k,'//div[@class="p_main"]//p/font')
xmlValue(text_data[[1]])  ###提取正文 



# d3 =debugGatherer()
# cHandle3<- getCurlHandle(httpheader=myHttpheader,followlocation=1,
#  debugfunction=d3$update,verbose=TRUE,
#  cookiefile="zhuyecookies.txt")

# url <-"http://www.microbell.com/result.asp?lm=0&area=DocTitle&timess=12&key=&dtype=&page=2"
# temp<- getURL(url ,curl=cHandle3,.encoding="GB2312")
# cat(temp)
# grep("saiwaiyanyu",temp)
# cat(getURL(url,curl=cHandle3,.encoding="GB2312"))


# a <- url %>% html_session()%>%html_nodes("div.classbaogao_sousuonew")
# repair_encoding(a)


