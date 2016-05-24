library(rvest)
url <-"http://blog.sina.com.cn/s/articlelist_1216826604_0_1.html"
a_session= url%>%html_session()
a = a_session%>%html_nodes("div.articleList")
repair_encoding(a%>%html_text())
# a%>%html_attr("href")



# 分别获取阅读发表时间。博客标题。博客地址
a%>%html_nodes("p.atc_info span.atc_tm.SG_txtc")%>%html_text()
urls <- a%>%html_nodes("span.atc_title a")%>%html_attr("href")
repair_encoding(a%>%html_nodes("span.atc_title a")%>%html_text())

url_i <- urls[2]
a_i= url_i%>%html_session()
# 爬取正文 
repair_encoding(a_i%>%html_nodes(".articalContent p")%>%html_text())
a_session%>%html_nodes("ul.SG_pages span")%>%html_text()%>%gsub("共|页","",.)%>%as.numeric

