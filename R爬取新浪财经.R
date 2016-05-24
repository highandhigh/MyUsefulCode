url <-"http://money.finance.sina.com.cn/corp/view/vCB_AllNewsStock.php?symbol=sh600418&Page=1"
a= url%>%html_session()%>%html_nodes("div.datelist a")
repair_encoding(a%>%html_text())
a%>%html_attr("href")










