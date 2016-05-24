library(rvest)
library(XML)
library(RCurl)
library(httr)
library(rvest)
library(magrittr)
# library(reshape)
url <- html("http://www.microbell.com/docdetail_1706366.html")
a=url%>%html_nodes("div.leftn2  td") %>%html_text()%>%.[1:10]
repair_encoding(a)




url <-html_session("http://www.microbell.com/toplogin.asp")
a=url%>%html_nodes("td")%>%extract2(1) %>% html_form() 

repair_encoding(a)




united <- html_session("https://www.united.com/ual/en/cn/?root=1")
account <- united %>% follow_link("Account")

login <- account %>%html_nodes("form") %>%extract2(2) 
%>%
  html_form() %>%
   set_values(
    `ctl00$ContentInfo$SignIn$onepass$txtField` = "GY797363",
    `ctl00$ContentInfo$SignIn$password$txtPassword` = password
  )
account <- account %>%
  submit_form(login, "ctl00$ContentInfo$SignInSecure")

logged_in %>% jump_to(gsub(" ", "+", headers(logged_in)$Location))

my_url = "https://www.openair.com/index.pl"
openair <- html_session(my_url)
login <-  html_form(openair) %>%extract2(1) 


my_url = "http://www.microbell.com/toplogin.asp"
openair <- html_session(my_url)
login <-  html_form(openair) %>%extract2(1) %>%set_values(
	'tijiao' = "",
   'name' = "saiwaiyanyu",
    'pwd' = "919812"
  )
login$url <- "http://www.microbell.com/toplogin.asp"
account <- openair %>%submit_form(login)



url <- "http://www.microbell.com/toplogin.asp"
pgsession <-html_session(url)               ## create session
pgform   <-html_form(pgsession)[[1]]       ## pull form from session
# Note the new variable assignment 
filled_form <- set_values(pgform,
  'name' = "saiwaiyanyu",
    'pwd' = "919812")
filled_form$url <- "http://www.microbell.com/toplogin.asp"
submit_form(pgsession,filled_form,submit=NULL)

pgsession$url
filled_form$url

submit_request(pgsession,filled_form)

form = filled_form
submit_request(form)
session =pgsession
 request <- submit_request(form, submit)
    url <- XML::getRelativeURL(session$url, form$url)
    if (request$method == "GET") {
        request_GET(session, url = url, params = request$values, 
            ...)
    }
    else if (request$method == "POST") {
        request_POST(session, url = url, body = request$values, 
            encode = request$encode, ...)
    }
    else {
        stop("Unknown method: ", request$method, call. = FALSE)
    }



header=c(
 
"User-Agent"="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:20.0) Gecko/20100101 Firefox/20.0",
"Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
"Accept-Language"="zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3",
"Accept-Encoding"= "gzip, deflate",
"Content-Type"="text/html;charset=UTF-8",
"Transfer-Encoding"="chunked",
"Connection"="keep-alive",
"Cache-Control"="no-cache",
"Pragma"="no-cache",
"Content-Encoding"="gzip")

header=c(
"User-Agent" = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)",
"Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
"Accept-Encoding" = "gzip, deflate, sdch",
"Accept-Language"= "zh-CN,zh;q=0.8,en;q=0.6",
"Cache-Control"="max-age=0",
"Pragma"="no-cache",
"Connection" = "keep-alive"
  )


temp=getURL("http://www.microbell.com/toplogin.asp",httpheader=header)