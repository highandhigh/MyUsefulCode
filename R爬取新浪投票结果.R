#!/usr/bin/env Rscript
# @Author: wuchlong
# @Date:   2015-12-02 13:39:42
# @Last Modified by:   wuchlong
# @Last Modified time: 2015-12-04 09:13:55
##############################################
##readme:
##############################################
library(rvest)
vote_dates <- as.character(as.Date(seq(as.POSIXlt("2009-11-04"), as.POSIXlt("2015-12-01"), "1 day")))
for(vote_date in vote_dates){
    tryCatch({
        url <- paste('http://survey.news.sina.com.cn/static/20205/',gsub('-','',vote_date),'.html',sep='')
        vote <- url%>%html_session(encoding="UTF-8")%>%html_nodes('div.wrap #S_Cont_10 div.vote')#%>%.[1]
        vote_class <- repair_encoding(vote%>%html_nodes('span.gd')%>%html_text())%>%gsub('[\r\n ]','',.)
        vote_percent <- vote%>%html_nodes('span.precent-num')%>%html_text()
        vote_results <- data.frame(vote_date = vote_date, vote_class = vote_class,vote_percent = vote_percent )
        # print(vote_results)
        write.table(vote_results, file = 'vote.txt', row.names = FALSE, col.names = FALSE, sep = "&", quote = F, fileEncoding = "UTF-8",append = TRUE)
        }, error=function(e){})
}


# url <- 'http://survey.news.sina.com.cn/static/20205/20091002.html'

