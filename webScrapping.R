

install.packages("XML2")
install.packages("rvest")
library(rvest)
library(XML)
library(xml2)
library(RCurl)


url <- "https://www.koreabaseball.com/Record/Team/Hitter/Basic1.aspx"

nv  <- read_html(url)

class(nv)
readHTMLTable(nv)

html_table(nv)


# -----------------------------------------------------------------------------------------------------------
# KBO 사이트에서 데이터 추출
install.packages('XML')
library(XML)
install.packages('httr')
library(httr)
url <- "https://www.koreabaseball.com/Record/Team/Hitter/Basic1.aspx"
get_url <- GET(url)
help(GET)
# class(rawToChar(get_url$content))
# help("readHTMLTable")

# html_cont <- readHTMLTable(rawToChar(get_url$content))
# names(html_cont)
# View(html_cont)


# HTML 테그중 테이블만 읽어 온다.
html_cont <- readHTMLTable(rawToChar(get_url$content),stringsAsFactors=F)
html_cont <- readHTMLTable(rawToChar(get_url$content),stringsAsFactors=F, fileEncoding = "EUC-KR")
html_cont <- readHTMLTable(rawToChar(get_url$content),stringsAsFactors=F, fileEncoding = "UTF-8")
html_cont
# stringsAsFactors = F -> 문자는 그대로 읽고 factor로 처리하지 말라

help(readHTMLTable)

html_cont

