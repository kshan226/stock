# 코스피, 코스닥 주가 정보

2021-9-24 주식시장 마감 후 주가 정보
- kospi.csv: kospi 1,658개 종목
- kosdaq.csv: kosdaq 1,511개 종목

| 변수| 설명 |
|---| --- |
|종목명  | |
|현재가  | |
|전일비  | |
|등락률 | 퍼센트 |
|액면가  | |
|시가총액  | 단위: 억 |
|상장주식수  |단위: 1,000 |
|외국인비율  |퍼센트 |
|거래량  |단위: 주 |
|PER  | |
|ROE  | |


```r
# R을 이용한 주가 정보 크롤링 
library(tidyverse)
library(lubridate)
library(rvest) 

# 총 페이지 수
get_last_page <- function(url) {
  html <- read_html(url, encoding="euc-kr")
  
  page_item <- html %>%
    html_nodes(".pgRR") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_split("page=") 
  
  last_page <- page_item[[1]][2]
  return(last_page)
}

# 각 종목의 주식 정보  
get_info <- function(url){
  html <- read_html(url, encoding="euc-kr")
  
  종목명 <- html %>%
    html_nodes(".tltle") %>%
    html_text()
  
  data <- html %>%
    html_nodes(".number") %>%
    html_text() %>%
    parse_number(na="N/A")
  
  data_name <- html %>%
    html_nodes("th") %>%
    html_text()
  
  var_names <- data_name[3:12]
  
  df <- data.frame(matrix(data, ncol = length(var_names), byrow = T))
  names(df) <- var_names
  df$종목명 <- 종목명   

  df <- df %>%
    select(종목명, everything())
  
  return(df)
}

# 한 페이지에 50개 종목, 총 페이지수 구한 후 각 종목의 주식 정보
get_stock_info <- function(base_url) {
  # 첫 페이지에 마지막 페이지 정보가 있음
  last_page <- get_last_page(paste0(base_url, 1))
  
  df <- data.frame()
  for(page in 1:last_page) {
    if(page %% 10 == 0) print(paste(page, "/", last_page))
    df <- rbind(df, get_info(paste0(base_url, page)))
  }
  return(df)
}

kospi_url <- "https://finance.naver.com/sise/sise_market_sum.nhn?&page="
kospi_stock <- get_stock_info(kospi_url)

kosdaq_url <- "https://finance.naver.com/sise/sise_market_sum.nhn?sosok=1&page=" 
kosdaq_stock <- get_stock_info(kosdaq_url)

# 소숫점 두자리까지
kospi_stock$등락률 <- round(kospi_stock$등락률, 2)
kospi_stock$외국인비율 <- round(kospi_stock$외국인비율, 2)
kospi_stock$PER <- round(kospi_stock$PER, 2)
kospi_stock$ROE <- round(kospi_stock$ROE, 2)

# 파일로 저장
write_csv(kospi_stock, "d:/data/kospi.csv")
write_csv(kosdaq_stock, "d:/data/kosdaq.csv")
```
