library(jsonlite)
library(xml2)
library(httr)
library(RCurl)
library(rvest)
options(stringsAsFactors = F)
source("./html_table_fixed.R") #course provide this function

# Crawl history data ------------------------------------------------------
# 從行政院農產品批發市場交易行情站 ( http://amis.afa.gov.tw/main/Main.aspx ) 抓原始蔬菜交易價錢與交易量

for (year in 100:101){
  for (month in 1:12){
    if (month < 10){ month <- sprintf('0%s',month)}
    
    else{ month <- sprintf('%s',month)}
    
#    if (year ==106 & month >=6){    
#      print ('106 finish')
#      break
#    }
    for (day in seq(1,26,5)){
      tryCatch({
        if (day < 10 ){ s_day <- sprintf('0%s',day)}
        else{ s_day <- day}

        #Setting Search range

        start_date <- sprintf('%s/%s/%s',year,month,s_day)

        if (day+4 < 10 ){ e_day <- sprintf('0%s',day+4)}
        else if (month %in% c('01','03','05','07','08','10','12') & day+4 == 30) {e_day <- day+5}
        else if (year+1911 != 2012 & month == '02' & day+4 == 30) {e_day <- day+2}
        else if (year+1911 == 2012 & month == '02' & day+4 == 30) {e_day <- day+3}
        else{ e_day <- day+4 }
        end_date <- sprintf('%s/%s/%s',year,month,e_day)
        print(start_date)
        print(end_date)

        url <- "http://amis.afa.gov.tw/veg/VegProdDayTransInfo.aspx"
        curl <- getCurlHandle()
        curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
        html <- getURL(url, curl = curl )
        viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
        viewstategenerator <- as.character(sub('.*id="__VIEWSTATEGENERATOR" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
        eventvalidation <- as.character(sub('.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
        # write(html, "./Final Project/no_parse.html")  #check whether it is original page or not

        params <- list(
          'ctl00$ScriptManager_Master' = 'ctl00$ScriptManager_Master|ctl00$contentPlaceHolder$btnQuery',
          'ctl00$contentPlaceHolder$ucDateScope$rblDateScope' = 'P',
          'ctl00$contentPlaceHolder$ucSolarLunar$radlSolarLunar'= 'S',
          'ctl00$contentPlaceHolder$txtSTransDate' = start_date,
          'ctl00$contentPlaceHolder$txtETransDate' = end_date,
          'ctl00$contentPlaceHolder$txtMarket' = '全部市場',
          'ctl00$contentPlaceHolder$hfldMarketNo' = 'ALL',
          'ctl00$contentPlaceHolder$txtProduct' = '全部產品',
          'ctl00$contentPlaceHolder$hfldProductNo' = 'ALL',
          'ctl00$contentPlaceHolder$hfldProductType' = 'A',
          '__EVENTTARGET' = '',
          '__EVENTARGUMENT' = '',
          '__VIEWSTATE' = viewstate,
          '__VIEWSTATEGENERATOR' = viewstategenerator,
          '__EVENTVALIDATION' = eventvalidation,
          '__ASYNCPOST' = 'true',
          'ctl00$contentPlaceHolder$btnQuery' = '查詢')

        #pseudo browser

        version = 'Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17'
        res <- POST(url, body = params , encode = "form" , add_headers("User-Agent" = version)) #use POST to get (consume time)

        #convert html table to dataframe(.RData)

        doc.str <- content(res, "text")  # convert our result to string
        doc <- read_html(doc.str)  # convert string to xml_nodes
        #write(doc.str, "./Final Project/test2.html")  # for testing , output an large form in html (waste time)

        source("../week10/html_table_fixed.R") #course provide this function

        tables <- html_nodes(doc, "table")
        res_table <- data.frame(html_table(tables[3], fill = TRUE)) #select useful table
        colnames(res_table) <- res_table[1,] #define columns names
        res_table <- res_table[-c(1,2),]  #delete unuseful rows

        filename <- sprintf('Veg_trade_data/%s%s%s_%s%s%s.RData',year,month,s_day,year,month,e_day)
        save(res_table,file = filename) #save RData
        Sys.sleep(120)
      },error = function(err) {
        print(paste0("Error at", Sys.time()))
        })
      
    }
  }
}





