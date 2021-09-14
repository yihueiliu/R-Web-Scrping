library(xml2)
library(httr)
library(jsonlite)
library(plyr) # for rbind.fill



# get data ----------------------------------------------------------------


# 1. 獨立套房
url1 <- "https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=2&searchtype=1&region=1"
res1 <- fromJSON(url1)
res1
res1$data$data
# 測試這真的是我們要的資料

# Get the number of records
res1$records #總共有多少筆資料
totalRows <- as.numeric(gsub(",", "",res1$records))  #先把逗號取代掉，然後轉成數字


# get data
alldf1 <- data.frame()

for(i in 0:(totalRows/30)){   #因為每30筆一頁。另外(totalRaws/30)括號要有，不然他會先算0:totalRaws，再除以30，這樣會有錯誤。
  pre_url <- url1
  tail <- sprintf("&firstRow=%s&totalRows=%s", i*30, totalRows)
  url <- paste0(pre_url, tail)
  
  res1 <- fromJSON(url)
  alldf1 <- rbind.fill(alldf1, res1$data$data)
  print(url)
} 

head(alldf1$kind_name)

# 2. 分租套房
url2 <- "https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=3&searchtype=1&region=1"
res2 <- fromJSON(url2)

totalRows <- as.numeric(gsub(",", "",res2$records)) 

# get data
alldf2 <- data.frame()

for(i in 0:(totalRows/30)){   #因為每30筆一頁。另外(totalRaws/30)括號要有，不然他會先算0:totalRaws，再除以30，這樣會有錯誤。
  pre_url <- "https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=3&searchtype=1&region=1"
  tail <- sprintf("&firstRow=%s&totalRows=%s", i*30, totalRows)
  url <- paste0(pre_url, tail)
  
  res2 <- fromJSON(url)
  alldf2 <- rbind.fill(alldf2, res2$data$data)
  print(url)
} 

head(alldf2$kind_name)

# 3. 雅房
url3 <- "https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=4&searchtype=1&region=1"
res3 <- fromJSON(url3)

totalRows <- as.numeric(gsub(",", "",res3$records)) 

# get data
alldf3 <- data.frame()

for(i in 0:(totalRows/30)){   #因為每30筆一頁。另外(totalRaws/30)括號要有，不然他會先算0:totalRaws，再除以30，這樣會有錯誤。
  pre_url <- "https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=4&searchtype=1&region=1"
  tail <- sprintf("&firstRow=%s&totalRows=%s", i*30, totalRows)
  url <- paste0(pre_url, tail)
  
  res3 <- fromJSON(url)
  alldf3 <- rbind.fill(alldf3, res3$data$data)
  print(url)
} 

head(alldf3$kind_name)



# 行政區、租金、坪數 ---------------------------------------------------------------

# 行政區
alldf1$section_name

# 租金
alldf1$price

# 坪數
alldf1$area


# 判斷是否為頂加 -----------------------------------------------------------------

cover1 <- alldf1[grepl("頂樓加蓋", alldf1$floorInfo), ]
notcover1 <- alldf1[!grepl("頂樓加蓋", alldf1$floorInfo), ]

cover2 <- alldf2[grepl("頂樓加蓋", alldf2$floorInfo), ]
notcover2 <- alldf2[!grepl("頂樓加蓋", alldf2$floorInfo), ]

cover3 <- alldf3[grepl("頂樓加蓋", alldf3$floorInfo), ]
notcover3 <- alldf3[!grepl("頂樓加蓋", alldf3$floorInfo), ]
