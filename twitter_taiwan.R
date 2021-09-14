# 使用tweet API或twitterR套件，嘗試抓取不同的資料並進行文字分析或進行計數，分析哪些人在談論台灣的相關消息。


# Pre-setting -------------------------------------------------------------

library(twitteR)
library(data.table)
options(stringsAsFactors = F)


# twitter API set ---------------------------------------------------------

consumer_key <- '3mlVSnmPBHWueYBDIvE4H7wjf'
consumer_secret <- 'vjEFVXpoueIEkSOagFYK12goDIIvONC4Y96rF7hcZGiDs8b65W'
access_token <- '1271704170-QOiHE4IBsdaINErgNA0MCyT1fhCgTDfIMQvpSbn'
access_secret <- 'ulZyd9WbJZou4jk2DI0vfJ50LfxQ3U28WOfDmTHycZ1RX'
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


# 尋找台灣相關的消息 ---------------------------------------------------------------

# 搜尋包含「#taiwan」的文。（先設n=500以免資料過大要跑很久）
tweets <- searchTwitter('#taiwan', n=500)   
# 將搜尋結果的list轉為dataframe
tweets_list <- twListToDF(tweets)           
# 找出使用「#taiwan」的使用者名稱，並轉為dataframe
tweets_name_list <-  rbindlist(lapply(tweets_list$screenName ,as.data.frame))  
# CHANGE THE COLUMNS NAME
names(tweets_name_list)[1] <- "Name"        


# 找到「#taiwan」使用者的位置 -----------------------------------------------------

# 先創造一個空的dataframe，準備之後來放使用者名稱、位置
all_location = data.frame()

# 用迴圈來抓每個使用者的位置
for (i in 1:500){ 
  tryCatch(   # 為了避免warning或error而使得其他正常部分無法跑，使用「tryCatch」
    {
      # get name from '#taiwan' users list
      tagger <- tweets_name_list$Name[i]
      # print query location，好確認真的跑了500次。
      print(paste(i, tagger))
      # get User's twitter account
      get_tagger_account <- getUser(tagger)
      # 得到這些使用者的位置
      tagger_location <- get_tagger_account$getLocation()
      # 將使用者名稱轉為dataframe
      tagger_df <- data.frame(tagger)
      # 將使用者之位置轉為dataframe
      location_df <- data.frame(tagger_location)
      # 將使用者名稱、位置用cbind結合起來
      tagger_location_df <- cbind(tagger_df, location_df)
      # 用all_location來收集所有的使用者名稱及位置
      all_location <- rbind(all_location, tagger_location_df)
    },
    # 如果出現warning的話怎麼辦
    warning = function(w){      #空的代表不做任何事
    },
    # 如果出現error的話怎麼辦
    error = function(e){
      #ERROR (need to store it?)
      #print(paste("ERROR", tagger))
    },
    # 迴圈每跑一次後要幹麻
    finally = {
      # print("End Try&Catch")
    })
  
  i = i+1
}


# 分析「#taiwan」使用者的位置 ------------------------------------------

# 算出各個位置出現的頻率
freq <- table(unlist(all_location$tagger_location))
length(freq)
# 將各地出現的頻率由大排到小
freq <- freq[order(freq, decreasing=T)]
# 算出來的結果可以發現沒有顯示地區的使用者最多，排在第一名，所以把第一筆資料去掉。
freq <- freq[-1]


# 視覺化 ---------------------------------------------------------------------

# barplot
barplot(freq[1:50],las=2,cex.names = 0.7, family="STHeitiTC-Light")
# las=0，x軸橫字、y軸直字，1:x軸、y軸都橫字，2:x軸直字、y軸橫字，3:x軸、y軸都直字
# family是用來指定字體，免得中文字跑不出來。

# 從長條圖可看出，大部分使用「#taiwan」的人都是台灣人。
# 然而其實仔細看其中的資料可發現，裡面有「Taiwan」、「台灣」、「Taipei, Taiwan」、「台湾」、「Taipei」等其實都是台灣或是台灣內的城市，探究其原因可能是因為我抓到的使用者之位置大概是由使用者自己填寫的資料，並不是由電腦判斷的結果，故每個人的寫法不同會造成這樣混淆結果的情形。


# wordcloud
library(wordcloud)
library(RColorBrewer)
toplot <- as.data.frame(freq)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = toplot$Var1, freq = toplot$Freq, min.freq = 3,
          random.order = F, colors = pal, family="STHeitiTC-Light")
# family是用來指定字體，免得中文字跑不出來

# 文字雲的結果與長條圖類似，可看出最多台灣人會使用「#taiwan」標籤。
