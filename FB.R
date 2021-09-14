library(httr)
library(rjson) 
# 因為facebook api傳回來的是json，因為是中文所以用rjson可能會比較好用
library(plyr) # for rbind.fill

options(fileEncoding = 'utf-8')
options(encoding = "utf-8")
options(stringsAsFactors = F)  # 避免文字被轉成factor，難處理。

# Add a global option here to avoid converting string to factors




# Use Facebook Graph API --------------------------------------------------
# 1. Selecting one fan page such as President Tsai's or Taipei Mayor Ke's
# 		https://www.facebook.com/tsaiingwen/?fref=ts
# 		https://www.facebook.com/DoctorKoWJ/?fref=ts
#		https://www.facebook.com/llchu/?fref=ts
# 2. Identifying their page names
# 3. Visiting Facebook graph API explorer 
# 		https://developers.facebook.com/tools/explorer/ 
# 4. Sending queries with their page names



# Send a graph api query by GET() -----------------------------------------
# Get a query url from facegook graph api, using llchu as an example
url <- ""
# 取得json的url


# Get the result by 
# 1. httr::GET(): getting the text
# 2. httr::content(), converting response to character vector and 
# 3. rjson::fromJSON() , converting character vector r object accotrding to JSON format
res <- fromJSON(content(GET(url), "text"))
# 把取回來的文字用content轉成character vector, 之後再用fromJSON


# Where is your data? Using length() to test or check the Environment panel
length(res)
tes$id
res$posts
length(res$posts) # =2，大概不是我們要的東西
# 繼續找我們要的posts資料到底在哪
res$posts$paging  
res$posts$data
length(res$posts$data) # =25，合理!有25個observation



# Convert response to data.frame ------------------------------------------
# Try to convert res to data.frame, but get an ill-formated data.frame
res.df <- as.data.frame(res$posts$data)  # 只有一個observation，不合理

# Check the length of res 
length(res$posts$data) # 25
length(unlist(res$posts$data)) # 85 why?

# 把每一筆資料的欄位長度印出來
for(d in res$posts$data){
	print(length(d))
}

# Possible solution 1: Specify data field you want (X)
url <- ""
# 一開始搜尋時就限定要id,message,story,created time四個欄位，
# 但還是沒用，每筆資料還是有長有短，有缺少欄位的就直接缺、不會傳NA。

# take a look at your data, still length=76
res <- fromJSON(content(GET(url),'text'))
length(res$posts$data) # 25
length(unlist(res$posts$data)) # still the same

# Possible solution 2: only convert one data to data.frame (V)
d <- as.data.frame(res$posts$data[[1]]) # 成功了耶


# Merge all data to a data.frame ------------------------------------------
# Formal method 1
res1 <- res$posts$data[[1]]
post.df <- data.frame(id = res1$id,
	created_time = res1$created_time,
	story = if(!is.null(res1$story)) res1$story else NA,
	message = if(!is.null(res1$message)) res1$message else NA
)

for(p in res$posts$data[2:length(res$posts$data)]){
	temp.df <- data.frame(id = p$id,
		created_time = p$created_time,
		story = if(!is.null(p$story)) p$story else NA,
		message = if(!is.null(p$message)) p$message else NA
	)
	post.df <- rbind(post.df, temp.df)
}

# Faster method 2 by plyr::rbind.fill()
post.df = data.frame()
for(p in res$posts$data){
	post.df <- rbind.fill(post.df, as.data.frame(p))
}
# 先把每則post都轉成dataframe，再把他們黏起來
# rbind.fill: 有缺少的會自動幫忙補

# The best method for dealing with large dataset
post.df <- do.call("rbind.fill", res$posts$data) # will raise error
# ERROR: All inputs to rbind.fill must be data.frames

# Using lapply to convert each element in post.list to data.frame
post.list <- lapply(post.list, as.data.frame)
post.df <- do.call("rbind.fill", res$posts$data)

# THE RIGHT VERSION
post.df <- do.call("rbind.fill", lapply(res$posts$data, as.data.frame))



# Practice ----------------------------------------------------------------
# Creating 3 different querys with more arguments



# Decompose your query to get data repeatedly -----------------------------
# https://developers.facebook.com/tools-and-support/

# copy your user token
token <- 'EAACEdEose0cBAGea2JlHUgCJ0AAYQ9HzUda5FfyAq9p2Og8ETA9w1StjdahDZBhUbz0OOZBcGFZBlROWtqodyAylWQQ1SLmtUcVY7lJrT1THKAyhwycKtQVaao0xV9NPz3qmqfw5HZAJDVZC17o8VxNjJTviyhi2uVIcz03tHeKZA68JNbC9WpJZA9yUdJCEEUZD' 
# 每一個小時就會過期，所以要重新取

# pageid <- '109249609124014' yahoo news
# pageid <- '352962731493606' apple news
# pageid <- '46251501064' #tsai
pageid <- '136845026417486' #柯文哲
# pageid <- '278059089045255' #正晶限時批


# query <- "fields=posts"

query <- 'fields=posts.limit(50){id,message,type,created_time,updated_time,shares,comments.limit(0).summary(true),likes.limit(0).summary(true)}'
# query時: don't use v2.9, use v2.8
# 抓posts, posts裡面的limit(50), typr, id, message, created_time, updated_time, shares, comments


url <- sprintf("https://graph.facebook.com/v2.8/%s?%s&access_token=%s", pageid, query, token)
# 這樣就不用每一次都要在搜尋、複製一次，只要改必須改的東西就好了
# sprintf:字串列印格式化(?)

res <- fromJSON(content(GET(url),'text'))
post.df <- do.call("rbind.fill", lapply(res$posts$data, as.data.frame))


# Where is your next page?
nexturl <- res$posts$paging$next  # next is a controll vocabulary in R
nexturl <- res$posts$paging$"next" # 所以要加上「""」
# $paging表示分頁，存著後面的資料


# Get the next response (下25筆資料)
nextres <- fromJSON(content(GET(nexturl),'text'))

next.df <- do.call("rbind.fill", lapply(nextres$posts$data, as.data.frame))

# next.df <- do.call("rbind.fill", lapply(nextres$posts$data, as.data.frame)) # ERROR: nextres has no posts
# 是個null
# 因為res下面有posts、data (就是說res$posts$data存在)
# 第二頁以後反正他知道你就是要撈posts，所以沒有posts了，只有nextres$data

next.df <- do.call("rbind.fill", lapply(nextres$data, as.data.frame))


# Combine the post.df and next.df by row, using plyr::rbind.fill()
post.df <- rbind.fill(post.df, next.df)
# 比較複雜的query可能會產生錯誤

# 較複雜的query:
parsePost <- function(x){
  data.frame(created_time = x$created_time,
             id = x$id,
             updated_time = x$updated_time,
             type = if(!is.null(x$type)) x$type else NA,
             message = if(!is.null(x$message)) x$message else NA,
             shares_count = if(!is.null(x$shares)) x$shares$count else NA,
             likes_count = if(!is.null(x$likes)) x$likes$summary$total_count else NA,
             comments_count = if(!is.null(x$comments)) x$comments$summary$total_count else NA
  )
}
# 每一次丟一筆資料進去，產生一個row
# 之後再用rbind結合起來就可以了。(因為已經處理過NA值了，所以用rbind就可以了不用用rbind.fill)


# Let's write a while loop to repeat the process --------------------------
pageid <- '136845026417486' #柯文哲
query <- "fields=posts"
url <- sprintf("https://graph.facebook.com/v2.8/%s?%s&access_token=%s", pageid, query, token)

# Get the first data.frame of post
res <- fromJSON(content(GET(url),'text'))
post.df <- do.call("rbind.fill", lapply(res$posts$data, as.data.frame))
nexturl <- res$posts$paging$"next"

# Use while(T){} to loop forevert (稍微錯誤寫法)
# 讓他不斷取得下25筆資料
while(T){  
	nextres <- fromJSON(content(GET(nexturl),'text'))
	next.df <- do.call("rbind.fill", lapply(nextres$data, as.data.frame))
	post.df <- rbind.fill(post.df, next.df)
	print(nrow(post.df)) # print現在有幾行(幾筆資料)
	nexturl <- nextres$paging$"next"
}
# 通常: while(條件)，當符合條件時就跑下面的東西
# while(T): 沒有寫條件了、只寫TRUE表示一直跑
# 跑到最後因為沒有下一頁了，所以會出現error

# Get an error 
# [1] 7714
# Error in nextres$data[[i]] : 下標超出邊界

# Print out nexturl to see 
nexturl

# 正確寫法
while(T){
	nextres <- fromJSON(content(GET(nexturl),'text'))
	next.df <- do.call("rbind.fill", lapply(nextres$data, as.data.frame))
	post.df <- rbind.fill(post.df, next.df)
	print(nrow(post.df))
	nexturl <- nextres$paging$"next"
	if(is.null(nexturl)){break}   # 如果nexturl = null，則跳出迴圈
}





# Practice: Compose your own clean case here ------------------------------



