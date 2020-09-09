
########################第一題#############################
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud2)

data <-  read.csv("Womens Clothing E-Commerce Reviews.csv")

#切割推薦與不推薦的客戶，分別建立dataframe
positive <- data[data$Recommended.IND == 1,]
negative <- data[data$Recommended.IND == 0,]

## Make a vector source and a corpus
x <- Corpus(VectorSource(positive$Review.Text))
y <- Corpus(VectorSource(negative$Review.Text))

##Clean text
x <- tm_map(x, tolower) #convert to lower case
x <- tm_map(x, removePunctuation) #remove punctuation

y <- tm_map(y, tolower) #convert to lower case
y <- tm_map(y, removePunctuation) #remove punctuation

#Remove stopwords
x <- tm_map(x, removeWords, stopwords("english"))
y <- tm_map(y, removeWords, stopwords("english"))

x <- tm_map(x, stemDocument)
y <- tm_map(y, stemDocument)

x_tdm <- TermDocumentMatrix(x)
y_tdm <- TermDocumentMatrix(y)

#查看TermDocumentMatrix
inspect(x_tdm)
inspect(y_tdm)

# Convert TDM to matrix
review_m_x <- as.matrix(x_tdm)
review_m_y <- as.matrix(y_tdm)

# Sum rows and frequency data frame
freq_df_x <- rowSums(review_m_x)
freq_df_y <- rowSums(review_m_y)

# Sort term_frequency in descending order
freq_df_x <- sort(freq_df_x, decreasing = T)
freq_df_y <- sort(freq_df_y, decreasing = T)

# View the top 10 most common words and draw the bar chart
# 直方圖 bar chart
freq_df_x[1:10]
barplot(freq_df_x[1:20], col = "royalblue", las = 2) + title("positive customers")

freq_df_y[1:10]
barplot(freq_df_y[1:20], col = "royalblue", las = 2) + title("negative customers")

#分析

#發現會推薦的顧客的留言中，前幾名分別是dress,love,fit與size
#而不推薦的顧客的留言中，前幾名分別是look,dress與like
#發現"相同點"是dress出現頻率在兩類客戶中都很高
#而不同的地方是推薦的客戶會提及較多的love，很可能是表達出喜愛的意思；而不推薦的則提及很多次"look"，推論可能是評論衣服"看起來的樣子"之類的

#轉成data frame
freq_df_x <- data.frame(term = names(freq_df_x),num = freq_df_x)
freq_df_y <- data.frame(term = names(freq_df_y),num = freq_df_y)

#文字雲
wordcloud2(freq_df_x,size=0.5)
wordcloud2(freq_df_y,size=0.5)

#分析

#從兩張直方圖(推薦與不推薦的客戶)中還可以看出，
#推薦的顧客的留言前幾名中出現了perfect、nice、comfort與great等正面形容詞，而不推薦的顧客的留言前幾名中則是出現了back與return等退貨相關詞語，
#且雖然推薦與不推薦中都有出現love與fit等詞，但我認為不推薦的客戶中可能出現的是didn't love 或 didn't fit等

#從文字雲中可以發現，
#不推薦的客戶的word cloud中出現了蠻明顯的didn't，這證實了前面的推論應該是正確的，也就是love與fit等詞有很大的可能是與didn't搭配的！

########################第二題#############################
library(rvest)
library(magrittr)
library(wordcloud2)

#選擇整理的資料為 "CNN 的 World 新聞"

#建構儲存新聞內容的list
content_list = list()

#讀取網頁的html與節點
doc <- read_html("https://edition.cnn.com/world") # 把網頁先存在一個變數裡，不用每次都連線造訪
title <- doc %>% html_nodes(".cd__headline") %>% html_text() # 取得 css 路徑下的所有節點

#所有新聞的標題
title

#爬出所有新聞的網址連結
url <- doc %>% html_nodes(".cd__headline a") %>% html_attr("href")
url <- as.vector(url)

#所有新聞的網址連結
url

#使用迴圈從上面抓到的每個網址的連結爬出每篇新聞的文章內容，並依序加入content_list中
for (i in url) {
  string = paste("https://edition.cnn.com",i,sep = "")
  doc_temp <- read_html(string)
  paragraph <- doc_temp %>% html_nodes(".zn-body__paragraph") %>% html_text()
  content_list <- c(content_list,paragraph)
}

#整理爬到的新聞內容
z <- Corpus(VectorSource(content_list))
z <- tm_map(z, tolower) #convert to lower case
z <- tm_map(z, removePunctuation) #remove punctuation
stop <-  c("said","will","also")
z <- tm_map(z, removeWords, stopwords("english"))
z <- tm_map(z,removeWords,stop)
z <- tm_map(z, stemDocument)

z_tdm <- TermDocumentMatrix(z)
inspect(z_tdm)
review_m_z <- as.matrix(z_tdm)

freq_df_z <- rowSums(review_m_z)
freq_df_z <- sort(freq_df_z, decreasing = T)
freq_df_z <- data.frame(term = names(freq_df_z),num = freq_df_z)

#製作wordcloud文字雲
wordcloud2(freq_df_z,size=0.5)

#分析

#發現CNN的World新聞的wordcloud中"taiwan出現在滿明顯的位置"，有可能與最近台灣法規更動導致各國爭相報導有關
#又再發現"samesex"也出現在明顯的位置，這更證實了上一行的推論是正確的
#此外，一些新聞報導常見字眼如said,report,told等字眼的出現也很合理
#而可以看出最近被報導的國家除了台灣以外就以iran(伊朗)與yemen(葉門)居多
#最後，"aid"字眼出現在很明顯的位置，可以推論world新聞上各國間有很多互相援助相關的新聞


