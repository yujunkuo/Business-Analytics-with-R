library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(broom)
library(car)
facebook <- read.csv("/Users/john/R working_dir/dataset_Facebook.csv",sep=";")

###############################Question1.#####################################
#查看資料的內容
str(facebook)
head(facebook)
summary(facebook)
#查看變數型態
facebook_var_type <- cbind(colnames(facebook)[1:7],c("continuous data","categorical data","categorical data","continuous data","continuous data","continuous data","categorical data")) %>% as.data.frame()
colnames(facebook_var_type) <- c("variable name","variable type")
#將類別項目轉為類別變數
facebook$Category %<>% as.factor()
facebook$Paid %<>% as.factor()
facebook$Type %<>% as.factor()
facebook$Post.Month %<>% as.factor()
facebook$Post.Weekday %<>% as.factor()
facebook$Post.Hour %<>% as.factor()
class(facebook$Category)
#查看是否有缺值,並移除缺值
table(is.na(facebook))
facebook %<>% na.omit()
#檢查缺值是否確定移除
table(is.na(facebook))
#EDA分析
#各Type次數的Bar Chart,看出Photo Type佔絕大多數
ggplot(data = facebook) + geom_bar(aes(x = facebook$Type)) + labs(title = "Type Count",x ="Type")
#各category次數的Bar Chart,看出第一類(Action)佔多數
ggplot(data = facebook) + geom_bar(aes(x = facebook$Category)) + labs(title = "Category Count",x = "Category")
#各Post.Month次數的Bar Chart,看出年底(10~12月)發文數量較多
ggplot(facebook) + geom_bar(aes(x = facebook$Post.Month)) + labs(title = "Post Month Count",x = "Post Month")
#各Post.Weekday次數的Bar Chart,看出週末(六、日)發文數量較多
ggplot(facebook) + geom_bar(aes(x = facebook$Post.Weekday)) + labs(title = "Post Weekday Count",x = "Post Weekday")
#各Post.Hour次數的Bar Chart,看出半夜(2~4點)以及中午(10~13點)的時候發布較多篇貼文
ggplot(facebook) + geom_bar(aes(x = facebook$Post.Hour)) + labs(title = "Post Hour Count",x = "Post Hour")
#比較Paid項目付費與沒付費的數量差異,看出沒付費數量約為付費的兩倍
ggplot(facebook) + geom_bar(aes(x = facebook$Paid)) + labs(title = "Paid or Unpaid Count",x = "Paid / Unpaid")
#like的數量分布,可以發現大部分讚數大約在0~500之間，只有少數幾篇讚數會到1000,2000,甚至5000
ggplot(facebook) + geom_histogram(aes(x = facebook$like),bins = 500,alpha = 0.8) + labs(title = "Total Like Count",x = "Like")
#觀察like和comment的關係,可以發現為正相關,且有離群值
ggplot(facebook) + geom_point(aes(x = like,y = comment))

###############################Question2.#####################################
###我有興趣的問題1：我想知道越接近週末"貼文的互動次數"會不會比較多,也就是探討Total.Interactions會不會受到Post.Weekday的影響,因此我的model的Y放的是Total.Interactions
#建立迴歸模型
facebook_model1 <- lm(Total.Interactions~Post.Weekday,facebook)
summary(facebook_model1)
#由迴歸分析發現F-statistic(global test)的p-value=0.32>0.05,表示此類自變數對應變數沒有產生顯著影響
#結論是Post.Weekday對Total.Interactions沒有顯著影響,也就是貼文的互動次數並沒有因為禮拜幾張貼而受到顯著影響

###我有興趣的問題2:我想知道是否有某一類的文章分享次數較多,也就是探討文章種類(Category)和分享次數的關係,因此我的model的Y放的是share
#首先先繪製Boxplot觀察數據的分佈情形
ggplot(facebook) + geom_boxplot(aes(x = Category,y = share))
#由Boxplot看出第三類的平均share次數最多,其次是第二類,最少的是第一類;然而第二類有一個share次數很大的極端值
#移除極端值
facebook[facebook$share>600,]#發現極端值是第245筆資料,再推算扣除先前刪除的NA資料,得知為目前第241筆資料
facebook_temp <- facebook[-241,]#暫時移除
#再畫一次圖觀察移除極端值後的數據分佈
ggplot(facebook_temp) + geom_boxplot(aes(x = Category,y = share))
#查看dummy variable的編碼方式
contrasts(facebook$Category)#發現是以Category1作為基準來比較
#建立迴歸模型
facebook_model2 <- lm(share~Category,facebook_temp)
summary(facebook_model2)
#觀察迴歸結果,發現F-statistic(global test)的p-value=1.733e-07<0.05,且每個自變數的p-value都小於0.025,表示自變數對該應變數都有產生影響,因此迴歸模型建構完成
#文章種類(Category)和分享次數的關係的回歸模型：share = 18.725 + 9.775*factor(x = Category2) + 14.223*factor(x = Category3)
#此類別變數的分析是與Category1相比,也就是與Category1相比之下,Category2的分享次數多9.775次,而Category3的分享次數多14.223次
#由此迴歸結果可以推測Category3也就是inspiration (non-explicit brand related content)類的貼文分享次數較多。
#透過此結果可以推論一般顧客對於非直接品牌相關的貼文會有更大的分享意願,我認為這是合理的。因為其實對我而言品牌的臉書專頁我也都只是看看而已,分享某產品的貼文對我也沒什麼意義
#然而若是某篇貼文是有趣的小故事或者心情小語等等,剛好打中我的心,那我就有可能會因為想讓其他朋友看到這篇有趣的文章而分享這篇貼文
#此外，產品類(ptoduct)的貼文分享次數也相對第一類來得高,我認為這也是合理的。因為通常產品類的貼文會有"分享貼文可以抽該產品"等活動,因此也會提高客戶的分享意願,像是剛剛刪去的那一個產品類的share極端值,我認為便很可能是一個很吸引人的產品的抽獎文
#對facebook_model2的殘差(residuals)進行分析
facebook_model2$residuals
mean(facebook_model2$residuals)
sd(facebook_model2$residuals)
shapiro.test(facebook_model2$residuals)

###我有興趣的問題3:我想知道支付臉書廣告費是否會增加貼文被展示的次數,也就是探討支付廣告費(Paid)和貼文被展示的次數(Lifetime.Post.Total.Impressions)的關係,因此我的model的Y放的是Lifetime.Post.Total.Impressions
#首先先繪製Boxplot觀察數據的分佈情形
ggplot(facebook) + geom_boxplot(aes(x = Paid,y = Lifetime.Post.Total.Impressions))
#建立迴歸模型
facebook_model3 <- lm(Lifetime.Post.Total.Impressions~Paid,facebook)
summary(facebook_model3)
#由迴歸分析發現F-statistic(global test)的p-value=0.17>0.05,表示此類自變數對應變數沒有產生顯著影響
#結論是Paid對Lifetime.Post.Total.Impressions沒有顯著影響,也就是說付臉書廣告費對於增加貼文被展示的次數沒有顯著的幫助

###我有興趣的問題4:既然付廣告費對於貼文"所有的被展示次數"沒有顯著影響,那我想知道廣告費對於"已按粉絲專頁讚的人,這篇發文總展示次數"會不會有影響,也就是探討Paid和Lifetime.Post.Impressions.by.people.who.have.liked.your.Page的關係,因此我的model的Y放的是Lifetime.Post.Impressions.by.people.who.have.liked.your.Page
#首先先繪製Boxplot觀察數據的分佈情形
ggplot(facebook) + geom_boxplot(aes(x = Paid,y = facebook$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page))
#建立迴歸模型
facebook_model4 <- lm(facebook$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page~Paid,facebook)
summary(facebook_model4)
#由迴歸分析發現F-statistic(global test)的p-value=0.96>0.05,表示此類自變數對應變數沒有產生顯著影響
#結論是Paid對Lifetime.Post.Impressions.by.people.who.have.liked.your.Page沒有顯著影響,也就是說付臉書廣告費對於增加貼文對已按讚粉專的人的被展示的次數也非常沒有幫助

###我有興趣的問題5:我想知道貼文的Type(照片、影片、連結等)會不會影響"參與互動的總人數"(即點擊貼文的人數),也就是探討Type與Lifetime.Engaged.Users的關係,因此我的model的Y放的是Lifetime.Engaged.Users
#首先先繪製Boxplot觀察數據的分佈情形
ggplot(facebook) + geom_boxplot(aes(x = Type,y = facebook$Lifetime.Engaged.Users))
#由Boxplot發現Video和Status類貼文的平均點擊次數較高
#查看dummy variable的編碼方式
contrasts(facebook$Type)#發現是以Link Type作為基準來比較
#建立迴歸模型
facebook_model5 <- lm(facebook$Lifetime.Engaged.Users~Type,facebook)
summary(facebook_model5)
#觀察迴歸結果,發現F-statistic(global test)的p-value小於2.2e-16,遠小於0.05,且每個自變數的p-value都小於0.025,表示自變數對該應變數都有產生影響,因此迴歸模型建構完成
#貼文類型(Type)和"參與互動的總人數"的關係的回歸模型：Lifetime.Engaged.Users = 342.8 + 482.5*factor(x = TypePhoto) + 1697.4*factor(x = TypeStatus) + 1364.2*factor(x = TypeVideo)
#此類別變數的分析是與Link的這類Type相比,也就是與TypeLink相比之下,TypePhoto的平均點擊次數多482.5次,TypeStatus的平均點擊次數多1697.4次,而TypeVideo的平均點擊次數多1364.2次
#由此迴歸結果可以推測Status與Video這兩類(Type)的貼文點擊次數較多。
#分析這項迴歸的結果,Video Type貼文的平均點擊次數較多感覺很合理,因為畢竟影片要點進去才比較方便觀賞;而Photo Type的點擊次數較少也挺合理,因為照片不需點擊就可以觀看了！
#然而Link Type貼文的平均點擊次數竟然比其他類型貼文的平均點擊次數少很多很多,這個結果其實蠻令我意外的。我本來以為Link類的貼文因為一定要點進去才能開啟連結,因此點擊次數應該會很多,但卻不是這樣...
#因此從這個迴歸結果可以推論,顧客對於點擊貼文中的連結(Link)的意願其實並不高!因此我會建議該企業的粉專可以多透過影片來進行行銷宣傳,而減少用連結Link的方式進行宣傳
#此外,我認為對於貼文的點擊次數,其實"貼文的文章長度"也很重要,因為若是文章很長,便需要點擊"繼續閱讀"才能看完,也因此長篇文章我認為相對而言點擊次數就會較高。因此"文章長短"我認為也是需要考慮進這個模型的自變數之一。
#模型觀察與殘差分析Error assumption (residual analysis)
facebook_model5$residuals #殘差
mean(facebook_model5$residuals) #殘差平均
sd(facebook_model5$residuals) #殘差標準差
shapiro.test(facebook_model5$residuals)
durbinWatsonTest(facebook_model5)
plot(facebook_model5) #繪圖
anova(facebook_model5) #ANOVA分析
#使用broom這個package觀察模型資料
tidy(facebook_model5)
glance(facebook_model5)
#模型預測(拿原本dataframe的數據測試)
predict(facebook_model5,facebook[5,])
predict(facebook_model5,facebook[9,])
predict(facebook_model5,facebook[30,])
