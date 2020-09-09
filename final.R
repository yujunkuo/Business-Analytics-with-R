
####### Final Presentation #######

##############################################
# Part1. 資料清洗
##############################################

#導入套件
library(tidyverse)
library(magrittr)

# 讀取data
file_pre <- read.csv("movie_metadata.csv")
# 檢查是否有缺值
table(is.na(file_pre))
# 移除缺值
file <- file_pre[complete.cases(file_pre), ]
# 檢查缺值是否確定移除
table(is.na(file))
# 查看資料
head(file)

##############################################

# 將類別項目轉換為類別變數

# 1.轉換color
file$color %<>% as.factor()
class(file$color)

# 2.轉換country，將country只分成USA與Other兩類
file$country %<>% as.character()
file$country = ifelse(file$country == "USA","USA","Other")
file$country %<>% as.factor()
class(file$country)

##############################################
# Part2. 透過EDA對資料進行初步觀察
##############################################

# 觀察color的分佈
ggplot(file)+geom_bar(aes(x = file$color))+xlab("Color") #大部分電影為彩色

# 觀察duration(電影時長)分佈
ggplot(file)+geom_histogram(aes(x = file$duration))+xlab("Duration") #發現電影時長大部分都集中在100分鐘附近

# 觀察卡司的臉書總讚數分佈
ggplot(file)+geom_histogram(aes(x = file$cast_total_facebook_likes))+xlab("Cast Total Facebook Likes") #發現蠻集中，表示每部電影卡司讚數都差不多

# 觀察電影的country分佈
ggplot(file)+geom_bar(aes(x = file$country))+xlab("Country") #發現美國電影數量相較他國高出許多 

# 觀察電影的IMDB Score分佈
ggplot(file)+geom_histogram(aes(x = file$imdb_score))+xlab("IMDb Score") #發現分數大多集中在6到7分附近

##############################################
# Part3. 變數相關性分析與變數選擇
##############################################

# 查看各數值型態變數的相關性
library(corrgram)
library(corrplot)
#取資料為數值的欄位
corr = cor(file[,c(3,4,5,6,8,9,14,16,23,25,26,28)])
corr

# 查看相關係數>0.7或<(-0.7)的變數，避免變數相互影響，需要移除相關性大於0.7的變數
corr[corr>0.7]
corr[corr<(-0.7)]

# 繪製各變數的相關係數圖
# corrgram(corr)
corrplot(corr, method = "circle")
corrgram(corr,upper.panel=panel.cor, main="correlation")

# 發現"卡司的總讚數"與"演員1、2、3的個別讚數"的相關性都偏高
# 因此我們決定迴歸模型只放"卡司總讚數"這個變數，不放演員讚數
# 同時也發現"電影facebook讚數"與"num_critic_for_review"有很高的相關性(0.7)
# 因此我們決定放"電影讚數"這個變數，不放"num_critic_for_review"

# 推論：
# 一般來說電影的整體知名度幾乎都是靠主角在支持，又主角的影響最大，
# 因此上述分析中"actor1的讚數"與"卡司總讚數"有極高度(corr=0.95)相關是很合情合理的

##############################################
# Part4. 透過迴歸分析預測"電影毛利(gross)"
##############################################

# 我們想知道什麼因素會影響電影的毛利

# 觀察電影毛利(gross)的分佈
ggplot(file)+geom_histogram(aes(x = file$gross))+xlab("Gross")

# IMDB score與毛利的關係 
ggplot(file,aes(x = imdb_score,y = gross))+geom_point(aes(color=file$country))+geom_smooth(method = lm)+xlab("IMDb Score")+ylab("Gross")

# 可看出電影的IMDB評價分數與毛利為正相關的關係
# 即影評越高分，票房上成功的機會也越高

# 預算與毛利的關係
ggplot(file,aes(x = budget,y = gross))+geom_point()+geom_smooth(method=lm)+xlab("Budget")+ylab("Gross")
ggplot(file,aes(x = budget,y = gross))+geom_point()+geom_smooth(method=lm)+xlim(0,300000000)+xlab("Budget")+ylab("Gross")

# 可看出預算與毛利也是呈現正相關

# 針對"gross"作為應變數進行迴歸分析
model_gross <- lm(gross~color+duration+director_facebook_likes+cast_total_facebook_likes+facenumber_in_poster+country+budget+imdb_score+movie_facebook_likes,data = file)
summary(model_gross)

# 首先發現自變數"color"對模型預測的影響力最小(p-value=0.96)，因此將其移除後重新跑迴歸分析
model_gross <- lm(gross~duration+director_facebook_likes+cast_total_facebook_likes+facenumber_in_poster+country+budget+imdb_score+movie_facebook_likes,data = file)
summary(model_gross)

# 移除director_facebook_likes
model_gross <- lm(gross~duration+cast_total_facebook_likes+facenumber_in_poster+country+budget+imdb_score+movie_facebook_likes,data = file)
summary(model_gross)

# 至此所有自變數的p-value都小於0.05，因此模型建構完成

# model: gross = (-7.156e+07)+4.350e+05(duration)+5.076e+02(cast_total_facebook_likes)-1.729e+06(facenumber_in_poster)+3.115e+07(countryUSA)+2.380e-02(budget)+5.878e+06(imdb_score)+9.157e+02(movie_facebook_likes)

# 分析迴歸模型
# 1.電影時長越長，預期毛利將會越高，推論可能是因為時常較長的電影會多收費的關係
# 2.卡司的臉書讚數越多，預期毛利也會越高，這很合理！
# 3.海報上出現的人臉數越多，竟然會降低預期毛利！推論可能是會讓消費者覺得很雜亂
#   因此電影公司在宣傳時可以考慮減少海報的人臉數。
# 4.美國電影的預期毛利較其他國家電影高出很多很多！！
# 5.預算越高，毛利會增加，可能是某些片製片成本高，宣傳時也會比較吸引人!
# 6.IMDB Score 越高，預期毛利會越高
# 7.電影的臉書讚數越高，預期毛利也會越高。因此電影公司要努力提升電影的網路聲量！

# 圖形觀察
plot(model_gross)

# 第一張圖中殘差分佈都很接近0並且平均分佈，表示模型配適合理




##############################################

# 使用該模型對近期上映的電影做預測

# 以復仇者聯盟4為例 (預測毛利)
avengers_endgame <- data.frame(color = factor("Color"),num_critic_for_reviews = 483,duration = 181,director_facebook_likes = 917000,actor_3_facebook_likes = 3660000,actor_1_facebook_likes = 29470000,gross = 2731411643,genres = factor("Sci-Fi"),num_voted_users = 445892,cast_total_facebook_likes = 53000000,facenumber_in_poster = 12,num_user_for_reviews = 7292,language = factor("English"),country = factor("USA"),budget = 3.56e+08,title_year = 2019,actor_2_facebook_likes = 7410000,imdb_score = 8.7,aspect_ratio = 2.39,movie_facebook_likes = 32000)
avengers_gross <- predict(model_gross,avengers_endgame)
predictgross <- avengers_gross
real <- 2700000000

table(predictgross,real)
# 發現預測毛利(gross)為270億，但真實的毛利卻只有27億，呵呵！
# 推論可能是因為使用的資料及較老舊，因此臉書讚數那些都會相對較低(網路不發達)
# 導致預測能力不佳，因此我們認為我們的模型可能較適合預測早期的電影，或使用早期的資料！

##############################################
# Part5. 透過羅吉斯迴歸判斷是否為"推薦電影"
##############################################

# 我們想知道哪種電影較受觀眾歡迎，且跟哪些變數有關
# 因此要研究的是"什麼因素會影響電影的 IMDB score (影評分數)"

# 觀察"卡司的臉書讚數"與"影評分數"的關係
ggplot(file,aes(x = cast_total_facebook_likes,y = imdb_score))+geom_point()+geom_smooth(method = lm)+xlim(0,200000)

# 看出有微弱的正相關性

# 觀察"電影的臉書讚數"與"影評分數"的關係
ggplot(file,aes(x = movie_facebook_likes,y = imdb_score))+geom_point()+geom_smooth(method = lm)+xlim(0,200000)

# 可看出電影的臉書讚數與電影的IMDB分數為正相關的關係
# 即電影臉書讚數越多，影評分數預期也會越高

# 先透過 Histogram 觀察 IMDB score 的分佈
ggplot(file)+geom_histogram(aes(x = imdb_score))

# 大約為常態分佈，但有點左偏，可得知大部分的人不會給太低的影評分數

# 觀察IMDB score 的統計數據
summary(file$imdb_score)

# 發現平均為6.466，很接近6.5

# 因此我們將 IMDB score 大於6.5的電影當作"推薦電影"，小於等於6.5的當作"不推薦電影"
file_2 <- file %>% mutate(recommend = ifelse(imdb_score>6.5,1,0))

# 觀察推薦與不推薦的個數
table(file_2$recommend)
ggplot(file_2)+geom_bar(aes(x = file_2$recommend))+xlab("Recommended or Not")

##############################################

# 對推薦與不推薦的電影進行羅吉斯迴歸

# 首先，切割 Training 與 Test Data (0.75與0.25)

# Create Training Data
ones <- file_2[which(file_2$recommend == 1), ]
zeros <- file_2[which(file_2$recommend == 0), ]
set.seed(100)  # for repeatability of samples
train1_index <- sample(1:nrow(ones), 0.75*nrow(ones))
train0_index <- sample(1:nrow(zeros), 0.75*nrow(zeros))

train1 <- ones[train1_index, ]  
train0 <- zeros[train0_index, ]
traind <- rbind(train1, train0) 

# Create Test Data
test1 <- ones[-train1_index, ]
test0 <- zeros[-train0_index, ]
testd <- rbind(test1, test0)

# 配適羅吉斯迴歸模型
model_rec <- glm(recommend~color+duration+director_facebook_likes+gross+cast_total_facebook_likes+facenumber_in_poster+country+budget+movie_facebook_likes,data=traind,family = binomial(link = "logit"))
summary(model_rec)

# 發現自變數"color"對模型預測的影響力最小(p-value=0.95)，因此將其移除後重新跑迴歸分析
model_rec <- glm(recommend~duration+director_facebook_likes+gross+cast_total_facebook_likes+facenumber_in_poster+country+budget+movie_facebook_likes,data=traind,family = binomial(link = "logit"))
summary(model_rec)

# 發現自變數"cast_total_facebook_likes"對模型預測的影響力小，因此將其移除後重新跑迴歸分析
model_rec <- glm(recommend~duration+director_facebook_likes+gross+facenumber_in_poster+country+budget+movie_facebook_likes,data=traind,family = binomial(link = "logit"))
summary(model_rec)

# 模型建構完成，所有自變數的p-value都小於0.05

# 模型: log(p/(1-p)) = -2.632e+00 +2.932e-02(duration)+7.038e-05(director_facebook_likes)+2.325e-09(gross)-8.786e-02(facenumber_in_poster)-8.787e-01(countryUSA)-9.438e-10(budget)+2.037e-05(movie_facebook_likes)

# 模型分析：
# 1.發現卡司的臉書熱門程度對於電影的IMDB score並沒有顯著影響！
#   反而是導演的臉書熱門程度對於電影影評有很顯著的影響！
#   因此若是該影音平台想要買入電影的版權，則可以優先考慮導演的熱門程度，而不需先考慮卡司的熱門程度！
# 2.美國電影相較其他國家電影來說，和影評的關係竟然為負相關!這是我們蠻意外的地方，推論可能是太普通了，或其他國家電影都是比較經典的電影(好評較高)
# 3.片長越長的電影，預期影評也會較高
# 4.預算越高，預期影評會愈低，認為可能是觀眾人為投入成本那麼多，但沒有達到期待。
# 5.電影臉書熱門程度越高，或者毛利越高，則預期影評也會較高
# 6.海報上的人臉數越多，影評分數會較低！因此可以把各電影的海報當作買入電影的依據之一

# 模型預測1(以0.5為臨界值)
temp <- predict(model_rec,testd,type = "response")
pred <- ifelse(temp>0.5,1,0)

# 建構table觀察預測結果
pred_table <- table(pred,testd$recommend)
pred_table

# 計算正確率
accuracy_rate <- (pred_table[1,1]+pred_table[2,2])/sum(pred_table)
accuracy_rate # 正確率0.68

# 模型預測2(以optimal cutoff選擇最佳臨界值)
library(InformationValue)
thres <- optimalCutoff(testd$recommend,temp)
thres # 得知最佳臨界值為0.456
pred2 <- ifelse(temp>thres,1,0)

# 用第二個模型預測結果來做Confusion Matrix
# 並透過Confusion Matrix觀察預測結果
cm <- confusionMatrix(testd$recommend,pred2,threshold = thres)
cm

# 計算正確率
accuracy_rate2 <- (cm[1,1]+cm[2,2])/sum(cm)
accuracy_rate2 # 正確率為0.703

# 繪製ROC curve
plotROC(testd$recommend,pred2)

sen <- sensitivity(testd$recommend,pred2)
sen # sensitivity為0.722
spe <- specificity(testd$recommend,pred2)
spe # specificity為0.684


# 給影音平台的建議 (透過分析recommend羅吉斯迴歸模型)
# 1.發現卡司的臉書熱門程度對於電影的IMDB score並沒有顯著影響，
#   反而是導演的臉書熱門程度對於電影影評有很顯著的影響！
#   因此若是該影音平台想要買入電影的版權，則可以"優先考慮導演的熱門程度"，而不需先考慮卡司的熱門程度。
# 3.片長越長的電影，預期影評也會較高。因此可以考慮"優先買入片長較長的電影"。
# 4.電影的社群平台熱門程度越高，或者毛利越高，則預期影評也會較高。因此可以優先購入"網路聲量高的電影"


##############################################
# Part6. 透過決策樹預測是否為"推薦電影"
##############################################

# 用上面羅吉思模型找出的顯著變數來建構"決策樹"，判斷在各種情形下，該部電影會如何被歸類
# (即預測會被歸類在推薦或不推薦)

library(rpart)
library(rpart.plot)

# Decision Tree 
dtree <- rpart(recommend~duration+director_facebook_likes+gross+facenumber_in_poster+country+budget+movie_facebook_likes, data = traind, method="class")

# 繪製 Decision Tree
rpart.plot(dtree)

# 發現電影時長(duration)會是決策樹分類的關鍵
# 電影時長大於111分鐘的電影幾乎都會被歸類為推薦電影(及影評分數較高)

# 查看預測結果
pred <- predict(dtree, newdata=testd, type="class")
table(Real = testd$recommend, Predict = pred)

# 計算預測的正確率
(305+375)/(305+375+164+107) # 0.715

##############################################

# 使用該模型對近期上映的電影做預測

# 以"復仇者聯盟4"為例 (判斷是否為推薦電影)
result <- predict(dtree, newdata = avengers_endgame, type = "class")
result 

# 決策樹模型判斷為推薦電影！而真實評分為8.7分 (大於6.5 故為推薦電影)！符合預測！

##############################################
# Part7. 透過隨機森林預測"電影類型(genres)"
##############################################

# 首先對"電影類型(genres)"做概括的分類

# 我們決定將電影類別概括性的區分成10類
file_cat <- file
file_cat$genres %<>% as.character()
for (i in c(1:nrow(file_cat))) {
  
  if (grepl("Animation",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Animation"
  }
  else if(grepl("Biography",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Biography"
  }
  else if(grepl("Crime",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Crime"
  }
  else if(grepl("Romance",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Romance"
  }
  else if(grepl("Sci-Fi",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Sci-Fi"
  }
  else if(grepl("Thriller",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Thriller"
  }
  else if (grepl("Adventure",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Adventure"
  }
  else if (grepl("Comedy",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Comedy"
  }
  else if (grepl("Drama",file_cat$genres[i])== TRUE){
    file_cat$genres[i] = "Drama"
  }
  else{
      file_cat$genres[i] = "Other"
  }
}

# 將genres轉換回factor
file_cat$genres %<>% as.factor()

# 觀察分類結束後的電影類型分佈
table(file_cat$genres)
ggplot(file_cat)+geom_bar(aes(x = genres))

##############################################

# 透過"隨機森林"建模來預測電影如何分類

# 首先切割訓練與測試Data (訓練0.75,測試0.25)
trainI <- sample(1:3801, 2850)
traind <- file_cat[trainI,]
testd <- file_cat[-trainI,]

# 隨機森林 (Random Forest)
library(randomForest)
rf <- randomForest(genres ~ ., data = file_cat[,-c(2,7,11,12,15,17,18,22)], importance=TRUE,ntree=100) 
rf 

# 透過圖表發現，選100棵數最適合
plot(rf)
legend("topright", colnames(rf$err.rate),col=1:10,cex=0.5,fill=1:10)

# 查看變數的重要性
importance(rf)
varImpPlot(rf)

# 看出分類各種不同類型電影的最主要變數是"預算(budget)"與"電影時長(duration)"

# 預測
pred=predict(rf, newdata = testd[,-c(2,7,11,12,15,17,18,22)])
t <- table(Real = testd$genres, Predict = pred)
t


# 1.從預測結果可以看出，冒險片與動畫片的分類的正確率很高，
#   可見這些類型的影片可以很容易地透過我們選取的自變數來預測出來(特徵明顯)
# 2.喜劇片與浪漫愛情片這兩種很容易被分錯成彼此，因此可以知道這兩種類型的電影特徵其實是很相似的
# 3.驚悚片與科幻片、犯罪片也是很容易被誤分成彼此，可見他們之間相似性也蠻高的

##############################################
# Part8. 結論
##############################################
# 給電影製造商的建議 (透過分析gross的迴歸模型)
# 1.海報上出現的臉孔數量越多，預期會降低電影毛利！推論可能是會讓消費者覺得很雜亂
#   因此電影製造商在進行電影宣傳時建議可以考慮"減少電影海報上出現的臉孔數量"。
# 2.預算越高，毛利也會增加，可能是因為觀眾的期待心理!
# 3.IMDB Score 越高，預期毛利會越高。因此電影製造商可以"投入資源去提升影音平台上的影評分數"。
# 4.電影的臉書讚數越高，預期毛利也會越高。因此電影公司要努力"提升電影的網路聲量"！


# 給影音平台的建議 (透過分析recommend羅吉斯迴歸模型)
# 1.發現卡司的臉書熱門程度對於電影的IMDB score並沒有顯著影響，
#   反而是導演的臉書熱門程度對於電影影評有很顯著的影響！
#   因此若是該影音平台想要買入電影的版權，則可以"優先考慮導演的熱門程度"，而不需先考慮卡司的熱門程度。
# 2.美國電影相較其他國家電影來說，影評會較高，因此也可以"優先買入美國片"！
# 3.片長越長的電影，預期影評也會較高。因此可以考慮"優先買入片長較長的電影"。
# 4.電影的社群平台熱門程度越高，或者毛利越高，則預期影評也會較高。因此可以優先購入"網路聲量高的電影"

##############################################
# Part9. 文字探勘 Text Mining
##############################################

# Text Mining (文字分析)

# 首先找出IMDB score最高的電影
max(file$imdb_score)
imdb_max <- file[which(file$imdb_score == max(file$imdb_score)),]
View(imdb_max) # The Shawshank Redemption (刺激1995)

# 再來找出"電影臉書讚數"最高的電影
max(file$movie_facebook_likes)
fb_max <- file[which(file$movie_facebook_likes == max(file$movie_facebook_likes)),]
View(fb_max) # Interstellar (星際效應)  

# 最後找出"毛利"最高的電影
max(file$gross)
gross_max <- file[which(file$gross == max(file$gross)),]
View(gross_max) # Avatar (阿凡達)


