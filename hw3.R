library(tidyverse)
library(magrittr)
file <- read.csv("airline.csv")

#變數轉換 將目標變數轉為1與0
file$is_loyal <- ifelse(file$is_loyal == "Satisfied",1,0)
table(file$is_loyal)
file$class %<>% as.factor()

# Create Training Data
ones <- file[which(file$is_loyal == 1), ]  # all Satisfied's
zeros <- file[which(file$is_loyal == 0), ]  # all Unsatisfied's
set.seed(100)  # for repeatability of samples
train1_index <- sample(1:nrow(ones), 0.7*nrow(ones))  # Satisfied's for training
train0_index <- sample(1:nrow(zeros), 0.7*nrow(zeros))  # Unsatisfied's for training.
#Pick as many Unsatisfied's as Satisfied's
train1 <- ones[train1_index, ]  
train0 <- zeros[train0_index, ]
traind <- rbind(train1, train0)  # row bind the Satisfied's and Unsatisfied's 

# Create Test Data
test1 <- ones[-train1_index, ]
test0 <- zeros[-train0_index, ]
testd <- rbind(test1, test0)

#配適羅吉斯迴歸
model <- glm(is_loyal~depart_on_time+arrive_on_time+register_method+register_rate+class+seat_rate+meal_rate+flight_rate+package_rate+tv_ad+youtube_ad_1+youtube_ad_2+youtube_ad_3+dm_message+dm_post+dm_email+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#從summary發現dm_post的p值最大，故先移除dm_post，再重跑迴歸
model <- glm(is_loyal~depart_on_time+arrive_on_time+register_method+register_rate+class+seat_rate+meal_rate+flight_rate+package_rate+tv_ad+youtube_ad_1+youtube_ad_2+youtube_ad_3+dm_message+dm_email+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除register_rate
model <- glm(is_loyal~depart_on_time+arrive_on_time+register_method+class+seat_rate+meal_rate+flight_rate+package_rate+tv_ad+youtube_ad_1+youtube_ad_2+youtube_ad_3+dm_message+dm_email+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除youtube_ad_3
model <- glm(is_loyal~depart_on_time+arrive_on_time+register_method+class+seat_rate+meal_rate+flight_rate+package_rate+tv_ad+youtube_ad_1+youtube_ad_2+dm_message+dm_email+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除register_method
model <- glm(is_loyal~depart_on_time+arrive_on_time+class+seat_rate+meal_rate+flight_rate+package_rate+tv_ad+youtube_ad_1+youtube_ad_2+dm_message+dm_email+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除youtube_ad_1
model <- glm(is_loyal~depart_on_time+arrive_on_time+class+seat_rate+meal_rate+flight_rate+package_rate+tv_ad+youtube_ad_2+dm_message+dm_email+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除package_rate
model <- glm(is_loyal~depart_on_time+arrive_on_time+class+seat_rate+meal_rate+flight_rate+tv_ad+youtube_ad_2+dm_message+dm_email+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除dm_email
model <- glm(is_loyal~depart_on_time+arrive_on_time+class+seat_rate+meal_rate+flight_rate+tv_ad+youtube_ad_2+dm_message+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除youtube_ad_2
model <- glm(is_loyal~depart_on_time+arrive_on_time+class+seat_rate+meal_rate+flight_rate+tv_ad+dm_message+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#移除meal_rate
model <- glm(is_loyal~depart_on_time+arrive_on_time+class+seat_rate+flight_rate+tv_ad+dm_message+credit_card_vendor+credit_card_bonus+coupon,data=traind,family = binomial(link = "logit"))
summary(model)
#模型建構完成，所有自變數的p-value都小於0.05

#模型預測1(以0.5為臨界值)
temp <- predict(model,testd,type = "response")
pred <- ifelse(temp>0.5,1,0)
#建構table觀察預測結果
pred_table <- table(pred,testd$is_loyal)
pred_table
#計算正確率
accuracy_rate <- (pred_table[1,1]+pred_table[2,2])/sum(pred_table)
accuracy_rate #正確率0.907

#模型預測2(以optimal cutoff選擇最佳臨界值)
library(InformationValue)
thres <- optimalCutoff(testd$is_loyal,temp)
thres #得知最佳臨界值為0.34
pred2 <- ifelse(temp>0.34,1,0)
#用第二個模型預測結果來做Confusion Matrix
#並透過Confusion Matrix觀察預測結果
cm <- confusionMatrix(testd$is_loyal,pred2,threshold = thres)
cm
#計算正確率
accuracy_rate2 <- (cm[1,1]+cm[2,2])/sum(cm)
accuracy_rate2 #正確率為0.917
#計算錯誤率
error_rate2 <- misClassError(testd$is_loyal,pred2,threshold = thres)
error_rate2 #錯誤率為0.083，也等於1-accurancy rate = 1 - 0.917 = 0.083
#繪製ROC curve
plotROC(testd$is_loyal,pred2)

sen <- sensitivity(testd$is_loyal,pred2)
sen #sensitivity為0.963
spe <- specificity(testd$is_loyal,pred2)
spe #specificity為0.838

#第一小題:辨認出忠誠與不忠誠客戶
#用羅吉斯回歸模型預測
#模型: log(p/(1-p)) = -7.86 +3.77(depart_on_time) +2.34(arrive_on_time) -6.8(class2) -17.24(class3) + 2.34(seat_rate) +0.88(flight_rate) +6.99(tv_ad) -13.07(dm_message) +1.42(credit_card_vendorVendor B) +1.98(credit_card_vendorVendor C) +2.82(credit_card_bonus) +0.05(coupon)

#第二小題:哪些因素影響客戶忠誠度
#可分為問卷調查中的變數與公司資料中的變數
#問卷調查中影響客戶忠誠度的因素:是否準點起飛、是否準點抵達、搭乘之座艙等級、座位舒適程度、飛行滿意度、是否看過小花公司的電視廣告
#公司資料中影響客戶忠誠度的因素:訂位前三個⽉內是否曾經寄送過簡訊銷售訊息、本次消費之信用卡的發行商、本次消費之信用卡的紅利等級、該客戶本次消費的折扣金額

#第三小題:如何提升客戶保留率，降低公司行銷成本

# 首先，從羅吉斯迴歸模型可以看出與預期
# 1.準點起飛與準點抵達都會提升客戶忠誠度
# 2.搭乘頭等艙(class1)的客戶相對於搭乘商務與經濟艙的客戶而言，有較高的忠誠度
# 3.座位舒適度與飛行滿意度越高，該客戶忠誠度也會越高
# 4.有看過小花公司的電視廣告會大幅提升客戶忠誠度！！
# 5.訂位前三個⽉內，公司若是曾經寄送過簡訊銷售訊息給客戶，竟然會大幅降低客戶忠誠度！！
# 6.信用卡發行商Vendor C的客戶相對其他發行商而言，有較高的客戶忠誠度
# 7.信用卡紅利等級越高，客戶忠誠度會相對越高
# 8.消費的折扣金額越多，客戶忠誠度也會越高

# 因此，若是想提升客戶保留率，並降低行銷成本，我會建議該航空公司的"行銷部門":
# 1.投入多一點經費在電視廣告，並減少youtube廣告的預算！如此便可降低行銷成本，同時提升客戶忠誠度。因為迴歸模型顯示出電視廣告對提升客戶忠誠度的幫助較大
# 2.訂位前三個⽉內，不要寄送簡訊銷售訊息給客戶！因為這個行為會降低客戶忠誠度
# 3.可與信用卡發行商C合作推出里程紅利卡！因為迴歸模型顯示出信用卡發行商C的客戶相對其他發行商而言忠誠度較高，且當客戶紅利等級越高或消費折扣金額越多時，客戶忠誠度也會相對越高。
#   因此若是與發行商C合作推出里程紅利卡，便能給予客戶動機去"取得發行商C發行的信用卡，並提升擁有該發行商之信用卡的紅利等級，以便得到更多折扣"，如此便能同時達到提升客戶忠誠度的目標。

