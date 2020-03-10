## Basic Step Statistics Project Based Learning
## Bank Marketing
library(precrec)
library(ggplot2)
library(rpart.plot)
library(rpivotTable)
library(pscl)
library(car)
library(rpart)
library(rpart.plot)
library(randomForest)

# 出力したCSVデータを読み込めます
bank_marketing_train <- read.csv("bank_marketing_train.csv")
kekka_test <- read.csv("bank_marketing_test.csv")

# 統計量
summary(bank_marketing_train)
colnames(bank_marketing_train)


#前処理するならここで
#とりあえず、ファクターにしました
bank_marketing_train$age<-as.factor(bank_marketing_train$age)
bank_marketing_train$marital<-as.factor(bank_marketing_train$marital)
bank_marketing_train$education<-as.factor(bank_marketing_train$education)
bank_marketing_train$default<-as.factor(bank_marketing_train$default)
bank_marketing_train$housing<-as.factor(bank_marketing_train$housing)
bank_marketing_train$loan<-as.factor(bank_marketing_train$loan)
bank_marketing_train$contact<-as.factor(bank_marketing_train$contact)
bank_marketing_train$day_of_week<-as.factor(bank_marketing_train$day_of_week)
bank_marketing_train$poutcome<-as.factor(bank_marketing_train$poutcome)
bank_marketing_train$y<-as.factor(bank_marketing_train$y)

kekka_test$job<-as.factor(kekka_test$job)
kekka_test$marital<-as.factor(kekka_test$marital)
kekka_test$education<-as.factor(kekka_test$education)
kekka_test$default<-as.factor(kekka_test$default)
kekka_test$housing<-as.factor(kekka_test$housing)
kekka_test$loan<-as.factor(kekka_test$loan)
kekka_test$contact<-as.factor(kekka_test$contact)
kekka_test$day_of_week<-as.factor(kekka_test$day_of_week)
kekka_test$poutcome<-as.factor(kekka_test$poutcome)
kekka_test$y<-as.factor(kekka_test$y)

# 型のチェック
sapply(bank_marketing_train, class)
sapply(kekka_test,class)

#yは1と0にしない？
#bank_marketing_train$y<-ifelse(bank_marketing_train$y=="no",0,1)

#年齢帯で分ける
bank_marketing_train$age_range <- ifelse(bank_marketing_train$age<27,"under26",ifelse(bank_marketing_train$age>60,"upper60","addult"))
table(bank_marketing_train$y,bank_marketing_train$age_range)
bank_marketing_train$age_range<-as.factor(bank_marketing_train$age_range)

#pdaysは接触がなかろうが0回でよくない？
bank_marketing_train$pdays <- ifelse(bank_marketing_train$pdays==999,0,bank_marketing_train$pdays)
head(bank_marketing_train)

#年齢帯で分ける
kekka_test$age_range <- ifelse(kekka_test$age<27,"under26",ifelse(kekka_test$age>60,"upper60","addult"))
table(kekka_test$y,kekka_test$age_range)
kekka_test$age_range<-as.factor(kekka_test$age_range)

#pdaysは接触がなかろうが0回でよくない？
kekka_test$pdays <- ifelse(kekka_test$pdays==999,0,kekka_test$pdays)
head(kekka_test)

#全体のデータをtableで確認
table(bank_marketing_train$y)
table(bank_marketing_train$job)
table(bank_marketing_train$poutcome)
y_marital <- table(bank_marketing_train$y,bank_marketing_train$marital)
y_marital
y_education <- table(bank_marketing_train$y,bank_marketing_train$education)
y_education
y_default <- table(bank_marketing_train$y,bank_marketing_train$default)
y_default
y_contact <- table(bank_marketing_train$y,bank_marketing_train$contact)
y_contact
y_campaign <- table(bank_marketing_train$y,bank_marketing_train$campaign)
y_campaign
y_pdays <- table(bank_marketing_train$y,bank_marketing_train$pdays)
y_pdays
y_previous <- table(bank_marketing_train$y,bank_marketing_train$previous)
y_previous
y_poutcome <- table(bank_marketing_train$y,bank_marketing_train$poutcome)
y_poutcome

barplot(table(bank_marketing_train$job))
barplot(y_marital,legend = TRUE,col=c("royalblue3","brown3"))

#ピポットテーブル これ超便利
#install.packages("rpivotTable")
rpivotTable(bank_marketing_train)

# データの数の確認
# y_under27_data <- subset(bank_marketing_train, bank_marketing_train$age < 27)
# y_between_data <- subset(bank_marketing_train, (bank_marketing_train$age > 27) & (bank_marketing_train$age < 60))
# y_upper60_data <- subset(bank_marketing_train, bank_marketing_train$age > 60)
# table(y_under27_data$y) #1942  80%:20% 
# table(y_between_data$y) #30117 90%:10%
# table(y_upper60_data$y) #771   55%:45%

# トレーニングとテストを分けておこう
temp_rows<-dim(bank_marketing_train)[1] # 行数33744行
temp_idx<-sample(c(1:temp_rows), size = temp_rows*0.7 )# 7割を抽出
temp_data<-bank_marketing_train[temp_idx, ]# モデリング用データに分割
test_data<-bank_marketing_train[-temp_idx, ]# テスト用データに分割
rownames(temp_data) <- 1:nrow(temp_data)# 番号の振り直しと思われる
train_rows<-dim(temp_data)[1]# モデリング用の行数23620
train_idx <- sample(c(1:train_rows), size = train_rows*0.7 )　# さらに7割にする
train_data<-temp_data[train_idx, ]　# モデリングデータ
validation_data<-temp_data[-train_idx, ]　# ヴァリデーションデータ

# 不均衡データを調節してやってみる
y_data <- subset(bank_marketing_train, bank_marketing_train$y == "yes")
n_data <- subset(bank_marketing_train, bank_marketing_train$y == "no")
y_data_rows <- dim(y_data) # 3796件
n_data_rows <- dim(n_data) # 29948件
temp_idx_n<-sample(c(1:n_data_rows), size = y_data_rows )# ydata_rowsと同じ数
data<-rbind(n_data[temp_idx_n,],y_data)# これが不均衡データの全量になる

temp_rows_2<-dim(data)[1] # 行数7592行
temp_idx_2<-sample(c(1:temp_rows_2), size = temp_rows_2*0.7 )# 7割を抽出
temp_data_2<-data[temp_idx_2, ]# モデリング用データに分割
test_data_2<-data[-temp_idx_2, ]# テスト用データに分割
rownames(temp_data_2) <- 1:nrow(temp_data_2)# 番号の振り直しと思われる
train_rows_2<-dim(temp_data_2)[1]# モデリング用の行数23620
train_idx_2 <- sample(c(1:train_rows_2), size = train_rows_2*0.7 )　# さらに7割にする
train_data_2<-temp_data_2[train_idx_2, ]　# モデリングデータ
validation_data_2<-temp_data_2[-train_idx_2, ]　# ヴァリデーションデータ

# 通話時間以外の変数でモデル作成
base_model<-glm(y~.-age -duration -emp.var.rate -cons.price.idx
                -cons.conf.idx -euribor3m -nr.employed,data = bank_marketing_train,family = "binomial")
#base_model<-glm(y~.-duration ,data = train_data,family = "binomial")
summary(base_model)
step(base_model)

# ランダムフォレストでも調べてみる


# フルモデルのAUCを計算　ヴァリデーションデータでやる
pred_score<-predict(base_model, validation_data, type = "response")
# ROCの可視化ができるやつあるんじゃん
sscurves <- evalmod(scores = pred_score, labels = validation_data$y)
autoplot(sscurves)
auc(sscurves)# ROCの表示

#オッズ比可視化
par(cex.axis=0.5,las=1,mar=c(3, 7, 1, 1))
sort(exp(base_model$coefficients))# オッズ順に並べ替えしている
barplot(sort(exp(base_model$coefficients)),
        horiz=T, main = "Odds Ratio")# オッズの
abline(v=1)# なんだろ

# 今ある行
colnames(bank_marketing_train)

# 成約率の低い大人だけで絞ってみる
data_ad <- subset(bank_marketing_train, bank_marketing_train$age_range == "addult")
table(data_ad$age_range)
head(data_ad)

# 前回のキャンペーンに不参加だけで絞ってみる
data_pre <- subset(bank_marketing_train, bank_marketing_train$poutcome == "nonexistent")
table(data_pre$poutcome)
head(data_pre)

# ちょっとロジスティック回帰をする
model<-glm(y~age_range + job + marital + education + default + contact + campaign + pdays + previous 
           + poutcome, data=bank_marketing_train, family="binomial")
model<-glm(y~age + job + marital + education + default + contact + campaign + pdays + previous +
            loan + housing + poutcome, data=data_ad, family="binomial")
model<-glm(y~age_range + job + marital + education + default + contact + campaign + 
             loan + housing , data=data_pre, family="binomial")
summary(model)
step(model)
par(cex.axis=0.5,las=1,mar=c(3, 7, 1, 1))
sort(exp(model$coefficients))# オッズ順に並べ替えしている
barplot(sort(exp(model$coefficients)),
        horiz=T, main = "Odds Ratio")# オッズの
abline(v=1)# 1をこれで引いてる

vif(model)

pR2(model)


# ランダムフォレストでも確認しておく
#install.packages("rpart.plot")

model_rf = randomForest(y~age_range + job + marital + education + default + contact + campaign + pdays + previous
                     + poutcome, data = bank_marketing_train)
model_rf = randomForest(y~.-age_range -duration -emp.var.rate -cons.price.idx
                        -cons.conf.idx -euribor3m -nr.employed, data = bank_marketing_train)
model_rf #モデル直接やると確かめてくれて、OBB estimate of  error rate: 10.37%が間違っていた率になる。
importance(model_rf) # 主要な要因が何かをチェック(ジニ係数)
varImpPlot(model_rf) # 視覚化する
summary(model_rf)

# 決定木で分析

bank_marketing_train_rpart <- subset(bank_marketing_train, select = c(-age,-day_of_week,-duration,-emp.var.rate,-cons.price.idx,-cons.conf.idx,-euribor3m,-nr.employed))
head(bank_marketing_train_rpart)
model_r = rpart(y ~ ., data = bank_marketing_train_rpart,control = rpart.control(cp=0.0009,maxdepth = 5))
model_r
summary(model_r)

#install.packages("rpart.plot")
rpart.plot(model_r, extra = 4)

bank_marketing_train_rpart <- subset(bank_marketing_train, select = c(-pdays,-previous,
                                                                      -age,-day_of_week,-duration,-emp.var.rate,-poutcome,-cons.price.idx,-cons.conf.idx,-euribor3m,-nr.employed))
head(bank_marketing_train_rpart)
model_r = rpart(y ~ ., data = bank_marketing_train_rpart,control = rpart.control(cp=0.0005,maxdepth = 5))
model_r
summary(model_r)

#install.packages("rpart.plot")
rpart.plot(model_r, extra = 4)

#######################################################
# モデルの作成

sales <- 5000
cost <- 500

# 不均衡調整前
model<-glm(y~age_range + job + marital + education + default + contact + campaign + pdays + previous 
           + poutcome, data=train_data, family="binomial")
# 不均衡調整後
model_rebalnce<-glm(y~age_range + job + marital + education + default + contact + campaign + pdays + previous 
           + poutcome, data=train_data_2, family="binomial")

# RFもやってみる　不均衡調整前
model_rf = randomForest(y~age_range + job + marital + education + default + contact + campaign + pdays + previous
                        + poutcome, data = train_data)
model_rf

# RF靄ってみる　不均衡調整後
model_rf2 = randomForest(y~age_range + job + marital + education + default + contact + campaign + pdays + previous
                        + poutcome, data = train_data, sampsize=c(1000,1000))
model_rf2

#summary
summary(model)
summary(model_rebalnce)
summary(model_rf)
summary(model_rf2)

par(cex.axis=0.5,las=1,mar=c(3, 7, 1, 1))
sort(exp(model$coefficients))# オッズ順に並べ替えしている
barplot(sort(exp(model$coefficients)),
        horiz=T, main = "Odds Ratio1")# オッズの
abline(v=1)# 1をこれで引いてる

par(cex.axis=0.5,las=1,mar=c(3, 7, 1, 1))
sort(exp(model_rebalnce$coefficients))# オッズ順に並べ替えしている
barplot(sort(exp(model_rebalnce$coefficients)),
        horiz=T, main = "Odds Ratio2")# オッズの
abline(v=1)# 1をこれで引いてる

# フルモデルのAUCを計算　ヴァリデーションデータでやる
pred_score<-predict(model, validation_data, type = "response")
sscurves <- evalmod(scores = pred_score, labels = validation_data$y)
autoplot(sscurves)
auc(sscurves)# ROCの表示

pred_score_re<-predict(model_rebalnce, validation_data, type = "response")
sscurves_re <- evalmod(scores = pred_score_re, labels = validation_data_2$y)
autoplot(sscurves_re)
auc(sscurves_re)# ROCの表示


# 精度でないかな
step(model)# これが一番精度いいらしい。いいのかー。

#####################################################
#予測を出す(テストデータで)
ypred1<-predict(model, newdata = test_data, type="response")
hist(ypred1)
ypred2<-predict(model_rebalnce, newdata = test_data, type="response")
hist(ypred2)

#ランダムフォレスト
ypred3<-predict(model_rf,  newdata = test_data, type = "prob")[,2]
ypred4<-predict(model_rf2, newdata = test_data, type = "prob")[,2]

#####################################################

#尤度関数を定義します
yuudo_1<-function(x){
  ypred_test_flag<-ifelse(ypred1 > x, 1, 0)
  conf_mat<-table(test_data$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}

#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_1,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

#尤度関数を定義します
yuudo_2<-function(x){
  ypred_test_flag<-ifelse(ypred2 > x, 1, 0)
  conf_mat<-table(test_data$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}
#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_2,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

#尤度関数を定義します
yuudo_3<-function(x){
  ypred_test_flag<-ifelse(ypred3 > x, 1, 0)
  conf_mat<-table(test_data$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}
#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_3,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

#尤度関数を定義します
yuudo_4<-function(x){
  ypred_test_flag<-ifelse(ypred4 > x, 1, 0)
  conf_mat<-table(test_data$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}
#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_4,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

######################################################
# とりあえず閾値
ypred_test_flag_1<-ifelse(ypred1 > 0.09219783, 1, 0)
ypred_test_flag_2<-ifelse(ypred2 > 0.4148965, 1, 0)
ypred_test_flag_3<-ifelse(ypred3 > 0.06288678, 1, 0)
ypred_test_flag_4<-ifelse(ypred4 > 0.3988359, 1, 0)

#confusion matrixを作成
conf_mat1<-table(test_data$y, ypred_test_flag_1)
conf_mat1
conf_mat2<-table(test_data$y, ypred_test_flag_2)
conf_mat2
conf_mat3<-table(test_data$y, ypred_test_flag_3)
conf_mat3
conf_mat4<-table(test_data$y, ypred_test_flag_4)
conf_mat4

attack_num1<-conf_mat1[3] + conf_mat1[4]# フラグが1だったものを計算
expected_revenue1<-conf_mat1[4] * sales# 収益予想の計算
your_cost1<-attack_num1 * cost# コストの計算
roi1 = expected_revenue1 - your_cost1
print(roi1)# 儲けの計算

attack_num2<-conf_mat2[3] + conf_mat2[4]# フラグが1だったものを計算
expected_revenue2<-conf_mat2[4] * sales# 収益予想の計算
your_cost2<-attack_num2 * cost# コストの計算
roi2 = expected_revenue2 - your_cost2
print(roi2)# 儲けの計算

#####################################################
#予測を出す(本物で！）
ypred1<-predict(model, newdata = kekka_test, type="response")
hist(ypred1)
ypred2<-predict(model_rebalnce, newdata = kekka_test, type="response")
hist(ypred2)

#ランダムフォレスト
ypred3<-predict(model_rf,  newdata = kekka_test, type = "prob")[,2]
ypred4<-predict(model_rf2, newdata = kekka_test, type = "prob")[,2]

#####################################################
# 閾値最適解はどれぐらい差があったんでしょうね
#尤度関数を定義します
yuudo_1<-function(x){
  ypred_test_flag<-ifelse(ypred1 > x, 1, 0)
  conf_mat<-table(kekka_test$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}

#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_1,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

#尤度関数を定義します
yuudo_2<-function(x){
  ypred_test_flag<-ifelse(ypred2 > x, 1, 0)
  conf_mat<-table(kekka_test$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}
#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_2,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

#尤度関数を定義します
yuudo_3<-function(x){
  ypred_test_flag<-ifelse(ypred3 > x, 1, 0)
  conf_mat<-table(kekka_test$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}
#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_3,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

#尤度関数を定義します
yuudo_4<-function(x){
  ypred_test_flag<-ifelse(ypred4 > x, 1, 0)
  conf_mat<-table(kekka_test$y, ypred_test_flag)
  conf_mat
  attack_num<-conf_mat[3] + conf_mat[4]# フラグが1だったものを計算
  expected_revenue<-conf_mat[4] * 5000# 収益予想の計算
  your_cost<-attack_num * 500# コストの計算
  roi = expected_revenue - your_cost
  return(roi)
}
#上記の尤度関数を最大にするprobを求めます
optimise.output <- optimise(yuudo_4,interval = c(0,1),maximum = TRUE)
#尤度関数が最大となるprobをみてみましょう
prob <- optimise.output
prob

######################################################
# とりあえず閾値
ypred_test_flag_1<-ifelse(ypred1 > 0.092, 1, 0)
ypred_test_flag_2<-ifelse(ypred2 > 0.41, 1, 0)
ypred_test_flag_3<-ifelse(ypred3 > 0.062, 1, 0)
ypred_test_flag_4<-ifelse(ypred4 > 0.40, 1, 0)

#confusion matrixを作成
conf_mat1<-table(kekka_test$y, ypred_test_flag_1)
conf_mat1
conf_mat2<-table(kekka_test$y, ypred_test_flag_2)
conf_mat2
conf_mat3<-table(kekka_test$y, ypred_test_flag_3)
conf_mat3
conf_mat4<-table(kekka_test$y, ypred_test_flag_4)
conf_mat4

sscurves1 <- evalmod(scores = ypred4, labels = kekka_test$y)
autoplot(sscurves1)
auc(sscurves1)# ROCの表示
