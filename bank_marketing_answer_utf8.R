#ロジスティック回帰

## ワーキングディレクトリを設定しましょう
## Session -> Set Working Directory -> Choose Directory

## まずデータを読み込みます
bank_marketing_train <- read.csv("bank_marketing_train.csv")
# bank_marketing_train<-read.csv("data/HR_comma_sep.csv")

#学習データとテストデータに分割しておきます（あとで予測のデモのため）
train_idx<-sample(c(1:dim(bank_marketing_train)[1]), size = dim(bank_marketing_train)[1]*0.7)
train<-bank_marketing_train[train_idx, ]
test<-bank_marketing_train[-train_idx, ]

# Stratified Sampling 
target_0 <- subset(train, y=="no")
target_1 <- subset(train, y=="yes")
target_0

head(target_0)
head(target_1)
summary(target_0)
summary(target_1)
dim(target_0)
dim(target_1)

head(bank_marketing_train)

target_0_srs_ind <- sample(c(1:dim(target_0)[1]), replace=FALSE, size=3000)
target_0_srs <- target_0[target_0_srs_ind, ]
dim(target_0_srs)
summary(target_0_srs)


train_srs <- rbind(target_0_srs,target_1)
dim(train_srs)

## ロジスティック回帰をやってみましょう
#bank_marketing_train.lr<-glm(y~emp.var.rate+
#                          nr.employed+
#                          poutcome+
#                          previous+
#                          contact+
#                          job
#                          , data=train, family="binomial")

#bank_marketing_train.lr<-glm(y~
#                               nr.employed+
#                               poutcome+
#                               previous+
#                               contact+
#                               job
#                             , data=train, family="binomial")

train_srs.lr<-glm(y~
                    nr.employed+
                    poutcome+
                    previous+
                    contact+
                    job
                  , data=train_srs, family="binomial")

## 線形回帰と同じようにsummaryで各種統計値が見れます。
summary(train_srs.lr)


## オッズ比の計算
exp(train_srs.lr$coefficients)


#McFaddenの疑似決定係数
install.packages("pscl")
library(pscl)
pR2(bank_marketing_train.lr)


## (ご参考) F検定の代わりに尤度比検定
null_train_srs.lr<-glm(y~1, data=train_srs, family="binomial")
anova(train_srs.lr, null_train_srs.lr, test = "LRT")
step(train_srs.lr)

## 予測
#Log Oddsを予測する
ypred<-predict(train_srs.lr, newdata = test, type="link")
ypred

#確率を予測する
#ypred<-predict(train_srs.lr, newdata = test, type="response")

# use test data from web 
bank_marketing_test <- read.csv("bank_marketing_test.csv")

ypred<-predict(train_srs.lr, newdata = bank_marketing_test, type="response")
hist(ypred)

#もし確率をフラグに変換したい場合は、閾値を決める必要がある
mean(ypred)
ypred_flag<-ifelse(ypred > 0.5, 1, 0)

# confusion matrixを作る
conf_mat<-table(bank_marketing_test$y, ypred_flag)
conf_mat

# 正解率
accuracy<-(conf_mat[1] + conf_mat[4]) /(conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
accuracy

# 適合率(precision)
precision<-conf_mat[1] / (conf_mat[1] + conf_mat[2])
precision

# 再現率(Recall)
recall<-conf_mat[1]/ (conf_mat[1] + conf_mat[3]) 
recall

# F値(F - Measure)
f_measure <- 2*precision*recall/(precision+recall)
f_measure

