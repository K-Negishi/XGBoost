# 発現ばらつきをXgboostで予測 ---------------------------------------------------
# 20181111
# Kohei
# 多クラス分類


# 使用するパッケージ ---------------------------------------------------------------
# caret packageはこちら(https://bit.ly/2AYnTlt)
library(dplyr)
library(tidyr)
require(Matrix)
library(xgboost)
library(ggplot2)



# 使用するデータの読み込み ----------------------------------------------------------------
raw.data <- readRDS("Input/RDS/t_data_group.rds")



# num.intron == 0 & intronless == 0のものはすべて除去 ------------------------------
# この条件に該当する遺伝子は、他の特徴量においてもNAを多く含んでいた
raw.data <- raw.data %>% 
  filter(!is.na(num.intron) | intronless != 0)

raw.data %>% 
  select(Group) %>% 
  unlist() %>% 
  as.factor() %>% 
  summary()



# trainとtestに分割 -----------------------------------------------------------
temp <- sample(1:nrow(raw.data), round(0.7 * nrow(raw.data)))
train <- raw.data[temp,,]
test <- raw.data[-temp,,]

train %>% 
  select(Group) %>% 
  unlist() %>% 
  as.factor() %>% 
  summary()
test %>% 
  select(Group) %>% 
  unlist() %>% 
  as.factor() %>% 
  summary()




# 欠損値などの処理 ----------------------------------------------------------------
train %>% summary()
test %>% summary()

train %>% dim()
test %>% dim()

sum(is.na(train))
sum(is.na(test))




# 各Groupから100個ずつランダムに遺伝子を持ってくる
# https://uribo.hatenablog.com/entry/2015/10/12/164129
train.rndm <- c()
for (i in 1:9) {
  tmp <- train %>% 
    filter(Group == i) %>% 
    sample_n(size = 100, replace = T)
  
  train.rndm <- bind_rows(train.rndm, tmp)
}

train <- train.rndm
train %>% 
  select(Group) %>% 
  unlist() %>% 
  as.factor() %>% 
  summary()



# Xgboostへかけるための準備 --------------------------------------------------------
# http://puyokw.hatenablog.com/entry/2015/04/29/000557

# XGBoost用のデータへ変換
y <- train$Group  # 目的変数
y <- as.integer(y) - 1  # xgboost で既定されいるクラスは 0 base


train.x <- train %>% 
  select(-Group)  # 説明変数
test.x <- test %>% 
  select(-Group)
x <- bind_rows(train.x, test.x)  # xgboost を使うときのため
x <- as.matrix(x)

trind <- 1:length(y) # 先程定義したx の中の訓練データを指すのに使う
teind <- (nrow(train.x)+1):nrow(x) # 先程定義したx の中の検証用データを指すのに使う




# パラメータの設定(必要最小限)
set.seed(131) # 固定シードで試す
param <- list("objective" = "multi:softmax", # 多クラスの分類で各クラスに所属する確率を求める
              "eval_metric" = "mlogloss", # 損失関数の設定
              "num_class" = 9 # class がいくつ存在するのか
)


# 予測をする前に最適な木の数を探す
k　<-　round(1+log2(nrow(train.x)))
cv.nround <- 100 #search
bst.cv <- xgb.cv(param = param, data = x[trind,], label = y,  nfold = k, nrounds=cv.nround)






# 予測(XGBoostにかける) ---------------------------------------------------------
t.min <- bst.cv$evaluation_log[,4] %>% 
  unlist() %>% 
  which.min()
nround <- t.min + 1

bst <- xgboost(param = param, data = x[trind,], label = y, nrounds = nround)
pred <- predict(bst, x[teind,])





# 予測結果 --------------------------------------------------------------------
table(test$Group, pred + 1)

pdat <- tibble(ans = as.character(test$Group),
               pred = as.character(pred + 1)) %>% 
  group_by(ans, pred) %>% 
  summarize(count = n())

g <- ggplot(pdat, aes(x = pred, y = ans, size = count)) +
  geom_point()
plot(g)




# Feature Importance ------------------------------------------------------
# 変数重要度を求める
imp <- xgb.importance(feature_names = colnames(test)[-1], model = bst)
print(imp)

xgb.plot.importance(imp) # グラフで表示





