# 発現ばらつきをXgboostで予測 ---------------------------------------------------
# 20181111
# Kohei
# 発現ばらつき(SDoCV)を予測


# 使用するパッケージ ---------------------------------------------------------------
# caret packageはこちら(https://bit.ly/2AYnTlt)
library(dplyr)
library(tidyr)
require(Matrix)
library(xgboost)



# 使用するデータの読み込み ----------------------------------------------------------------
raw.data <- readRDS("Input/RDS/t_data_sdocv.rds")



# num.intron == 0 & intronless == 0のものはすべて除去 ------------------------------
# この条件に該当する遺伝子は、他の特徴量においてもNAを多く含んでいた
raw.data <- raw.data %>% 
  filter(!is.na(num.intron) | intronless != 0)




# trainとtestに分割 -----------------------------------------------------------
temp <- sample(1:nrow(raw.data), round(0.7 * nrow(raw.data)))
train <- raw.data[temp,,]
test <- raw.data[-temp,,]





# 欠損値などの処理 ----------------------------------------------------------------
train %>% summary()
test %>% summary()

train %>% dim()
test %>% dim()

sum(is.na(train))
sum(is.na(test))




# Xgboostへかけるための準備 --------------------------------------------------------
# スパース行列に変換
sparse.train <- sparse.model.matrix(SDoCV~.-1, data = train)
sparse.test <- sparse.model.matrix(SDoCV~.-1, data = test)

# スパース行列から、よく分からん行列へ
d.train <- xgb.DMatrix(data=sparse.train, label=train$SDoCV) 
d.test <- xgb.DMatrix(data=sparse.test) 





# 予測(Xgboostにかける) -----------------------------------------------------------------
bst <- xgboost(data = sparse.train, label = train$SDoCV, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10, objective = "reg:linear")
pred.rslt <- predict(bst, d.test)
tmp <- predict(bst, d.train)



# RMSEを計算 -----------------------------------------------------------------
# RMSEとは(https://mathwords.net/rmsemae)
sqrt(sum((test$SDoCV - pred.rslt)^2) / length(pred.rslt))
plot(test$SDoCV, pred.rslt)

sqrt(sum((train$SDoCV - tmp)^2) / length(tmp))
plot(train$SDoCV, tmp)




# Feature Importance ------------------------------------------------------
# 変数重要度を求める
imp <- xgb.importance(feature_names = colnames(test)[-1], model = bst)
print(imp)

xgb.plot.importance(imp, 
                    main="Feature Importance: SDoCV") # グラフで表示



