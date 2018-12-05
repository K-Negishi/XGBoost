###################################################################
# 20180317
# XGBoostの練習
###################################################################

library(xgboost)
require(Matrix)
library(dplyr)
library(mlr)
library(data.table)

RawData <- fread("titanic.txt")

temp <- sample(1:nrow(RawData), 800)
Train <- RawData[temp,,]
Test <- RawData[-temp,,]

Train[is.na(Train)] <- 0
Test[is.na(Test)] <- 0
TEST <- Test
TEST$survived <- rep(20, times=nrow(TEST))


sparse_matrix <- sparse.model.matrix(survived~.-1, data = Train)
sTEST <- sparse.model.matrix(survived~.-1, data = TEST)
sparse_matrix_test <- sparse.model.matrix(survived~.-1, data = Test)

dtrain <- xgb.DMatrix(data=sparse_matrix, label=Train$survived) 
sdTEST <- xgb.DMatrix(data=sTEST)
dtest <- xgb.DMatrix(data=sparse_matrix_test) 


bst <- xgboost(data = sparse_matrix,label = Train$survived, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")
TestRes <- predict(bst, dtest)
TESTres <- predict(bst, sdTEST)

TestRes[TestRes < 0.5] <- 0
TestRes[TestRes >= 0.5] <- 1
sum(TestRes == Test$survived)/length(Test$survived) 

TESTres[TESTres < 0.5] <- 0
TESTres[TESTres >= 0.5] <- 1
sum(TESTres == Test$survived)/length(Test$survived) 

sum(TESTres == TestRes) == length(TestRes)


















params <- list(booster = "gbtree", objective = "binary:logistic",
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1,
               subsample=1, colsample_bytree=1)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 79, 
                   print.every.n = 10, early.stop.round = 10, 
                   aximize = F , eval_metric = "error")







n <- rnorm(500)
write.table(n, "20180419.txt", quote=F, append=F, col.names=F, row.names=F)






