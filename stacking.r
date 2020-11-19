setwd("X:/30～40主計関連/45随時更新/paid_predict")

# read file ---------------------------------------------------------------

if(!exists("train_df")){
  train_df <- readRDS("try_20201119/data/train_df.rds")  
}

if(!exists("test_df")){
  test_df <- readRDS("try_20201119/data/test_df.rds")  
}

if(!exists("cv_no")){
  cv_no <- readRDS("try_20201119/data/cv_no.rds")  
}

# model1 ------------------------------------------------------------------

data_x_bi <- data.matrix(subset(train_df,select = -y))
data_y_bi <- if_else(train_df[,"y"]>=1,1,0)

param_base_bi<-list(
  booster = 'gbtree'
  ,objective = 'binary:logistic'#'binary:logistic'
  ,eval_metric = 'logloss'#'logloss',mae,rmse
  ,eta = 0.1
  ,max_depth = 5L
  ,min_child_weight = 1
  ,colsample_bytree = 0.8
  ,subsample = 0.8
  ,gamma = 0
  ,alpha = 0
  ,lambda = 1
  ,seed=100
)

xgb_cv_bi <- xgb.cv(
  data = data_x_bi
  ,label = data_y_bi
  ,nrounds = 10000
  ,nfold = 2
  ,params = param_base
  ,early_stopping_rounds = 50
  ,verbose = 0
)

xgb_model_bi <- xgboost(
  data = data_x_bi
  ,label = data_y_bi
  ,nrounds = xgb_cv_bi$best_iteration
  ,params = param_base_bi
  ,verbose = 0
)

data_x <- data.matrix(
  subset(
    data.frame(train_df
               ,pre=predict(xgb_model_bi,newdata = data_x_bi))
    ,select = -y)
)

data_y <- train_df$y

param_base<-list(
  booster = 'gbtree'
  ,objective = 'reg:linear'#'binary:logistic'
  ,eval_metric = 'rmse'#'logloss',mae,rmse
  ,eta = 0.1
  ,max_depth = 5L
  ,min_child_weight = 1
  ,colsample_bytree = 0.8
  ,subsample = 0.8
  ,gamma = 0
  ,alpha = 0
  ,lambda = 1
  ,seed=100
)

xgb_cv <- xgb.cv(
  data = data_x
  ,label = data_y
  ,nrounds = 10000
  ,nfold = 2
  ,params = param_base
  ,early_stopping_rounds = 50
  ,verbose = 0
)

xgb_model <- xgboost(
  data = data_x
  ,label = data_y
  ,nrounds = xgb_cv$best_iteration
  ,params = param_base
  ,verbose = 0
)

RMSE(predict(xgb_model,newdata = data.matrix(
  subset(
    data.frame(test_df
               ,pre=predict(xgb_model_bi
                            ,newdata = data.matrix(
                              subset(test_df,select = -y)
                            )
               ))
    ,select = -y)
))
,test_df$y)


imp_bi <- xgb.importance(model=xgb_model_bi)

imp <- xgb.importance(model=xgb_model)


