library(tidyverse)
library(xgboost)
library(foreach)

options(na.action='na.pass')

calc_xgb <- function(train, predict_type) {
  # predict_type:"linear","binary","multi"
  
  # parameter ---------------------------------------------------------------
  
  if(predict_type=="multi"){
    num_class <- length(unique(train$y))
  }
  
  param_base <- list(
    booster = 'gbtree'
    ,eta = 0.1
    ,max_depth = 5
    ,min_child_weight = 1
    ,colsample_bytree = 0.8
    ,subsample = 0.8
    ,gamma = 0
    ,alpha = 0
    ,lambda = 1
  )
  
  if(predict_type=="linear"){
    param_base$objective <- 'reg:linear'
    param_base$eval_metric <- 'rmse'
  }else if(predict_type=="binary"){
    param_base$objective <- 'binary:logistic'
    param_base$eval_metric <- 'logloss'
  }else if(predict_type=="multi"){
    param_base$objective <- 'multi:softmax'
    param_base$eval_metric <- 'mlogloss'
    param_base$num_class <- num_class#クラス数
  }
  
  # fit ---------------------------------------------------------------------
  train_mt <- model.matrix(y ~ ., data = train)
  train_dmat <- xgb.DMatrix(train_mt,label = train$y)
  
  xgb_cv <- xgb.cv(
    data = train_dmat
    ,nrounds = 10000
    ,nfold = 2
    ,params = param_base
    ,early_stopping_rounds = 100
    ,verbose = 0
  )
  
  xgb_model <- xgb.train(
    data = train_dmat
    ,nrounds = xgb_cv$best_iteration
    ,params = param_base
    ,verbose = 0
  )
  
  imp_tbl <- xgb.importance(model=xgb_model)
  
  return(
    list(train_mt=train_mt
         ,train_dmat=train_dmat
         ,xgb_model=xgb_model
         ,imp_tbl=imp_tbl)
  )
}


calc_stacking <- function(train, test, predict_type, cv_num = 4) {
  
  test_mt <- model.matrix(y ~ ., data = test)
  test_dmat <- xgb.DMatrix(test_mt,label = test$y)
  
  cv_list<-cvTools::cvFolds(nrow(train),K=cv_num)
  
  train_new <- list(NULL)
  test_pred <- list(NULL)
  
  foreach(cv_no=seq(cv_num))%do%{
    cat(as.character(Sys.time()),":cv=",cv_no,"/",cv_num,"\n")
    train_cv<-train %>% 
      dplyr::slice(
        with(cv_list,subsets[which!=cv_no])
      )
    
    test_cv<-train %>% 
      dplyr::slice(
        with(cv_list,subsets[which==cv_no])
      )
    
    result_cv <- calc_xgb(train = train_cv
                          ,predict_type = predict_type)
    
    test_cv_mt <- model.matrix(y ~ ., data = test_cv)
    test_cv_dmat <- xgb.DMatrix(test_cv_mt,label = test_cv$y)
    
    train_new[[sprintf("cv_%d",cv_no)]] <- cbind(test_cv
                                                 ,pred=predict(result_cv$xgb_model,test_cv_dmat)
                                                 ,stringsAsFactors = F)
    
    test_pred[[sprintf("cv_%d",cv_no)]] <- predict(result_cv$xgb_model,test_dmat)
  }
  
  
  train_new <- bind_rows(train_new)
  test_new <- cbind(test
                    ,pred=test_pred %>% bind_cols() %>% rowMeans()
                    ,stringsAsFactors = F)
  
  return(
    list(train_new=train_new
         ,test_new=test_new)
  )
}

# # test
# df <- iris %>% rename(y=Species) %>% mutate(y=as.integer(y)-1)
# 
# test_tf<-caTools::sample.split(seq(nrow(df)),SplitRatio = .2)#testの割合をSplitRatioに指定する
# train <- df %>% filter(!test_tf)
# test <- df %>% filter(test_tf)
# 
# result_xgb <- calc_xgb(train = train
#                        ,predict_type = "multi"
# )
# 
# #confusion matrix
# test_mt <- model.matrix(y ~ ., data = test)
# test_dmat <- xgb.DMatrix(test_mt,label = test$y)
# 
# table(predict(result_xgb$xgb_model,test_dmat)
#       ,test$y)

# new_data <- calc_stacking(train,test,predict_type,cv_num = 2)
# 
# with(new_data$test_new,table(y,pred))
