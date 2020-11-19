setwd("X:/30～40主計関連/45随時更新/paid_predict")

if(!all(sapply(c("train_df","test_df","cv_no"),exists))){
  source("script/02_set_data.R",encoding = "UTF-8")
}

fun_calc<-function(train_cv,test_cv){
  train_dmat<-xgb.DMatrix(
    data=sparse.model.matrix(y~.
                             ,data = train_cv)
    ,label=train_cv$y
  )
  
  test_dmat<-xgb.DMatrix(
    data=sparse.model.matrix(y~.
                             ,data = test_cv)
    ,label=test_cv$y
  )
  
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
    data = train_dmat 
    ,nrounds = 10000
    ,nfold = 2
    ,params = param_base
    ,early_stopping_rounds = 50
    ,verbose = 0
  )
  
  xgb_model <- xgb.train(
    data = train_dmat
    ,nrounds = xgb_cv$best_iteration
    ,params = param_base
    ,watchlist = list(train = train_dmat, eval = test_dmat)
    ,verbose = 0
  )
  
  feature_imp<-xgb.importance(model=xgb_model)
  
  predict_test_cv<-predict(xgb_model,newdata = test_dmat)
  
  list(
    imp=feature_imp
    ,model=xgb_model
    ,y_test_cv=sum(test_cv$y)
    ,diff_test_cv=sum(test_cv$y-predict_test_cv)
    ,eval=RMSE(y_pred=predict_test_cv,y_true=test_cv$y)
  )
}


cores<-detectCores(logical = FALSE)
cluster<-makeCluster(cores)
registerDoParallel(cluster)


result_cv<-foreach(test_k=seq(cv_no$K))%do%{
  cat(as.character(Sys.time()),":cv=",test_k,"/",cv_no$K,"\n")
  train_cv<-train_df %>% 
    dplyr::slice(
      with(cv_no,subsets[which!=test_k])
    )
  
  test_cv<-train_df %>% 
    dplyr::slice(
      with(cv_no,subsets[which==test_k])
    )
  
  fun_calc(train_cv,test_cv)
}

stopCluster(cluster)

sapply(result_cv, function(x)x$eval)

foreach(i=seq(length(result_cv)))%do%{
  result_cv[[i]]$imp
} %>% bind_rows(.id="cv_no") %>% 
  group_by(cv_no) %>% 
  mutate(rank=row_number()) %>% ungroup() %>% 
  group_by(Feature) %>% 
  summarise(rank_m=mean(rank),rank_high=min(rank),rank_low=max(rank)
            ,Gain_m=mean(Gain),Gain_high=min(Gain),Gain_low=max(Gain)) %>% ungroup() %>% 
  arrange(desc(Gain_m)) %>% 
  view

test_dmat<-xgb.DMatrix(
  data=sparse.model.matrix(y~.
                           ,data = test_df)
  ,label=test_df$y
)


pred_list<-foreach(i=seq(length(result_cv)))%do%{
  predict(result_cv[[i]]$model,newdata = test_dmat)
}


pred_summary <- data.frame(cv=sprintf("cv_%d",seq(cv_no$K))
                           ,pred=sapply(pred_list, sum)
                           ,rmse=sapply(pred_list, function(x)RMSE(x,test_df$y))
                           ,stringsAsFactors = F) 
bind_rows(pred_summary
          ,pred_summary %>% 
            group_by() %>% 
            summarise(pred=mean(pred)) %>% ungroup() %>% 
            mutate(cv="cv_mean"
                   ,rmse=RMSE(pred_list %>% bind_cols() %>% rowMeans(),test_df$y))
          )%>% 
  mutate(diff=pred-sum(test_df$y)
         ,colour=cv=="cv_mean") %>% 
  ggplot(aes(x=cv,y=pred,fill=colour))+
  geom_bar(stat = "identity",alpha=0.5)+
  geom_text(y=0,hjust=0,aes(label=sprintf("%s(diff:%s,rmse:%s)",comma(pred),comma(diff),round(rmse,digits = 3))))+
  geom_hline(yintercept = sum(test_df$y),linetype="dashed",colour="red")+
  annotate(geom = "text"
           ,label=sprintf("real=%s",comma(sum(test_df$y)))
           ,y=sum(test_df$y),x="cv_mean"
           ,hjust=0
           ,vjust=-0.5
           ,angle=-90
           ,colour="red")+
  theme(legend.position = "none")+coord_flip()

pred <- data.frame(pred=pred_list %>% bind_cols() %>% rowMeans()
                   ,real=test_df$y)

pred %>% 
  pivot_longer(cols = c(pred,real),names_to = "type",values_to = "value") %>% 
  mutate(value=round(value)) %>% 
  group_by(value,type) %>% 
  summarise(N=n()) %>% ungroup() %>% 
  pivot_wider(names_from = type,values_from = N,values_fill = list(N=0)) %>% 
  view

pred %>% 
  sample_n(size = 10000) %>% 
  ggplot(aes(x=real,y=pred))+geom_point()
