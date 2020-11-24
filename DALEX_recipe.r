library(DALEX)

predict_logit <- function(model, x) {
  raw_x <- predict(model, x)
  exp(raw_x)/(1 + exp(raw_x))
}
logit <- function(x) exp(x)/(1+exp(x))

explainer_xgb_train <- explain(model = result_xgb$xgb_model
                         ,data = result_xgb$train_mt
                         ,y=train$y
                         # ,predict_function = predict_logit#2値分類の場合
                         # ,link = logit#2値分類の場合
                         ,type = "classification"#"regression","classification"
                         ,label = "xgboost")

explainer_xgb_test <- explain(model = result_xgb$xgb_model
                               ,data = test_mt
                               ,y=test$y
                              # ,predict_function = predict_logit#2値分類の場合
                              # ,link = logit#2値分類の場合
                              ,type = "classification"#"regression","classification"
                               ,label = "xgboost")


# model_performance -------------------------------------------------------

(pf_train <- model_performance(explainer_xgb_train))
(pf_test <- model_performance(explainer_xgb_test))

plot(pf_train, geom = "lift")
plot(pf_train, geom = "roc")
plot(pf_train, geom = "boxplot")
plot(pf_train, geom = "histogram")
plot(pf_train, geom = "prc")

plot(pf_test, geom = "lift")
plot(pf_test, geom = "roc")
plot(pf_test, geom = "boxplot")
plot(pf_test, geom = "histogram")
plot(pf_test, geom = "prc")


# variable importance -----------------------------------------------------

#B=10:デフォルト,B=1:単一順列ベース
#N=1000:デフォルト,N=NULL:all data
vi_train <- model_parts(explainer_xgb_train,  B = 10, N = NULL)
vi_test <- model_parts(explainer_xgb_test,  B = 10, N = NULL)

plot(vi_train)
plot(vi_test)


# partial dependence prot -------------------------------------------------

pro_train <- model_profile(explainer_xgb_train
                           ,type = "partial"#"partial","conditional","accumulated"
                           ,variables = result_xgb$imp_tbl$Feature
                           ,k=3)
pro_test <- model_profile(explainer_xgb_test
                          ,variables = result_xgb$imp_tbl$Feature
                          ,k=3)

plot(pro_train,alpha=0.5)  
plot(pro_test,alpha=0.5)  
