setwd("C:/Users/R_user/Documents/2class_model")

source("C:/Users/R_user/Documents/R_common/eval_metric.R")
source("C:/Users/R_user/Documents/R_common/ordinal_dummy.R")
source("C:/Users/R_user/Documents/R_common/glmnet_solution_path_plot.R")
source("C:/Users/R_user/Documents/R_common/graphical_lasso.R")
library(tidyverse)
df<-readRDS("data/df.rds")

set.seed(1982)
sample_<-sample(nrow(df),nrow(df)*.9)
df.train<-df[sample_,]
df.test<-df[-sample_,]

tmp<-df.train %>% mutate_if(is.character,as.factor) %>% mutate_all(as.numeric)
omega<-cov(tmp)
draw_glasso_graph(omega)

library(glmnet)
library(dummies)
library(doParallel)

#odummy glmnet----
##data set
x_od_<-df.train %>% select(gender,age,payment,rnd) %>% add_o_dummy(labels="age") %>% dummy.data.frame(sep="_") %>% as.matrix()
y_<-df.train$n

test_od_<-df.test %>% select(gender,age,payment,rnd) %>% add_o_dummy(labels="age") %>% dummy.data.frame() %>% as.matrix()

##calc
cores<-detectCores(logical=FALSE)
cluster<-makeCluster(cores)
registerDoParallel(cluster)

system.time(
  cv.model_od<-cv.glmnet(x=x_od_
                         ,y=y_
                         ,family = "binomial"
                         ,parallel = T
  )
)
stopCluster(cluster)

##plot
plot(cv.model_od)

solution_path_plot(cv.model_od)

#coef min
coef(cv.model_od,s="lambda.min") %>% exp()

##eval mae
pred_glmnet_od_min<-predict(cv.model_od,newx = test_od_,s="lambda.min",type="response")
(mae_glmnet_od_min<-eval_mae(y_obs = df.test$prob
               ,y_pred = pred_glmnet_od_min))

#coef 1se
coef(cv.model_od,s="lambda.1se") %>% exp()

##eval mae
pred_glmnet_od_1se<-predict(cv.model_od,newx = test_od_,s="lambda.1se",type="response")
mae_glmnet_od_1se<-eval_mae(y_obs = df.test$prob
                 ,y_pred = pred_glmnet_od_1se)


#offset----
##data set
df.train_summary<-df.train %>% 
  group_by(gender,age,payment,rnd) %>% 
  summarise_at(vars(n,N),sum) %>% ungroup() %>% as.data.frame(stringsAsFactors=F)


x_od2_<-df.train_summary %>% select(gender,age,payment,rnd) %>% add_o_dummy(labels="age") %>% dummy.data.frame(sep="_") %>% as.matrix()
x_od2_offset<-df.train_summary$N
y2_<-df.train_summary$n

##calc
cores<-detectCores(logical=FALSE)
cluster<-makeCluster(cores)
registerDoParallel(cluster)

system.time(
  cv.model_od2<-cv.glmnet(x=x_od2_
                         ,y=y2_
                         ,offset = log(x_od2_offset)
                         ,family = "poisson"
                         # ,parallel = T
  )
)
stopCluster(cluster)

#coef
coef(cv.model_od2,s="lambda.min") %>% exp()

#eval_mae
pred_glmnet_od2_min<-predict(cv.model_od2,newoffset=log(df.test$N),newx = test_od_,s="lambda.min",type="response")
(mae_glmnet_od2_min<-eval_mae(y_obs = df.test$prob
                             ,y_pred = pred_glmnet_od2_min))



#udummy glmnet----
x_<-df.train %>% select(gender,age,payment,rnd) %>% dummy.data.frame(sep="_") %>% as.matrix()
y_<-df.train$n
test_<-df.test %>% select(gender,age,payment,rnd) %>% dummy.data.frame(sep="_") %>% as.matrix()

##calc
cores<-detectCores(logical=FALSE)
cluster<-makeCluster(cores)
registerDoParallel(cluster)

system.time(
  cv.model<-cv.glmnet(x=x_
                      ,y=y_
                      ,family = "binomial"
                      ,parallel = T
                      )
)
stopCluster(cluster)

##plot
plot(cv.model)
solution_path_plot(cv.model)

#coef
coef(cv.model,s="lambda.min") %>% exp()
pred_glmnet_min<-predict(cv.model,newx = test_,s="lambda.min",type="response")
mae_glmnet_min<-eval_mae(y_obs = df.test$prob
                    ,y_pred = pred_glmnet_min)


pred_glmnet_1se<-predict(cv.model,newx = test_,s="lambda.1se",type="response")
mae_glmnet_1se<-eval_mae(y_obs = df.test$prob
                 ,y_pred = pred_glmnet_1se)


#offset----
##data set

x2_<-df.train_summary %>% select(gender,age,payment,rnd) %>% dummy.data.frame(sep="_") %>% as.matrix()
x2_offset<-df.train_summary$N
y2_<-df.train_summary$n

##calc
cores<-detectCores(logical=FALSE)
cluster<-makeCluster(cores)
registerDoParallel(cluster)

system.time(
  cv.model2<-cv.glmnet(x=x2_
                          ,y=y2_
                          ,offset = log(x2_offset)
                          ,family = "poisson"
                          ,parallel = T
  )
)
stopCluster(cluster)

#coef
coef(cv.model2,s="lambda.min") %>% exp()

#eval_mae
pred_glmnet2_min<-predict(cv.model2,newoffset=log(df.test$N),newx = test_,s="lambda.min",type="response")
(mae_glmnet2_min<-eval_mae(y_obs = df.test$prob
                              ,y_pred = pred_glmnet2_min))
