setwd("X:/30～40主計関連/45随時更新/paid_predict")
# install.packages("cvTools")
library(tidyverse)
library(caTools)
library(cvTools)
library(Matrix)
library(xgboost)
library(foreach)
library(MLmetrics)
library(doParallel)
library(scales)
library(ggrepel)

options(na.action='na.pass')

pol_data<-read_rds("data/rds/pol_data.rds")

paid_data<-read_rds("data/rds/paid_data.rds")

assessment_data<-read_rds("../InsRisk_S_Base/Rate/assessment_data/data/rds/data.rds")


df<-pol_data %>%
  #y
  left_join(paid_data %>% 
              filter(base_ym=="202003") %>%
              group_by(pno) %>% 
              summarise_at(vars(days),sum) %>% ungroup()
            ,by="pno") %>% 
  #old
  left_join(paid_data %>% 
              filter(base_ym %in% c("201903","201803")) %>%
              group_by(pno,base_ym,code_paid) %>% 
              summarise_at(vars(days),sum) %>% ungroup() %>% 
              pivot_wider(names_from = c(base_ym,code_paid)
                          ,names_prefix = "old_"
                          ,values_from = days
                          ,values_fill = list(days=0))
            ,by="pno") %>% 
  #disease
  left_join(paid_data %>% 
              filter(base_ym %in% c("201903","201803")) %>%
              distinct(pno,code_class_disease) %>% mutate(n=1) %>% 
              pivot_wider(names_from = code_class_disease
                          ,names_prefix = "disease_"
                          ,values_from = n
                          ,values_fill = list(n=0))
            ,by="pno") %>% 
  #op
  left_join(paid_data %>% 
              filter(base_ym %in% c("201903","201803")) %>%
              distinct(pno,code_class_op) %>% mutate(n=1) %>% 
              pivot_wider(names_from = code_class_op
                          ,names_prefix = "op_"
                          ,values_from = n
                          ,values_fill = list(n=0))
            ,by="pno") %>% 
  #assessment_data
  left_join(assessment_data %>% 
              mutate_at(vars(Type
                             ,starts_with("DiseaseCode")
                             ,starts_with("SpPart"))
                        ,as.numeric)
            ,by="pno") %>% 
  mutate_at(vars(days
                 ,starts_with("old_")
                 ,starts_with("disease_")
                 ,starts_with("op_")
                 ,Type
                 ,starts_with("DiseaseCode")
                 ,starts_with("SpPart"))
            ,.funs=list(~coalesce(.,0)))


zero_var<-which(0==(df %>% 
                      mutate_if(is.character,as.factor) %>% 
                      mutate_all(as.numeric) %>% 
                      sapply(var)))

df_ana<-df %>% 
  select(-zero_var,-pno,y=days) %>% 
  mutate_all(as.numeric)

set.seed(71)

test_tf<-sample.split(seq(nrow(df_ana)),SplitRatio = .2)

train_df<-df_ana %>% filter(!test_tf)
test_df<-df_ana %>% filter(test_tf)

cv_no<-cvFolds(nrow(train_df),K=4)

#分割されたデータの確認
bind_rows(
  train_df %>% 
    mutate(row_num=row_number()) %>% 
    left_join(data.frame(row_num=cv_no$subsets
                         ,type=sprintf("train_%d",cv_no$which)
                         ,stringsAsFactors = F)
              ,by="row_num") %>% 
    group_by(type) %>% 
    summarise(y=sum(y)) %>% ungroup() %>% 
    mutate()
  ,data.frame(type="test"
              ,y=sum(test_df$y)
              ,stringsAsFactors = F)
) %>% 
  mutate(colour=type=="test") %>% 
  ggplot(aes(x=type,y=y,fill=colour)) +
  geom_bar(stat="identity",alpha=0.5) +
  geom_text(hjust=1,aes(label=comma(y)))+
  theme(legend.position = "none")+coord_flip()
