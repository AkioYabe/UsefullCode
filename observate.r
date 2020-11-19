library(pdp)
library(vip)

system.time(
  p <- partial(xgb_model_bi
               ,pred.var = "DiseaseCode1"
               ,ice = FALSE
               ,center = TRUE
               ,plot = TRUE
               ,rug = TRUE
               ,alpha = 1
               ,plot.engine = "ggplot2"
               ,train = data_x_bi)  
)
p
p$data %>% 
  mutate(sig=sig(yhat))

test_df %>%
  mutate(y=if_else(y>0,1,0)) %>% 
  group_by(DiseaseCode1) %>% 
  summarise(y=sum(y),N=n()) %>% 
  mutate(ratio=y/N) %>% view

system.time(
  p2 <- partial(xgb_model_bi
                ,pred.var = "op_001"
                ,ice = FALSE
                ,center = TRUE
                ,plot = TRUE
                ,rug = TRUE
                ,alpha = 1
                ,plot.engine = "ggplot2"
                ,train = data_x_bi)  
)
p2

p2$data %>% 
  mutate(sig=sig(yhat))


sig <- function(x){1/(1+exp(-x))}

test_df %>%
  mutate(y=if_else(y>0,1,0)) %>% 
  group_by(op_001) %>% 
  summarise(y=sum(y),N=n()) %>% 
  mutate(ratio=y/N)

train_df %>%
  mutate(y=if_else(y>0,1,0)) %>% 
  group_by(op_001) %>% 
  summarise(y=sum(y),N=n()) %>% 
  mutate(ratio=y/N)


# pdpを一括処理 ----------------------------------------------------------------

system.time(
  p <- foreach(i=1:16)%do%{
    cat("now:",i,"=",imp$Feature[i],"-----",as.character(Sys.time()),"\n")
    partial(xgb_model
            ,pred.var = imp$Feature[i]
            ,ice = FALSE
            ,center = TRUE
            ,plot = TRUE
            ,rug = TRUE
            ,alpha = 1
            ,plot.engine = "ggplot2"
            ,train = data_x) 
  }  
)

gridExtra::marrangeGrob(p,ncol = 4,nrow = 4)

system.time(
  p1 <- foreach(i=c("DiseaseCode1","DiseaseCode2"))%do%{
    cat("now:",i,"-----",as.character(Sys.time()),"\n")
    partial(xgb_model
            ,pred.var = i
            ,ice = FALSE
            ,center = TRUE
            ,plot = TRUE
            ,rug = TRUE
            ,alpha = 1
            ,plot.engine = "ggplot2"
            ,train = data_x) 
  }  
)

gridExtra::marrangeGrob(p1,ncol = 2,nrow = 1)

# 変数間の相関関係を確認 -------------------------------------------------------------

anp_km <- kmeans(train_df$anp,10)

anp_km_label <- round(anp_km$centers)
names(anp_km_label) <- seq(length(anp_km$centers))
anp_km_label

train_df %>% 
  mutate(cluster=as.character(km_label[as.character(km$cluster)])) %>% 
  ggplot(aes(x=anp,y=..density..,colour=cluster))+
  geom_line(stat="density")

train_df %>% 
  mutate(anp_clust=anp_km_label[as.character(anp_km$cluster)]) %>% 
  group_by(anp_clust,age_ins) %>% 
  summarise(N=n()) %>% ungroup() %>% 
  pivot_wider(names_from = anp_clust,values_from = N,values_fill = list(N=0)) %>%
  column_to_rownames("age_ins") %>% 
  as.matrix() %>% 
  heatmap(Rowv = NA,Colv = NA)

train_df %>% 
  group_by(age_ins,s1) %>% 
  summarise(N=n()) %>% ungroup() %>% 
  pivot_wider(names_from = s1,values_from = N,values_fill = list(N=0)) %>%
  column_to_rownames("age_ins") %>% 
  as.matrix() %>% 
  heatmap(Rowv = NA,Colv = NA)


train_df %>% 
  group_by(age_ins,prefecture) %>% 
  summarise(N=n()) %>% ungroup() %>% 
  pivot_wider(names_from = prefecture,values_from = N,values_fill = list(N=0)) %>%
  column_to_rownames("age_ins") %>% 
  as.matrix() %>% 
  heatmap(Rowv = NA,Colv = NA)

train_df %>% 
  group_by(old_201903_1,disease_8800) %>% 
  summarise(N=n()) %>% ungroup() %>% 
  pivot_wider(names_from = disease_8800,values_from = N,values_fill = list(N=0)) %>%
  column_to_rownames("old_201903_1") %>% 
  as.matrix() %>% 
  heatmap(Rowv = NA,Colv = NA)


# 密度曲線 --------------------------------------------------------------------

scott <- function(x){
  ceiling((max(x)-min(x)))/grDevices::nclass.scott(x)
}

plotly::ggplotly(
  train_df %>%
    filter(prefecture %in% c(1,13,14,27,28,40)) %>% 
    mutate(prefecture=as.character(prefecture)) %>% 
    ggplot(aes(x=age_ins,y=..density..,colour=prefecture,fill=prefecture))+
    # geom_histogram(position = "dodge",alpha=0.2)+
    geom_line(stat="density",size=1)  
)

  
train_df %>%
  ggplot(aes(x=old_201903_1,y=..density..,colour=as.character(disease_8800)))+
  geom_line(stat="density",size=1)  



# ヴァイオリンプロット --------------------------------------------------------------

train_df %>%
  mutate_at(vars(disease_8800),as.character) %>% 
  ggplot(aes(x=disease_8800,y=old_201903_1))+
  geom_violin(trim = F)+
  geom_boxplot(width=0.1,fill="forestgreen",alpha=0.5)


# work --------------------------------------------------------------------

train_df %>% 
  group_by(s1_MA10) %>% 
  summarise(y=sum(y>0),N=n()) %>% 
  ungroup() %>% 
  mutate(ratio=y/N) %>% view

test_df %>% 
  mutate(pred=predict(xgb_model,newdata = data.matrix(subset(test_df_tmp,select = -y)))) %>% 
  group_by(DiseaseCode1,gender_ins) %>% 
  summarise(pred=mean(pred)) %>% view
  ungroup() %>% 
  ggplot(aes(x=DiseaseCode1,y=pred,colour=gender_ins,group=gender_ins))+
  geom_line()

train_df %>% 
  filter(DiseaseCode1=="152306")
