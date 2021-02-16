#library(dummies) usual dummy

o_dummy_data<-function(x,label){
  library(tidyverse)
  
  x<-x %>% unlist() %>% as.character() %>% sort()
  
  o_dummy<-diag(rep(0,length(x)))
  o_dummy[upper.tri(o_dummy)]<-1
  o_dummy<-as.data.frame(o_dummy)
  o_dummy<-data.frame(x,o_dummy,stringsAsFactors=F)
  names(o_dummy)<-c(label,paste(label,x,sep = "_od_"))
  
  return(o_dummy)
}


add_o_dummy<-function(df,labels){

  library(tidyverse)
  library(foreach)
  
  foreach(label_=labels)%do%{
    if(df[[label_]] %>% unique() %>% length()>2){
      df<-df %>% left_join(o_dummy_data(x=df[label_] %>% unique()
                                        ,label = label_)
                           ,by=label_) %>% 
        select(-label_)
    }
  }
   
  return(df)
}

#test code----
df_test<-data.frame(x=letters[1:3]
                    ,y=letters[5:7]
                    ,z=1:3
                    ,stringsAsFactors = F)

add_o_dummy(df_test,labels = c("x","y"))
