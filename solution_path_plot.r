library(tidyverse)
library(plotly)

solution_path_plot<-function(cv_model){
  
  add_horizontal_line<-function(p,value,y_min,y_max,name="",linetype="dot"){
    p<-p %>% add_segments(x =value
                          ,xend =value
                          ,y=y_min
                          ,yend=y_max
                          ,line = list(dash=linetype)
                          ,color = I('gray')
                          ,alpha=0.5
                          ,showlegend=F)
    
    if(name!=""){
      p %>% layout(annotations = list(x = value
                                      ,y = y_max
                                      ,text = name)) %>% return()
    }else{return(p)}
  }
  
  beta<-bind_cols(
    lambda=cv_model$lambda
    ,cv_model$glmnet.fit$beta %>% as.matrix() %>% t() %>% as.data.frame()) %>% 
    gather(key,value,-lambda) %>% 
    mutate(lm_lambda=log(lambda))
  
  beta %>%
    plot_ly(x=~lm_lambda
            ,y=~value
            ,color=~key
            ,colors="Spectral"
            ,text=~paste0('log(lambda)=',lm_lambda
                          ,'<br>,name=',key
                          ,'<br>,coef=',value)
            ,hoverinfo="text"
            ,type = 'scatter'
            ,mode='lines'
            ,alpha=1) %>% 
    add_horizontal_line(value = log(cv_model$lambda.min)
                        ,y_min=min(beta$value)
                        ,y_max=max(beta$value)
                        ,name="min"
                        ,linetype = "dot") %>% 
    add_horizontal_line(value = log(cv_model$lambda.1se)
                        ,y_min=min(beta$value)
                        ,y_max=max(beta$value)
                        ,name="1se"
                        ,linetype = "dash") %>% 
    layout(xaxis = list(title="log(lambda)")
           ,yaxis=list(title="coef")) %>% return()
}
