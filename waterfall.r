data <- bind_rows(event_value_all_old %>% 
            mutate_at(vars(risk_value),.funs = list(~.*-1))
          ,event_value_all_new) %>% 
  filter(event=="0002") %>% 
  rename(name=code_prod) %>% 
  group_by(event_name,name) %>%
  summarise(value=sum(risk_value)) %>% ungroup() %>% 
  mutate(end=cumsum(value)
         ,start=lag(end,default = 0)
         ,type=if_else(value>=0,"plus","minus"))

data <- bind_rows(data
          ,data %>% group_by(event_name) %>% summarise(value=sum(value)) %>% ungroup() %>% 
            mutate(name="total",end=value,start=0,type="total")) %>% 
  mutate(id=seq(1,n()))

ggplot(data,aes(x=id,fill=type))+
  geom_rect(aes(xmin=id-0.5,xmax=id+0.5
                ,ymin=end,ymax=start))+
  geom_text(aes(y=(start+end)/2
                ,label=million(value)))+coord_flip()+
  scale_y_continuous(labels = million)+
  scale_x_continuous(breaks = data$id,labels = data$name)+
  scale_fill_manual(values = c("plus"="#00BFC4","minus"="#F8766D","total"="#7CAE00"))+
  ggtitle(with(event_name_new,event_name[which(event=="0002")]))+
  theme(axis.title.x = element_blank()
        ,axis.title.y = element_blank()
        ,legend.position = "none")
