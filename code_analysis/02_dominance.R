#02. Dominance

### Landau's h, Triangle transitivity
set.seed(100)
cohort_devries <- lapply(cohortmatrices, devries)
set.seed(100)
cohort_ttritest <- lapply(cohortmatrices, ttri_test)
set.seed(100)
cohort_dctest <- lapply(cohortmatrices, dc_test)


summary(unlist(sapply(cohort_devries, "[", 1)))
unlist(sapply(cohort_devries, "[", 2))

summary(unlist(sapply(cohort_ttritest, "[", 2)))
unlist(sapply(cohort_ttritest, "[", 3))

summary(unlist(sapply(cohort_dctest, "[", 7)))
unlist(sapply(cohort_dctest, "[", 1))



### Despotism
cohort<-names(cohortmatrices)
despot<-cohortmatrices %>% 
  map(despotism %>% as.data.frame) %>%  
  map2_df(.,names(.),~mutate(.x,cohort=.y)) %>% 
  group_by(cohort) %>% 
  top_n(1,value)

despot<-unique(despot) %>% as.data.frame #because of cohort 56
colnames(despot)<-c("despotism","cohort")
summary(despot$despotism)


### Glicko Ratings
#### Final glicko ratings
df.l1 %>%
  map(~ filter(., Actor!="Start" & Recipient!="End")) %>%
  map(~ arrange(., day,hour,minute,secs)) %>%
  map(~ mutate(., event=row_number(), score=1)) %>% 
  map(~ select(., event,Actor,Recipient,score)) %>%
  map(~ map_if(., is.factor,as.character) ) %>%
  map(~as.data.frame(.,stringsAsFactors =FALSE))%>%
  map(~ glicko(., cval=3, history=T)) %>%
  map(~ .$ratings) %>%
  map(~ mutate(., 
               grank=as.numeric(rownames(.)), 
               winprop = Win/sum(Win), 
               loseprop = Loss/sum(Loss), 
               winloseratio = Win/(Win+Loss))) -> finalglickos

glicko<-finalglickos %>% 
  map2_df(.,names(.),~mutate(.x,cohort=.y)) %>% 
  mutate(mouse=as.character(Player))


#### Glicko plot
df.l1 %>%
  map(~ filter(., Actor!="Start" & Recipient!="End")) %>%
  map(~ arrange(., day,hour,minute,secs)) %>%
  map(~ mutate(., event=row_number(), score=1)) %>% 
  map(~ select(., event,Actor,Recipient,score)) %>%
  map(~ map_if(., is.factor,as.character) ) %>%
  map(~as.data.frame(.,stringsAsFactors =FALSE)) %>% 
  map(~plotglicko(., cval=2, ylim1=1500, ylim2=3200, linewd=.5))->glickoplot

grid.arrange(glickoplot[[1]],glickoplot[[2]],glickoplot[[3]],glickoplot[[4]],
             glickoplot[[5]],glickoplot[[6]],glickoplot[[7]],glickoplot[[8]],nrow=4)

### David's score
david<-cohortmatrices %>%  
  map(compete::ds %>% as.data.frame) %>% 
  map(~mutate(.,mouse=row.names(.))) %>% 
  map2_df(.,names(.),~mutate(.x,cohort=.y)) %>% 
  mutate(david_score=value) %>% select(-value)


### ISI rank 
isi_new<-cohortmatrices %>%  
  map(compete::isi13) 

isi_old<-cohortmatrices %>%  
  map(compete::isi98) 

isinew<-isi_new %>% 
  lapply(.,function(x) x$best_order) %>% 
  as.data.frame() %>% 
  mutate(isirank=c(1:12)) %>% 
  gather(cohort,mouse,1:8)

isiold<-isi_old %>% 
  lapply(.,function(x) x$best_order) %>% 
  as.data.frame() %>% 
  mutate(isirank_old=c(1:12)) %>% 
  gather(cohort,mouse,1:8)

 
#### Incorporate all info into one dataframe 
behavior_all<-left_join(glicko,david) %>% 
  left_join(.,isinew) %>% 
  left_join(.,isiold) %>% 
  left_join(.,bw %>% mutate(mouse=as.character(mouse))) %>% 
  left_join(.,despot)


behavior<-behavior_all%>% 
  select(cohort,mouse,grank,isirank,isirank_old,david_score,
         Rating,despotism,winprop,loseprop,winloseratio) %>% 
  mutate(subjectid=paste(cohort,
                         paste("M",str_pad(as.numeric(mouse),2,pad="0"),sep=""),sep="-")) %>% 
  mutate(domgroup=ifelse(grank==1,"Alpha",ifelse(Rating>=2200,"Subdominant","Subordinate"))) %>% 
  mutate(domgroup2=ifelse(isirank==1,"Alpha",ifelse(david_score>0,"Subdominant","Subordinate")))

 

