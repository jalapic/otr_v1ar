#03. Bodyweight 
bwdf<-left_join(bw %>% 
                  mutate(mouse=as.character(mouse)),                  
                behavior) %>% 
  mutate(grankx=factor(grank,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"),ordered = T)) %>% 
  mutate(domgroupx=factor(domgroup,levels=c("Alpha","Subdominant","Subordinate",ordered=T)))

#starbw
cor<-bwdf %>% 
  filter(!is.na(start)) %>% 
  split(.$cohort) %>% 
  map(~ cor.test(.$start, .$grank, method="s",exact=FALSE))
#exact=FALSE, as original Spearman rank test cannot handle ties 
#Correct for FDR
bw.pval<-c(cor[[1]][3],cor[[2]][3],cor[[3]][3],cor[[4]][3],cor[[5]][3],cor[[6]][3]) #as always, there's gotta be smarter way but this is quicker than finding that out....
p.adjust(bw.pval,method="BH") 

