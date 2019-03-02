
cohort_info<-read.csv("data_raw/bodyweight.csv",stringsAsFactors = F) %>% 
  select(cohort) %>% 
  unique() %>% 
  mutate(cohortx=LETTERS[order(cohort)])
  
bw <- read.csv("data_raw/bodyweight.csv",stringsAsFactors = F) %>% 
  left_join(.,cohort_info) %>% 
  mutate(cohort=cohortx) %>% 
  select(cohort,mouse,start,end)



write.csv(bw,"data_clean/bodyweight_clean.csv")
