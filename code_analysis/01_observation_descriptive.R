#01. Descriptive statistics of behavioral observation data

df.l1 <- lapply(df.l, get_dfy)

alldata <- df.l1 %>% map2_df(.,names(.),~mutate(.x,cohort=.y))


#### convert to socialmatrices
df.l1 %>%
  map(~ filter(., Actor!="Start" & Recipient!="End")) %>%
  map(~ select(., Actor,Recipient)) %>% 
  map(get_wl_matrix) -> cohortmatrices


# number of days of observation
day<-alldata %>%
  group_by(cohort) %>%
  summarize(maxday = max(day)) %>%
  ungroup() %>%
  .$maxday %>% as.numeric() 
day
day%>%summary()

# number of observation hours
hours<-alldata %>%
  group_by(cohort) %>%
  summarize(totalobs = length(unique(uniqueobs)))
hours
hours%>%
  ungroup() %>%
  .$totalobs %>%
  sum

#total contests
alldata %>% 
  filter(Actor!="Start" & Actor!="End") %>%
  nrow()

#total hours PER day
alldata %>%
  filter(Actor=="End") %>% group_by(cohort) %>% top_n(1,uniqueobs) %>% as.data.frame() %>%
  summarise(ave_total_obs_hour=mean(uniqueobs/day))