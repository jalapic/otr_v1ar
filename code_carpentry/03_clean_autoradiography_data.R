### V1aR 
v1ar<-read.csv("data_raw/brain/V1aR_exp_recovery.csv",stringsAsFactors = F)

df1<-v1ar %>% 
  mutate(cohort=as.character(str_sub(Label,2,3)),
         mouse=as.character(str_sub(Label,6,7))) %>% 
  mutate(cohort=as.integer(cohort)) %>% 
  left_join(.,cohort_info) %>% 
  mutate(cohort=cohortx,
         section_order=paste(Section, Order, sep="")) %>% 
  mutate(subjectid=paste(cohort,paste("M",mouse,sep=""),sep="-")) %>% 
  select(cohort,mouse,Mean, Region,Order,Section,Film,subjectid,section_order)
  

## Get the averaged then subtracted data for each brain region 
     
df2<-df1 %>% 
  group_by(subjectid,section_order,Region,Film) %>%
  summarise(mean_density=mean(Mean,na.rm = T))


df2a<-df2 %>% filter(Region!="control")
df2b<-df2 %>% 
  filter(Region=="control") %>% 
  mutate(Control=mean_density) %>% 
  ungroup() %>% 
  select(-Region) %>% select(-mean_density)  

v1ar_od<-left_join(df2a,df2b) %>% 
  mutate(od=mean_density-Control) %>% 
  group_by(subjectid,Region,Film) %>% 
  summarize(mean_od=mean(od,na.rm = T)) %>% 
  ungroup() 

v1ar_od
str(v1ar_od)
table(v1ar_od$Region)

v1ar_od<-v1ar_od %>% 
  mutate(region=ifelse(Region=="DB","DBr",
                              ifelse(Region=="LSc","LSr",
                                     ifelse(Region=="LSr_d","LSc",
                                            ifelse(Region=="LSr_v","LSc_v",Region))))) %>% 
  filter(region!="LSc_v") %>% 
  filter(region!="DBr") %>% 
  select(Film,subjectid,region,mean_od) %>% 
  mutate(dpm_mg_TE=mean_od*1565100*0.00232231360252137/1000) %>% #to convert to TE dpm/mg from optical density)
  mutate(receptor="V1aR") 

table(v1ar_od$region)
head(v1ar_od)
  

     
write.csv(v1ar_od,"data_clean/V1aR.csv",row.names = F)
  

### OTR 
otr<-read.csv("data_raw/brain/OTR_exp_recovery.csv",stringsAsFactors = F) %>% 
  mutate(Label=replace(Label,Label=="C11-M02.tif:MEA_c_1a","C11-M02.tif:MEA_c_1")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:MEA_c_1a","C11-M04.tif:MEA_c_4")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:MEA_l_1","C11-M04.tif:MEA_l_4")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:ACo_l_1","C11-M04.tif:ACo_l_4")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:MEA_c_2a","C11-M04.tif:MEA_c_5")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:MEA_l_2","C11-M04.tif:MEA_l_5")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:ACo_l_2","C11-M04.tif:ACo_l_5")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:MEA_c_3a","C11-M04.tif:MEA_c_6")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:MEA_l_3","C11-M04.tif:MEA_l_6")) %>% 
  mutate(Label=replace(Label,Label=="C11-M04.tif:ACo_l_3 ","C11-M04.tif:ACo_l_6")) %>% 
  mutate(Label=replace(Label,Label=="C83-M11.tif:MEA_c_3a","C83-M11.tif:MEA_c_3"))
  
head(otr)

otrx<-otr %>% mutate(cohort=as.integer(as.character(str_sub(Label,2,3))),
                     mouse=as.character(str_sub(Label,6,7)),
                   region=str_sub(Label,13,15),
                   section=str_extract(str_sub(Label,8),"[[:digit:]]+")) %>% 
  mutate(region=ifelse(region=="LS_","LS",
                       ifelse(region=="Ls_","LS",region))) %>% 
  mutate(region=ifelse(region=="Aco","ACo",region)) %>% 
  mutate(side_control=ifelse(section=="10",str_sub(Label,-4,-4),str_sub(Label,-3,-3))) %>% 
  select(-Label) %>% 
  left_join(.,cohort_info) %>% 
  mutate(cohort=cohortx) %>% 
  mutate(subjectid=paste(cohort,paste("M",mouse,sep=""),sep="-"))

table(otrx$region)
table(otrx$section)
table(otrx$side)

control<-otrx %>% 
  filter(side_control=="c") %>% 
  mutate(control_Mean=Mean) %>% 
  mutate(control_region=region) %>% 
  select(Film,subjectid,control_region,section,control_Mean) 

df2<-otrx %>% 
  filter(side_control!="c") %>% 
  mutate(control_region=ifelse(region=="ACo","MEA",
                               ifelse(region=="PiC","LS",region))) %>% 
  mutate(side=side_control) %>% 
  select(-side_control)

otr_od<-left_join(df2,control) %>% 
  mutate(od=Mean-control_Mean)  %>% 
  group_by(Film,subjectid,region) %>% 
  summarise(mean_od=mean(od,na.rm = T)) %>% 
  mutate(region=ifelse(region=="ACo","COApl",
                       ifelse(region=="Nac","NAcc",region))) %>% 
  mutate(dpm_mg_TE=mean_od*1565100*0.0019990334022113/1000) %>%
  mutate(receptor="OTR") %>% 
  arrange(subjectid)

table(otr_od$region)

#additional data for BLA region 
otr_bla<-read.csv("data_raw/brain/OTR_exp_recovery_bla.csv",stringsAsFactors = F) 

df1<-otr_bla %>% 
  mutate(cohort=as.character(str_sub(Label,2,3)),
         mouse=as.character(str_sub(Label,6,7))) %>% 
  mutate(cohort=as.integer(cohort)) %>% 
  left_join(.,cohort_info) %>% 
  mutate(cohort=cohortx,
         section_order=paste(Section, Order, sep="")) %>% 
  mutate(subjectid=paste(cohort,paste("M",mouse,sep=""),sep="-")) %>% 
  select(cohort,mouse,Mean, Region,Order,Section,Film,subjectid,section_order)


## Get the averaged then subtracted data for each brain region 

df2<-df1 %>% 
  group_by(subjectid,section_order,Region,Film) %>%
  summarise(mean_density=mean(Mean,na.rm = T))


df2a<-df2 %>% filter(Region!="control")
df2b<-df2 %>% 
  filter(Region=="control") %>% 
  mutate(Control=mean_density) %>% 
  ungroup() %>% 
  select(-Region) %>% select(-mean_density)  

otr_od_bla<-left_join(df2a,df2b) %>% 
  mutate(od=mean_density-Control) %>% 
  group_by(subjectid,Region,Film) %>% 
  summarize(mean_od=mean(od,na.rm = T)) %>% 
  ungroup() 

otr_od_bla

table(otr_od_bla$Region)

otr_od_bla<-otr_od_bla %>% 
  mutate(region=ifelse(Region=="BLA","BLAp",Region)) %>% 
  select(Film,subjectid,region,mean_od) %>% 
  mutate(dpm_mg_TE=mean_od*1565100*0.0019990334022113/1000) %>% #to convert to TE dpm/mg from optical density)
  mutate(receptor="OTR") 

otr_od
otr_od_bla

otr_od<-rbind(as.data.frame(otr_od),as.data.frame(otr_od_bla))

##
write.csv(otr_od,"data_clean/OTR.csv",row.names=F)

