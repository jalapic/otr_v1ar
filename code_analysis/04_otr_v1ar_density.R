df<-rbind(as.data.frame(v1ar_od),as.data.frame(otr_od)) %>% 
  left_join(.,behavior)

## Plot first - but remember; the plot doesn't really reflect (1|cohort) and (1|Film) effects 
df %>% filter(receptor=="OTR") %>% 
  filter(subjectid!="C06-M07") %>% 
  filter(subjectid!="C12-M03") %>% 
  ggplot(.,aes(domgroup,dpm_mg_TE,color=domgroup))+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_colorblind()+
  xlab("Social status")+
  ylab("OTR density (dpm/mg TE)")+
  facet_wrap(~region)#+geom_text(aes(label=subjectid))


df %>% filter(receptor=="V1aR") %>% 
  filter(subjectid!="C06-M07") %>% 
  filter(subjectid!="C12-M03") %>% 
  ggplot(.,aes(domgroup,dpm_mg_TE,color=domgroup))+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_colorblind()+
  xlab("Social status")+
  ylab("V1aR density (dpm/mg TE)")+
  facet_wrap(~region)#+geom_text(aes(label=subjectid))


#### Individual across regions 
df %>% filter(receptor=="OTR") %>% 
  ggplot(., aes(region,dpm_mg_TE,color=subjectid,group=subjectid))+geom_point()+geom_line()

df %>% filter(receptor=="V1aR") %>% 
  ggplot(., aes(region,dpm_mg_TE,color=subjectid,group=subjectid))+geom_point()+geom_line()



## Regression analysis 

df2<-df %>% mutate(domgroup=factor(domgroup, levels=c("Alpha","Subdominant","Subordinate"))) %>% 
  mutate(domsub=ifelse(domgroup!="Subordinate","Dom","Sub")) %>% 
  filter(subjectid!="C06-M07") %>% #because this mouse got displaced 
  filter(subjectid!="C12-M03") #because this mouse got displaced 

table(df2$region)

## Fit 1: only social status group as a predictor

# fit1<-list()
# fit1$AON_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="AON"),control = list(adapt_delta=0.95))
# fit1$BLAp_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="BLAp"),control = list(adapt_delta=0.95))
# fit1$COApl_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="COApl"),control = list(adapt_delta=0.95))
# fit1$HDB_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="HDB"),control = list(adapt_delta=0.95))
# fit1$LPO_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LPO"),control = list(adapt_delta=0.95))
# fit1$LS_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LS"),control = list(adapt_delta=0.95))
# fit1$LSc_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LSc"),control = list(adapt_delta=0.95))
# fit1$LSr_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LSr"),control = list(adapt_delta=0.95))
# fit1$MEA_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="MEA"),control = list(adapt_delta=0.95))
# fit1$MS_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="MS"),control = list(adapt_delta=0.95))
# fit1$NAcc_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="NAcc"),control = list(adapt_delta=0.95))
# fit1$PiC_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="PiC"),control = list(adapt_delta=0.95))
# fit1$VDB_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="VDB"),control = list(adapt_delta=0.95))
# fit1$VP_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="VP"),control = list(adapt_delta=0.98))
# fit1$VTA_fit1<-brm(dpm_mg_TE~domgroup+(1|cohort)+(1|Film),data=df2 %>% filter(region=="VTA"),control = list(adapt_delta=0.95))
#saveRDS(fit1,"data_clean/stat_RDS/fit1_delta095.RDS")
 
fit1<-readRDS("data_clean/stat_RDS/fit1_delta095.RDS")



## Fit 2: social status group and despotism as predictors

# fit2<-list()
# fit2$AON_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="AON"),control = list(adapt_delta=0.95))
# fit2$BLAp_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="BLAp"),control = list(adapt_delta=0.95))
# fit2$COApl_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="COApl"),control = list(adapt_delta=0.95))
# fit2$HDB_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="HDB"),control = list(adapt_delta=0.95))
# fit2$LPO_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LPO"),control = list(adapt_delta=0.95))
# fit2$LS_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LS"),control = list(adapt_delta=0.95))
# fit2$LSc_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LSc"),control = list(adapt_delta=0.95))
# fit2$LSr_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="LSr"),control = list(adapt_delta=0.95))
# fit2$MEA_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="MEA"),control = list(adapt_delta=0.95))
# fit2$MS_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="MS"),control = list(adapt_delta=0.95))
# fit2$NAcc_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="NAcc"),control = list(adapt_delta=0.95))
# fit2$PiC_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="PiC"),control = list(adapt_delta=0.95))
# fit2$VDB_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="VDB"),control = list(adapt_delta=0.95))
# fit2$VP_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="VP"),control = list(adapt_delta=0.95))
# fit2$VTA_fit2<-brm(dpm_mg_TE~domgroup+despotism+(1|cohort)+(1|Film),data=df2 %>% filter(region=="VTA"),control = list(adapt_delta=0.95))
# saveRDS(fit2,"data_clean/stat_RDS/fit2_despot_delta095.RDS")

fit2<-readRDS("data_clean/stat_RDS/fit2_despot_delta095.RDS")

## Model comparison 
###Adding despotism does not help and also summaries indicate despotism does not have an effect

# loo<-list()
# loo$loo_AON<-loo(fit1$AON_fit1,fit2$AON_fit2,reloo = TRUE)
# loo$loo_BLAp<-loo(fit1$BLAp_fit1,fit2$BLAp_fit2,reloo = TRUE)
# loo$loo_COApl<-loo(fit1$COApl_fit1,fit2$COApl_fit2,reloo = TRUE)
# loo$loo_HDB<-loo(fit1$HDB_fit1,fit2$HDB_fit2,reloo = TRUE)
# loo$loo_LPO<-loo(fit1$LPO_fit1,fit2$LPO_fit2,reloo = TRUE)
# loo$loo_LS<-loo(fit1$LS_fit1,fit2$LS_fit2,reloo = TRUE)
# loo$loo_LSc<-loo(fit1$LSc_fit1,fit2$LSc_fit2,reloo = TRUE)
# loo$loo_LSr<-loo(fit1$LSr_fit1,fit2$LSr_fit2,reloo = TRUE)
# loo$loo_MEA<-loo(fit1$MEA_fit1,fit2$MEA_fit2,reloo = TRUE)
# loo$loo_MS<-loo(fit1$MS_fit1,fit2$MS_fit2,reloo = TRUE)
# loo$loo_NAcc<-loo(fit1$NAcc_fit1,fit2$NAcc_fit2,reloo = TRUE)
# loo$loo_PiC<-loo(fit1$PiC_fit1,fit2$PiC_fit2,reloo = TRUE)
# loo$loo_VDB<-loo(fit1$VDB_fit1,fit2$VDB_fit2,reloo = TRUE)
# loo$loo_VP<-loo(fit1$VP_fit1,fit2$VP_fit2,reloo = TRUE)
# loo$loo_VTA<-loo(fit1$VTA_fit1,fit2$VTA_fit2,reloo = TRUE)
# saveRDS(loo,"data_clean/stat_RDS/loo.RDS")

# loo<-readRDS("data_clean/stat_RDS/loo.RDS")
# 
# loo[[1]] 
# loo[[2]] 
# loo[[3]] 
# loo[[4]] 
# loo[[5]] 
# loo[[6]] 
# loo[[7]] 
# loo[[8]] 
# loo[[9]]
# loo[[10]] 
# loo[[11]] 



#### Summarize estimates

est<-rbind(
  getall2(fit1$AON_fit1,"AON"),
  getall2(fit1$BLAp_fit1,"BLAp"),
  getall2(fit1$COApl_fit1,"COApl"),
  getall2(fit1$HDB_fit1,"HDB"),
  getall2(fit1$LPO_fit1,"LPO"),
  getall2(fit1$LS_fit1,"LS"),
  getall2(fit1$LSc_fit1,"LSc"),
  getall2(fit1$LSr_fit1,"LSr"),
  getall2(fit1$MEA_fit1,"MEA"),
  getall2(fit1$MS_fit1,"MS"),
  getall2(fit1$NAcc_fit1,"NAcc"),
  getall2(fit1$PiC_fit1,"PiC"),
  getall2(fit1$VDB_fit1,"VDB"),
  getall2(fit1$VP_fit1,"VP"),
  getall2(fit1$VTA_fit1,"VTA")
  
) %>% 
  as.data.frame()

row.names(est)<-est$region
est[,1:3]


## extract posteriors from the model 
p_otr<-rbind(
  get_posterior(fit1$AON_fit1) %>% mutate(region="AON"),
  get_posterior(fit1$BLAp_fit1) %>% mutate(region="BLA"),
  get_posterior(fit1$COApl_fit1) %>% mutate(region="COApl"),
  get_posterior(fit1$LS_fit1) %>% mutate(region="LS"),
  get_posterior(fit1$MEA_fit1) %>% mutate(region="MEA"),
  get_posterior(fit1$NAcc_fit1) %>% mutate(region="NAcc"),
  get_posterior(fit1$PiC_fit1) %>% mutate(region="PiC")
) %>% 
  mutate(region=factor(region,levels=c("AON","NAcc","LS","PiC","MEA","COApl","BLA"))) %>% 
  mutate(comps=factor(comps,levels=c( "Subdominant - Alpha","Subordinate - Alpha","Subordinate - Subdominant")))

p_otr_sum<-rbind(pairwise(fit1$AON_fit1) %>% mutate(region="AON"),
                 pairwise(fit1$BLAp_fit1) %>% mutate(region="BLA"),
                 pairwise(fit1$COApl_fit1) %>% mutate(region="COApl"),
                 pairwise(fit1$LS_fit1) %>% mutate(region="LS"),
                 pairwise(fit1$MEA_fit1) %>% mutate(region="MEA"),
                 pairwise(fit1$NAcc_fit1) %>% mutate(region="NAcc"),
                 pairwise(fit1$PiC_fit1) %>% mutate(region="PiC")) %>% 
  mutate(region=factor(region,levels=c("AON","NAcc","LS","PiC","MEA","COApl","BLA"))) %>% 
  mutate(comps=term) %>% 
  mutate(lwr=conf.low) %>% 
  mutate(upr=conf.high) %>% 
  select(comps,estimate,lwr,upr,region)%>% 
  mutate(text=paste(round(estimate,0)," [",paste(round(lwr,0),round(upr,0),sep=", "),"]",sep="")) %>% 
  mutate(sig=ifelse(lwr*upr>0,"sig","ns")) %>% 
  mutate(comps=factor(comps,levels=c( "Subdominant - Alpha","Subordinate - Alpha","Subordinate - Subdominant")))

p_v1ar<-rbind(
  get_posterior(fit1$HDB_fit1) %>% mutate(region="HDB"),
  get_posterior(fit1$LPO_fit1) %>% mutate(region="LPO"),
  get_posterior(fit1$LSr_fit1) %>% mutate(region="LSr"),
  get_posterior(fit1$LSc_fit1) %>% mutate(region="LSc"),
  get_posterior(fit1$VDB_fit1) %>% mutate(region="VDB"),
  get_posterior(fit1$VP_fit1) %>% mutate(region="VP"),
  get_posterior(fit1$VTA_fit1) %>% mutate(region="VTA")
) %>% 
  mutate(region=factor(region,levels=c("LSr","MS", "HDB","VDB","VP","LSc","LPO","VTA"))) %>% 
  mutate(comps=factor(comps,levels=c( "Subdominant - Alpha","Subordinate - Alpha","Subordinate - Subdominant")))

p_v1ar_sum<-rbind(pairwise(fit1$HDB_fit1) %>% mutate(region="HDB"),
                  pairwise(fit1$LPO_fit1) %>% mutate(region="LPO"),
                  pairwise(fit1$LSr_fit1) %>% mutate(region="LSr"),
                  pairwise(fit1$LSc_fit1) %>% mutate(region="LSc"),
                  pairwise(fit1$VDB_fit1) %>% mutate(region="VDB"),
                  pairwise(fit1$VP_fit1) %>% mutate(region="VP"),
                  pairwise(fit1$VTA_fit1) %>% mutate(region="VTA")) %>% 
  mutate(region=factor(region,levels=c("LSr","MS", "HDB","VDB","VP","LSc","LPO","VTA"))) %>% 
  mutate(comps=term) %>% 
  mutate(lwr=conf.low) %>% 
  mutate(upr=conf.high) %>% 
  select(comps,estimate,lwr,upr,region)%>% 
  mutate(text=paste(round(estimate,0)," [",paste(round(lwr,0),round(upr,0),sep=", "),"]",sep="")) %>% 
  mutate(sig=ifelse(lwr*upr>0,"sig","ns")) %>% 
  mutate(comps=factor(comps,levels=c( "Subdominant - Alpha","Subordinate - Alpha","Subordinate - Subdominant")))



## Figure 1: OTR data

#### Figure 1a: representative figures with a label to each brain region
#### Figure 1b: raw data points & boxplots (with one y axis)
#### Figure 1c: Bayesian stat result; mean estimate and 95% CI, posterior sentity plots from group comparisons with point range plot right underneath


df3<-df2 %>% 
  mutate(region=replace(region,region=="BLAp","BLA")) %>% 
  as.data.frame() %>% 
  mutate(region=factor(region,
                       levels=c("AON","NAcc","LS","PiC","MEA","COApl","BLA","LSr","MS", "HDB","VDB","VP","LSc","LPO","VTA")))

fig1b<-df3 %>% filter(receptor=="OTR") %>% 
  ggplot(.,aes(region,dpm_mg_TE,color=domgroup,fill=domgroup))+
  geom_boxplot(aes(color=domgroup),alpha=0.1,outlier.colour = NA,size=0.7,width=0.5,position=position_dodge(width=0.6))+
  geom_point(aes(group=domgroup,color=domgroup),position = position_dodge(0.6),size=1.2)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(fill="",color="",y="OTR Density (dpm/mg TE)", x="")+
  newggtheme_with_legends+
  theme(legend.text = element_text(family="Helvetica",size=10),
        axis.text.x = element_text(family="Helvetica",size=15),
        axis.text.y = element_text(family="Helvetica"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(family="Helvetica",size=15,vjust=2),
        panel.spacing.y=unit(0, "lines"),
        panel.background = element_rect(fill=NULL)
  )
print(fig1b)

p_otr_sum$comps<-factor(p_otr_sum$comps,levels=c("Subordinate - Subdominant","Subordinate - Alpha","Subdominant - Alpha"))

fig1c<-p_otr %>% 
  mutate(comps=factor(comps,levels=c("Subordinate - Subdominant","Subordinate - Alpha","Subdominant - Alpha"))) %>% 
  ggplot(.,aes(value,as.numeric(comps)-.15))+
  geom_density_ridges(scale=0.7,aes(fill=comps,color=comps),color="white",alpha=0.3,rel_min_height = .05)+
  geom_point(data=p_otr_sum,aes(x=estimate,y=as.numeric(comps)-.15,color=comps),size=3)+
  geom_segment(data=p_otr_sum,aes(x=lwr,xend=upr,y=as.numeric(comps)-.15,yend=as.numeric(comps)-.15,color=comps),size=1.2)+
  geom_vline(xintercept = 0,linetype="dashed",color="grey")+
  geom_text(data=p_otr_sum %>% filter(sig=="sig"),
            aes(label=text,x=Inf,as.numeric(comps)),hjust="inward",fontface="bold",vjust=-0.0001)+
  geom_text(data=p_otr_sum %>% filter(sig!="sig"),aes(label=text,x=Inf,as.numeric(comps)),hjust="inward",vjust=-0.0001)+
  facet_wrap(~region,ncol = 1,strip.position = "left")+
  scale_color_manual(values= c("#FDE725FF","#21908CFF","#440154FF"),guide = guide_legend(reverse = TRUE))+
  scale_fill_manual(values= c("#FDE725FF","#21908CFF","#440154FF"),guide = guide_legend(reverse = TRUE))+
  newggtheme_with_legends+
  theme(
        legend.text = element_text(family="Helvetica",size=10,hjust=-2),
        strip.text.y = element_text(family="Helvetica",size=15,angle=180),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family="Helvetica"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(family="Helvetica",size=12),
        panel.spacing.y=unit(0, "lines"),
        panel.background = element_rect(fill=NULL))+
  scale_x_continuous(breaks = c(-1500,-1000,-500,0,500,1000),limits = c(-1600,1200))+
  labs(fill="",color="", y="", x="Parameter estimate (mean ± 95% CI)")
 

print(fig1c)



## Figure 2: V1aR data 

fig2b<-df3 %>% filter(receptor=="V1aR") %>% 
  ggplot(.,aes(region,dpm_mg_TE,color=domgroup,fill=domgroup))+
  geom_boxplot(aes(color=domgroup),alpha=0.1,outlier.colour = NA,size=0.7,width=0.5,position=position_dodge(width=0.6))+
  geom_point(aes(group=domgroup,color=domgroup),position = position_dodge(0.6),size=1.2)+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  labs(fill="",color="",y="V1aR Density (dpm/mg TE)", x="")+
  newggtheme_with_legends+
  theme(legend.text = element_text(family="Helvetica",size=10),
        axis.text.x = element_text(family="Helvetica",size=15),
        axis.text.y = element_text(family="Helvetica"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(family="Helvetica",size=15,vjust=2),
        panel.spacing.y=unit(0, "lines"),
        panel.background = element_rect(fill=NULL)
  )


print(fig2b)


p_v1ar_sum$comps<-factor(p_v1ar_sum$comps,levels=c("Subordinate - Subdominant","Subordinate - Alpha","Subdominant - Alpha"))

fig2c<-p_v1ar %>% 
  mutate(comps=factor(comps,levels=c("Subordinate - Subdominant","Subordinate - Alpha","Subdominant - Alpha"))) %>% 
  ggplot(.,aes(value,as.numeric(comps)-.15))+
  geom_density_ridges(scale=0.7,aes(fill=comps,color=comps),color="white",alpha=0.3,rel_min_height = .05)+
  geom_point(data=p_v1ar_sum,aes(x=estimate,y=as.numeric(comps)-.15,color=comps),size=3)+
  geom_segment(data=p_v1ar_sum,aes(x=lwr,xend=upr,y=as.numeric(comps)-.15,yend=as.numeric(comps)-.15,color=comps),size=1.2)+
  geom_vline(xintercept = 0,linetype="dashed",color="grey")+
  geom_text(data=p_v1ar_sum %>% filter(sig=="sig"),
            aes(label=text,x=Inf,as.numeric(comps)),hjust="inward",fontface="bold",vjust=-0.0001)+
  geom_text(data=p_v1ar_sum %>% filter(sig!="sig"),aes(label=text,x=Inf,as.numeric(comps)),hjust="inward",vjust=-0.0001)+
  facet_wrap(~region,ncol = 1,strip.position = "left")+
  scale_color_manual(values= c("#FDE725FF","#21908CFF","#440154FF"),guide = guide_legend(reverse = TRUE))+
  scale_fill_manual(values= c("#FDE725FF","#21908CFF","#440154FF"),guide = guide_legend(reverse = TRUE))+
  newggtheme_with_legends+
  theme(legend.text = element_text(family="Helvetica",size=10,hjust=-2),
        strip.text.y = element_text(family="Helvetica",size=15,angle=180),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family="Helvetica"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(family="Helvetica",size=12),
        panel.spacing.y=unit(0, "lines"),
        panel.background = element_rect(fill=NULL))+
  scale_x_continuous(breaks = c(-1000,-500,0,500,1000,1500),limits = c(-1100,2100))+
  labs(fill="",color="",y="",x="Parameter estimate (mean ± 95% CI)")

print(fig2c)
