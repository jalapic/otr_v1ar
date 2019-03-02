## Figures
#### https://stackoverflow.com/questions/29263046/how-to-draw-the-boxplot-with-significant-level
#### sometimes primitive approach works best
 
est_df<-est %>% filter(region=="NAcc")
p<-df %>% filter(receptor=="OTR") %>% 
  filter(region=="NAcc") %>% 
  ggplot(.,aes(domgroup,dpm_mg_TE,color=domgroup))+
  geom_boxplot2(alpha=0.1,outlier.colour = NA)+
  geom_jitter(position = position_nudge(x=0.12),shape=21,size=2)+
  theme_classic()+
  scale_color_colorblind()+
  xlab("Social status")+
  ylab("Mean optical density")


#i alpha-sub
#j alpha-subdom
#k subdom-sub
#t teeth length
#u how much above teeth the text locates


df %>% filter(receptor=="OTR") %>% 
  filter(region=="NAcc") %>% 
  summarize(max=max(dpm_mg_TE)) %>% unlist() %>% ceiling(.)


i=3500
j=3250
k=3250
t=60
u=100

temp1<- data.frame(a = c(1, 1:3,3), b = c(i, i+t, i+t, i+t, i))
temp2<- data.frame(a = c(1, 1, 1.98, 1.98), b = c(j, j+t,j+t, j))
temp3<- data.frame(a = c(2.02, 2.02, 3, 3), b = c(k, k+t,k+t, k))

df %>% filter(receptor=="OTR") %>% 
  filter(region=="NAcc") %>% 
  ggplot(.,aes(as.factor(domgroup),dpm_mg_TE))+
  geom_boxplot2(aes(color=domgroup,fill=domgroup),alpha=0.1,outlier.colour = NA)+
  geom_jitter(aes(color=domgroup),position = position_nudge(x=0.12),shape=21,size=2)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  xlab("Social status")+
  ylab("Receptor density (dpm/mg TE)")+
  ggtitle("OTR-Nucleus Accumbens")+
  geom_line(data = temp1, aes(x = a, y = b),color="red") + 
  annotate("text", x = 2, y = i+t+u, label = est_df[1,2], size = 4,color="red") +
  geom_line(data = temp2, aes(x = a, y = b)) + 
  annotate("text", x = 1.5, y = j+t+u, label = est_df[1,1], size = 4) +
  geom_line(data = temp3, aes(x = a, y = b),color="red") + 
  annotate("text", x = 2.5, y = k+t+u, label = est_df[1,3], size = 4,color="red")