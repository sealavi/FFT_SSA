library(INLA)
library(ggplot2)

files=c("~/r.INLA.random_spider_nolag.RData",
"~/r.INLA.random_cap_nolag.RData",
"~/r.INLA.random_kink_nolag.RData",
"~/r.inla.random_coati_nolag.RData",

"~/r.INLA.random_spider_one_lag.RData",
"~/r.INLA.random_cap_one_lag.RData",
"~/r.INLA.random_kink_one_lag.RData",
"~/r.inla.random_coati_one_lag.RData",

"~/r.INLA.random_spider_two_lag.RData",
"~/r.INLA.random_cap_two_lag.RData",
"~/r.INLA.random_kink_two_lag.RData",
"~/r.inla.random_coati_two_lag.RData",

"~/r.INLA.random_spider_three_lag.RData",
"~/r.INLA.random_cap_three_lag.RData",
"~/r.INLA.random_kink_three_lag.RData",
"~/r.inla.random_coati_three_lag.RData",

"~/r.INLA.random_spider_four_lag.RData",
"~/r.INLA.random_cap_four_lag.RData",
"~/r.INLA.random_kink_four_lag.RData",
"~/r.inla.random_coati_four_lag.RData",

"~/r.INLA.random_spider.RData",
"~/r.INLA.random_cap.RData",
"~/r.INLA.random_kink.RData",
"~/r.inla.random_coati.RData",

"~/r.INLA.random_spider_six_lag.RData",
"~/r.INLA.random_cap_six_lag.RData",
#"~/r.INLA.random_kink_six_lag.RData",
#"~/r.inla.random_coati_six_lag.RData",

"~/r.INLA.random_spider_seven_lag.RData",
"~/r.INLA.random_cap_seven_lag.RData",
"~/r.INLA.random_kink_seven_lag.RData",
"~/r.inla.random_coati_seven_lag.RData")

for(i in 1:length(files)){
  load(files[i])
  print(gc())
}

models=c(r.INLA.random_spider_nolag,
        r.INLA.random_cap_nolag,
        r.INLA.random_kink_nolag,
        r.inla.random_coati_nolag,
        
        r.INLA.random_spider_one_lag,
        r.INLA.random_cap_one_lag,
        r.INLA.random_kink_one_lag,
        r.inla.random_coati_one_lag,
        
        r.INLA.random_spider_two_lag,
        r.INLA.random_cap_two_lag,
        r.INLA.random_kink_two_lag,
        r.inla.random_coati_two_lag,
        
        r.INLA.random_spider_three_lag,
        r.INLA.random_cap_three_lag,
        r.INLA.random_kink_three_lag,
        r.inla.random_coati_three_lag,
        
        r.INLA.random_spider_four_lag,
        r.INLA.random_cap_four_lag,
        r.INLA.random_kink_four_lag,
        r.inla.random_coati_four_lag,
        
        r.INLA.random_spider,
        r.INLA.random_cap,
        r.INLA.random_kink,
        r.inla.random_coati,
        
        r.INLA.random_spider_six_lag,
        r.INLA.random_cap_six_lag,
        #r.INLA.random_kink_six_lag,
        #r.inla.random_coati_six_lag,
        
        r.INLA.random_spider_seven_lag,
        r.INLA.random_cap_seven_lag,
        r.INLA.random_kink_seven_lag,
        r.inla.random_coati_seven_lag)

Intervals=c(4,8,12,16,32,60,120,240)



Species=rep(c("Ateles geoffroyi","Cebus capucinus","Nasua narica","Potos flavus"), 8)
Species=Species[-c(27,28)]


results=c()
output=c()
count=1
for(i in 2:length(files)){
  load(files[i])
  print(gc())

  
  

  for(j in 1:46){

    
    results=data.frame(r.inla$summary.fixed)
    results$covariate=rownames(results)
    results$Species=Species[j]
    results$ID=IDs[j]
    results$Interval=Intervals[i]
    output[count]=list(results)
    print(count)
    print(head(output[[count]]))
    
    count=count+1
  }
}


ggplot(results[-1,],aes(x=mean,y=covariate))+geom_point()+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+ggtitle("Spider monkey")

ggarrange(viz,posteriors)

results_cap=data.frame(r.inla.random_cap$summary.fixed)
results_cap$covariate=rownames(results_cap)
results_cap$Species="Capuchin"
ggplot(results_cap[-1,],aes(x=mean,y=covariate))+geom_point()+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+ggtitle("Capuchin")

results_coati=data.frame(r.inla.random_coati$summary.fixed)
results_coati$covariate=rownames(results_coati)
results_coati$Species="Coati"
ggplot(results_coati[-1,],aes(x=mean,y=covariate))+geom_point()+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+ggtitle("Coati)")

results_kink=data.frame(r.inla.random_kink$summary.fixed)
results_kink$covariate=rownames(results_kink)
ggplot(results_kink[-1,],aes(x=mean,y=covariate))+geom_point()+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+ggtitle("Kinkajou")
results_kink$Species="Kinkajou"

alldata=rbind(results[-c(1,11:14),],results_cap[-c(1,11:14),],results_coati[-c(1,11:14),],results_kink[-c(1,11:14),])
alldata$Species=as.factor(alldata$Species)
ggplot(alldata,aes(x=mean,y=covariate,color=Species))+geom_point()+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+ggtitle("Posteriors")

ggplot(alldata,aes(x=mean,y=covariate,color=Species))+geom_point()+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+ggtitle("Posteriors")+facet_wrap(~Species)

load("C:/Users/salavi/Downloads/modelsummariesINLA.RData")
output3=output2[-which((output2$Interval)=="240"),]
output3=output3[-which(as.character(output3$covariate)=="(Intercept)" |
                         as.character(output3$covariate)=="steplength_scaled"|
                         as.character(output3$covariate)=="steplengthdiff_scaled"|
                         as.character(output3$covariate)=="orientation_scaled"|
                         as.character(output3$covariate)=="turnangles_scaled" ),]
output3$Species=as.factor(output3$Species)
output3$Interval=as.factor(output3$Interval)

ggplot(output3,aes(x=mean,y=covariate, color=Interval),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant),position = position_dodge2(width = 0.5))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+
  facet_wrap(~Species, scale="free_x")+
  theme(text=element_text(size=15))
ggsave("Species_Level_Inla.tiff", units="in", height=3.6, width=6.4,dpi=300,scale=3, device = "tiff")
