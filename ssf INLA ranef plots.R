library(INLA)
library(ggplot2)
memory.limit(1e+10)
load("//10.126.19.90/EAS_shared/food-for-thought/working/processed/New folder/r.INLA.random_cap_four_lag.RData")
load("//10.126.19.90/EAS_shared/food-for-thought/working/processed/New folder/r.INLA.random_spider_four_lag.RData")
load("//10.126.19.90/EAS_shared/food-for-thought/working/processed/New folder/r.INLA.random_coati_four_lag.RData")
load("//10.126.19.90/EAS_shared/food-for-thought/working/processed/New folder/r.INLA.random_kink_four_lag.RData")

covariates=c("Distance from Dipteryx",
  "Canopy height",
  "Canopy shape",
  "Slope",
  "Crown density",
  "Crown thickness",
  "Distance to canopy gap",
  "Canopy cover",
  "Lateral continuity",
  "Step-length",
  "Step-length difference",
  "Orientation",
  "Turning angle")

plotdata=c()
for(i in 2:14){
  temp=r.inla.random_spider_four_lag$summary.random[[i]]
  temp$Covariate=covariates[i-1]
  plotdata[i]=list(temp)
}
plotdata2=do.call(rbind,plotdata)
plotdata2$ID=as.factor(plotdata2$ID)
plotdata3=plotdata2[-which((plotdata2$Covariate)=="Step-length"|
                    (plotdata2$Covariate)=="Step-length difference"|
                    (plotdata2$Covariate)=="Orientation"|
                    (plotdata2$Covariate)=="Turning angle" ),]
plotdata3$Covariate=as.factor(plotdata3$Covariate)

plotdata_cap=c()
for(i in 2:14){
  temp=r.inla.random_cap_four_lag$summary.random[[i]]
  temp$Covariate=covariates[i-1]
  plotdata_cap[i]=list(temp)
}
plotdata_cap2=do.call(rbind,plotdata_cap)
plotdata_cap2$ID=as.factor(plotdata_cap2$ID)
plotdata_cap3=plotdata_cap2[-which((plotdata_cap2$Covariate)=="Step-length"|
                             (plotdata_cap2$Covariate)=="Step-length difference"|
                             (plotdata_cap2$Covariate)=="Orientation"|
                             (plotdata_cap2$Covariate)=="Turning angle" ),]
plotdata_cap3$Covariate=as.factor(plotdata_cap3$Covariate)

plotdata_coati=c()
for(i in 2:14){
  temp=r.inla.random_coati_four_lag$summary.random[[i]]
  temp$Covariate=covariates[i-1]
  plotdata_coati[i]=list(temp)
}
plotdata_coati2=do.call(rbind,plotdata_coati)
plotdata_coati2$ID=as.factor(plotdata_coati2$ID)
plotdata_coati3=plotdata_coati2[-which((plotdata_coati2$Covariate)=="Step-length"|
                                     (plotdata_coati2$Covariate)=="Step-length difference"|
                                     (plotdata_coati2$Covariate)=="Orientation"|
                                     (plotdata_coati2$Covariate)=="Turning angle" ),]
plotdata_coati3$Covariate=as.factor(plotdata_coati3$Covariate)

plotdata_kink=c()
for(i in 2:14){
  temp=r.inla.random_kink_four_lag$summary.random[[i]]
  temp$Covariate=covariates[i-1]
  plotdata_kink[i]=list(temp)
}
plotdata_kink2=do.call(rbind,plotdata_kink)
plotdata_kink2$ID=as.factor(plotdata_kink2$ID)
plotdata_kink3=plotdata_kink2[-which((plotdata_kink2$Covariate)=="Step-length"|
                                         (plotdata_kink2$Covariate)=="Step-length difference"|
                                         (plotdata_kink2$Covariate)=="Orientation"|
                                         (plotdata_kink2$Covariate)=="Turning angle" ),]
plotdata_kink3$Covariate=as.factor(plotdata_kink3$Covariate)

ggplot(plotdata3,aes(x=mean,y=Covariate, color=ID),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+geom_linerange(aes(xmin=`0.025quant`,xmax=`0.975quant`),position = position_dodge2(width = 0.5))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+
  facet_wrap(~ID)+
  theme(text=element_text(size=15))
ggsave("Species_Level_Inla_ranef.pdf", units="in", height=3.6, width=6.4,dpi=300,scale=3, device = "pdf")

output2$covariate[which((as.character(output2$covariate))=="distance_from_dip_scaled")]=covariates[1]
output2$covariate[which((as.character(output2$covariate))=="canopyheight_scaled")]=covariates[2]
output2$covariate[which((as.character(output2$covariate))=="CanopyShape_scaled")]=covariates[3]
output2$covariate[which((as.character(output2$covariate))=="BCI_slope_scaled")]=covariates[4]
output2$covariate[which((as.character(output2$covariate))=="crden_clip_scaled")]=covariates[5]
output2$covariate[which((as.character(output2$covariate))=="crth_clip_scaled")]=covariates[6]
output2$covariate[which((as.character(output2$covariate))=="gap5m_clip_scaled")]=covariates[7]
output2$covariate[which((as.character(output2$covariate))=="CanopyCover_20m_scaled")]=covariates[8]
output2$covariate[which((as.character(output2$covariate))=="LatCon_3m_prj_scaled")]=covariates[9]
output2$covariate[which((as.character(output2$covariate))=="steplength_scaled")]=covariates[10]
output2$covariate[which((as.character(output2$covariate))=="steplengthdiff_scaled")]=covariates[11]
output2$covariate[which((as.character(output2$covariate))=="orientation_scaled")]=covariates[12]
output2$covariate[which((as.character(output2$covariate))=="turnangles_scaled")]=covariates[13]

Spiderspecies=output2[which(output2$Species=="Ateles geoffroyi"),]
Spiderspecies[which(Spiderspecies$Interval==32),]
Spiderspecies2=Spiderspecies[-which(as.character(Spiderspecies$covariate)=="(Intercept)" |
                                     as.character(Spiderspecies$covariate)=="Step-length"|
                                     as.character(Spiderspecies$covariate)=="Step-length difference"|
                                     as.character(Spiderspecies$covariate)=="Orientation"|
                                     as.character(Spiderspecies$covariate)=="Turning angle" ),]
Capspecies=output2[which(output2$Species=="Cebus capucinus"),]
Capspecies[which(Capspecies$Interval==32),]
Capspecies2=Capspecies[-which(as.character(Capspecies$covariate)=="(Intercept)" |
                                      as.character(Capspecies$covariate)=="Step-length"|
                                      as.character(Capspecies$covariate)=="Step-length difference"|
                                      as.character(Capspecies$covariate)=="Orientation"|
                                      as.character(Capspecies$covariate)=="Turning angle" ),]

Coatispecies=output2[which(output2$Species=="Nasua narica"),]
Coatispecies[which(Coatispecies$Interval==32),]
Coatispecies2=Coatispecies[-which(as.character(Coatispecies$covariate)=="(Intercept)" |
                                as.character(Coatispecies$covariate)=="Step-length"|
                                as.character(Coatispecies$covariate)=="Step-length difference"|
                                as.character(Coatispecies$covariate)=="Orientation"|
                                as.character(Coatispecies$covariate)=="Turning angle" ),]

Kinkspecies=output2[which(output2$Species=="Potos flavus"),]
Kinkspecies[which(Kinkspecies$Interval==32),]
Kinkspecies2=Kinkspecies[-which(as.character(Kinkspecies$covariate)=="(Intercept)" |
                                    as.character(Kinkspecies$covariate)=="Step-length"|
                                    as.character(Kinkspecies$covariate)=="Step-length difference"|
                                    as.character(Kinkspecies$covariate)=="Orientation"|
                                    as.character(Kinkspecies$covariate)=="Turning angle" ),]

ggplot(output2[-which(as.character(output2$covariate)=="(Intercept)" |
                       as.character(output2$covariate)=="Step-length"|
                       as.character(output2$covariate)=="Step-length difference"|
                       as.character(output2$covariate)=="Orientation"|
                       as.character(output2$covariate)=="Turning angle"),],aes(x=mean,y=covariate, color=as.factor(Interval)),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant),position = position_dodge2(width = 0.5))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+facet_wrap(~Species)+
  theme(text=element_text(size=15))+guides(color=guide_legend(title="Interval"))
  ggsave("Species_Level_Inla_fixed_with240.pdf", units="in", height=3.6, width=6.4,dpi=300,scale=3, device = "pdf")
 
  ggplot(output2[-which(as.character(output2$Interval)=="240" |
                          as.character(output2$covariate)=="(Intercept)" |
                          as.character(output2$covariate)=="Step-length"|
                          as.character(output2$covariate)=="Step-length difference"|
                          as.character(output2$covariate)=="Orientation"|
                          as.character(output2$covariate)=="Turning angle"),],aes(x=mean,y=covariate, color=as.factor(Interval)),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant),position = position_dodge2(width = 0.5))+
    geom_vline(xintercept = 0)+
    theme_classic() +ylab("Covariate")+facet_wrap(~Species)+
  theme(text=element_text(size=15))+guides(color=guide_legend(title="Interval"))
  ggsave("Species_Level_Inla_fixed.pdf", units="in", height=3.6, width=6.4,dpi=300,scale=3, device = "pdf")
  

ggplot(Spiderspecies2[which(Spiderspecies2$Interval==32),],aes(x=mean,y=covariate),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+geom_linerange(aes(xmin=X0.025quant,xmax=X0.975quant),position = position_dodge2(width = 0.5))+
  geom_vline(xintercept = 0)+
  theme_classic() +ylab("Covariate")+
  theme(text=element_text(size=15))
ggsave("Species_Level_Inla.pdf", units="in", height=3.6, width=6.4,dpi=300,scale=3, device = "pdf")

Spiderspecies2_32=Spiderspecies2[which(Spiderspecies2$Interval==32),]
Spiderspecies2_32

Capspecies2_32=Capspecies2[which(Capspecies2$Interval==32),]
Capspecies2_32

Coatispecies2_32=Coatispecies2[which(Coatispecies2$Interval==32),]
Coatispecies2_32

Kinkspecies2_32=Kinkspecies2[which(Kinkspecies2$Interval==32),]
Kinkspecies2_32

inds=split(plotdata3,plotdata3$Covariate)
spcov=split(Spiderspecies2_32,as.factor(Spiderspecies2_32$covariate))
count=1
PDF_New=c()

for(i in 1:length(spcov)){
  x <- rnorm(1000, mean = spcov[[i]]$mean, sd = spcov[[i]]$sd)
  y=c()
  for(j in 1:nrow(inds[[i]])){
    y[j] <- list(rnorm(1000, mean = inds[[i]]$mean[j], sd = inds[[i]]$sd[j]))
    Z=x+y[[j]]
    postmean=mean(Z)
    quantZ=quantile(Z, probs = c(0.025,0.975))
    minZ=as.numeric(quantZ[1])
    maxZ=as.numeric(quantZ[2])
    rID=as.character(inds[[i]]$ID[j])
    rID=trimws(gsub('[[:digit:]]+', '', rID))
    Cov=names(inds[i])
    Spec="Ateles geoffroyi"
    res=c(rID,Spec,Cov,postmean,minZ,maxZ)
    PDF_New[count]=list(res)
    count=count+1
      }

}
PDF_spider=do.call(rbind,PDF_New)
PDF_spider=data.frame(PDF_spider)
colnames(PDF_spider)=c("ID","Species","Covariate","mean","Lower","Upper")

PDF_spider$mean=as.numeric(PDF_spider$mean)
PDF_spider$Lower=as.numeric(PDF_spider$Lower)
PDF_spider$Upper=as.numeric(PDF_spider$Upper)
PDF_spider$ID=as.factor(PDF_spider$ID)
PDF_spider$Covariate=as.factor(PDF_spider$Covariate)

spiderplot=ggplot(PDF_spider,aes(x=mean,y=Covariate, color=ID),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+
  geom_linerange(aes(xmin=Lower,xmax=Upper),position = position_dodge2(width = 0.5))+
  geom_vline(xintercept = 0)+ggtitle("Ateles geoffroyi")+
  theme_classic() +ylab("Covariate")+
  theme(text=element_text(size=15))
  
  inds=split(plotdata3,plotdata3$Covariate)
  spcov=split(Spiderspecies2_32,as.factor(Spiderspecies2_32$covariate))
  count=1
  PDF_New=c()
  
  for(i in 1:length(spcov)){
    x <- rnorm(1000, mean = spcov[[i]]$mean, sd = spcov[[i]]$sd)
    y=c()
    for(j in 1:nrow(inds[[i]])){
      y[j] <- list(rnorm(1000, mean = inds[[i]]$mean[j], sd = inds[[i]]$sd[j]))
      Z=x+y[[j]]
      postmean=mean(Z)
      quantZ=quantile(Z, probs = c(0.025,0.975))
      minZ=as.numeric(quantZ[1])
      maxZ=as.numeric(quantZ[2])
      rID=as.character(inds[[i]]$ID[j])
      rID=trimws(gsub('[[:digit:]]+', '', rID))
      Cov=names(inds[i])
      Spec="Ateles geoffroyi"
      res=c(rID,Spec,Cov,postmean,minZ,maxZ)
      PDF_New[count]=list(res)
      count=count+1
    }
    
  }
  PDF_spider=do.call(rbind,PDF_New)
  PDF_spider=data.frame(PDF_spider)
  colnames(PDF_spider)=c("ID","Species","Covariate","mean","Lower","Upper")
  
  PDF_spider$mean=as.numeric(PDF_spider$mean)
  PDF_spider$Lower=as.numeric(PDF_spider$Lower)
  PDF_spider$Upper=as.numeric(PDF_spider$Upper)
  PDF_spider$ID=as.factor(PDF_spider$ID)
  PDF_spider$Covariate=as.factor(PDF_spider$Covariate)
  
  spiderplot=ggplot(PDF_spider,aes(x=mean,y=Covariate, color=ID),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+
    geom_linerange(aes(xmin=Lower,xmax=Upper),position = position_dodge2(width = 0.5))+
    geom_vline(xintercept = 0)+ggtitle("Ateles geoffroyi")+
    theme_classic() +ylab("Covariate")+
    theme(text=element_text(size=15))
  
  inds_cap=split(plotdata_cap3,plotdata_cap3$Covariate)
  spcov_cap=split(Capspecies2_32,as.factor(Capspecies2_32$covariate))
  count=1
  PDF_New_cap=c()
  
  for(i in 1:length(spcov_cap)){
    x <- rnorm(1000, mean = spcov_cap[[i]]$mean, sd = spcov_cap[[i]]$sd)
    y=c()
    for(j in 1:nrow(inds_cap[[i]])){
      y[j] <- list(rnorm(1000, mean = inds_cap[[i]]$mean[j], sd = inds_cap[[i]]$sd[j]))
      Z=x+y[[j]]
      postmean=mean(Z)
      quantZ=quantile(Z, probs = c(0.025,0.975))
      minZ=as.numeric(quantZ[1])
      maxZ=as.numeric(quantZ[2])
      rID=as.character(inds_cap[[i]]$ID[j])
      rID=trimws(gsub('[[:digit:]]+', '', rID))
      Cov=names(inds_cap[i])
      Spec="Ateles geoffroyi"
      res=c(rID,Spec,Cov,postmean,minZ,maxZ)
      PDF_New_cap[count]=list(res)
      count=count+1
    }
    
  }
  PDF_cap=do.call(rbind,PDF_New_cap)
  PDF_cap=data.frame(PDF_cap)
  colnames(PDF_cap)=c("ID","Species","Covariate","mean","Lower","Upper")
  
  PDF_cap$mean=as.numeric(PDF_cap$mean)
  PDF_cap$Lower=as.numeric(PDF_cap$Lower)
  PDF_cap$Upper=as.numeric(PDF_cap$Upper)
  PDF_cap$ID=as.factor(PDF_cap$ID)
  PDF_cap$Covariate=as.factor(PDF_cap$Covariate)
  
  capplot=ggplot(PDF_cap,aes(x=mean,y=Covariate, color=ID),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+
    geom_linerange(aes(xmin=Lower,xmax=Upper),position = position_dodge2(width = 0.5))+
    geom_vline(xintercept = 0)+ggtitle("Cebus capucinus")+
  theme_classic() +ylab("Covariate")+
    theme(text=element_text(size=15))
  

  
  inds_coati=split(plotdata_coati3,plotdata_coati3$Covariate)
  spcov_coati=split(Coatispecies2_32,as.factor(Coatispecies2_32$covariate))
  count=1
  PDF_New_coati=c()
  
  for(i in 1:length(spcov_coati)){
    x <- rnorm(1000, mean = spcov_coati[[i]]$mean, sd = spcov_coati[[i]]$sd)
    y=c()
    for(j in 1:nrow(inds_coati[[i]])){
      y[j] <- list(rnorm(1000, mean = inds_coati[[i]]$mean[j], sd = inds_coati[[i]]$sd[j]))
      Z=x+y[[j]]
      postmean=mean(Z)
      quantZ=quantile(Z, probs = c(0.025,0.975))
      minZ=as.numeric(quantZ[1])
      maxZ=as.numeric(quantZ[2])
      rID=as.character(inds_coati[[i]]$ID[j])
      rID=trimws(gsub('[[:digit:]]+', '', rID))
      Cov=names(inds_coati[i])
      Spec="Ateles geoffroyi"
      res=c(rID,Spec,Cov,postmean,minZ,maxZ)
      PDF_New_coati[count]=list(res)
      count=count+1
    }
    
  }
  PDF_coati=do.call(rbind,PDF_New_coati)
  PDF_coati=data.frame(PDF_coati)
  colnames(PDF_coati)=c("ID","Species","Covariate","mean","Lower","Upper")
  
  PDF_coati$mean=as.numeric(PDF_coati$mean)
  PDF_coati$Lower=as.numeric(PDF_coati$Lower)
  PDF_coati$Upper=as.numeric(PDF_coati$Upper)
  PDF_coati$ID=as.factor(PDF_coati$ID)
  PDF_coati$Covariate=as.factor(PDF_coati$Covariate)
  
  coatiplot=ggplot(PDF_coati,aes(x=mean,y=Covariate, color=ID),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+
    geom_linerange(aes(xmin=Lower,xmax=Upper),position = position_dodge2(width = 0.5))+
    geom_vline(xintercept = 0)+ggtitle("Nasua narica")+
    theme_classic() +ylab("Covariate")+
    theme(text=element_text(size=15))
  
  inds_kink=split(plotdata_kink3,plotdata_kink3$Covariate)
  spcov_kink=split(Kinkspecies2_32,as.factor(Kinkspecies2_32$covariate))
  count=1
  PDF_New_kink=c()
  
  for(i in 1:length(spcov_kink)){
    x <- rnorm(1000, mean = spcov_kink[[i]]$mean, sd = spcov_kink[[i]]$sd)
    y=c()
    for(j in 1:nrow(inds_kink[[i]])){
      y[j] <- list(rnorm(1000, mean = inds_kink[[i]]$mean[j], sd = inds_kink[[i]]$sd[j]))
      Z=x+y[[j]]
      postmean=mean(Z)
      quantZ=quantile(Z, probs = c(0.025,0.975))
      minZ=as.numeric(quantZ[1])
      maxZ=as.numeric(quantZ[2])
      rID=as.character(inds_kink[[i]]$ID[j])
      if(rID=="Abby 4652"){rID="Abby (a)"}
      else {if(rID=="Abby 5767"){rID="Abby (b)"}else
      {if(rID=="Ripley 4650"){rID="Ripley (a)"}else{
        if(rID=="Ripley 5771"){rID="Ripley (b)"}
      }
          
        }}
      rID=trimws(gsub('[[:digit:]]+', '', rID))
      Cov=names(inds_kink[i])
      Spec="Ateles geoffroyi"
      res=c(rID,Spec,Cov,postmean,minZ,maxZ)
      PDF_New_kink[count]=list(res)
      count=count+1
    }
    
  }
  PDF_kink=do.call(rbind,PDF_New_kink)
  PDF_kink=data.frame(PDF_kink)
  colnames(PDF_kink)=c("ID","Species","Covariate","mean","Lower","Upper")
  
  PDF_kink$mean=as.numeric(PDF_kink$mean)
  PDF_kink$Lower=as.numeric(PDF_kink$Lower)
  PDF_kink$Upper=as.numeric(PDF_kink$Upper)
  PDF_kink$ID=as.factor(PDF_kink$ID)
  PDF_kink$Covariate=as.factor(PDF_kink$Covariate)
  
  kinkplot=ggplot(PDF_kink,aes(x=mean,y=Covariate, color=ID),na.rm=TRUE,position = position_dodge2(width = 0.5))+geom_point(position = position_dodge2(width = 0.5))+
    geom_linerange(aes(xmin=Lower,xmax=Upper),position = position_dodge2(width = 0.5))+
    geom_vline(xintercept = 0)+ggtitle("Potos flavus")+
    theme_classic() +ylab("Covariate")+
    theme(text=element_text(size=15))
  
  
library(ggpubr)
ggarrange(spiderplot,capplot,coatiplot,kinkplot)  
ggsave("Species_Level_Inla_ranef_posteriors.pdf", units="in", height=3.6, width=6.4,dpi=300,scale=3, device = "pdf")

PDF_spider$Species="Ateles geoffroyi"
PDF_cap$Species="Cebus capucinus"
PDF_coati$Species="Nasua narica"
PDF_kink$Species="Potos flavus"
PDF_spider$ID=as.character(PDF_spider$ID)
PDF_cap$ID=as.character(PDF_cap$ID)
PDF_coati$ID=as.character(PDF_coati$ID)
PDF_kink$ID=as.character(PDF_kink$ID)
PDF_spider2=reshape(PDF_spider[,-c(5:6)], idvar = c("ID","Species"), timevar = "Covariate", direction = "wide")
PDF_cap2=reshape(PDF_cap[,-c(5:6)], idvar = c("ID","Species"), timevar = "Covariate", direction = "wide")
PDF_coati2=reshape(PDF_coati[,-c(5:6)], idvar = c("ID","Species"), timevar = "Covariate", direction = "wide")
PDF_kink2=reshape(PDF_kink[,-c(5:6)], idvar = c("ID","Species"), timevar = "Covariate", direction = "wide")
effectsizedata=rbind(PDF_spider2,PDF_cap2,PDF_coati2,PDF_kink2)

colnames(effectsizedata)=sub("mean.", "", names(effectsizedata))
write.csv(effectsizedata,file="ssf_effectsizedata32.csv")
