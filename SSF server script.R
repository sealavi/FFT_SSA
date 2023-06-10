library(INLA)

inla.setOption(pardiso.license="~/eas_shared/food-for-thought/working/processed/ssf lag workspaces/pardiso.lic")

inla.pardiso.check()

workspaces = FileList <- list.files(path = "//10.126.19.90/EAS_shared/food-for-thought/working/processed/ssf lag workspaces/", pattern = ".RData", all.files = TRUE, full.names = TRUE)

lags=c("five","four","one","seven","six","three","two","no_lag")

for(q in 1:length(workspaces)){
  load(workspaces[q])
  IDs= c("Abby 4652","Abby 5767","Avery 4671",
         "Ben Bob 4653","Bob 4661","Bonnie 4658",
         "Carlsberg 4673","Chibi 4693","Chloe 4052",
         "Clementina 4672","Da Vinci 5764","Eli 5765",
         "Ellie 4668","Emma 5762","Fonta Flora 4689",
         "Galena 5775","Gamer 5772","Gillian 4671",
         "Golliath 5214","Greg 4689","Ibeth 4654",
         "Inez 5213","Jeff 5769","Judy 4656",
         "Kyle 4692","Limon 5215","Mario 5768",
         "Martinelli 5763","Mimi 4660","Molly 5770",
         "Norah 4655","Olga 4657","Ornette 4669",
         "Peter Nelson 5774", "Pliny 4675","Ripley 4650",
         "Ripley 5771","Riwaka 4669","Sahti 4693",
         "Sofie 4674","Thelonious 4668","Tony Stark 4659",
         "Valoy 5766","Veruca 4690","Vielle 4670",
         "Zola 5212")
  
  Species=c( "Potos flavus","Potos flavus","Nasua narica",
             "Potos flavus","Cebus capucinus","Potos flavus",
             "Nasua narica","Ateles geoffroyi", "Potos flavus",
             "Nasua narica","Cebus capucinus","Potos flavus",
             "Nasua narica","Ateles geoffroyi", "Nasua narica",
             "Nasua narica","Potos flavus","Nasua narica",
             "Nasua narica","Ateles geoffroyi", "Cebus capucinus",
             "Ateles geoffroyi", "Potos flavus","Potos flavus",
             "Ateles geoffroyi", "Ateles geoffroyi", "Potos flavus",
             "Cebus capucinus","Cebus capucinus","Potos flavus",
             "Cebus capucinus","Cebus capucinus", "Nasua narica",
             "Nasua narica","Nasua narica","Potos flavus",
             "Potos flavus","Nasua narica","Nasua narica",
             "Nasua narica","Nasua narica","Potos flavus",
             "Cebus capucinus","Ateles geoffroyi", "Nasua narica",
             "Ateles geoffroyi")
  
  for (i in 1:length(save_tracks)){
    print(paste("Updating data", i, sep=" "))
    save_tracks[[i]]$ID=IDs[[i]]
    save_tracks[[i]]$Species=Species[[i]]
    
    if(max(lubridate::year(save_tracks[[i]]$Time),na.rm=TRUE)<2017){
      save_tracks[[i]]=save_tracks[[i]][which(save_tracks[[i]]$Time>=lubridate::with_tz(as.POSIXct("2015-12-14 18:00:00", tz="America/Panama"),tzone="UTC") &save_tracks[[i]]$Time<=lubridate::with_tz(as.POSIXct("2016-3-16 18:00:00", tz="America/Panama"),tzone="UTC")),]
    } else {
      save_tracks[[i]]=save_tracks[[i]][which(save_tracks[[i]]$Time>=lubridate::with_tz(as.POSIXct("2017-12-14 18:00:00", tz="America/Panama"),tzone="UTC") &save_tracks[[i]]$Time<=lubridate::with_tz(as.POSIXct("2018-3-16 18:00:00", tz="America/Panama"),tzone="UTC")),]
    }
  }
  
  for (i in 2:length(save_tracks)){
    print(paste("Updating data", i, sep=" "))
    save_tracks[[i]]$REFID=save_tracks[[i]]$REFID+max(save_tracks[[i-1]]$REFID, na.rm=TRUE)
  }
  
  track_Steps=do.call(rbind,save_tracks)
  track_Steps$Species=as.factor(track_Steps$Species)
  
  track_Steps$ID=as.character(track_Steps$ID)
  track_Steps=track_Steps[order(track_Steps$REFID),]
  track_Steps$steplength_scaled=(track_Steps$steplength-mean(track_Steps$steplength,na.rm=TRUE))/sd(track_Steps$steplength,na.rm=TRUE)
  track_Steps$steplengthdiff_scaled=(track_Steps$steplengthdiff-mean(track_Steps$steplengthdiff,na.rm=TRUE))/sd(track_Steps$steplengthdiff,na.rm=TRUE)
  track_Steps$orientation_scaled=(track_Steps$orientation-mean(track_Steps$orientation,na.rm=TRUE))/sd(track_Steps$orientation,na.rm=TRUE)
  track_Steps$turnangles_scaled=(track_Steps$turnangles-mean(track_Steps$turnangles,na.rm=TRUE))/sd(track_Steps$turnangles,na.rm=TRUE)
  track_Steps$CanopyShape_scaled=(track_Steps$CanopyShape-mean(track_Steps$CanopyShape,na.rm=TRUE))/sd(track_Steps$CanopyShape,na.rm=TRUE)
  track_Steps$BCI_slope_scaled=(track_Steps$BCI_slope-mean(track_Steps$BCI_slope,na.rm=TRUE))/sd(track_Steps$BCI_slope,na.rm=TRUE)
  track_Steps$crden_clip_scaled=(track_Steps$crden_clip-mean(track_Steps$crden_clip,na.rm=TRUE))/sd(track_Steps$crden_clip,na.rm=TRUE)
  track_Steps$crth_clip_scaled=(track_Steps$crth_clip-mean(track_Steps$crth_clip,na.rm=TRUE))/sd(track_Steps$crth_clip,na.rm=TRUE)
  track_Steps$gap5m_clip_scaled=(track_Steps$gap5m_clip-mean(track_Steps$gap5m_clip,na.rm=TRUE))/sd(track_Steps$gap5m_clip,na.rm=TRUE)
  track_Steps$CanopyCover_20m_scaled=(track_Steps$CanopyCover_20m-mean(track_Steps$CanopyCover_20m,na.rm=TRUE))/sd(track_Steps$CanopyCover_20m,na.rm=TRUE)
  track_Steps$LatCon_3m_prj_scaled=(track_Steps$LatCon_3m_prj-mean(track_Steps$LatCon_3m_prj,na.rm=TRUE))/sd(track_Steps$LatCon_3m_prj,na.rm=TRUE)
  track_Steps$cumulativeElevationGain_scaled=(track_Steps$cumulativeElevationGain-mean(track_Steps$cumulativeElevationGain,na.rm=TRUE))/sd(track_Steps$cumulativeElevationGain,na.rm=TRUE)
  track_Steps$distance_from_dip_scaled=(track_Steps$distance_from_dip-mean(track_Steps$distance_from_dip,na.rm=TRUE))/sd(track_Steps$distance_from_dip,na.rm=TRUE)
  track_Steps$canopyheight_scaled=(track_Steps$canopyheight-mean(track_Steps$canopyheight,na.rm=TRUE))/sd(track_Steps$canopyheight,na.rm=TRUE)
  
  track_Steps$ID2=track_Steps$ID
  track_Steps$ID3=track_Steps$ID
  track_Steps$ID4=track_Steps$ID
  track_Steps$ID5=track_Steps$ID
  track_Steps$ID6=track_Steps$ID
  track_Steps$ID7=track_Steps$ID
  track_Steps$ID8=track_Steps$ID
  track_Steps$ID9=track_Steps$ID
  track_Steps$ID10=track_Steps$ID
  track_Steps$ID11=track_Steps$ID
  track_Steps$ID12=track_Steps$ID
  track_Steps$ID13=track_Steps$ID
  track_Steps$ID14=track_Steps$ID
  
  track_Steps$Species2=track_Steps$Species
  track_Steps$Species3=track_Steps$Species
  track_Steps$Species4=track_Steps$Species
  track_Steps$Species5=track_Steps$Species
  track_Steps$Species6=track_Steps$Species
  track_Steps$Species7=track_Steps$Species
  track_Steps$Species8=track_Steps$Species
  track_Steps$Species9=track_Steps$Species
  track_Steps$Species10=track_Steps$Species
  track_Steps$Species11=track_Steps$Species
  track_Steps$Species12=track_Steps$Species
  track_Steps$Species13=track_Steps$Species
  track_Steps$Species14=track_Steps$Species
  
  gc()
  splitdata=split(track_Steps,track_Steps$Species)
  names(splitdata)
  mean.beta <- 0
  prec.beta <- 1e-4
  
  for(i in 1:length(splitdata)){
    splitdata[[i]]$ID=as.character(splitdata[[i]]$ID)
    splitdata[[i]]$ID2=splitdata[[i]]$ID
    splitdata[[i]]$ID3=splitdata[[i]]$ID
    splitdata[[i]]$ID4=splitdata[[i]]$ID
    splitdata[[i]]$ID5=splitdata[[i]]$ID
    splitdata[[i]]$ID6=splitdata[[i]]$ID
    splitdata[[i]]$ID7=splitdata[[i]]$ID
    splitdata[[i]]$ID8=splitdata[[i]]$ID
    splitdata[[i]]$ID9=splitdata[[i]]$ID
    splitdata[[i]]$ID10=splitdata[[i]]$ID
    splitdata[[i]]$ID11=splitdata[[i]]$ID
    splitdata[[i]]$ID12=splitdata[[i]]$ID
    splitdata[[i]]$ID13=splitdata[[i]]$ID
    splitdata[[i]]$ID14=splitdata[[i]]$ID
  }
  
  
  
  
  formula.random <-OBSERVED ~ distance_from_dip_scaled + canopyheight_scaled +  CanopyShape_scaled + BCI_slope_scaled + crden_clip_scaled + crth_clip_scaled + gap5m_clip_scaled + CanopyCover_20m_scaled +LatCon_3m_prj_scaled + steplength_scaled + steplengthdiff_scaled + orientation_scaled +turnangles_scaled+
    f(REFID,model="iid",hyper=list(theta = list(initial=log(1e-6),fixed=T))) +
    f(ID,model="iid", hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID2,distance_from_dip_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID3,canopyheight_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID4,CanopyShape_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID5,BCI_slope_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID6,crden_clip_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID7,crth_clip_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID8,gap5m_clip_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID9,CanopyCover_20m_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID10,LatCon_3m_prj_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID11,steplength_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID12,steplengthdiff_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID13,orientation_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
    f(ID14,turnangles_scaled,model="iid",
      hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) 
  
  r.inla.random_ateles <- inla(formula.random,
                                        family ="Poisson", 
                                        data=splitdata[[1]], 
                                        inla.mode="experimental",
                                        verbose=TRUE,
                                        control.compute=list(openmp.strategy="huge"),
                                        control.fixed = list(
                                          mean = mean.beta,
                                          prec = list(default = prec.beta)
                                        )
  )
  
  save(r.inla.random_ateles, file=paste("~/r.inla.random_ateles_",lags[q],"_lag.RData", sep=""))
  
  gc()
  r.inla.random_cebus <- inla(formula.random,
                                       family ="Poisson", 
                                       data=splitdata[[2]], 
                                       inla.mode="experimental",
                                       verbose=TRUE,
                                       control.compute=list(openmp.strategy="huge"),
                                       control.fixed = list(
                                         mean = mean.beta,
                                         prec = list(default = prec.beta)
                                       )
  )    
  
  save(r.inla.random_cebus, file=paste("~/r.inla.random_cebus",lags[q],"_lag.RData", sep=""))
  
  gc()
  r.inla.random_nasua <- inla(formula.random,
                                       family ="Poisson", 
                                       data=splitdata[[3]], 
                                       inla.mode="experimental",
                                       verbose=TRUE,
                                       control.compute=list(openmp.strategy="huge"),
                                       control.fixed = list(
                                         mean = mean.beta,
                                         prec = list(default = prec.beta)
                                       )
  )    
  
  save(r.inla.random_nasua, file="~/r.inla.random_nasua_four_lag.RData")
  save(r.inla.random_nasua, file=paste("~/r.inla.random_nasua",lags[q],"_lag.RData", sep=""))
  
  
  gc()
  r.inla.random_potos <- inla(formula.random,
                                       family ="Poisson", 
                                       data=splitdata[[4]], 
                                       inla.mode="experimental",
                                       verbose=TRUE,
                                       control.compute=list(openmp.strategy="huge"),
                                       control.fixed = list(
                                         mean = mean.beta,
                                         prec = list(default = prec.beta)
                                       )
  )    
  
  save(r.inla.random_potos, file=paste("~/r.inla.random_potos",lags[q],"_lag.RData", sep=""))
  
  gc()
  
}