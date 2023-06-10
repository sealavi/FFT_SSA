library(brms)
library(ggplot2)
options(mc.cores = parallel::detectCores())

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
splitdata=split(track_Steps,track_Steps$Species)

for(i in 1:length(splitdata)){
  
  
  splitdata[[i]]$ID=as.factor(splitdata[[i]]$ID)
  splitdata[[i]]=splitdata[[i]][order(splitdata[[i]]$REFID),]
  splitdata[[i]]$steplength_scaled=(splitdata[[i]]$steplength-mean(splitdata[[i]]$steplength,na.rm=TRUE))/sd(splitdata[[i]]$steplength,na.rm=TRUE)
  splitdata[[i]]$steplengthdiff_scaled=(splitdata[[i]]$steplengthdiff-mean(splitdata[[i]]$steplengthdiff,na.rm=TRUE))/sd(splitdata[[i]]$steplengthdiff,na.rm=TRUE)
  splitdata[[i]]$orientation_scaled=(splitdata[[i]]$orientation-mean(splitdata[[i]]$orientation,na.rm=TRUE))/sd(splitdata[[i]]$orientation,na.rm=TRUE)
  splitdata[[i]]$turnangles_scaled=(splitdata[[i]]$turnangles-mean(splitdata[[i]]$turnangles,na.rm=TRUE))/sd(splitdata[[i]]$turnangles,na.rm=TRUE)
  splitdata[[i]]$CanopyShape_scaled=(splitdata[[i]]$CanopyShape-mean(splitdata[[i]]$CanopyShape,na.rm=TRUE))/sd(splitdata[[i]]$CanopyShape,na.rm=TRUE)
  splitdata[[i]]$BCI_slope_scaled=(splitdata[[i]]$BCI_slope-mean(splitdata[[i]]$BCI_slope,na.rm=TRUE))/sd(splitdata[[i]]$BCI_slope,na.rm=TRUE)
  splitdata[[i]]$crden_clip_scaled=(splitdata[[i]]$crden_clip-mean(splitdata[[i]]$crden_clip,na.rm=TRUE))/sd(splitdata[[i]]$crden_clip,na.rm=TRUE)
  splitdata[[i]]$crth_clip_scaled=(splitdata[[i]]$crth_clip-mean(splitdata[[i]]$crth_clip,na.rm=TRUE))/sd(splitdata[[i]]$crth_clip,na.rm=TRUE)
  splitdata[[i]]$gap5m_clip_scaled=(splitdata[[i]]$gap5m_clip-mean(splitdata[[i]]$gap5m_clip,na.rm=TRUE))/sd(splitdata[[i]]$gap5m_clip,na.rm=TRUE)
  splitdata[[i]]$CanopyCover_20m_scaled=(splitdata[[i]]$CanopyCover_20m-mean(splitdata[[i]]$CanopyCover_20m,na.rm=TRUE))/sd(splitdata[[i]]$CanopyCover_20m,na.rm=TRUE)
  splitdata[[i]]$LatCon_3m_prj_scaled=(splitdata[[i]]$LatCon_3m_prj-mean(splitdata[[i]]$LatCon_3m_prj,na.rm=TRUE))/sd(splitdata[[i]]$LatCon_3m_prj,na.rm=TRUE)
  splitdata[[i]]$cumulativeElevationGain_scaled=(splitdata[[i]]$cumulativeElevationGain-mean(splitdata[[i]]$cumulativeElevationGain,na.rm=TRUE))/sd(splitdata[[i]]$cumulativeElevationGain,na.rm=TRUE)
  splitdata[[i]]$distance_from_dip_scaled=(splitdata[[i]]$distance_from_dip-mean(splitdata[[i]]$distance_from_dip,na.rm=TRUE))/sd(splitdata[[i]]$distance_from_dip,na.rm=TRUE)
  splitdata[[i]]$canopyheight_scaled=(splitdata[[i]]$canopyheight-mean(splitdata[[i]]$canopyheight,na.rm=TRUE))/sd(splitdata[[i]]$canopyheight,na.rm=TRUE)

  
}
track_Steps=track_Steps[which(track_Steps$OBSERVED==1),]
names(track_Steps)
hist(track_Steps$canopyheight)
canopyheight_model=brm(bf(canopyheight~Species+(1|ID), sigma ~ Species),
                       family = student,
                       data=track_Steps)
summary(canopyheight_model)
canopyheight_plot=plot(conditional_effects(canopyheight_model,spaghetti=FALSE))[[1]]

canopyheight_est <- as.data.frame(canopyheight_plot[[1]])

my_custom_labels = c(" "," "," "," ")

canopyheight_plot=ggplot() +
  geom_errorbar(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), width = 0.2,
    canopyheight_est,
  ) +
  geom_pointrange(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), 
    canopyheight_est,
  ) +
  ylab("Canopy height")+ggtitle("a.")+xlab(" ")+
  scale_x_discrete(labels = my_custom_labels)+
  theme(axis.ticks.x = element_blank())+
  theme_classic()

save(canopyheight_model, file="canopyheight_model.RData")

CanopyShape_model=brm(bf(CanopyShape~Species+(1|ID), sigma ~ Species),
                       family = student,
                       data=track_Steps)
summary(CanopyShape_model)
plot(conditional_effects(CanopyShape_model,spaghetti=FALSE))
save(CanopyShape_model, file="CanopyShape_model.RData")

CanopyShape_plot=plot(conditional_effects(CanopyShape_model,spaghetti=FALSE))[[1]]

CanopyShape_model_est <- as.data.frame(CanopyShape_plot[[1]])
CanopyShape_plot=ggplot() +
  geom_errorbar(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), width = 0.2,
    CanopyShape_model_est,
  ) +
  geom_pointrange(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), 
    CanopyShape_model_est,
  ) +
  ylab("Canopy shape")+ggtitle("b.")+xlab(" ")+
  scale_x_discrete(labels = my_custom_labels)+
  theme(axis.ticks.x = element_blank())+
  theme_classic()
BCI_slope_model=brm(bf(BCI_slope~Species+(1|ID), sigma ~ Species),
                      family = student,
                      data=track_Steps)
summary(BCI_slope_model)
plot(conditional_effects(BCI_slope_model,spaghetti=FALSE))
save(BCI_slope_model, file="BCI_slope_model.RData")

BCI_slope_plot=plot(conditional_effects(BCI_slope_model,spaghetti=FALSE))[[1]]
BCI_slope_est <- as.data.frame(BCI_slope_plot[[1]])
BCI_slope_plot=ggplot() +
  geom_errorbar(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), width = 0.2,
    BCI_slope_est,
  ) +
  geom_pointrange(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), 
    BCI_slope_est,
  ) +
  ylab("Slope")+ggtitle("c.")+xlab(" ")+
  scale_x_discrete(labels = my_custom_labels)+
  theme(axis.ticks.x = element_blank())+
  theme_classic()

crden_clip_model=brm(bf(crden_clip~Species+(1|ID), sigma ~ Species),
                    family = student,
                    data=track_Steps)
summary(crden_clip_model)
plot(conditional_effects(crden_clip_model,spaghetti=FALSE))
save(crden_clip_model, file="crden_clip_model.RData")

crden_clip_plot=plot(conditional_effects(crden_clip_model,spaghetti=FALSE))[[1]]
crden_clip_est <- as.data.frame(crden_clip_plot[[1]])
crden_clip_plot=ggplot() +
  geom_errorbar(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), width = 0.2,
    crden_clip_est,
  ) +
  geom_pointrange(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), 
    crden_clip_est,
  ) +
  ylab("Crown density")+ggtitle("d.")+
  scale_x_discrete(labels = my_custom_labels)+
  theme(axis.ticks.x = element_blank())+
  theme_classic()

crth_clip_model=brm(bf(crth_clip~Species+(1|ID), sigma ~ Species),
                     family = student,
                     data=track_Steps)
summary(crth_clip_model)
plot(conditional_effects(crth_clip_model,spaghetti=FALSE))
save(crth_clip_model, file="crth_clip_model.RData")

crth_clip_plot=plot(conditional_effects(crth_clip_model,spaghetti=FALSE))[[1]]
crth_clip_est <- as.data.frame(crth_clip_plot[[1]])
crth_clip_plot=ggplot() +
  geom_errorbar(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), width = 0.2,
    crth_clip_est,
  ) +
  geom_pointrange(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), 
    crth_clip_est,
  ) +
  ylab("Crown thickness")+ggtitle("e.")+
  scale_x_discrete(labels = my_custom_labels)+
  theme(axis.ticks.x = element_blank())+
  theme_classic()

CanopyCover_20m_model=brm(bf(CanopyCover_20m~Species+(1|ID), sigma ~ Species),
                     family = student,
                     data=track_Steps)
summary(CanopyCover_20m_model)
plot(conditional_effects(CanopyCover_20m_model,spaghetti=FALSE))
save(CanopyCover_20m_model, file="CanopyCover_20m_model.RData")


LatCon_3m_prj_model=brm(bf(LatCon_3m_prj~Species+(1|ID), sigma ~ Species),
                          family = student,
                          data=track_Steps)
summary(LatCon_3m_prj_model)
plot(conditional_effects(LatCon_3m_prj_model,spaghetti=FALSE))
save(LatCon_3m_prj_model, file="LatCon_3m_prj_model.RData")

LatCon_3m_prj_plot=plot(conditional_effects(LatCon_3m_prj_model,spaghetti=FALSE))[[1]]
LatCon_3m_prj_est <- as.data.frame(LatCon_3m_prj_plot[[1]])
LatCon_3m_prj_plot=ggplot() +
  geom_errorbar(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), width = 0.2,
    LatCon_3m_prj_est,
  ) +
  geom_pointrange(
    aes(Species, estimate__, ymin = lower__, ymax = upper__, color = Species), 
    LatCon_3m_prj_est,
  ) +
  ylab("Lateral continuity")+ggtitle("f.")+
  scale_x_discrete(labels = my_custom_labels)+
  theme(axis.ticks.x = element_blank())+
  theme_classic()

library(ggplot2)
library(ggpubr)
ggarrange(canopyheight_plot,
          CanopyShape_plot,
          BCI_slope_plot,
          crden_clip_plot,
          crth_clip_plot,
          LatCon_3m_prj_plot,
          common.legend = TRUE, legend = "bottom")
ggsave("SSF used canopy values.pdf", units="in", width=10.375, height=5.344,dpi=300, device = "pdf")
