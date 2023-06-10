
library(sp)
library(spdep)
library(rgeos)
library(rgdal)
library(raster)
library(maptools)

#Load and georeference tree crowns and island boundary

crowns= readOGR("C:/Users/salavi/Documents/BCI_Dipteryx_Patches.shp")
crowns2 <- spTransform(crowns, CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

outline= readOGR("C:/Users/salavi/Documents/BCI_outline.shp")
outline <- spTransform(outline, CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##Load cluster quest filtered data
cquest=read.csv("C:/Users/salavi/Documents/smoove_filtered_2020_final.csv")
cquest$timestamp<-as.POSIXct(cquest$timestamp, format="%Y-%m-%d %H:%M:%S",origin="01-01-1900", tz="America/Panama")

cquestall=split(cquest,cquest$individual.local.identifier)

##Project data
All_points=SpatialPointsDataFrame(coords=cquest[c(34,35)],data=cquest,proj4string = CRS('+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'))
all_visits <- All_points %over% crowns2 ##Associate movement data with crowns
cquest$PatchID =all_visits$PatchID
cquest$PatchID_calc=NA
cquest$PatchID_prev=NA
colnames(cquest)[29]="id"
colnames(cquest)[34]="x"
colnames(cquest)[35]="y"
cquest$Z<- cquest$x + 1i*cquest$y
cquest2all=split(cquest,cquest$id)
cquest2all=cquest2all[-c(3,30,42)]

for (i in 1:46){
  
  tmp=which(!is.na(cquest2all[[i]]$PatchID))
  cquest2all[[i]]$PatchID_calc[seq(from=1,to=tmp[1],by=1)]=cquest2all[[i]]$PatchID[tmp[1]]
  for (j in 1:(length(tmp)-1)){
    x1=tmp[j]+1
    x2=tmp[j+1]
    cquest2all[[i]]$PatchID_calc[seq(from=x1,to=x2,by=1)]=cquest2all[[i]]$PatchID[tmp[j+1]]
  }
  cquest2all[[i]]$PatchID_calc[seq(from=tmp[(length(tmp)-1)],to=tmp[length(tmp)],by=1)]=cquest2all[[i]]$PatchID[tmp[length(tmp)]]
  cquest2all[[i]]$PatchID_calc[tmp]=cquest2all[[i]]$PatchID[tmp]
}

##keep track of current and previously visited patch

for (i in 1:46){
  
  tmp=which(!is.na(cquest2all[[i]]$PatchID))
  for (j in 1:(length(tmp)-1)){
    x1=tmp[j]+1
    x2=tmp[j+1]
    cquest2all[[i]]$PatchID_prev[seq(from=x1,to=x2,by=1)]=cquest2all[[i]]$PatchID[tmp[j]]
  }
  cquest2all[[i]]$PatchID_prev[seq(from=tmp[(length(tmp)-1)],to=tmp[length(tmp)],by=1)]=cquest2all[[i]]$PatchID[tmp[length(tmp)]]
  cquest2all[[i]]$PatchID_prev[tmp]=cquest2all[[i]]$PatchID[tmp]
}
#library(plyr)
# cquest3 <- plyr::ldply(cquest2all, data.frame)
# cquest4=cquest3
# cquest4all=split(cquest4,cquest4$id)
# cquest5=c()
# for (i in 1:46){
#   tempdat=cquest4all[[i]]
#   n <- length(tempdat$Z)
#   Z0 <- tempdat$Z[-((n-1):n)]
#   Z1 <- tempdat$Z[-c(1,n)]
#   PatchID_calc=tempdat$PatchID_calc[-c(1,n)]
#   PatchID_prev=tempdat$PatchID_prev[-c(1,n)]
#   REFID=seq(from=1,to=length(Z1),by=1)
#   ID=as.character(tempdat$id[-c(1,n)])
#   tempdat2=cbind(ID,Z0,Z1,REFID,PatchID_calc,PatchID_prev)
#   tempdat2=as.data.frame(tempdat2)
#   cquest5[i]=list(tempdat2)
# }
# cquest5all <- plyr::ldply(cquest5, data.frame)
# cquest5all$ID=as.factor(cquest5all$ID)
# cquest5all$Z0=as.complex(as.character(cquest5all$Z0))
# cquest5all$Z1=as.complex(as.character(cquest5all$Z1))
# cquest5all$REFID=as.character(cquest5all$REFID)
# cquest5all$PatchID_calc=as.numeric(as.character(cquest5all$PatchID_calc))
# cquest5all$PatchID_prev=as.numeric(as.character(cquest5all$PatchID_prev))
# 
# cquest5all=split(cquest5all,cquest5all$ID)


##Associate raster data with movement data

canopyheight=raster("C:/Users/salavi/Documents/canht_clip.tif")
CanopyShape=raster("C:/Users/salavi/Documents/CanopyShape.tif")
BCI_slope=raster("C:/Users/salavi/Documents/BCI_slope.tif")
crden_clip=raster("C:/Users/salavi/Documents/crden_clip.tif")
crth_clip=raster("C:/Users/salavi/Documents/crth_clip.tif")
gap5m_clip=raster("C:/Users/salavi/Documents/gap5m_clip.tif")
CanopyCover_10m=raster("C:/Users/salavi/Documents/CanopyCover_10m.tif")
CanopyCover_20m=raster("C:/Users/salavi/Documents/CanopyCover_20m.tif")
CanopyCover_30m=raster("C:/Users/salavi/Documents/CanopyCover_30m.tif")
LatCon_3m_prj=raster("C:/Users/salavi/Documents/LatCon_3m_prj.tif")
LatCon_5m_prj=raster("C:/Users/salavi/Documents/LatCon_5m_prj.tif")
top_clip=raster("C:/Users/salavi/Documents/top_clip.tif")
BCNM_LiDAR_DEM=raster("C:/Users/salavi/Documents/BCNM_LiDAR_DEM.tif")

#Reproject 
crs(canopyheight)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(CanopyShape)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(BCI_slope)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(crden_clip)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(crth_clip)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(gap5m_clip)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(CanopyCover_10m)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(CanopyCover_20m)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(CanopyCover_30m)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(LatCon_3m_prj)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(LatCon_5m_prj)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(top_clip)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(BCNM_LiDAR_DEM)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

##Downsample data so all are 10m scale
canopyheight=aggregate(canopyheight,fact = 10/1.12)
BCI_slope=aggregate(BCI_slope,fact = 10)
gap5m_clip=aggregate(gap5m_clip,fact = 10/1.12)
BCNM_LiDAR_DEM=aggregate(BCNM_LiDAR_DEM,fact = 10)

library(fitdistrplus)

#count=1
#alldata=c()
timescales=c(1,2,3,4,8,15,30,60) ##Initialize timescales of interest
timescale_labels=c("nolag","_one_lag","_two_lag","_three_lag","_four_lag","_five_lag","_six_lag","_seven_lag")

save_tracks=c()
timescales=c(1,2,3,4,8,15,30,60)

for(lags in 1:length(timescales)){
  for(q in 1:46){
    print(q)

    track=cquest2all[[q]][seq(1, nrow(cquest2all[[q]]), timescales[lags]), ]
    
    #Calculate steps, turn angles, and autocorrelation metrics
    track$Z <- track$x + 1i*track$y
    steplength <- Mod(diff(track$Z))
    steplength <- c(NA,steplength)
    track$steplength <- steplength
    orientations=Arg(diff(track$Z))
    orientations=c(NA,orientations)
    turnangles=diff(orientations)
    turnangles=c(NA,turnangles)
    track$orientations=orientations
    track$turnangles=turnangles
    steplengthdiff2 <- c(NA,diff(steplength))
    track$steplengthdiff <- steplengthdiff2
    
    #Fit best fit theoretical distribution
    fit.gamma1 <- try(fitdist(as.numeric(na.omit(as.numeric(steplength))), distr = "gamma", method = "mle",lower = c(0, 0)))
    if(as.character(class(fit.gamma1))=="try-error"){
      fit.gamma1 <- fitdist(as.numeric(na.omit(as.numeric(steplength))), distr = "gamma", method = "mme",lower = c(0, 0))
    }
    shape1=fit.gamma1$estimate[[1]]
    rate1=fit.gamma1$estimate[[2]]
    
    Z=track$Z
    time=track$timestamp
    STL=track$steplength
    STDIFF=track$steplengthdiff
    ORIENT=track$orientations
    TURNS=track$turnangles
    
    n <- length(Z)
    S <- Mod(diff(Z))
    Phi <- Arg(diff(Z))
    Theta <- diff(Phi)
    
    
    # calculate null set
    Z0 <- Z[-((n-1):n)]
    Z1 <- Z[-c(1,n)]
    time0 <- time[-((n-1):n)]
    time1 <- time[-c(1,n)]
    STL0 <- STL[-((n-1):n)]
    STL1 <- STL[-c(1,n)]
    STDIFF0 <- STDIFF[-((n-1):n)]
    STDIFF1 <- STDIFF[-c(1,n)]
    ORIENT0 <- ORIENT[-((n-1):n)]
    ORIENT1 <- ORIENT[-c(1,n)]
    TURNS0 <- TURNS[-((n-1):n)]
    TURNS1 <- TURNS[-c(1,n)]
    
    #refid is reference ID to associate true steps from simulated steps
    
    Real.refid=seq(1,length(Z1),by=1)
    Real.refid.sample=rep(1,length(Z1))
    Real.refid.observed=rep(1,length(Z1))
    realtmp=data.frame(cbind(Z1,Z0,as.character(time1),as.character(Real.refid),as.character(Real.refid.sample),as.character(Real.refid.observed),STL1,STDIFF1,ORIENT1,TURNS1))
    colnames(realtmp)=c("Z","Z0","Time","REFID","SAMPLEID","OBSERVED","steplength","steplengthdiff","orientation","turnangles")
    realtmp$Z=as.character(realtmp$Z)
    realtmp$Z=as.complex(realtmp$Z)
    
    #Prepare matrices to store real and null steps
    Z.temp <- matrix(0, ncol=30, nrow=n-2)
    Z.temp2 <- matrix(0, ncol=30, nrow=n-2)
    z.refid <- matrix(0, ncol=30, nrow=n-2)
    z.sample <- matrix(0, ncol=30, nrow=n-2)
    z.observed <- matrix(0, ncol=30, nrow=n-2)
    z.steplength <- matrix(0, ncol=30, nrow=n-2)
    z.steplengthdiff <- matrix(0, ncol=30, nrow=n-2)
    z.orientations <- matrix(0, ncol=30, nrow=n-2)
    z.turnangles <- matrix(0, ncol=30, nrow=n-2)
    Z.0 <- matrix(0, ncol=30, nrow=n-2)
    
    
    for(i in 1:length(Z1)){
      S=rgamma(30, shape=shape1, rate = rate1) #generate step length from best fit theoretical distribution
      Theta=runif(30, min = 0, max = 2*pi) #simulated random turning angle
      RelSteps <- complex(mod = S, arg=Arg(Z1[i])) #combine to vector
      Rotate <- complex(mod = 1, arg=Theta)
      Z.temp[i,] <- Z1[i] + RelSteps * Rotate
      Z.temp2[i,]=rep(as.character(time1[i]),30)
      z.refid[i,]=rep(i,30)
      z.sample[i,]=seq(2,(30+1),by=1)
      z.observed[i,]=rep(0,30)
      z.steplength[i,]= Mod(diff(c(Z1[i],c(Z1[i] + RelSteps * Rotate))))
      z.steplengthdiff[i,] <- diff(c(STL1[i],c(Mod(diff(c(Z1[i],c(Z1[i] + RelSteps * Rotate)))))))
      z.orientations[i,] <- Arg(diff(c(Z1[i],c(Z1[i] + RelSteps * Rotate))))
      z.turnangles[i,] <- diff(c(Arg(Z1[i]),c(Arg(diff(c(Z1[i],c(Z1[i] + RelSteps * Rotate)))))))
      Z.0[i,] <-rep(Z0[i],30)
      
    }
    
    
    Z.temp=as.data.frame(as.table(Z.temp))
    Z.temp2=as.data.frame(as.table(Z.temp2))
    z.refid=as.data.frame(as.table(z.refid))
    z.sample=as.data.frame(as.table(z.sample))
    z.observed=as.data.frame(as.table(z.observed))
    z.steplength=as.data.frame(as.table(z.steplength))
    z.steplengthdiff=as.data.frame(as.table(z.steplengthdiff))
    z.orientations=as.data.frame(as.table(z.orientations))
    z.turnangles=as.data.frame(as.table(z.turnangles))
    Z.0=as.data.frame(as.table(Z.0))
    
    tmp=as.data.frame(cbind(Z.temp$Freq,Z.0$Freq,as.character(Z.temp2$Freq),as.character(z.refid$Freq),as.character(z.sample$Freq),as.character(z.observed$Freq),as.character(z.steplength$Freq),as.character(z.steplengthdiff$Freq),as.character(z.orientations$Freq),as.character(z.turnangles$Freq)))
    colnames(tmp)=c("Z","Z0","Time","REFID","SAMPLEID","OBSERVED","steplength","steplengthdiff","orientation","turnangles")
    
    
    
    track_Steps=rbind(realtmp,tmp)
    track_Steps$Z=as.character(track_Steps$Z)
    track_Steps$Z=as.complex(track_Steps$Z)
    track_Steps$Z0=as.character(track_Steps$Z0)
    track_Steps$Z0=as.complex(track_Steps$Z0)
    track_Steps$Time=as.POSIXct(as.character(track_Steps$Time),format="%Y-%m-%d %H:%M:%S",origin="01-01-1900")
    track_Steps$X=Re(track_Steps$Z)
    track_Steps$Y=Im(track_Steps$Z)
    track_Steps$X0=Re(track_Steps$Z0)
    track_Steps$Y0=Im(track_Steps$Z0)
    track_Steps=na.omit(track_Steps)
    track_Steps$REFID=as.integer(track_Steps$REFID)
    track_Steps$OBSERVED=as.numeric(as.character(track_Steps$OBSERVED))
    track_Steps$steplength=as.numeric(as.character(track_Steps$steplength))
    track_Steps$steplengthdiff=as.numeric(as.character(track_Steps$steplengthdiff))
    track_Steps$orientation=as.numeric(as.character(track_Steps$orientation))
    track_Steps$turnangles=as.numeric(as.character(track_Steps$turnangles))
    
    track_Steps$canopyheight=raster::extract(canopyheight, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$CanopyShape=raster::extract(CanopyShape, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$BCI_slope=raster::extract(BCI_slope, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$crden_clip=raster::extract(crden_clip, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$crth_clip=raster::extract(crth_clip, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$gap5m_clip=raster::extract(gap5m_clip, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$CanopyCover_10m=raster::extract(CanopyCover_10m, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$CanopyCover_20m=raster::extract(CanopyCover_20m, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$CanopyCover_30m=raster::extract(CanopyCover_30m, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$LatCon_3m_prj=raster::extract(LatCon_3m_prj, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$LatCon_5m_prj=raster::extract(LatCon_5m_prj, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$top_clip=raster::extract(top_clip, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    #track_Steps$BCI_Soil=extract(BCI_Soil, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    track_Steps$BCNM_LiDAR_DEM=raster::extract(BCNM_LiDAR_DEM, SpatialPoints(as.data.frame(cbind(track_Steps$X,track_Steps$Y))))
    
    #calculate cumulative elevatoin accross step
    samplingInterval <- 5
    npts <- floor(track_Steps[,7]/samplingInterval)
    segDat2 <- cbind(track_Steps$X0,track_Steps$X, npts)
    segDat3 <- cbind(track_Steps$Y0,track_Steps$Y, npts)
    xpts <- apply(segDat2, 1, function(v) seq(from=v[1], to=v[2], length.out=2+v[3]))
    ypts <- apply(segDat3, 1, function(v) seq(from=v[1], to=v[2], length.out=2+v[3]))
    track_Steps$index=1:nrow(track_Steps)
    names(xpts) <- names(ypts) <- track_Steps[,28]
    resampledPts <- data.frame(x=unlist(xpts), y=unlist(ypts), id=rep(track_Steps[,28], unlist(lapply(xpts, length))))
    resampledPtsSpatialObj <- SpatialPoints(coords= resampledPts[,1:2])
    rasterValueAtEachPoint <- raster::extract(BCNM_LiDAR_DEM, resampledPtsSpatialObj)##voilÃ
    ##calculate cumulative elevation gain per segment
    resampledPts2 <- data.frame(resampledPts, rasterValueAtEachPoint)
    cumulativeElevationGain <- tapply(resampledPts2[,4], resampledPts2[,3], function(v) {
      temp <- v[-1]-v[-length(v)]
      return(sum(temp[temp>0]))
    })
    track_Steps$cumulativeElevationGain=as.numeric(cumulativeElevationGain)
    
    #Prepare to calculate distance from nearest dipteryx 
    files=list.files("C:/Users/salavi/Documents/Shapefiles",pattern=".shp",full.names=TRUE)
    UD=readOGR(files[[q]])
    
    
    track_UD=spTransform(UD, CRS('+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'))
    
    crowns_centroid= gCentroid(crowns2,byid=TRUE)
    crowns_centroid=as.data.frame(crowns_centroid)
    crowns_centroid$z=crowns_centroid$x+1i*crowns_centroid$y
    crowns_centroid$polyID=as.numeric(rownames(crowns_centroid))+1
    
    track_trees=crop(crowns2,track_UD)
    
    track_centroid= gCentroid(track_trees,byid=TRUE)
    
    track_centroid=as.data.frame(track_centroid)
    
    track_centroid$z=track_centroid$x+1i*track_centroid$y
    
    track_centroid$polyID=as.numeric(rownames(track_centroid))+1
    
    
    track_Steps$distance_from_dip=NA
    treelists=list()
    for (i in 1:length(levels(as.factor(track_Steps$REFID)))){
      if(is.na(track$PatchID_prev[i])){treelists[i]=list(track_centroid)}else{
        if(nrow(track_centroid[-which(as.character(track_centroid$polyID)==as.character(track$PatchID_prev[i])),])==0){
          treelists[i]=list(crowns_centroid[-which(as.character(crowns_centroid$polyID)==as.character(track$PatchID_prev[i])),])
        } else {treelists[i]=list(track_centroid[-which(as.character(track_centroid$polyID)==as.character(track$PatchID_prev[i])),])}
      } 
      
    }
    track_Steps$REFID=as.numeric(track_Steps$REFID)
    for (i in 1:length(unique(track_Steps$REFID))){
      dips=treelists[[i]]
      for(j in 1:length(which(track_Steps$REFID==i))){
        if(length(which(track_Steps$REFID==i))==0){next}else{
          track_Steps$distance_from_dip[which(track_Steps$REFID==i)][j]=min(Mod(track_Steps[which(track_Steps$REFID==i),1][j]-dips$z))
          
        }
        #setTxtProgressBar(pb,i)
        
      }
    }
    track_Steps$distance_from_dip=as.numeric(track_Steps$distance_from_dip)
    save_tracks[q]=list(track_Steps)
    
  }
  save(save_tracks, file = paste("ssfdata",timescale_labels[lag], sep = ""))
}
