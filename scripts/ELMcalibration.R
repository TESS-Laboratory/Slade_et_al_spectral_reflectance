#make raster reflectance stacks from pix4D outputs

library(rgdal)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(MASS)
library(splines)
library(rgeos)

#Tramway ROI 

tramwayROI <- readOGR(dsn = 'D:/New_Mexico/JOR_Tramway_Project', layer = "TramwayROI")

canvasROIs <- readOGR(dsn = 'D:/New_Mexico/JOR_Tramway_Project', layer = "largeCalTargets")

seqOwnPanelReflStackCrop <- stack("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ownPanel/TRM_2_SEQ_proj2lines_ownPanelRefl_stackcrop.tif")
names(seqOwnPanelReflStackCrop) <- c('green','red','redEdge','NIR')

MREOwnPanelReflStackCrop <- stack("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ownPanel/TRM_2_MRE_proj2lines_ownPanelRefl_stackcrop.tif")
names(MREOwnPanelReflStackCrop) <- c('blue','green','red','redEdge','NIR')

#Spectra

setwd('D:/New_Mexico/JOR_Tramway_Project/JOR_SPE/spectradata/')


filenames <- list.files(getwd(), pattern="*.txt", full.names=TRUE)

#get spectrometer wavelengths
wavelengths <- read.table(filenames[1],skip=14,header=F)[,1]


#Spectralon FSF reference (pre-clean)

WRFSFref <- read.csv("D:/Spectroscopy/DroneLabPanelsCalibrations/smallWhiteSpectralon/Exeter WR Panel -- Pre-clean.csv")
WRcalvalues <- 1/WRFSFref$Reflectance


#perform linear interpolation to get reflectance for each wavelength (not needed if already resolved per wavelength)
interpolateSpectrum <- function(inputSpec){
  
  interpSpec <- approx(inputSpec[,1],inputSpec[,2],xout=seq(ceiling(min(inputSpec[,1])),floor(max(inputSpec[,1])),1))
  interpSpec <- data.frame(interpSpec)
  names(interpSpec) <- c('wvl','refl')
  return(interpSpec)
} 


#MRE TRM 2 ELM

#MRE TRM 2 DC value

DCstartind <- 376
DCendind <- 385 

DCmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(DCstartind:DCendind))

for(i in DCstartind:DCendind){
spectrum <- read.table(filenames[i],skip=14,header=F)
names(spectrum) <- c('wvl','rad')
plot(spectrum,type='n')
lines(spectrum)
DCmat[,i-DCstartind+1] <- spectrum$rad
}

DCmedianvec <- apply(DCmat, 1, median, na.rm=T)
DCmedianvec[DCmedianvec<0] <- 0

#MRE TRM 2 black canvas refl

WRstartind <- 396
WRendind <- 400

WRmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(WRstartind:WRendind))

for(i in WRstartind:WRendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  WRmat[,i-WRstartind+1] <- spectrum$rad
}

WRmedianvec <- apply(WRmat, 1, median, na.rm=T)
WRmedianvec[WRmedianvec<0] <- 0

blackstartind <- 401
blackendind <- 405


blackmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(blackstartind:blackendind))


for(i in blackstartind:blackendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  blackmat[,i-blackstartind+1] <- spectrum$rad
}

blackmedianvec <- apply(blackmat, 1, mean, na.rm=T)
blackmedianvec[blackmedianvec<0] <- 0

blackrefl <- ((blackmedianvec-DCmedianvec)/(WRmedianvec-DCmedianvec))
blackrefldf <- data.frame(wvl=wavelengths,black=blackrefl)

blackrefldfinterpnorm <- interpolateSpectrum(blackrefldf[26:2048,])
blackrefldfinterpnorm$refl <- blackrefldfinterpnorm$refl/WRcalvalues[1:680]



#MRE TRM 2 grey canvas refl

WRstartind <- 406
WRendind <- 410

WRmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(WRstartind:WRendind))

for(i in WRstartind:WRendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  WRmat[,i-WRstartind+1] <- spectrum$rad
}

WRmedianvec <- apply(WRmat, 1, median, na.rm=T)
WRmedianvec[WRmedianvec<0] <- 0

greystartind <- 411
greyendind <- 415


greymat <- matrix(data=NA,nrow=length(15:2062),ncol=length(greystartind:greyendind))


for(i in greystartind:greyendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  greymat[,i-greystartind+1] <- spectrum$rad
}

greymedianvec <- apply(greymat, 1, mean, na.rm=T)
greymedianvec[greymedianvec<0] <- 0

greyrefl <- ((greymedianvec-DCmedianvec)/(WRmedianvec-DCmedianvec))
greyrefldf <- data.frame(wvl=wavelengths,grey=greyrefl)


greyrefldfinterpnorm <- interpolateSpectrum(greyrefldf[26:2048,])
greyrefldfinterpnorm$refl <- greyrefldfinterpnorm$refl/WRcalvalues[1:680]


#function to calculate Micasense RedEdge spectrum, NOTE: To be updated if RSRFs become available
getREMSpec <- function(inputSpec){
  
  #or if spectra is already per wvl: 
  interpSpec <- inputSpec
  if(length(inputSpec)>2){
    blueFactors <- colMeans(interpSpec[interpSpec$wvl>=(475-20/2)&interpSpec$wvl<=(475+20/2),-1])
    greenFactors <- colMeans(interpSpec[interpSpec$wvl>=(560-20/2)&interpSpec$wvl<=(560+20/2),-1])#*sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])/sum(sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])
    redFactors <- colMeans(interpSpec[interpSpec$wvl>=(668-10/2)&interpSpec$wvl<=(668+10/2),-1])#*sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])/sum(sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])
    redEdgFactors <- colMeans(interpSpec[interpSpec$wvl>=(717-10/2)&interpSpec$wvl<=(717+10/2),-1])#*sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])/sum(sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])
    NIRFactors <- colMeans(interpSpec[interpSpec$wvl>=(840-40/2)&interpSpec$wvl<=(840+40/2),-1])#*sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])/sum(sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])
  }else{
    blueFactors <- mean(interpSpec[interpSpec$wvl>=(475-20/2)&interpSpec$wvl<=(475+20/2),-1])
    greenFactors <- mean(interpSpec[interpSpec$wvl>=(560-20/2)&interpSpec$wvl<=(560+20/2),-1])#*sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])/sum(sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])
    redFactors <- mean(interpSpec[interpSpec$wvl>=(668-10/2)&interpSpec$wvl<=(668+10/2),-1])#*sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])/sum(sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])
    redEdgFactors <- mean(interpSpec[interpSpec$wvl>=(717-10/2)&interpSpec$wvl<=(717+10/2),-1])#*sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])/sum(sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])
    NIRFactors <- mean(interpSpec[interpSpec$wvl>=(840-40/2)&interpSpec$wvl<=(840+40/2),-1])#*sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])/sum(sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])
  }
  reflFactors <- cbind(blueFactors,greenFactors,redFactors,redEdgFactors,NIRFactors)
  cwvl <- cbind(475,560,668,717,840)
  REMRefl <- data.frame(rbind(cwvl,reflFactors))
  return(REMRefl)
}

greyreflMRE <- getREMSpec(greyrefldfinterpnorm)
blackreflMRE <- getREMSpec(blackrefldfinterpnorm)

refValsBlue <- c(blackreflMRE$blueFactors[2],greyreflMRE$blueFactors[2])
refValsGreen <- c(blackreflMRE$greenFactors[2],greyreflMRE$greenFactors[2])
refValsRed <- c(blackreflMRE$redFactors[2],greyreflMRE$redFactors[2])
refValsRedEdge <- c(blackreflMRE$redEdgFactors[2],greyreflMRE$redEdgFactors[2])
refValsNIR <-  c(blackreflMRE$NIRFactors[2],greyreflMRE$NIRFactors[2])

#get values for calibration targets

canvasValues <- extract(MREOwnPanelReflStackCrop,canvasROIs,fun=mean,df=T)

MREValsBlue <- c(canvasValues$blue[1],canvasValues$blue[2]) 
MREValsGreen <- c(canvasValues$green[1],canvasValues$green[2]) 
MREValsRed <-  c(canvasValues$red[1],canvasValues$red[2]) 
MREValsRedEdge <- c(canvasValues$redEdge[1],canvasValues$redEdge[2]) 
MREValsNIR <- c(canvasValues$NIR[1],canvasValues$NIR[2]) 

#make linear models

elmBlue <- lm(refValsBlue~MREValsBlue,na.action=na.omit)
elmGreen <- lm(refValsGreen~MREValsGreen,na.action=na.omit)
elmRed <- lm(refValsRed~MREValsRed,na.action=na.omit)
elmRedEdge <- lm(refValsRedEdge~MREValsRedEdge,na.action=na.omit)
elmNIR <- lm(refValsNIR~MREValsNIR,na.action=na.omit)

#save linear models
elmlist <- list(elmBlue, elmGreen,elmRed,elmRedEdge,elmNIR)
saveRDS(elmlist,'D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/elmdatarefl.RData')

#apply linear models

elmlist <- readRDS('D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/elmdatarefl.RData')

names(MREOwnPanelReflStackCrop) <- c('MREValsBlue','MREValsGreen','MREValsRed','MREValsRedEdge','MREValsNIR')

reflStack <- stack()

for (i in 1:5){
  predRefl <- predict(MREOwnPanelReflStackCrop[[i]],elmlist[[i]])
  reflStack <- stack(reflStack,predRefl)
}
names(reflStack) <- c('blue','green','red','redEdge','NIR')
NDVIraster <- (reflStack$NIR-reflStack$red)/(reflStack$NIR+reflStack$red)

writeRaster(reflStack,filename="D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ELM/TRM_2_MRE_proj2lines_ELMRefl_stackcrop.tif",format="GTiff",overwrite=TRUE)
writeRaster(NDVIraster,filename="D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ELM/indices/TRM_2_MRE_proj2lines_ELMindex_NDVI.tif",format="GTiff",overwrite=TRUE)


#SEQ TRM 2 ELM

#SEQ TRM 2 DC value

DCstartind <- 476
DCendind <- 485 

DCmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(DCstartind:DCendind))

for(i in DCstartind:DCendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  DCmat[,i-DCstartind+1] <- spectrum$rad
}

DCmedianvec <- apply(DCmat, 1, median, na.rm=T)
DCmedianvec[DCmedianvec<0] <- 0

#SEQ TRM 2 black canvas refl

WRstartind <- 496
WRendind <- 500

WRmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(WRstartind:WRendind))

for(i in WRstartind:WRendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  WRmat[,i-WRstartind+1] <- spectrum$rad
}

WRmedianvec <- apply(WRmat, 1, median, na.rm=T)
WRmedianvec[WRmedianvec<0] <- 0

blackstartind <- 501
blackendind <- 505


blackmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(blackstartind:blackendind))


for(i in blackstartind:blackendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  blackmat[,i-blackstartind+1] <- spectrum$rad
}

blackmedianvec <- apply(blackmat, 1, mean, na.rm=T)
blackmedianvec[blackmedianvec<0] <- 0

blackrefl <- ((blackmedianvec-DCmedianvec)/(WRmedianvec-DCmedianvec))
blackrefldf <- data.frame(wvl=wavelengths,black=blackrefl)

blackrefldfinterpnorm <- interpolateSpectrum(blackrefldf[26:2048,])
blackrefldfinterpnorm$refl <- blackrefldfinterpnorm$refl/WRcalvalues[1:680]



#SEQ TRM 2 grey canvas refl

WRstartind <- 506
WRendind <- 510

WRmat <- matrix(data=NA,nrow=length(15:2062),ncol=length(WRstartind:WRendind))

for(i in WRstartind:WRendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  WRmat[,i-WRstartind+1] <- spectrum$rad
}

WRmedianvec <- apply(WRmat, 1, median, na.rm=T)
WRmedianvec[WRmedianvec<0] <- 0

greystartind <- 511
greyendind <- 515


greymat <- matrix(data=NA,nrow=length(15:2062),ncol=length(greystartind:greyendind))


for(i in greystartind:greyendind){
  spectrum <- read.table(filenames[i],skip=14,header=F)
  names(spectrum) <- c('wvl','rad')
  plot(spectrum,type='n')
  lines(spectrum)
  greymat[,i-greystartind+1] <- spectrum$rad
}

greymedianvec <- apply(greymat, 1, mean, na.rm=T)
greymedianvec[greymedianvec<0] <- 0

greyrefl <- ((greymedianvec-DCmedianvec)/(WRmedianvec-DCmedianvec))
greyrefldf <- data.frame(wvl=wavelengths,grey=greyrefl)


greyrefldfinterpnorm <- interpolateSpectrum(greyrefldf[26:2048,])
greyrefldfinterpnorm$refl <- greyrefldfinterpnorm$refl/WRcalvalues[1:680]



#function to calculate sequoia spectrum
getSeqSpec <- function(inputSpec,sequoiaResponse){
  #interpSpec <- interpolateSpectrum(inputSpec)
  #or if spectra is already per wvl: 
  interpSpec <- inputSpec
  if(length(inputSpec)>2){
    greenFactors <- colSums(interpSpec[interpSpec$wvl>=450&interpSpec$wvl<=650,-1]*sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])/sum(sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])
    redFactors <- colSums(interpSpec[interpSpec$wvl>=560&interpSpec$wvl<=760,-1]*sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])/sum(sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])
    redEdgFactors <- colSums(interpSpec[interpSpec$wvl>=635&interpSpec$wvl<=835,-1]*sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])/sum(sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])
    NIRFactors <- colSums(interpSpec[interpSpec$wvl>=690&interpSpec$wvl<=890,-1]*sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])/sum(sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])
  }else{
    greenFactors <- sum(interpSpec[interpSpec$wvl>=450&interpSpec$wvl<=650,-1]*sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])/sum(sequoiaResponse$green[sequoiaResponse$wvl>=450&sequoiaResponse$wvl<=650])
    redFactors <- sum(interpSpec[interpSpec$wvl>=560&interpSpec$wvl<=760,-1]*sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])/sum(sequoiaResponse$red[sequoiaResponse$wvl>=560&sequoiaResponse$wvl<=760])
    redEdgFactors <- sum(interpSpec[interpSpec$wvl>=635&interpSpec$wvl<=835,-1]*sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])/sum(sequoiaResponse$redge[sequoiaResponse$wvl>=635&sequoiaResponse$wvl<=835])
    NIRFactors <- sum(interpSpec[interpSpec$wvl>=690&interpSpec$wvl<=890,-1]*sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])/sum(sequoiaResponse$NIR[sequoiaResponse$wvl>=690&sequoiaResponse$wvl<=890])
  }
  reflFactors <- cbind(greenFactors,redFactors,redEdgFactors,NIRFactors)
  cwvl <- cbind(550,660,735,790)
  seqRefl <- data.frame(rbind(cwvl,reflFactors))
  return(seqRefl)
}

#Sequoia digitised CMOS response


#change path to spectral transmissivity file
sequoiaTrans <- read.csv("D:/Sequoia/Spectral transmissivity Monocam.csv",sep=",",header=TRUE)

greenTrans <- sequoiaTrans[sequoiaTrans$wvl>=450&sequoiaTrans$wvl<=650,c(1,5)]
redTrans <- sequoiaTrans[sequoiaTrans$wvl>=560&sequoiaTrans$wvl<=760,c(1,4)]
rededgeTrans <- sequoiaTrans[sequoiaTrans$wvl>=635&sequoiaTrans$wvl<=835,c(1,3)]
NIRTrans <- sequoiaTrans[sequoiaTrans$wvl>=690&sequoiaTrans$wvl<=890,c(1,2)]

#change path to file
sequoiaCMOSresp <- read.table("D:/Sequoia/sequoiasensitivitydigitised.txt",sep=',',skip=4,header=FALSE)
names(sequoiaCMOSresp) <- c('wvl','sens')
#get true Sequoia response (filter transmission * CMOS)
sequoiaCMOSrespinterp <- data.frame(interpolateSpectrum(sequoiaCMOSresp))
names(sequoiaCMOSrespinterp) <- c('wvl','sens')

sequoiaTransSens <- merge(sequoiaCMOSrespinterp,sequoiaTrans,by='wvl')
sequoiaResponse <- data.frame(wvl=sequoiaTransSens$wvl,sequoiaTransSens[,3:6]*sequoiaTransSens$sens/100)

greyreflSEQ <- getSeqSpec(greyrefldfinterpnorm,sequoiaResponse)
blackreflSEQ <- getSeqSpec(blackrefldfinterpnorm,sequoiaResponse)

refValsGreen <- c(blackreflSEQ$greenFactors[2],greyreflSEQ$greenFactors[2])
refValsRed <- c(blackreflSEQ$redFactors[2],greyreflSEQ$redFactors[2])
refValsRedEdge <- c(blackreflSEQ$redEdgFactors[2],greyreflSEQ$redEdgFactors[2])
refValsNIR <-  c(blackreflSEQ$NIRFactors[2],greyreflSEQ$NIRFactors[2])

#get values for calibration targets

canvasValues <- extract(seqOwnPanelReflStackCrop,canvasROIs,fun=mean,df=T)

SEQValsGreen <- c(canvasValues$green[1],canvasValues$green[2]) 
SEQValsRed <-  c(canvasValues$red[1],canvasValues$red[2]) 
SEQValsRedEdge <- c(canvasValues$redEdge[1],canvasValues$redEdge[2]) 
SEQValsNIR <- c(canvasValues$NIR[1],canvasValues$NIR[2]) 

#make linear models

elmGreen <- lm(refValsGreen~SEQValsGreen,na.action=na.omit)
elSEQd <- lm(refValsRed~SEQValsRed,na.action=na.omit)
elSEQdEdge <- lm(refValsRedEdge~SEQValsRedEdge,na.action=na.omit)
elmNIR <- lm(refValsNIR~SEQValsNIR,na.action=na.omit)

#save linear models
elmlist <- list(elmGreen,elSEQd,elSEQdEdge,elmNIR)
saveRDS(elmlist,'D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/elmdatarefl.RData')

#apply linear models

elmlist <- readRDS('D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/elmdatarefl.RData')

names(seqOwnPanelReflStackCrop) <- c('SEQValsGreen','SEQValsRed','SEQValsRedEdge','SEQValsNIR')

reflStack <- stack()

for (i in 1:4){
  predRefl <- predict(seqOwnPanelReflStackCrop[[i]],elmlist[[i]])
  reflStack <- stack(reflStack,predRefl)
}
names(reflStack) <- c('green','red','redEdge','NIR')
NDVIraster <- (reflStack$NIR-reflStack$red)/(reflStack$NIR+reflStack$red)

writeRaster(reflStack,filename="D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ELM/TRM_2_SEQ_proj2lines_ELSEQfl_stackcrop.tif",format="GTiff",overwrite=TRUE)
writeRaster(NDVIraster,filename="D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ELM/indices/TRM_2_SEQ_proj2lines_ELMindex_NDVI.tif",format="GTiff",overwrite=TRUE)

