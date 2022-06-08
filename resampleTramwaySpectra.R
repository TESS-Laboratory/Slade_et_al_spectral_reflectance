
#read Tweedy Jornada tramway spectral measurements

setwd("D:/New_Mexico/JOR_Tramway_Project/Tram_data/Raw_reflectance/")

filenames <- list.files(getwd(), pattern="*.csv", full.names=TRUE)
filenamesshort <- list.files(getwd(), pattern="*.csv", full.names=F)

ldf <- lapply(filenames, read.table,header=T,sep=",")



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

#perform linear interpolation to get reflectance for each wavelength (not needed if already resolved per wavelength)
interpolateSpectrum <- function(inputSpec){
  
  interpSpec <- approx(inputSpec$wvl,inputSpec[,2],xout=seq(ceiling(min(inputSpec$wvl)),floor(max(inputSpec$wvl)),1))
  interpSpec <- data.frame(interpSpec)
  names(interpSpec) <- c('wvl','refl')
  return(interpSpec)
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


seqNDVIperrun <- matrix(data=NA,nrow=110,ncol=9)
seqNDVIperrun[,1] <- seq(1:110)
REMNDVIperrun <- seqNDVIperrun

for(j in 1:length(filenames)){

firstRunBack <- ldf[[j]]


tramSeqSpec <- data.frame(wvl=character(),refl=character(),position=character()) 
tramREMSpec <- data.frame(wvl=character(),refl=character(),position=character()) 


for(i in 1:110){

  
currentSpec <- firstRunBack[firstRunBack$location==i,]  

specToResamp <- data.frame(wvl=currentSpec$wavelength,refl=currentSpec$normrefl)



#compute sequoia reflectances and plot them versus interpolated spectrum

#resampSpec <- interpolateSpectrum(specToResamp) #not needed if spectrum is already per wvl
seqSpec <- getSeqSpec(specToResamp,sequoiaResponse)
REMSpec <- getREMSpec(specToResamp)
rownames(seqSpec) <- c()
rownames(REMSpec) <- c()
outSeqDF <- data.frame(wvl=unlist(seqSpec[1,]),refl=unlist(seqSpec[2,]),position=c(rep(i,4))) 
outREMDF <- data.frame(wvl=unlist(REMSpec[1,]),refl=unlist(REMSpec[2,]),position=c(rep(i,5))) 

tramSeqSpec <- rbind(tramSeqSpec,outSeqDF)
tramREMSpec <- rbind(tramREMSpec,outREMDF)
}

filenameout <- filenamesshort[j] 
filenameout <- sub('.csv','',filenameout)
write.table(tramSeqSpec,paste0("D:/New_Mexico/JOR_Tramway_Project/Tram_data/Resamp_reflectance/",filenameout,'_SeqResamp.csv'),sep=',',row.names=F)
write.table(tramREMSpec,paste0("D:/New_Mexico/JOR_Tramway_Project/Tram_data/Resamp_reflectance/",filenameout,'_REMResamp.csv'),sep=',',row.names=F)
# 
# seqreflRed <-  tramSeqSpec$refl[tramSeqSpec$wvl==660]
# seqreflNIR <- tramSeqSpec$refl[tramSeqSpec$wvl==790]
# 
# seqNDVI <- (seqreflNIR-seqreflRed)/(seqreflNIR+seqreflRed)
# 
# seqNDVIperrun[,j+1] <- seqNDVI
# 
# REMreflRed <-  tramREMSpec$refl[tramREMSpec$wvl==668]
# REMreflNIR <- tramREMSpec$refl[tramREMSpec$wvl==840]
# 
# REMNDVI <- (REMreflNIR-REMreflRed)/(REMreflNIR+REMreflRed)
# 
# REMNDVIperrun[,j+1] <- REMNDVI

}


#seqouia NDVI

plot(1:110,seqNDVI,xlab='metres',ylab='NDVI',main=filenameout,type='n')
#lines(1:110,NDVIperrun[,c(2)])
lines(1:110,seqNDVIperrun[,c(4)])
lines(1:110,seqNDVIperrun[,c(6)],lty=2,col='blue')
lines(1:110,seqNDVIperrun[,c(8)],lty=4,col='red')

lines(1:110,seqNDVIperrun[,c(5)])
lines(1:110,seqNDVIperrun[,c(7)],lty=2,col='blue')
lines(1:110,seqNDVIperrun[,c(9)],lty=4,col='red')


legend(20, 0.4, legend=c("First (10:30 and 10:53)", "Second (12:22 and 12:57)", "Third (13:40 and 13:55)"),
       lty=c(1,2,4),col=c('black','blue','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)

#REM NDVI

plot(1:110,REMNDVI,xlab='metres',ylab='NDVI',main=filenameout,type='n')
#lines(1:110,NDVIperrun[,c(2)])
lines(1:110,REMNDVIperrun[,c(4)])
lines(1:110,REMNDVIperrun[,c(6)],lty=2,col='blue')
lines(1:110,REMNDVIperrun[,c(8)],lty=4,col='red')

lines(1:110,REMNDVIperrun[,c(5)])
lines(1:110,REMNDVIperrun[,c(7)],lty=2,col='blue')
lines(1:110,REMNDVIperrun[,c(9)],lty=4,col='red')


legend(20, 0.4, legend=c("First (10:30 and 10:53)", "Second (12:22 and 12:57)", "Third (13:40 and 13:55)"),
       lty=c(1,2,4),col=c('black','blue','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)


# 
# plot(refloutdf$wvl,refloutdf$midGrey,ylim=c(-0.1,1),xlim=c(390,900),type='n')
# lines(refloutdf$wvl,refloutdf$midGrey,ylim=c(-0.1,1),xlim=c(390,900))
# lines(refloutdf$wvl,refloutdf$darkGrey,ylim=c(-0.1,1),xlim=c(390,900))
# lines(refloutdf$wvl,refloutdf$lightGrey,ylim=c(-0.1,1),xlim=c(390,900))
# lines(refloutdf$wvl,refloutdf$black,ylim=c(-0.1,1),xlim=c(390,900))
# lines(refloutdf$wvl,refloutdf$fourtyfour,ylim=c(-0.1,1),xlim=c(390,900))

