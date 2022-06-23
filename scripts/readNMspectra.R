library(prospectr)

#read OceanOptics FLAME spectrometer spectra and calculate relative reflectances
#example from trelusback 04_07_2019

setwd('D:/New Mexico/Day 1/Spectra/')

filenames <- list.files(getwd(), pattern="*.txt", full.names=TRUE)
ldf <- lapply(filenames, read.table,skip=14,header=FALSE)
#ldfmeas <- ldf[1:60]

#get DC
# ldfdc <- ldf[61:65]
# dfdc <- do.call('cbind',ldfdc)
# dcmeas <- dfdc[ , !c(TRUE,FALSE) ]#select only even columns (measured data)
# mediandc <- apply(dcmeas,1,median)
# mediandcdf <- data.frame(wvl=dfdc[,1],dn=mediandc)


test <- ldf[10:14]

whiteref <- do.call('cbind',test)
test2 <- whiteref[ , !c(TRUE,FALSE) ]
whitemedian <- apply(test2,1,median)

greytest <- ldf[9]$V2/whitemedian

plot(ldf[9]$V1,greytest,xlab='wvl',ylab='DN')



ntargets <- 6
smoothingwindow <- 21 #odd number
wvlvec <- seq(ceiling(min(mediandcdf$wvl)),floor(max(mediandcdf$wvl)),1)
reflout <- matrix(data=NA,nrow=length(wvlvec)-(smoothingwindow-1),ncol=ntargets+1)
reflout[,1] <- wvlvec[(floor(smoothingwindow/2)+1):(length(wvlvec)-floor(smoothingwindow/2))]

#perform linear interpolation to get reflectance for each wavelength (not needed if already resolved per wavelength)
interpolateSpectrum <- function(inputSpec){
  
  interpSpec <- approx(inputSpec$wvl,inputSpec[,2],xout=seq(ceiling(min(inputSpec$wvl)),floor(max(inputSpec$wvl)),1))
  interpSpec <- data.frame(interpSpec)
  names(interpSpec) <- c('wvl','refl')
  return(interpSpec)
} 

#compute reflectance by taking median values 
for(i in 1:ntargets){
  lwhiteref <- ldf[((i-1)*10+1):((i-1)*10+5)]
  ldn <- ldf[((i-1)*10+6):((i)*10)]
  dfwhiteref <- do.call('cbind',lwhiteref)
  dfdn <- do.call('cbind',ldn)
  whiteref<- dfwhiteref[ , !c(TRUE,FALSE) ]
  dn <- dfdn[ , !c(TRUE,FALSE) ]
  medianwhite <- apply(whiteref,1,median)
  mediandn <- apply(dn,1,median)
  refl <- (mediandn-mediandcdf$dn)/(medianwhite-mediandcdf$dn)
  #nterpolation of O2A band
  refl[(mediandcdf$wvl>757)&(mediandcdf$wvl<771)] <- NA
  #resampling to fixed wavelengths
  refldf <- data.frame(wvl=mediandcdf$wvl,refl=refl)
  reflinterp <- interpolateSpectrum(refldf)
  #smoothing step
  smrefl <- savitzkyGolay(reflinterp$refl,p=3,w=smoothingwindow,m=0)
  reflout[,i+1] <- smrefl

  
}
refloutdf <- data.frame(reflout)
names(refloutdf) <- c('wvl','midGrey','darkGrey','lightGrey','black','fourtyfour','greyCard')

write.csv(refloutdf,paste(getwd(),'panelReflProcessed.csv',sep=''))

plot(refloutdf$wvl,refloutdf$midGrey,ylim=c(-0.1,1),xlim=c(390,900),type='n')
lines(refloutdf$wvl,refloutdf$midGrey,ylim=c(-0.1,1),xlim=c(390,900))
lines(refloutdf$wvl,refloutdf$darkGrey,ylim=c(-0.1,1),xlim=c(390,900))
lines(refloutdf$wvl,refloutdf$lightGrey,ylim=c(-0.1,1),xlim=c(390,900))
lines(refloutdf$wvl,refloutdf$black,ylim=c(-0.1,1),xlim=c(390,900))
lines(refloutdf$wvl,refloutdf$fourtyfour,ylim=c(-0.1,1),xlim=c(390,900))

