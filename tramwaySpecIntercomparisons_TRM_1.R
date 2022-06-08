#Analyze TRM 1 drone surveys
# Drone image data from TRM_1 for Sequoia (start 11:59) and MRE drone surveys (start 12:12) 
# comparison made with second tramway run Second Tramway run forward run started at meter_1 at 12:22 am and Second backward at 12:57 am
# Due to issues with reconstruction of the tramway infrastructure on the tramway footprints in the  Multispectral orthomosaic - the image processing 
#for the comparison was restricted to the 2 lines (MRE) and 3 lines (Seq) of the survey that were directly above the footprints
#In order to compare between surveys - drone data from both sensors (MRE and SEQ)  has been calibrated against the spectralon reflectance panel.
# 
library(tidyverse)
library(viridis)
library(rgdal)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(MASS)
library(splines)
library(rgeos)
library(gridExtra)

#----------1.Theme--------

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

#----------2.Read in Tramway footprint and ROI shape files------
tramwayFootprintsShapes <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData/Footprints', layer = "TramwayMeasurementFootprintShapesNew")
tramwayROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData', layer = "TramwayROI")

# ----3.Read in SEQ Stacked images and NDVI, crop NDV----
seqSpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.tif")
seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/indices/TRM_1_SEQ_3lines_SPE_index_ndvi.tif")
seqSpectralonNDVICrop <- crop(seqSpectralonNDVI,tramwayROI)

##-----4. Read in MRE Stacked images and NDVI, crop NDV-----
MRESpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SpectralonRefl_stackcrop.tif")
MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/indices/TRM_1_MRE_2lines_SPE_index_ndvi.tif")
MRESpectralonNDVICrop <- crop(MRESpectralonNDVI,tramwayROI)

#----5.Read in tramway data----
SecondRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunForward_REMResamp.csv")
secondRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunBackward_REMResamp.csv")
SecondRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunForward_SeqResamp.csv")
SecondRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunBackward_SeqResamp.csv")



#----6.Extract tramway spectra for SEQ and MRE spectral bands and calculate NDVI-----

#second run backwards SEQ
SecondRunBkdSeqresampGreen <- SecondRunBkdSeqresampSpectra$refl[SecondRunBkdSeqresampSpectra$wvl==550] 
SecondRunBkdSeqresampRed <- SecondRunBkdSeqresampSpectra$refl[SecondRunBkdSeqresampSpectra$wvl==660] 
SecondRunBkdSeqresampRedEdge <- SecondRunBkdSeqresampSpectra$refl[SecondRunBkdSeqresampSpectra$wvl==735] 
SecondRunBkdSeqresampNIR <- SecondRunBkdSeqresampSpectra$refl[SecondRunBkdSeqresampSpectra$wvl==790] 
SecondRunBkdSeqresampdf <- data.frame(SecondRunBkdSeqresampGreen,SecondRunBkdSeqresampRed,SecondRunBkdSeqresampRedEdge,SecondRunBkdSeqresampNIR)
SecondRunBkdSeqresampNDVI <- (SecondRunBkdSeqresampNIR-SecondRunBkdSeqresampRed)/(SecondRunBkdSeqresampNIR+SecondRunBkdSeqresampRed)

#Second run forwards SEQ
SecondRunFwdSeqresampGreen <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==550] 
SecondRunFwdSeqresampRed <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==660] 
SecondRunFwdSeqresampRedEdge <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==735] 
SecondRunFwdSeqresampNIR <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==790] 
SecondRunFwdSeqresampdf <- data.frame(SecondRunFwdSeqresampGreen,SecondRunFwdSeqresampRed,SecondRunFwdSeqresampRedEdge,SecondRunFwdSeqresampNIR)
SecondRunFwdSeqresampNDVI <- (SecondRunFwdSeqresampNIR-SecondRunFwdSeqresampRed)/(SecondRunFwdSeqresampNIR+SecondRunFwdSeqresampRed)

#Second run forwards MRE
SecondRunFwdMREresampBlue <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==475] 
SecondRunFwdMREresampGreen <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==560] 
SecondRunFwdMREresampRed <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==668] 
SecondRunFwdMREresampRedEdge <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==717] 
SecondRunFwdMREresampNIR <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==840] 
SecondRunFwdMREresampdf <- data.frame(SecondRunFwdMREresampBlue,SecondRunFwdMREresampGreen,SecondRunFwdMREresampRed,SecondRunFwdMREresampRedEdge,SecondRunFwdMREresampNIR)
SecondRunFwdMREresampNDVI <- (SecondRunFwdMREresampNIR-SecondRunFwdMREresampRed)/(SecondRunFwdMREresampNIR+SecondRunFwdMREresampRed)

#Second run backwards MRE
secondRunBkdMREresampBlue <- secondRunBkdMREresampSpectra$refl[secondRunBkdMREresampSpectra$wvl==475] 
secondRunBkdMREresampGreen <- secondRunBkdMREresampSpectra$refl[secondRunBkdMREresampSpectra$wvl==560] 
secondRunBkdMREresampRed <- secondRunBkdMREresampSpectra$refl[secondRunBkdMREresampSpectra$wvl==668] 
secondRunBkdMREresampRedEdge <- secondRunBkdMREresampSpectra$refl[secondRunBkdMREresampSpectra$wvl==717] 
secondRunBkdMREresampNIR <- secondRunBkdMREresampSpectra$refl[secondRunBkdMREresampSpectra$wvl==840] 
secondRunBkdMREresampdf <- data.frame(secondRunBkdMREresampBlue,secondRunBkdMREresampGreen,secondRunBkdMREresampRed,secondRunBkdMREresampRedEdge,secondRunBkdMREresampNIR)
secondRunBkdMREresampNDVI <- (secondRunBkdMREresampNIR-secondRunBkdMREresampRed)/(secondRunBkdMREresampNIR+secondRunBkdMREresampRed)

#----7.extract reflectance data from imagery for Tramway footprints----
#SEQ Extract
seqFootprintSpectralonReflectance <- extract(seqSpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(seqFootprintSpectralonReflectance) <- c('location','green','red','redEdge','NIR')
seqFootprintSpectralonNDVI <- extract(seqSpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

#MRE Extract
MREFootprintSpectralonReflectance <- extract(MRESpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(MREFootprintSpectralonReflectance) <- c('location','blue','green','red','redEdge','NIR')
MREFootprintNDVI <- extract(MRESpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

#----8.PLOTS----

#SEQ NDVI VS TRAMWAY NDVI PLOT
plot(1:110, SecondRunFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, SecondRunFwdSeqresampNDVI,col='black')
lines(1:110, seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 SEQ Spectralon - Tramway Second Run Forwards")

#SEQ NDVI VS TRAMWAY NDVI PLOT
plot(1:110, SecondRunBkdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, SecondRunBkdSeqresampNDVI,col='black')
lines(1:110, seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 SEQ Spectralon - Tramway Second Run Backwards")

#MRE NDVI VS TRAMWAY NDVI PLOT (2nd run fwd)
plot(1:110, SecondRunFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, SecondRunFwdMREresampNDVI,col='black')
lines(1:110, MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi ,col='red')
legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 MRE Spectralon - Tramway Second Run Forwards")

#MRE NDVI VS TRAMWAY NDVI PLOT (2nd run back)
plot(1:110, secondRunBkdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, secondRunBkdMREresampNDVI,col='black')
lines(1:110, MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi ,col='red')
legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 MRE Spectralon - Tramway Second Run Backwards")

# REFLECTANCE PLOT across spectrum
plot(1:110, SecondRunFwdSeqresampGreen,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.1,col='green',lty=2,lwd=1.5)
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.2,col='red',lty=2,lwd=1.5)
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.3,col='orange',lty=2,lwd=1.5)
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.4,col='brown',lty=2,lwd=1.5)
lines(1:110, SecondRunFwdSeqresampGreen,col='green',lty=1,lwd=1.5)
lines(1:110, SecondRunFwdSeqresampRed,col='red',lty=1,lwd=1.5)
lines(1:110, SecondRunFwdSeqresampRedEdge,col='orange',lty=1,lwd=1.5)
lines(1:110,SecondRunFwdSeqresampNIR,col='brown',lty=1,lwd=1.5)

legend(20, 0.4, legend=c("Tramway Spectrometer refl", "Sequoia refl"),
       lty=c(1,1),col=c('green','red','orange','brown'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)

#SEQ Reflectance vs Tramway REFLECTANCE INDIVIDUAL BAND PLOTS
par(pty='s')
plot(seqFootprintSpectralonReflectance$green,SecondRunFwdSeqresampGreen,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl green',ylab='Tramway refl green')
abline(0,1,lty=2)
plot(seqFootprintSpectralonReflectance$red,SecondRunFwdSeqresampRed,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl red',ylab='Tramway refl red')
abline(0,1,lty=2)
plot(seqFootprintSpectralonReflectance$redEdge,SecondRunFwdSeqresampRedEdge,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl redEdge',ylab='Tramway refl redEdge')
abline(0,1,lty=2)
plot(seqFootprintSpectralonReflectance$NIR,SecondRunFwdSeqresampNIR,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl NIR',ylab='Tramway refl NIR')
abline(0,1,lty=2)


#MRE Reflectance vs Tramway REFLECTANCE INDIVIDUAL BAND PLOTS
par(pty='s')
plot(MREFootprintSpectralonReflectance$blue,SecondRunFwdMREresampBlue,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl blue',ylab='Tramway refl blue')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$green,SecondRunFwdMREresampGreen,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl green',ylab='Tramway refl green')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$red,SecondRunFwdMREresampRed,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl red',ylab='Tramway refl red')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$redEdge,SecondRunFwdMREresampRedEdge,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl redEdge',ylab='Tramway refl redEdge')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$NIR,SecondRunFwdMREresampNIR,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl NIR',ylab='Tramway refl NIR')
abline(0,1,lty=2)

#Sequoia Spectralon scatterplots
Sequoiareflscatterplotlist <- list()

wvlnamevec <- c('green','red','red edge','NIR')

for(i in 1:4){
  x <- as.vector(SecondRunFwdSeqresampdf[,i])
  y <- as.vector(seqFootprintSpectralonReflectance[,i+1])
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lSequoias <- lm(y~x)
  r2val <- summary(lSequoias)$r.squared
  
  p <- ggplot(df) +
    geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
    geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_bw() +
    
    geom_abline(intercept = 0, slope = 1) +
#    ggtitle(paste0(wvlnamevec[i],' (cwvl ',SecondRunFWdSeqresampSpectra$wvl[i],' nm)'))+
    #theme(aspect.ratio=1)+
    xlab('Tramway spectrometer refl. [HCRF]')+
    ylab('Sequoia refl. [HCRF]')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
  
  Sequoiareflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=Sequoiareflscatterplotlist,nrow=2,ncol=2)

#MRE Spectralon scatterplots
MREreflscatterplotlist <- list()

wvlnamevec <- c('blue','green','red','red edge','NIR')


for(i in 1:5){
x <- as.vector(SecondRunFwdMREresampdf[,i])
y <- as.vector(MREFootprintSpectralonReflectance[,i+1])
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_bw() +

  geom_abline(intercept = 0, slope = 1) +
  ggtitle(paste0(wvlnamevec[i],' (cwvl ',SecondRunFwdMREresampSpectra$wvl[i],' nm)'))+
  #theme(aspect.ratio=1)+
  xlab('Tramway spectrometer refl. [HCRF]')+
  ylab('MRE refl. [HCRF]')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))

MREreflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=MREreflscatterplotlist,nrow=2,ncol=3)

#*****************************#

#Seqouia spectralon NDVI

x <- as.vector(SecondRunFwdSeqresampNDVI)
y <- as.vector(seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_bw() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("TRM_1 Sequoia Drone Survey comparison with Tramway Run 2(FWD) ")+
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('Sequoia NDVI (Spectralon)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

#MRE spectralon NDVI

x <- as.vector(SecondRunFwdMREresampNDVI)
y <- as.vector(MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_bw() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("TRM_1 MRE Drone Survey comparison with Tramway Run 2(FWD) ")+
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('MRE NDVI (Spectralon)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

#MRE NDVI VS SEQ NDVI

x <- as.vector(seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi)
y <- as.vector(MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_bw() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("TRM_1 Drone Survey Sequoia vs MRE NDVI")+
  #theme(aspect.ratio=1)+
  xlab('SEQ NDVI (Spectralon)')+
  ylab('MRE NDVI (Spectralon)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

ggsave(
  Plot,
  # filename = "/plots/test.png",
  filename = "E:/Glenn/Tramway Experiment/Processed/Plots/TRM1_MRE_vs_SEQ_NDVI.png",
  width = 25,
  height = 16,
  units = "cm"
)
