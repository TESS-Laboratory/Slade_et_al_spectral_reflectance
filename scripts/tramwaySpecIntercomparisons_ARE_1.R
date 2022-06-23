#Analyze AREA drone surveys
#Drone Surveys started at 11.05 (MRE) and 11.32 (SEQ)
#Tramway run 1 started at 10.30 Forward and 10.53 Back
#Tramway run 2 started at 12.22 Forward and 12.57 Back
#This analysis is undertaken with the Backwards run of The First Tramway Run
# Due to issues with reconstruction of the tramway infrastructure on the tramway footprints in the  Multispectral orthomosaic - the image processing 
#for the comparison was restricted to the 2 lines (MRE) and 2 lines (Seq) of the survey that were directly above the footprints
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

# Read in Tramway footprint and ROOI shape files
tramwayFootprintsShapes <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData/Footprints', layer = "TramwayMeasurementFootprintShapesNew")
tramwayROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData', layer = "TramwayROI")

# Read in SEQ Stacked images and NDVI, crop NDV
seqSpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ_2lines/Spectralon/ARE_1_SEQ_2lines_SpectralonRefl_stackcrop.tif")
seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ_2lines/Spectralon/indices/ARE_1_SEQ_2lines_SPE_index_ndvi.tif")
seqSpectralonNDVICrop <- crop(seqSpectralonNDVI,tramwayROI)

## Read in MRE Stacked images and NDVI, crop NDV
MRESpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE_2lines/Spectralon/ARE_1_MRE_2lines_SpectralonRefl_stackcrop.tif")
MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE_2lines/Spectralon/indices/ARE_1_MRE_2lines_SPE_index_ndvi.tif")
MRESpectralonNDVICrop <- crop(MRESpectralonNDVI,tramwayROI)

# Read in tramway data
FirstRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunForward_REMResamp.csv")
FirstRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunForward_SeqResamp.csv")
FirstRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunBackward_SeqResamp.csv")
FirstRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunBackward_REMResamp.csv")


#Extract tramway spectra for SEQ and MRE spectral bands and calculate NDVI

#First run backwards SEQ
FirstRunBkdSeqresampGreen <- FirstRunBkdSeqresampSpectra$refl[FirstRunBkdSeqresampSpectra$wvl==550] 
FirstRunBkdSeqresampRed <- FirstRunBkdSeqresampSpectra$refl[FirstRunBkdSeqresampSpectra$wvl==660] 
FirstRunBkdSeqresampRedEdge <- FirstRunBkdSeqresampSpectra$refl[FirstRunBkdSeqresampSpectra$wvl==735] 
FirstRunBkdSeqresampNIR <- FirstRunBkdSeqresampSpectra$refl[FirstRunBkdSeqresampSpectra$wvl==790] 
FirstRunBkdSeqresampdf <- data.frame(FirstRunBkdSeqresampGreen,FirstRunBkdSeqresampRed,FirstRunBkdSeqresampRedEdge,FirstRunBkdSeqresampNIR)
FirstRunBkdSeqresampNDVI <- (FirstRunBkdSeqresampNIR-FirstRunBkdSeqresampRed)/(FirstRunBkdSeqresampNIR+FirstRunBkdSeqresampRed)

#First run forwards SEQ
FirstRunFwdSeqresampGreen <- FirstRunFwdSeqresampSpectra$refl[FirstRunFwdSeqresampSpectra$wvl==550] 
FirstRunFwdSeqresampRed <- FirstRunFwdSeqresampSpectra$refl[FirstRunFwdSeqresampSpectra$wvl==660] 
FirstRunFwdSeqresampRedEdge <- FirstRunFwdSeqresampSpectra$refl[FirstRunFwdSeqresampSpectra$wvl==735] 
FirstRunFwdSeqresampNIR <- FirstRunFwdSeqresampSpectra$refl[FirstRunFwdSeqresampSpectra$wvl==790] 
FirstRunFwdSeqresampdf <- data.frame(FirstRunFwdSeqresampGreen,FirstRunFwdSeqresampRed,FirstRunFwdSeqresampRedEdge,FirstRunFwdSeqresampNIR)
FirstRunFwdSeqresampNDVI <- (FirstRunFwdSeqresampNIR-FirstRunFwdSeqresampRed)/(FirstRunFwdSeqresampNIR+FirstRunFwdSeqresampRed)

#First run forwards MRE
FirstRunFwdMREresampBlue <- FirstRunFwdMREresampSpectra$refl[FirstRunFwdMREresampSpectra$wvl==475] 
FirstRunFwdMREresampGreen <- FirstRunFwdMREresampSpectra$refl[FirstRunFwdMREresampSpectra$wvl==560] 
FirstRunFwdMREresampRed <- FirstRunFwdMREresampSpectra$refl[FirstRunFwdMREresampSpectra$wvl==668] 
FirstRunFwdMREresampRedEdge <- FirstRunFwdMREresampSpectra$refl[FirstRunFwdMREresampSpectra$wvl==717] 
FirstRunFwdMREresampNIR <- FirstRunFwdMREresampSpectra$refl[FirstRunFwdMREresampSpectra$wvl==840] 
FirstRunFwdMREresampdf <- data.frame(FirstRunFwdMREresampBlue,FirstRunFwdMREresampGreen,FirstRunFwdMREresampRed,FirstRunFwdMREresampRedEdge,FirstRunFwdMREresampNIR)
FirstRunFwdMREresampNDVI <- (FirstRunFwdMREresampNIR-FirstRunFwdMREresampRed)/(FirstRunFwdMREresampNIR+FirstRunFwdMREresampRed)

#First run backwards MRE
FirstRunBkdMREresampBlue <- FirstRunBkdMREresampSpectra$refl[FirstRunBkdMREresampSpectra$wvl==475] 
FirstRunBkdMREresampGreen <- FirstRunBkdMREresampSpectra$refl[FirstRunBkdMREresampSpectra$wvl==560] 
FirstRunBkdMREresampRed <- FirstRunBkdMREresampSpectra$refl[FirstRunBkdMREresampSpectra$wvl==668] 
FirstRunBkdMREresampRedEdge <- FirstRunBkdMREresampSpectra$refl[FirstRunBkdMREresampSpectra$wvl==717] 
FirstRunBkdMREresampNIR <- FirstRunBkdMREresampSpectra$refl[FirstRunBkdMREresampSpectra$wvl==840] 
FirstRunBkdMREresampdf <- data.frame(FirstRunBkdMREresampBlue,FirstRunBkdMREresampGreen,FirstRunBkdMREresampRed,FirstRunBkdMREresampRedEdge,FirstRunBkdMREresampNIR)
FirstRunBkdMREresampNDVI <- (FirstRunBkdMREresampNIR-FirstRunBkdMREresampRed)/(FirstRunBkdMREresampNIR+FirstRunBkdMREresampRed)


#extract reflectance data from imagery for Tramway footprints
#SEQ Extract
seqFootprintSpectralonReflectance <- extract(seqSpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(seqFootprintSpectralonReflectance) <- c('location','green','red','redEdge','NIR')
seqFootprintSpectralonNDVI <- extract(seqSpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

#MRE Extract
MREFootprintSpectralonReflectance <- extract(MRESpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(MREFootprintSpectralonReflectance) <- c('location','blue','green','red','redEdge','NIR')
MREFootprintNDVI <- extract(MRESpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

##PLOTS##

#SEQ NDVI VS TRAMWAY NDVI PLOT
plot(1:110, FirstRunFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, FirstRunFwdSeqresampNDVI,col='black')
lines(1:110, seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi,col='red')

legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)

# REFLECTANCE PLOT across spectrum
plot(1:110, FirstRunFwdSeqresampGreen,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.1,col='green',lty=2,lwd=1.5)
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.2,col='red',lty=2,lwd=1.5)
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.3,col='orange',lty=2,lwd=1.5)
lines(1:110, seqFootprintSpectralonReflectance$TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.4,col='brown',lty=2,lwd=1.5)
lines(1:110, FirstRunFwdSeqresampGreen,col='green',lty=1,lwd=1.5)
lines(1:110, FirstRunFwdSeqresampRed,col='red',lty=1,lwd=1.5)
lines(1:110, FirstRunFwdSeqresampRedEdge,col='orange',lty=1,lwd=1.5)
lines(1:110,FirstRunFwdSeqresampNIR,col='brown',lty=1,lwd=1.5)

legend(20, 0.4, legend=c("Tramway Spectrometer refl", "Sequoia refl"),
       lty=c(1,1),col=c('green','red','orange','brown'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)

#SEQ Reflectance vs Tramway REFLECTANCE INDIVIDUAL BAND PLOTS
par(pty='s')
plot(seqFootprintSpectralonReflectance$green,FirstRunFwdSeqresampGreen,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl green',ylab='Tramway refl green')
abline(0,1,lty=2)
plot(seqFootprintSpectralonReflectance$red,FirstRunFwdSeqresampRed,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl red',ylab='Tramway refl red')
abline(0,1,lty=2)
plot(seqFootprintSpectralonReflectance$redEdge,FirstRunFwdSeqresampRedEdge,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl redEdge',ylab='Tramway refl redEdge')
abline(0,1,lty=2)
plot(seqFootprintSpectralonReflectance$NIR,FirstRunFwdSeqresampNIR,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl NIR',ylab='Tramway refl NIR')
abline(0,1,lty=2)

#MRE NDVI VS TRAMWAY NDVI PLOT
plot(1:110, FirstRunFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, FirstRunFwdMREresampNDVI,col='black')
lines(1:110, MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi ,col='red')

legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)

#MRE Reflectance vs Tramway REFLECTANCE INDIVIDUAL BAND PLOTS
par(pty='s')
plot(MREFootprintSpectralonReflectance$blue,FirstRunFwdMREresampBlue,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl blue',ylab='Tramway refl blue')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$green,FirstRunFwdMREresampGreen,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl green',ylab='Tramway refl green')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$red,FirstRunFwdMREresampRed,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl red',ylab='Tramway refl red')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$redEdge,FirstRunFwdMREresampRedEdge,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl redEdge',ylab='Tramway refl redEdge')
abline(0,1,lty=2)
plot(MREFootprintSpectralonReflectance$NIR,FirstRunFwdMREresampNIR,xlim=c(0,0.6), ylim=c(0,0.6),xlab='MRE refl NIR',ylab='Tramway refl NIR')
abline(0,1,lty=2)

#Sequoia Spectralon scatterplots
Sequoiareflscatterplotlist <- list()

wvlnamevec <- c('green','red','red edge','NIR')

for(i in 1:4){
  x <- as.vector(FirstRunFwdSeqresampdf[,i])
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
#    ggtitle(paste0(wvlnamevec[i],' (cwvl ',FirstRunFWdSeqresampSpectra$wvl[i],' nm)'))+
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
x <- as.vector(FirstRunFwdMREresampdf[,i])
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
  ggtitle(paste0(wvlnamevec[i],' (cwvl ',FirstRunFwdMREresampSpectra$wvl[i],' nm)'))+
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

x <- as.vector(FirstRunFwdSeqresampNDVI)
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

x <- as.vector(FirstRunFwdMREresampNDVI)
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
  ggtitle("ARE_1 MRE Drone Survey comparison with Tramway Run 1(Bkd) ")+
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
  ggtitle("ARE_1 Drone Survey Sequoia vs MRE NDVI")+
  #theme(aspect.ratio=1)+
  xlab('SEQ NDVI (Spectralon)')+
  ylab('MRE NDVI (Spectralon)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

