#Analyze second tramway run (noon, 12:22 to 12:57)

library(rgdal)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(MASS)
library(splines)
library(rgeos)
library(gridExtra)

tramwayFootprintsShapes <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData/Footprints', layer = "TramwayMeasurementFootprintShapesNew")
tramwayROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData', layer = "TramwayROI")


seqOwnPanelReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/ownPanel/TRM_2_SEQ_proj2lines_ownPanelRefl_stackcrop.tif")
seqOwnPanelNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/ownPanel/indices/TRM_2_SEQ_proj2lines_index_ndvi.tif")
seqOwnPanelNDVICrop <- crop(seqOwnPanelNDVI,tramwayROI)

seqSpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/TRM_2_SEQ_proj2lines_SpectralonRefl_stackcrop.tif")
seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/indices/TRM_2_SEQ_proj2lines_index_ndvi.tif")
seqSpectralonNDVICrop <- crop(seqSpectralonNDVI,tramwayROI)

seqELMReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/ELM/TRM_2_SEQ_proj2lines_ELSEQfl_stackcrop.tif")
seqELMNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/ELM/indices/TRM_2_SEQ_proj2lines_ELMindex_ndvi.tif")
seqELMNDVICrop <- crop(seqELMNDVI,tramwayROI)

MREOwnPanelReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/ownPanel/TRM_2_MRE_proj2lines_ownPanelRefl_stackcrop.tif")
#MREOwnPanelNDVI <- stack("D:/New_Mexico/JOR_Tramway_Project/TRM_2_MRE_proj2lines_bkp/4_index/indices/ndvi/TRM_2_MRE_proj2lines_bkp_index_ndvi.tif")
MREOwnPanelNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/ownPanel/indices/TRM_2_MRE_proj2lines_index_ndvi.tif")
MREOwnPanelNDVICrop <- crop(MREOwnPanelNDVI,tramwayROI)

MRESpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/TRM_2_MRE_proj2lines_SpectralonRefl_stackcrop.tif")
#MRESpectralonNDVI <- stack("D:/New_Mexico/JOR_Tramway_Project/TRM_2_MRE_proj2lines_bkp/4_index/indices/ndvi/TRM_2_MRE_proj2lines_bkp_index_ndvi.tif")
MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/indices/TRM_2_MRE_proj2lines_index_ndvi.tif")
MRESpectralonNDVICrop <- crop(MRESpectralonNDVI,tramwayROI)

MREELMReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/ELM/TRM_2_MRE_proj2lines_ELMRefl_stackcrop.tif")
MREELMNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/ELM/indices/TRM_2_MRE_proj2lines_ELMindex_ndvi.tif")
MREELMNDVICrop <- crop(MREELMNDVI,tramwayROI)

#MREOwnPanelNDVICrop <-(MREOwnPanelReflStackCrop[[5]]-MREOwnPanelReflStackCrop[[3]])/(MREOwnPanelReflStackCrop[[5]]+MREOwnPanelReflStackCrop[[3]])

secondRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunForward_REMResamp.csv")

secondRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunBackward_SeqResamp.csv")



#tramway spectra NDVI

secondRunBkdSeqresampGreen <- secondRunBkdSeqresampSpectra$refl[secondRunBkdSeqresampSpectra$wvl==550] 
secondRunBkdSeqresampRed <- secondRunBkdSeqresampSpectra$refl[secondRunBkdSeqresampSpectra$wvl==660] 
secondRunBkdSeqresampRedEdge <- secondRunBkdSeqresampSpectra$refl[secondRunBkdSeqresampSpectra$wvl==735] 
secondRunBkdSeqresampNIR <- secondRunBkdSeqresampSpectra$refl[secondRunBkdSeqresampSpectra$wvl==790] 

secondRunBkdSeqresampdf <- data.frame(secondRunBkdSeqresampGreen,secondRunBkdSeqresampRed,secondRunBkdSeqresampRedEdge,secondRunBkdSeqresampNIR)

secondRunBkdSeqresampNDVI <- (secondRunBkdSeqresampNIR-secondRunBkdSeqresampRed)/(secondRunBkdSeqresampNIR+secondRunBkdSeqresampRed)


secondRunFwdMREresampBlue <- secondRunFwdMREresampSpectra$refl[secondRunFwdMREresampSpectra$wvl==475] 
secondRunFwdMREresampGreen <- secondRunFwdMREresampSpectra$refl[secondRunFwdMREresampSpectra$wvl==560] 
secondRunFwdMREresampRed <- secondRunFwdMREresampSpectra$refl[secondRunFwdMREresampSpectra$wvl==668] 
secondRunFwdMREresampRedEdge <- secondRunFwdMREresampSpectra$refl[secondRunFwdMREresampSpectra$wvl==717] 
secondRunFwdMREresampNIR <- secondRunFwdMREresampSpectra$refl[secondRunFwdMREresampSpectra$wvl==840] 

secondRunFwdMREresampdf <- data.frame(secondRunFwdMREresampBlue,secondRunFwdMREresampGreen,secondRunFwdMREresampRed,secondRunFwdMREresampRedEdge,secondRunFwdMREresampNIR)

secondRunFwdMREresampNDVI <- (secondRunFwdMREresampNIR-secondRunFwdMREresampRed)/(secondRunFwdMREresampNIR+secondRunFwdMREresampRed)


#extract

#seqFootprintReflectance <- extract(seqOwnPanelReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
#seqFootprintSpectralonNDVI <- extract(seqOwnPanelNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

seqFootprintownPanelReflectance <- extract(seqOwnPanelReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(seqFootprintownPanelReflectance) <- c('location','green','red','redEdge','NIR')

seqFootprintSpectralonReflectance <- extract(seqSpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(seqFootprintSpectralonReflectance) <- c('location','green','red','redEdge','NIR')

seqFootprintELMReflectance <- extract(seqELMReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(seqFootprintELMReflectance) <- c('location','green','red','redEdge','NIR')

seqFootprintSpectralonNDVI <- extract(seqSpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
seqFootprintOwnPanelNDVI <- extract(seqOwnPanelNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
seqFootprintELMNDVI <- extract(seqELMNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)



MREFootprintownPanelReflectance <- extract(MREOwnPanelReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
#MREFootprintNDVI <- extract(MREOwnPanelNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

MREFootprintSpectralonReflectance <- extract(MRESpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(MREFootprintSpectralonReflectance) <- c('location','blue','green','red','redEdge','NIR')

MREFootprintELMReflectance <- extract(MREELMReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(MREFootprintELMReflectance) <- c('location','blue','green','red','redEdge','NIR')

MREFootprintNDVI <- extract(MRESpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

MREFootprintOwnPanelNDVI <- extract(MREOwnPanelNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
MREFootprintELMNDVI <- extract(MREELMNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)


plot(1:110, secondRunBkdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, secondRunBkdSeqresampNDVI,col='black')
lines(1:110, seqFootprintOwnPanelNDVI$TRM_2_SEQ_proj2lines_index_ndvi,col='red')

legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)


plot(1:110, secondRunBkdSeqresampGreen,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, seqFootprintReflectance$TRM_2_SEQ_proj2lines_ownPanelRefl_stackcrop.1,col='green',lty=2,lwd=1.5)
lines(1:110, seqFootprintReflectance$TRM_2_SEQ_proj2lines_ownPanelRefl_stackcrop.2,col='red',lty=2,lwd=1.5)
lines(1:110, seqFootprintReflectance$TRM_2_SEQ_proj2lines_ownPanelRefl_stackcrop.3,col='orange',lty=2,lwd=1.5)
lines(1:110, seqFootprintReflectance$TRM_2_SEQ_proj2lines_ownPanelRefl_stackcrop.4,col='brown',lty=2,lwd=1.5)
lines(1:110, secondRunBkdSeqresampGreen,col='green',lty=1,lwd=1.5)
lines(1:110, secondRunBkdSeqresampRed,col='red',lty=1,lwd=1.5)
lines(1:110, secondRunBkdSeqresampRedEdge,col='orange',lty=1,lwd=1.5)
lines(1:110,secondRunBkdSeqresampNIR,col='brown',lty=1,lwd=1.5)

legend(20, 0.4, legend=c("Tramway Spectrometer refl", "Sequoia refl"),
       lty=c(1,1),col=c('green','red','orange','brown'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)

par(pty='s')
plot(seqFootprintReflectance$green,secondRunBkdSeqresampGreen,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl green',ylab='Tramway refl green')
abline(0,1,lty=2)
plot(seqFootprintReflectance$red,secondRunBkdSeqresampRed,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl red',ylab='Tramway refl red')
abline(0,1,lty=2)
plot(seqFootprintReflectance$redEdge,secondRunBkdSeqresampRedEdge,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl redEdge',ylab='Tramway refl redEdge')
abline(0,1,lty=2)
plot(seqFootprintReflectance$NIR,secondRunBkdSeqresampNIR,xlim=c(0,0.6), ylim=c(0,0.6),xlab='Seq refl NIR',ylab='Tramway refl NIR')
abline(0,1,lty=2)


plot(1:110, secondRunFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, secondRunFwdMREresampNDVI,col='black')
lines(1:110, MREFootprintNDVI$TRM_2_MRE_proj2lines_index_ndvi ,col='red')

legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)



#Sequoia ownPanel scatterplots
Sequoiareflscatterplotlist <- list()

wvlnamevec <- c('green','red','red edge','NIR')


for(i in 1:4){
  x <- as.vector(secondRunBkdSeqresampdf[,i])
  y <- as.vector(seqFootprintownPanelReflectance[,i+1])
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
    ggtitle(paste0(wvlnamevec[i],' (cwvl ',secondRunBkdSeqresampSpectra$wvl[i],' nm)'))+
    #theme(aspect.ratio=1)+
    xlab('Tramway spectrometer refl. [HCRF]')+
    ylab('Sequoia refl. [HCRF]')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
  
  Sequoiareflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=Sequoiareflscatterplotlist,nrow=2,ncol=2)


#Sequoia Spectralon scatterplots
Sequoiareflscatterplotlist <- list()

wvlnamevec <- c('green','red','red edge','NIR')


for(i in 1:4){
  x <- as.vector(secondRunBkdSeqresampdf[,i])
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
    ggtitle(paste0(wvlnamevec[i],' (cwvl ',secondRunBkdSeqresampSpectra$wvl[i],' nm)'))+
    #theme(aspect.ratio=1)+
    xlab('Tramway spectrometer refl. [HCRF]')+
    ylab('Sequoia refl. [HCRF]')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
  
  Sequoiareflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=Sequoiareflscatterplotlist,nrow=2,ncol=2)

#Sequoia ELM scatterplots
Sequoiareflscatterplotlist <- list()

wvlnamevec <- c('green','red','red edge','NIR')


for(i in 1:4){
  x <- as.vector(secondRunBkdSeqresampdf[,i])
  y <- as.vector(seqFootprintELMReflectance[,i+1])
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
    ggtitle(paste0(wvlnamevec[i],' (cwvl ',secondRunBkdSeqresampSpectra$wvl[i],' nm)'))+
    #theme(aspect.ratio=1)+
    xlab('Tramway spectrometer refl. [HCRF]')+
    ylab('Sequoia refl. [HCRF]')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
  
  Sequoiareflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=Sequoiareflscatterplotlist,nrow=2,ncol=2)




#MRE ownPanel scatterplots
MREreflscatterplotlist <- list()

wvlnamevec <- c('blue','green','red','red edge','NIR')


for(i in 1:5){
  x <- as.vector(secondRunFwdMREresampdf[,i])
  y <- as.vector(MREFootprintownPanelReflectance[,i+1])
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
    ggtitle(paste0(wvlnamevec[i],' (cwvl ',secondRunFwdMREresampSpectra$wvl[i],' nm)'))+
    #theme(aspect.ratio=1)+
    xlab('Tramway spectrometer refl. [HCRF]')+
    ylab('MRE refl. [HCRF]')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
  
  MREreflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=MREreflscatterplotlist,nrow=2,ncol=3)


#MRE Spectralon scatterplots
MREreflscatterplotlist <- list()

wvlnamevec <- c('blue','green','red','red edge','NIR')


for(i in 1:5){
x <- as.vector(secondRunFwdMREresampdf[,i])
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
  ggtitle(paste0(wvlnamevec[i],' (cwvl ',secondRunFwdMREresampSpectra$wvl[i],' nm)'))+
  #theme(aspect.ratio=1)+
  xlab('Tramway spectrometer refl. [HCRF]')+
  ylab('MRE refl. [HCRF]')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))

MREreflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=MREreflscatterplotlist,nrow=2,ncol=3)



#MRE ELM scatterplots
MREreflscatterplotlist <- list()

wvlnamevec <- c('blue','green','red','red edge','NIR')


for(i in 1:5){
  x <- as.vector(secondRunFwdMREresampdf[,i])
  y <- as.vector(MREFootprintELMReflectance[,i+1])
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
    ggtitle(paste0(wvlnamevec[i],' (cwvl ',secondRunFwdMREresampSpectra$wvl[i],' nm)'))+
    #theme(aspect.ratio=1)+
    xlab('Tramway spectrometer refl. [HCRF]')+
    ylab('MRE refl. [HCRF]')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
  
  MREreflscatterplotlist[[i]] <- p
}

grid.arrange(grobs=MREreflscatterplotlist,nrow=2,ncol=3)


#Seqouia ownPanel NDVI

x <- as.vector(secondRunBkdSeqresampNDVI)
y <- as.vector(seqFootprintOwnPanelNDVI$TRM_2_SEQ_proj2lines_index_ndvi)
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
  
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('Sequoia NDVI (Spectralon)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

#Seqouia spectralon NDVI

x <- as.vector(secondRunBkdSeqresampNDVI)
y <- as.vector(seqFootprintSpectralonNDVI$TRM_2_SEQ_proj2lines_index_ndvi)
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
  
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('Sequoia NDVI (Spectralon)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

#Seqouia ELM NDVI

x <- as.vector(secondRunBkdSeqresampNDVI)
y <- as.vector(seqFootprintELMNDVI$TRM_2_SEQ_proj2lines_ELMindex_NDVI)
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
  
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('Sequoia NDVI (ELM)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

#MRE spectralon NDVI

x <- as.vector(secondRunFwdMREresampNDVI)
y <- as.vector(MREFootprintNDVI$TRM_2_MRE_proj2lines_index_ndvi)
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
  
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('MRE NDVI (Spectralon)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)

#MRE ownPanel NDVI

x <- as.vector(secondRunFwdMREresampNDVI)
y <- as.vector(MREFootprintOwnPanelNDVI$TRM_2_MRE_proj2lines_index_ndvi)
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
  
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('MRE NDVI (ownPanel)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)


#MRE ELM NDVI

x <- as.vector(secondRunFwdMREresampNDVI)
y <- as.vector(MREFootprintELMNDVI$TRM_2_MRE_proj2lines_ELMindex_NDVI)
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
  
  #theme(aspect.ratio=1)+
  xlab('Tramway NDVI')+
  ylab('MRE NDVI (ELM)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(p)
