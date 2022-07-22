###Analyse All Tramway Runs resampled for Sequoia or MRE bands
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
SecondRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunBackward_REMResamp.csv")
SecondRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunForward_SeqResamp.csv")
SecondRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunBackward_SeqResamp.csv")
FirstRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunForward_REMResamp.csv")
FirstRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunBackward_REMResamp.csv")
FirstRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunForward_SeqResamp.csv")
FirstRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunBackward_SeqResamp.csv")
ThirdRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunForward_REMResamp.csv")
ThirdRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunBackward_REMResamp.csv")
ThirdRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunForward_SeqResamp.csv")
ThirdRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunBackward_SeqResamp.csv")



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
SecondRunBkdMREresampBlue <- SecondRunBkdMREresampSpectra$refl[SecondRunBkdMREresampSpectra$wvl==475] 
SecondRunBkdMREresampGreen <- SecondRunBkdMREresampSpectra$refl[SecondRunBkdMREresampSpectra$wvl==560] 
SecondRunBkdMREresampRed <- SecondRunBkdMREresampSpectra$refl[SecondRunBkdMREresampSpectra$wvl==668] 
SecondRunBkdMREresampRedEdge <- SecondRunBkdMREresampSpectra$refl[SecondRunBkdMREresampSpectra$wvl==717] 
SecondRunBkdMREresampNIR <- SecondRunBkdMREresampSpectra$refl[SecondRunBkdMREresampSpectra$wvl==840] 
SecondRunBkdMREresampdf <- data.frame(SecondRunBkdMREresampBlue,SecondRunBkdMREresampGreen,SecondRunBkdMREresampRed,SecondRunBkdMREresampRedEdge,SecondRunBkdMREresampNIR)
SecondRunBkdMREresampNDVI <- (SecondRunBkdMREresampNIR-SecondRunBkdMREresampRed)/(SecondRunBkdMREresampNIR+SecondRunBkdMREresampRed)

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

#Third run backwards SEQ
ThirdRunBkdSeqresampGreen <- ThirdRunBkdSeqresampSpectra$refl[ThirdRunBkdSeqresampSpectra$wvl==550] 
ThirdRunBkdSeqresampRed <- ThirdRunBkdSeqresampSpectra$refl[ThirdRunBkdSeqresampSpectra$wvl==660] 
ThirdRunBkdSeqresampRedEdge <- ThirdRunBkdSeqresampSpectra$refl[ThirdRunBkdSeqresampSpectra$wvl==735] 
ThirdRunBkdSeqresampNIR <- ThirdRunBkdSeqresampSpectra$refl[ThirdRunBkdSeqresampSpectra$wvl==790] 
ThirdRunBkdSeqresampdf <- data.frame(ThirdRunBkdSeqresampGreen,ThirdRunBkdSeqresampRed,ThirdRunBkdSeqresampRedEdge,ThirdRunBkdSeqresampNIR)
ThirdRunBkdSeqresampNDVI <- (ThirdRunBkdSeqresampNIR-ThirdRunBkdSeqresampRed)/(ThirdRunBkdSeqresampNIR+ThirdRunBkdSeqresampRed)

#Third run forwards SEQ
ThirdRunFwdSeqresampGreen <- ThirdRunFwdSeqresampSpectra$refl[ThirdRunFwdSeqresampSpectra$wvl==550] 
ThirdRunFwdSeqresampRed <- ThirdRunFwdSeqresampSpectra$refl[ThirdRunFwdSeqresampSpectra$wvl==660] 
ThirdRunFwdSeqresampRedEdge <- ThirdRunFwdSeqresampSpectra$refl[ThirdRunFwdSeqresampSpectra$wvl==735] 
ThirdRunFwdSeqresampNIR <- ThirdRunFwdSeqresampSpectra$refl[ThirdRunFwdSeqresampSpectra$wvl==790] 
ThirdRunFwdSeqresampdf <- data.frame(ThirdRunFwdSeqresampGreen,ThirdRunFwdSeqresampRed,ThirdRunFwdSeqresampRedEdge,ThirdRunFwdSeqresampNIR)
ThirdRunFwdSeqresampNDVI <- (ThirdRunFwdSeqresampNIR-ThirdRunFwdSeqresampRed)/(ThirdRunFwdSeqresampNIR+ThirdRunFwdSeqresampRed)

#Third run forwards MRE
ThirdRunFwdMREresampBlue <- ThirdRunFwdMREresampSpectra$refl[ThirdRunFwdMREresampSpectra$wvl==475] 
ThirdRunFwdMREresampGreen <- ThirdRunFwdMREresampSpectra$refl[ThirdRunFwdMREresampSpectra$wvl==560] 
ThirdRunFwdMREresampRed <- ThirdRunFwdMREresampSpectra$refl[ThirdRunFwdMREresampSpectra$wvl==668] 
ThirdRunFwdMREresampRedEdge <- ThirdRunFwdMREresampSpectra$refl[ThirdRunFwdMREresampSpectra$wvl==717] 
ThirdRunFwdMREresampNIR <- ThirdRunFwdMREresampSpectra$refl[ThirdRunFwdMREresampSpectra$wvl==840] 
ThirdRunFwdMREresampdf <- data.frame(ThirdRunFwdMREresampBlue,ThirdRunFwdMREresampGreen,ThirdRunFwdMREresampRed,ThirdRunFwdMREresampRedEdge,ThirdRunFwdMREresampNIR)
ThirdRunFwdMREresampNDVI <- (ThirdRunFwdMREresampNIR-ThirdRunFwdMREresampRed)/(ThirdRunFwdMREresampNIR+ThirdRunFwdMREresampRed)

#Third run backwards MRE
ThirdRunBkdMREresampBlue <- ThirdRunBkdMREresampSpectra$refl[ThirdRunBkdMREresampSpectra$wvl==475] 
ThirdRunBkdMREresampGreen <- ThirdRunBkdMREresampSpectra$refl[ThirdRunBkdMREresampSpectra$wvl==560] 
ThirdRunBkdMREresampRed <- ThirdRunBkdMREresampSpectra$refl[ThirdRunBkdMREresampSpectra$wvl==668] 
ThirdRunBkdMREresampRedEdge <- ThirdRunBkdMREresampSpectra$refl[ThirdRunBkdMREresampSpectra$wvl==717] 
ThirdRunBkdMREresampNIR <- ThirdRunBkdMREresampSpectra$refl[ThirdRunBkdMREresampSpectra$wvl==840] 
ThirdRunBkdMREresampdf <- data.frame(ThirdRunBkdMREresampBlue,ThirdRunBkdMREresampGreen,ThirdRunBkdMREresampRed,ThirdRunBkdMREresampRedEdge,ThirdRunBkdMREresampNIR)
ThirdRunBkdMREresampNDVI <- (ThirdRunBkdMREresampNIR-ThirdRunBkdMREresampRed)/(ThirdRunBkdMREresampNIR+ThirdRunBkdMREresampRed)

#-----7. Plots--------

#Tramway runs resampled for Sequoia
plot(1:110, FirstRunFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, FirstRunFwdSeqresampNDVI,col='black')
lines(1:110, FirstRunBkdSeqresampNDVI,col='red')
lines(1:110, SecondRunFwdSeqresampNDVI,col='blue')
lines(1:110, SecondRunBkdSeqresampNDVI,col='green')
lines(1:110, ThirdRunFwdSeqresampNDVI,col='grey')
lines(1:110, ThirdRunBkdSeqresampNDVI,col='orange')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'orange'),box.lty=1,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for Sequoia bands")

#Tramway runs resampled for MRE
plot(1:110, FirstRunFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, FirstRunFwdMREresampNDVI,col='black')
lines(1:110, FirstRunBkdMREresampNDVI,col='red')
lines(1:110, SecondRunFwdMREresampNDVI,col='blue')
lines(1:110, SecondRunBkdMREresampNDVI,col='green')
lines(1:110, ThirdRunFwdMREresampNDVI,col='grey')
lines(1:110, ThirdRunBkdMREresampNDVI,col='orange')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'orange'),box.lty=1,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for MRE bands")

#Tramway runs comparison of resampling
plot(1:110, FirstRunFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, FirstRunFwdMREresampNDVI,col='black')
lines(1:110, FirstRunFwdSeqresampNDVI,col='red')
legend(20, 0.4, legend=c("First run FWd MRE", "First Run Fwd SEQ"),
       lty=c(1,1),col=c('black','red'),box.lty=1,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("Tramway runs comparison of resampling for Sequoia and MRE")

#Tramway runs resampled for MRE Blue band
plot(1:110, FirstRunFwdMREresampBlue,type='n',xlab='[m]',ylab='Reflectance')
lines(1:110, FirstRunFwdMREresampBlue,col='black')
lines(1:110, FirstRunBkdMREresampBlue,col='red')
lines(1:110, SecondRunFwdMREresampBlue,col='blue')
lines(1:110, SecondRunBkdMREresampBlue,col='green')
lines(1:110, ThirdRunFwdMREresampBlue,col='grey')
lines(1:110, ThirdRunBkdMREresampBlue,col='orange')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'orange'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for MRE Blue band")

#Tramway runs resampled for MRE Green band
plot(1:110, FirstRunFwdMREresampGreen,type='n',xlab='[m]',ylab='Reflectance')
lines(1:110, FirstRunFwdMREresampGreen,col='black')
lines(1:110, FirstRunBkdMREresampGreen,col='red')
lines(1:110, SecondRunFwdMREresampGreen,col='blue')
lines(1:110, SecondRunBkdMREresampGreen,col='green')
lines(1:110, ThirdRunFwdMREresampGreen,col='grey')
lines(1:110, ThirdRunBkdMREresampGreen,col='orange')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'orange'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for MRE Green band")

#Tramway runs resampled for MRE Red band
plot(1:110, FirstRunFwdMREresampRed,type='n',xlab='[m]',ylab='Reflectance')
lines(1:110, FirstRunFwdMREresampRed,col='black')
lines(1:110, FirstRunBkdMREresampRed,col='red')
lines(1:110, SecondRunFwdMREresampRed,col='blue')
lines(1:110, SecondRunBkdMREresampRed,col='green')
lines(1:110, ThirdRunFwdMREresampRed,col='grey')
lines(1:110, ThirdRunBkdMREresampRed,col='orange')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'orange'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for MRE Red band")

#Tramway runs resampled for MRE RedEdge band
plot(1:110, FirstRunFwdMREresampRedEdge,type='n',xlab='[m]',ylab='Reflectance')
lines(1:110, FirstRunFwdMREresampRedEdge,col='black')
lines(1:110, FirstRunBkdMREresampRedEdge,col='red')
lines(1:110, SecondRunFwdMREresampRedEdge,col='blue')
lines(1:110, SecondRunBkdMREresampRedEdge,col='green')
lines(1:110, ThirdRunFwdMREresampRedEdge,col='grey')
lines(1:110, ThirdRunBkdMREresampRedEdge,col='orange')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'orange'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for MRE RedEdge band")

#Tramway runs resampled for MRE NIR band
plot(1:110, FirstRunFwdMREresampNIR,type='n',xlab='[m]',ylab='Reflectance')
lines(1:110, FirstRunFwdMREresampNIR,col='black')
lines(1:110, FirstRunBkdMREresampNIR,col='red')
lines(1:110, SecondRunFwdMREresampNIR,col='blue')
lines(1:110, SecondRunBkdMREresampNIR,col='green')
lines(1:110, ThirdRunFwdMREresampNIR,col='grey')
lines(1:110, ThirdRunBkdMREresampNIR,col='orange')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'orange'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for MRE NIR band")


# Plot Tramway Run 2 FWD vs BKd NDVI comparison

x <- as.vector(SecondRunFwdSeqresampNDVI)
y <- as.vector(SecondRunBkdSeqresampNDVI)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 2 Forward and Backward sampled for Sequoia NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward NDVI')+
  ylab('Second run backward NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 1 FWD vs BKd NDVI comparison

x <- as.vector(FirstRunFwdSeqresampNDVI)
y <- as.vector(FirstRunBkdSeqresampNDVI)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 1 Forward and Backward sampled for Sequoia NDVI")+
  #theme(aspect.ratio=1)+
  xlab('First run forward NDVI')+
  ylab('First run backward NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 3 FWD vs BKd NDVI comparison

x <- as.vector(ThirdRunFwdSeqresampNDVI)
y <- as.vector(ThirdRunBkdSeqresampNDVI)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 3 Forward and Backward sampled for Sequoia NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Third run forward NDVI')+
  ylab('Third run backward NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd NIR comparison

x <- as.vector(SecondRunFwdSeqresampNIR)
y <- as.vector(SecondRunBkdSeqresampNIR)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 2 Forward and Backward sampled for Sequoia NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward NIR')+
  ylab('Second run backward NIR')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd Red comparison

x <- as.vector(SecondRunFwdSeqresampRed)
y <- as.vector(SecondRunBkdSeqresampRed)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 2 Forward and Backward sampled for Sequoia Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward Red')+
  ylab('Second run backward Red')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)


# Plot Tramway Run 2 FWD vs BKd Green comparison

x <- as.vector(SecondRunFwdSeqresampGreen)
y <- as.vector(SecondRunBkdSeqresampGreen)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 2 Forward and Backward sampled for Sequoia Green band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward Green')+
  ylab('Second run backward Green')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd RedEdge comparison

x <- as.vector(SecondRunFwdSeqresampRedEdge)
y <- as.vector(SecondRunBkdSeqresampRedEdge)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 2 Forward and Backward sampled for Sequoia RedEdge  band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward RedEdge')+
  ylab('Second run backward RedEdge')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 3 FWD vs BKd NIR comparison

x <- as.vector(ThirdRunFwdSeqresampNIR)
y <- as.vector(ThirdRunBkdSeqresampNIR)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 3 Forward and Backward sampled for Sequoia NIR band")+
  #theme(aspect.ratio=1)+
  xlab('Third run forward NIR')+
  ylab('Third run backward NIR')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)


# Plot Tramway Run 1 FWD vs BKd NIR comparison

x <- as.vector(FirstRunFwdSeqresampNIR)
y <- as.vector(FirstRunBkdSeqresampNIR)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 1 Forward and Backward sampled for Sequoia NIR band")+
  #theme(aspect.ratio=1)+
  xlab('First run forward NIR')+
  ylab('First run backward NIR')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)


# Plot Tramway Run 2 FWD vs BKd NDVI comparison (sampled for MRE)

x <- as.vector(SecondRunFwdMREresampNDVI)
y <- as.vector(SecondRunBkdMREresampNDVI)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 2 Forward and Backward sampled for MRE NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward NDVI')+
  ylab('Second run backward NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 1 FWD vs BKd NDVI comparison (sampled for MRE)

x <- as.vector(FirstRunFwdMREresampNDVI)
y <- as.vector(FirstRunBkdMREresampNDVI)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 1 Forward and Backward sampled for MRE NDVI")+
  #theme(aspect.ratio=1)+
  xlab('First run forward NDVI')+
  ylab('First run backward NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 3 FWD vs BKd NDVI comparison (sampled for MRE)

x <- as.vector(ThirdRunFwdMREresampNDVI)
y <- as.vector(ThirdRunBkdMREresampNDVI)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 3 Forward and Backward sampled for MRE NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Third run forward NDVI')+
  ylab('Third run backward NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd NIR comparison (sampled for MRE)

x <- as.vector(SecondRunFwdMREresampNIR)
y <- as.vector(SecondRunBkdMREresampNIR)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 2 Forward and Backward sampled for MRE NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward NIR')+
  ylab('Second run backward NIR')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd Red comparison (sampled for MRE)

x <- as.vector(SecondRunFwdMREresampRed)
y <- as.vector(SecondRunBkdMREresampRed)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 2 Forward and Backward sampled for MRE Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward Red')+
  ylab('Second run backward Red')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd Blue comparison (sampled for MRE)

x <- as.vector(SecondRunFwdMREresampBlue)
y <- as.vector(SecondRunBkdMREresampBlue)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 2 Forward and Backward sampled for MRE Blue band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward Blue ')+
  ylab('Second run backward Blue')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd Green comparison (sampled for MRE)

x <- as.vector(SecondRunFwdMREresampGreen)
y <- as.vector(SecondRunBkdMREresampGreen)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 2 Forward and Backward sampled for MRE Green band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward Green')+
  ylab('Second run backward Green')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 2 FWD vs BKd RedEdge comparison (sampled for MRE)

x <- as.vector(SecondRunFwdMREresampRedEdge)
y <- as.vector(SecondRunBkdMREresampRedEdge)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 2 Forward and Backward sampled for MRE RedEdge  band")+
  #theme(aspect.ratio=1)+
  xlab('Second run forward RedEdge')+
  ylab('Second run backward RedEdge')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

# Plot Tramway Run 3 FWD vs BKd NIR comparison (sampled for MRE)

x <- as.vector(ThirdRunFwdMREresampNIR)
y <- as.vector(ThirdRunBkdMREresampNIR)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 3 Forward and Backward sampled for MRE NIR band")+
  #theme(aspect.ratio=1)+
  xlab('Third run forward NIR')+
  ylab('Third run backward NIR')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)


# Plot Tramway Run 1 FWD vs BKd NIR comparison (sampled for MRE)

x <- as.vector(FirstRunFwdMREresampNIR)
y <- as.vector(FirstRunBkdMREresampNIR)
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
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("comparison of Tramway Run 1 Forward and Backward sampled for MRE NIR band")+
  #theme(aspect.ratio=1)+
  xlab('First run forward NIR')+
  ylab('First run backward NIR')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)
