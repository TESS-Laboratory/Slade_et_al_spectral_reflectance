###Analyse All Drone data from Are1, TRM1, TRM2, TRM3 and all Tramway passes

# Library
{
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
library(DescTools)  
}
#----------1.Theme--------

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
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
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
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
{
TRM_1_seqSpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SpectralonRefl_stackcrop.tif")
TRM_1_seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/indices/TRM_1_SEQ_3lines_SPE_index_ndvi.tif")
TRM_1_seqSpectralonNDVICrop <- crop(TRM_1_seqSpectralonNDVI,tramwayROI)

TRM_2_seqSpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/TRM_2_SEQ_proj2lines_SpectralonRefl_stackcrop.tif")
TRM_2_seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/indices/TRM_2_SEQ_proj2lines_index_ndvi.tif")
TRM_2_seqSpectralonNDVICrop <- crop(TRM_2_seqSpectralonNDVI,tramwayROI)

TRM_3_seqSpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/TRM_3_SEQ_2lines_SpectralonRefl_stackcrop.tif")
TRM_3_seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/indices/TRM_3_SEQ_2lines_SPE_index_ndvi.tif")
TRM_3_seqSpectralonNDVICrop <- crop(TRM_3_seqSpectralonNDVI,tramwayROI)

ARE_1_seqSpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ_2lines/Spectralon/ARE_1_SEQ_2lines_SpectralonRefl_stackcrop.tif")
ARE_1_seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ_2lines/Spectralon/indices/ARE_1_SEQ_2lines_SPE_index_ndvi.tif")
ARE_1_seqSpectralonNDVICrop <- crop(ARE_1_seqSpectralonNDVI,tramwayROI)


}

##-----4. Read in MRE Stacked images and NDVI, crop NDV-----
{
TRM_1_MRESpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SpectralonRefl_stackcrop.tif")
TRM_1_MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/indices/TRM_1_MRE_2lines_SPE_index_ndvi.tif")
TRM_1_MRESpectralonNDVICrop <- crop(TRM_1_MRESpectralonNDVI,tramwayROI)

TRM_2_MRESpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/TRM_2_MRE_proj2lines_SpectralonRefl_stackcrop.tif")
TRM_2_MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/indices/TRM_2_MRE_proj2lines_index_ndvi.tif")
TRM_2_MRESpectralonNDVICrop <- crop(TRM_2_MRESpectralonNDVI,tramwayROI)

TRM_3_MRESpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/TRM_3_MRE_2lines_SpectralonRefl_stackcrop.tif")
TRM_3_MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/indices/TRM_3_MRE_SPE_2lines_index_ndvi.tif")
TRM_3_MRESpectralonNDVICrop <- crop(TRM_3_MRESpectralonNDVI,tramwayROI)

ARE_1_MRESpectralonReflStackCrop <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE_2lines/Spectralon/ARE_1_MRE_2lines_SpectralonRefl_stackcrop.tif")
ARE_1_MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE_2lines/Spectralon/indices/ARE_1_MRE_2lines_SPE_index_ndvi.tif")
ARE_1_MRESpectralonNDVICrop <- crop(ARE_1_MRESpectralonNDVI,tramwayROI)
}
#----5.Read in tramway data----
{
SecondRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunForward_REMResampSRF.csv")
SecondRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunBackward_REMResampSRF.csv")
SecondRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunForward_SeqResamp.csv")
SecondRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-SecondRunBackward_SeqResamp.csv")
FirstRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunForward_REMResampSRF.csv")
FirstRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunBackward_REMResampSRF.csv")
FirstRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunForward_SeqResamp.csv")
FirstRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-FirstRunBackward_SeqResamp.csv")
ThirdRunFwdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunForward_REMResampSRF.csv")
ThirdRunBkdMREresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunBackward_REMResampSRF.csv")
ThirdRunFwdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunForward_SeqResamp.csv")
ThirdRunBkdSeqresampSpectra <- read.csv("E:/Glenn/Tramway Experiment/Processed/TramwayData/Resamp_reflectance/eventdata-2020-02-24-ThirdRunBackward_SeqResamp.csv")

MeanFwdSeqresampSpectra <- (FirstRunFwdSeqresampSpectra+SecondRunFwdSeqresampSpectra+ThirdRunFwdSeqresampSpectra)/3
MeanFwdMREresampSpectra <-  (FirstRunFwdMREresampSpectra+SecondRunFwdMREresampSpectra+ThirdRunFwdMREresampSpectra)/3
}

#----6.Extract tramway spectra for SEQ and MRE spectral bands and calculate NDVI-----
{
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

#Average of Forward Runs MRE

MeanFwdMREresampBlue <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==475] 
MeanFwdMREresampGreen <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==560] 
MeanFwdMREresampRed <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==668] 
MeanFwdMREresampRedEdge <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==717] 
MeanFwdMREresampNIR <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==840] 
MeanFwdMREresampdf <- data.frame(MeanFwdMREresampBlue,MeanFwdMREresampGreen,MeanFwdMREresampRed,MeanFwdMREresampRedEdge,MeanFwdMREresampNIR)
MeanFwdMREresampNDVI <- (MeanFwdMREresampNIR-MeanFwdMREresampRed)/(MeanFwdMREresampNIR+MeanFwdMREresampRed)

# Average of Forward Runs Sequoia

MeanFwdSeqresampGreen <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==550] 
MeanFwdSeqresampRed <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==660] 
MeanFwdSeqresampRedEdge <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==735] 
MeanFwdSeqresampNIR <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==790] 
MeanFwdSeqresampdf <- data.frame(MeanFwdSeqresampGreen,MeanFwdSeqresampRed,MeanFwdSeqresampRedEdge,MeanFwdSeqresampNIR)
MeanFwdSeqresampNDVI <- (MeanFwdSeqresampNIR-MeanFwdSeqresampRed)/(MeanFwdSeqresampNIR+MeanFwdSeqresampRed)

}

#----7.Extract reflectance data from Stacked image data for Tramway footprints----
#SEQ Extract
TRM_1_seqFootprintSpectralonReflectance <- extract(TRM_1_seqSpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(TRM_1_seqFootprintSpectralonReflectance) <- c('location','green','red','redEdge','NIR')
TRM_1_seqFootprintSpectralonNDVI <- extract(TRM_1_seqSpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_2_seqFootprintSpectralonReflectance <- extract(TRM_2_seqSpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(TRM_2_seqFootprintSpectralonReflectance) <- c('location','green','red','redEdge','NIR')
TRM_2_seqFootprintSpectralonNDVI <- extract(TRM_2_seqSpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_3_seqFootprintSpectralonReflectance <- extract(TRM_3_seqSpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(TRM_3_seqFootprintSpectralonReflectance) <- c('location','green','red','redEdge','NIR')
TRM_3_seqFootprintSpectralonNDVI <- extract(TRM_3_seqSpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

ARE_1_seqFootprintSpectralonReflectance <- extract(ARE_1_seqSpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(ARE_1_seqFootprintSpectralonReflectance) <- c('location','green','red','redEdge','NIR')
ARE_1_seqFootprintSpectralonNDVI <- extract(ARE_1_seqSpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

# Average Sequoia reflectance for Footprint

TRM_Mean_SEQ_FootprintSpectralonReflectance <- (TRM_1_seqFootprintSpectralonReflectance+TRM_2_seqFootprintSpectralonReflectance+TRM_3_seqFootprintSpectralonReflectance)/3
TRM_Mean_SEQ_FootprintSpectralonNDVI <- (TRM_1_seqFootprintSpectralonNDVI+TRM_2_seqFootprintSpectralonNDVI+TRM_3_seqFootprintSpectralonNDVI)/3

#MRE Extract

ARE_1_MREFootprintSpectralonReflectance <- extract(ARE_1_MRESpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(ARE_1_MREFootprintSpectralonReflectance) <- c('location','blue','green','red','redEdge','NIR')
ARE_1_MREFootprintNDVI <- extract(ARE_1_MRESpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_MREFootprintSpectralonReflectance <- extract(TRM_1_MRESpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(TRM_1_MREFootprintSpectralonReflectance) <- c('location','blue','green','red','redEdge','NIR')
TRM_1_MREFootprintNDVI <- extract(TRM_1_MRESpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_2_MREFootprintSpectralonReflectance <- extract(TRM_2_MRESpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(TRM_2_MREFootprintSpectralonReflectance) <- c('location','blue','green','red','redEdge','NIR')
TRM_2_MREFootprintNDVI <- extract(TRM_2_MRESpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_3_MREFootprintSpectralonReflectance <- extract(TRM_3_MRESpectralonReflStackCrop,tramwayFootprintsShapes,fun=mean,df=TRUE)
names(TRM_3_MREFootprintSpectralonReflectance) <- c('location','blue','green','red','redEdge','NIR')
TRM_3_MREFootprintNDVI <- extract(TRM_3_MRESpectralonNDVICrop,tramwayFootprintsShapes,fun=mean,df=TRUE)

#Average MRE Reflectance for Footprint

TRM_Mean_MRE_FootprintSpectralonReflectance <- (TRM_1_MREFootprintSpectralonReflectance+TRM_2_MREFootprintSpectralonReflectance+TRM_3_MREFootprintSpectralonReflectance)/3
TRM_Mean_MRE_FootprintSpectralonNDVI <- (TRM_1_MREFootprintNDVI+TRM_2_MREFootprintNDVI+TRM_3_MREFootprintNDVI)/3


#-----8. Tramway Data Only Plots--------

#Tramway runs resampled for Sequoia
plot(1:110, FirstRunFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, FirstRunFwdSeqresampNDVI,col='black')
lines(1:110, FirstRunBkdSeqresampNDVI,col='red')
lines(1:110, SecondRunFwdSeqresampNDVI,col='blue')
lines(1:110, SecondRunBkdSeqresampNDVI,col='green')
lines(1:110, ThirdRunFwdSeqresampNDVI,col='grey')
lines(1:110, ThirdRunBkdSeqresampNDVI,col='yellow')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'yellow'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for Sequoia bands")

#Tramway runs resampled for MRE
plot(1:110, FirstRunFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, FirstRunFwdMREresampNDVI,col='black')
lines(1:110, FirstRunBkdMREresampNDVI,col='red')
lines(1:110, SecondRunFwdMREresampNDVI,col='blue')
lines(1:110, SecondRunBkdMREresampNDVI,col='green')
lines(1:110, ThirdRunFwdMREresampNDVI,col='grey')
lines(1:110, ThirdRunBkdMREresampNDVI,col='yellow')
legend(20, 0.4, legend=c("First run FWd", "First Run Bkd", "Second Run Fwd", "Second Run Bkd", "Third Run Fwd", "Third Run Bkd"),
       lty=c(1,1),col=c('black','red', 'blue','green', 'grey', 'yellow'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("All Tramway passes resampled for MRE bands")

#Tramway runs comparison of resampling
plot(1:110, MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdMREresampNDVI,col='black')
lines(1:110, MeanFwdSeqresampNDVI,col='red')
legend(20, 0.4, legend=c("Mean FWd Runs MRE", "Mean FWd Runs SEQ"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("Tramway Mean Forward Runs NDVI comparison of resampling for Sequoia and MRE band wavelengths")



#-----9. TRM 1 SEQ Drone Survey and Tramway Data Plots Mean Forward Runs --------

#SEQ NDVI VS TRAMWAY NDVI PLOT
plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdSeqresampNDVI,col='black')
lines(1:110, TRM_1_seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi,col='red')
legend(0, 0.4, legend=c("Tramway Spectrometer NDVI (resampled for Sequoia bandwidth)", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("Parrot Sequoia NDVI and Tramway Data Mean NDVI")

#MRE NDVI VS TRAMWAY NDVI PLOT
plot(1:110, MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdMREresampNDVI,col='black')
lines(1:110, TRM_1_MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi, col='blue')
legend(0, 0.45, legend=c("Tramway Spectrometer NDVI (resampled for MRE bandwidth)", "MRE NDVI"),
       lty=c(1,1),col=c('black','blue'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("MicaSense RedEdge NDVI and Tramway Data Mean NDVI")


#TRM 1 SEQ NDVI VS TRM 1 MRE NDVI PLOT
plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, TRM_1_MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi ,col= 'black')
lines(1:110, TRM_1_seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi,col='red')
#lines(1:110, SecondRunFwdSeqresampNDVI,col='green')
legend(20, 0.4, legend=c("MRE NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 SEQ Drone Survey and  TRM1 MRE Drone Survey")



# Plot Tramway Mean Fwd Sequoia TRM1 Green Band

x <- as.vector(MeanFwdSeqresampGreen)
y <- as.vector(TRM_1_seqFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1sg <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Mean Tramway Data with Sequoia \n Survey TRM1 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Green band')+
  ylab('Reflectance Sequioa Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p1sg)

# Plot Tramway Mean Data against Sequoia TRM1 Red Band

x <- as.vector(MeanFwdSeqresampRed)
y <- as.vector(TRM_1_seqFootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1sr <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n TRM1 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Red band')+
  ylab('Reflectance Sequioa Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Meant data compared Sequoia TRM1 RedEdge Band

x <- as.vector(MeanFwdSeqresampRedEdge)
y <- as.vector(TRM_1_seqFootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1sre <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n TRM1 RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia RedEdge band')+
  ylab('Reflectance Sequioa RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))


#Plot Tramway Mean Data Compared with Sequoia TRM1 NIR Band

x <- as.vector(MeanFwdSeqresampNIR)
y <- as.vector(TRM_1_seqFootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1sni <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey TRM1 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NIR band')+
  ylab('Reflectance Sequioa NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data compared with Sequoia TRM1 NDVI

x <- as.vector(MeanFwdSeqresampNDVI)
y <- as.vector(TRM_1_seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1snv <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data \n with Sequoia Survey TRM1 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NDVI')+
  ylab('Sequoia NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)
#----10. TRM 1 MRE Drone Survey and Tramway Mean Forward Runs------
{
#MRE NDVI VS TRAMWAY NDVI PLOT (2nd run fwd)
plot(1:110, MeanFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdMREresampNDVI,col='black')
lines(1:110, TRM_1_MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi ,col='red')
legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 MRE Drone Survey - Tramway Mean Data")

# Plot Tramway Mean Data compared with MRE TRM1 Blue Band

x <- as.vector(MeanFwdMREresampBlue)
y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$blue)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1mb <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 Blue Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Blue band')+
  ylab('Reflectance MRE Blue Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data Compared with  MRE TRM1 Green Band

x <- as.vector(MeanFwdMREresampGreen)
y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1mg <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Green band')+
  ylab('Reflectance MRE Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data compared with MRE TRM1 Red Band

x <- as.vector(MeanFwdMREresampRed)
y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1mr <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE Survey \n TRM1 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Red band')+
  ylab('Reflectance MRE Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data compared with MRE TRM1 RedEdge Band

x <- as.vector(MeanFwdMREresampRedEdge)
y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1mre <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE Survey \n TRM1 RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE RedEdge band')+
  ylab('Reflectance MRE RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data Compared with MRE TRM1 NIR Band

x <- as.vector(MeanFwdMREresampNIR)
y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1mni <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NIR band')+
  ylab('Reflectance MRE NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data compared with MRE TRM1 NDVI

x <- as.vector(MeanFwdMREresampNDVI)
y <- as.vector(TRM_1_MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p1mnv <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data \n with MRE Survey TRM1 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NDVI')+
  ylab('MRE NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p1mnv)
}

#-----11. TRM 2 Sequoia Drone Survey and Tramway Mean Data Plots--------
{
#TRM 2 SEQ NDVI VS TRM 2 MRE PLOT
plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, TRM_2_MREFootprintNDVI$TRM_2_MRE_proj2lines_index_ndvi ,col= 'black')
lines(1:110, TRM_2_seqFootprintSpectralonNDVI$TRM_2_SEQ_proj2lines_index_ndvi,col='red')
#lines(1:110, SecondRunFwdSeqresampNDVI,col='green')
legend(20, 0.4, legend=c("MRE NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM2 SEQ compared with TRM2 MRE ")

#TRM 2 SEQ Spectralon Mean Forward Tramway
plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdSeqresampNDVI,col='black')
lines(1:110, TRM_2_seqFootprintSpectralonNDVI$TRM_2_SEQ_proj2lines_index_ndvi,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM2 Sequioa Drone Survey - Tramway Mean Data")


# Plot Tramway Mean Data compared with Sequoia TRM2 Green Band

x <- as.vector(MeanFwdSeqresampGreen)
y <- as.vector(TRM_2_seqFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2sg <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey TRM2 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Green band')+
  ylab('Reflectance Sequioa Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data compared with Sequoia TRM2 Red Band

x <- as.vector(MeanFwdSeqresampRed)
y <- as.vector(TRM_2_seqFootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2sr <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n TRM2 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Red band')+
  ylab('Reflectance Sequioa Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data compared with Sequoia TRM2 RedEdge Band

x <- as.vector(MeanFwdSeqresampRedEdge)
y <- as.vector(TRM_2_seqFootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2sre <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n TRM2 RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia RedEdge band')+
  ylab('Reflectance Sequioa RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data compared with Sequoia TRM2 NIR Band

x <- as.vector(MeanFwdSeqresampNIR)
y <- as.vector(TRM_2_seqFootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2sni <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey TRM2 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NIR band')+
  ylab('Reflectance Sequioa NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data with Sequoia TRM2 NDVI

x <- as.vector(MeanFwdSeqresampNDVI)
y <- as.vector(TRM_2_seqFootprintSpectralonNDVI$TRM_2_SEQ_proj2lines_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2snv <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data \n with Sequoia Survey TRM2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NDVI')+
  ylab('Sequoia NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)
}

#-----12. TRM 2 MRE survey and Tramway Mean Data plots-----
{
#TRM 2 MRE Spectralon vs Mean Tramway Data
plot(1:110, MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdMREresampNDVI,col='black')
lines(1:110, TRM_2_MREFootprintNDVI$TRM_2_MRE_proj2lines_index_ndvi,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM2 MRE Compared with Tramway Mean Data")

# Plot Tramway Mean Data MRE TRM2 Blue Band

x <- as.vector(MeanFwdMREresampBlue)
y <- as.vector(TRM_2_MREFootprintSpectralonReflectance$blue)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2mb <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM2 Blue Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Blue band')+
  ylab('Reflectance MRE Blue Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data vs MRE TRM2 Green Band

x <- as.vector(MeanFwdMREresampGreen)
y <- as.vector(TRM_2_MREFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2mg <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM2 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Green band')+
  ylab('Reflectance MRE Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data vs MRE TRM2 Red Band

x <- as.vector(MeanFwdMREresampRed)
y <- as.vector(TRM_2_MREFootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2mr <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE Survey \n TRM2 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Red band')+
  ylab('Reflectance MRE Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data vs MRE TRM2 RedEdge Band

x <- as.vector(MeanFwdMREresampRedEdge)
y <- as.vector(TRM_2_MREFootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2mre <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE Survey \n TRM2 RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE RedEdge band')+
  ylab('Reflectance MRE RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data vs MRE TRM2 NIR Band

x <- as.vector(MeanFwdMREresampNIR)
y <- as.vector(TRM_2_MREFootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2mni <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM2 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NIR band')+
  ylab('Reflectance MRE NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data vs MRE TRM2 NDVI

x <- as.vector(MeanFwdMREresampNDVI)
y <- as.vector(TRM_2_MREFootprintNDVI$TRM_2_MRE_proj2lines_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p2mnv <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data \n with MRE Survey TRM2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NDVI')+
  ylab('MRE NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p2mnv)
}
#----13. TRM 3 Sequoia Drone Survey and Tramway Mean Data plots -----
{
  #TRM 3 SEQ Spectralon 2nd run Bk
  plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
  lines(1:110, MeanFwdSeqresampNDVI,col='black')
  lines(1:110, TRM_3_seqFootprintSpectralonNDVI$TRM_3_SEQ_2lines_SPE_index_ndvi,col='red')
  legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
         lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
  title("TRM3 Sequioa Drone Survey - Tramway Mean Data")
  
  
  # Plot Tramway Mean data vs Sequoia TRM3 Green Band
  
  x <- as.vector(MeanFwdSeqresampGreen)
  y <- as.vector(TRM_3_seqFootprintSpectralonReflectance$green)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  p3sg <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey TRM3 Green Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for Sequoia Green band')+
    ylab('Reflectance Sequioa Green Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  # Plot Tramway Mean data vs Sequoia TRM3 Red Band
  
  x <- as.vector(MeanFwdSeqresampRed)
  y <- as.vector(TRM_3_seqFootprintSpectralonReflectance$red)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  p3sr <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Run Mean Data with Sequoia Survey \n TRM3 Red Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for Sequoia Red band')+
    ylab('Reflectance Sequioa Red Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  # Plot Tramway Mean Data vs Sequoia TRM3 RedEdge Band
  
  x <- as.vector(MeanFwdSeqresampRedEdge)
  y <- as.vector(TRM_3_seqFootprintSpectralonReflectance$redEdge)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  p3sre <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n TRM3 RedEdge Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for Sequoia RedEdge band')+
    ylab('Reflectance Sequioa RedEdge Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  #Plot Tramway Mean data vs Sequoia TRM3 NIR Band
  
  x <- as.vector(MeanFwdSeqresampNIR)
  y <- as.vector(TRM_3_seqFootprintSpectralonReflectance$NIR)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  p3sni <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey TRM3 NIR Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for Sequoia NIR band')+
    ylab('Reflectance Sequioa NIR Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  #Plot Tramway Mean Data vs Sequoia TRM3 NDVI
  
  x <- as.vector(MeanFwdSeqresampNDVI)
  y <- as.vector(TRM_3_seqFootprintSpectralonNDVI$TRM_3_SEQ_2lines_SPE_index_ndvi)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  p3snv <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data \n with Sequoia Survey TRM3 NDVI")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for Sequoia NDVI')+
    ylab('Sequoia NDVI')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p3snv)
}

#----14. TRM 3 MRE Drone Survey and Tramway Mean data plots -----

{
#TRM 3 MRE Spectralon 2nd run Bk
plot(1:110, MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdMREresampNDVI,col='black')
lines(1:110, TRM_3_MREFootprintNDVI$TRM_3_MRE_SPE_2lines_index_ndvi,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM3 MRE Spectralon - Tramway Mean Data")

# Plot Tramway Mean data MRE TRM3 Blue Band

x <- as.vector(MeanFwdMREresampBlue)
y <- as.vector(TRM_3_MREFootprintSpectralonReflectance$blue)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p3mb <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM3 Blue Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Blue band')+
  ylab('Reflectance MRE Blue Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean data vs MRE TRM3 Green Band

x <- as.vector(MeanFwdMREresampGreen)
y <- as.vector(TRM_3_MREFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p3mg <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM3 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Green band')+
  ylab('Reflectance MRE Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean Data vs MRE TRM3 Red Band

x <- as.vector(MeanFwdMREresampRed)
y <- as.vector(TRM_3_MREFootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p3mr <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE Survey \n TRM3 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Red band')+
  ylab('Reflectance MRE Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean data vs MRE TRM3 RedEdge Band

x <- as.vector(MeanFwdMREresampRedEdge)
y <- as.vector(TRM_3_MREFootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p3mre <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean data with MRE Survey \n TRM3 RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE RedEdge band')+
  ylab('Reflectance MRE RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean data vs MRE TRM3 NIR Band

x <- as.vector(MeanFwdMREresampNIR)
y <- as.vector(TRM_3_MREFootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p3mni <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean data with MRE \n Survey TRM3 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NIR band')+
  ylab('Reflectance MRE NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean data vs MRE TRM3 NDVI

x <- as.vector(MeanFwdMREresampNDVI)
y <- as.vector(TRM_3_MREFootprintNDVI$TRM_3_MRE_SPE_2lines_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

p3mnv <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data \n with MRE Survey TRM3 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NDVI')+
  ylab('MRE NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p3mnv)
}

#----15. ARE 1 Sequoia Drone Survey and Tramway Mean Data plots -----
#ARE 1 SEQ vs Mean Data
{
plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdSeqresampNDVI,col='black')
lines(1:110, ARE_1_seqFootprintSpectralonNDVI$ARE_1_SEQ_2lines_SPE_index_ndvi,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("ARE1 Sequoia Drone Survey - Tramway Mean Data")


# Plot Tramway Mean Data vs Sequoia ARE1 Green Band

x <- as.vector(MeanFwdSeqresampGreen)
y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pasg <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey ARE1 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Green band')+
  ylab('Reflectance Sequioa Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Mean data vs Sequoia ARE1 Red Band

x <- as.vector(MeanFwdSeqresampRed)
y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pasr <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n ARE1 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Red band')+
  ylab('Reflectance Sequioa Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

# Plot Tramway Run 1 Bkd Sequoia ARE1 RedEdge Band

x <- as.vector(MeanFwdSeqresampRedEdge)
y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pasre <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n ARE1 RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia RedEdge band')+
  ylab('Reflectance Sequioa RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean data vs Sequoia ARE1 NIR Band

x <- as.vector(MeanFwdSeqresampNIR)
y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pasni <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey ARE1 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NIR band')+
  ylab('Reflectance Sequioa NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#plot(p)

#Plot Tramway Mean Data vs Sequoia ARE1 NDVI

x <- as.vector(MeanFwdSeqresampNDVI)
y <- as.vector(ARE_1_seqFootprintSpectralonNDVI$ARE_1_SEQ_2lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pasnv <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1,col='grey' ) +
  ggtitle("Comparison of Tramway Mean data \n with Sequoia Survey ARE1 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NDVI')+
  ylab('Sequoia NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pasnv)
}

#----16. ARE 1 MRE Drone Survey and Tramway Mean Data plots -----
{
  
  plot(1:110, MeanFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
  lines(1:110, MeanFwdMREresampNDVI,col='black')
  lines(1:110, ARE_1_MREFootprintNDVI$ARE_1_MRE_2lines_SPE_index_ndvi ,col='red')
  legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
         lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
  title("ARE1 MRE Spectralon - Tramway Mean Data")
  
  # Plot Tramway Run 1 Bkd MRE ARE1 Blue Band
  
  x <- as.vector(MeanFwdMREresampBlue)
  y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$blue)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pamb <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with MRE \n Survey ARE1 Blue Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for MRE Blue band')+
    ylab('Reflectance MRE Blue Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  # Plot Tramway Mean Data vs MRE ARE1 Green Band
  
  x <- as.vector(MeanFwdMREresampGreen)
  y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$green)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pamg <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with MRE \n Survey ARE1 Green Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for MRE Green band')+
    ylab('Reflectance MRE Green Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  # Plot Tramway Mean data vs MRE ARE1 Red Band
  
  x <- as.vector(MeanFwdMREresampRed)
  y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$red)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pamr <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with MRE Survey \n ARE1 Red Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for MRE Red band')+
    ylab('Reflectance MRE Red Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  # Plot Tramway Mean Data vs MRE ARE1 RedEdge Band
  
  x <- as.vector(MeanFwdMREresampRedEdge)
  y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$redEdge)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pamre <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with MRE Survey \n ARE1 RedEdge Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for MRE RedEdge band')+
    ylab('Reflectance MRE RedEdge Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  #Plot Tramway Mean Data vs MRE ARE1 NIR Band
  
  x <- as.vector(MeanFwdMREresampNIR)
  y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$NIR)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pamni <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data with MRE \n Survey ARE1 NIR Band")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for MRE NIR band')+
    ylab('Reflectance MRE NIR Band')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  #plot(p)
  
  #Plot Tramway Mean data vs  MRE ARE1 NDVI
  
  x <- as.vector(MeanFwdMREresampNDVI)
  y <- as.vector(ARE_1_MREFootprintNDVI$ARE_1_MRE_2lines_SPE_index_ndvi)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pamnv <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1,col='grey' ) +
    ggtitle("Comparison of Tramway Mean Data \n with MRE Survey ARE1 NDVI")+
    #theme(aspect.ratio=1)+
    xlab('Tramway Relectance resampled for MRE NDVI')+
    ylab('MRE NDVI')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pamnv)
}

#----17. Panel arrangement of Plots-----
PlotTRM1_SEQ <- grid.arrange(p1sg, p1sr, p1sre, p1sni,p1snv, nrow = 3)#Plots of TRM1 Sequoia survey
PlotTRM1_MRE <-grid.arrange(p1mb,p1mg, p1mr, p1mre, p1mni,p1mnv, nrow = 3)#Plots of TRM1 MRE Survey
PlotTRM2_SEQ <-grid.arrange(p2sg, p2sr, p2sre, p2sni,p2snv, nrow = 3)#Plots of TRM2 Sequoia survey
PlotTRM2_MRE <-grid.arrange(p2mb,p2mg, p2mr, p2mre, p2mni,p2mnv, nrow = 3)#Plots of TRM2 MRE Survey
PlotTRM3_SEQ <- grid.arrange(p3sg, p3sr, p3sre, p3sni,p3snv, nrow = 3)#Plots of TRM3 Sequoia survey
PlotTRM3_MRE <-grid.arrange(p3mb,p3mg, p3mr, p3mre, p3mni,p3mnv, nrow = 3)#Plots of TRM3 MRE Survey
PlotARE1_SEQ <-grid.arrange(pasg, pasr, pasre, pasni,pasnv, nrow = 3)#Plots of ARE1 Sequoia survey
PlotARE1_MRE <-grid.arrange(pamb,pamg, pamr, pamre, pamni,pamnv, nrow = 3)#Plots of ARE1 MRE Survey
PlotNDVI <-grid.arrange(p1snv,p1mnv,p2snv, p2mnv, p3snv,p3mnv,pasnv,pamnv, nrow = 4)#Plots of NDVI all Surveys
PlotGreen<-grid.arrange(p1sg,p1mg,p2sg, p2mg, p3sg,p3mg,pasg,pamg, nrow = 4)#Plots of Green Band all Surveys
PlotRed<-grid.arrange(p1sr,p1mr,p2sr, p2mr, p3sr,p3mr,pasr,pamr, nrow = 4)#Plots of Red Band all Surveys
PlotRedEdge<-grid.arrange(p1sre,p1mre,p2sre, p2mre, p3sre,p3mre,pasre,pamre, nrow = 4)#Plots of RedEdge Band all Surveys
PlotNIR<-grid.arrange(p1sni,p1mni,p2sni, p2mni, p3sni,p3mni,pasni,pamni, nrow = 4)#Plots of NIR Band all Surveys
PlotBlue<-grid.arrange(p1mb,p2mb,p3mb,pamb, nrow = 2)#Plots of Blue Band all Surveys

plot(1:110, MeanFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdMREresampNDVI,col='black')
lines(1:110, TRM_1_MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi ,col='red')
lines(1:110, TRM_2_MREFootprintNDVI$TRM_2_MRE_proj2lines_index_ndvi ,col='green')
lines(1:110, TRM_3_MREFootprintNDVI$TRM_3_MRE_SPE_2lines_index_ndvi ,col='blue')
lines(1:110, ARE_1_MREFootprintNDVI$ARE_1_MRE_2lines_SPE_index_ndvi ,col='orange')

legend(20, 0.6, legend=c("Tramway Mean Data NDVI", "TRM_1 NDVI", "TRM_2 NDVI","TRM_3 NDVI", "ARE_1 NDVI"),
       lty=c(1,1),col=c('black','red', 'green','blue','orange'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("NDVI MRE Surveys")

plot(1:110, MeanFwdSeqresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdSeqresampNDVI,col='black')
lines(1:110, TRM_1_seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi ,col='red')
lines(1:110, TRM_2_seqFootprintSpectralonNDVI$TRM_2_SEQ_proj2lines_index_ndvi ,col='green')
lines(1:110, TRM_3_seqFootprintSpectralonNDVI$TRM_3_SEQ_2lines_SPE_index_ndvi ,col='blue')
lines(1:110, ARE_1_seqFootprintSpectralonNDVI$ARE_1_SEQ_2lines_SPE_index_ndvi ,col='orange')

legend(20, 0.6, legend=c("Tramway Mean Data NDVI", "TRM_1 NDVI", "TRM_2 NDVI","TRM_3 NDVI", "ARE_1 NDVI"),
       lty=c(1,1),col=c('black','red', 'green','blue','orange'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("NDVI Sequoia Surveys")

#-----18. Save Plots------

ggsave(
  PlotNDVI,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/NDVI_all_surveys_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotBlue,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/Blue_all_surveys_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotGreen,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/Green_all_surveys_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotRed,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/Red_all_surveys_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  PlotRedEdge,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/RedEdge_all_surveys_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  PlotNIR,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/NIR_all_surveys_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  PlotTRM1_SEQ,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/TRM1_SEQ_All bands_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM2_SEQ,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/TRM2_SEQ_All bands_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM3_SEQ,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/TRM3_SEQ_All bands_M.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM1_MRE,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/TRM1_MRE_All bands_M.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM2_MRE,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/TRM2_MRE_All bands_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM3_MRE,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/TRM3_MRE_All bands_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotARE1_MRE,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/ARE1_MRE_All bands_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotARE1_SEQ,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/ARE1_SEQ_All bands_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)

#----19. SEQ Vs MRE TRM1 Survey----
# Plot SEQ TRM1 vs MRE TRM1 Green
{
x <- as.vector(TRM_1_seqFootprintSpectralonReflectance$green)
y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pGreenTRM1 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of  Sequoia \n Survey TRM1 with MRE TRM1 Green")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance (Green) Sequoia')+
  ylab('Reflectance (Green) MRE')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pGreenTRM1)
}
# Plot SEQ TRM1 vs MRE TRM1 Red
{
  x <- as.vector(TRM_1_seqFootprintSpectralonReflectance$red)
  y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$red)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pRedTRM1 <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n Survey TRM1 with MRE TRM1 Red")+
    #theme(aspect.ratio=1)+
    xlab('Reflectance (Red) Sequoia')+
    ylab('Reflectance (Red) MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pRedTRM1)
}
# Plot SEQ TRM1 vs MRE TRM1 RedEdge
{
  x <- as.vector(TRM_1_seqFootprintSpectralonReflectance$redEdge)
  y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$redEdge)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pRedEdgeTRM1 <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n Survey TRM1 with MRE TRM1 RedEdge")+
    #theme(aspect.ratio=1)+
    xlab('Reflectance (RedEdge) Sequoia')+
    ylab('Reflectance (RedEdge) MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pRedEdgeTRM1)
}
# Plot SEQ TRM1 vs MRE TRM1 NIR
{
  x <- as.vector(TRM_1_seqFootprintSpectralonReflectance$NIR)
  y <- as.vector(TRM_1_MREFootprintSpectralonReflectance$NIR)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pNIRTRM1 <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n Survey TRM1 with MRE TRM1 NIR")+
    #theme(aspect.ratio=1)+
    xlab('Reflectance (NIR) Sequoia')+
    ylab('Reflectance (NIR) MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pNIRTRM1)
}
# Plot SEQ TRM1 vs MRE TRM1 NDVI
{
  x <- as.vector(TRM_1_seqFootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi)
  y <- as.vector(TRM_1_MREFootprintNDVI$TRM_1_MRE_2lines_SPE_index_ndvi)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pNDVITRM1 <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n Survey TRM1 with MRE TRM1 NDVI")+
    #theme(aspect.ratio=1)+
    xlab('NDVI Sequoia')+
    ylab('NDVI MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pNDVITRM1)
}

PlotTRM1SEQvMRE <-grid.arrange(pGreenTRM1, pRedTRM1, pRedEdgeTRM1, pNIRTRM1,pNDVITRM1, nrow = 3)#Plots of TRM1 SEQ vs MRE

ggsave(
  PlotTRM1SEQvMRE,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/TRM1_SEQvsMRE_M_SRF.png",
  width = 16,
  height = 25,
  units = "cm"
)


-# ----20 Mean MRE reflectance vs Mean Tramway Data-----

#MRE Blue
x <- as.vector(MeanFwdMREresampBlue)
y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$blue)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMMB <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey Mean Data Blue Band")+
#theme(aspect.ratio=1)+
xlab('Tramway Relectance resampled for MRE Blue band')+
  ylab('Reflectance MRE Blue Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))

plot(pMMB)


#MRE Green
x <- as.vector(MeanFwdMREresampGreen)
y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMMG <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey Mean Data Green Band")+
#theme(aspect.ratio=1)+
xlab('Tramway Relectance resampled for MRE Green band')+
ylab('Reflectance MRE Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMMG)

#MRE Red
x <- as.vector(MeanFwdMREresampRed)
y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMMR <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey Mean Data Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE Red band')+
  ylab('Reflectance MRE Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMMR)

#MRE RedEdge
x <- as.vector(MeanFwdMREresampRedEdge)
y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMMRE <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey Mean Data RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE RedEdge band')+
  ylab('Reflectance MRE RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMMRE)

#MRE NIR
x <- as.vector(MeanFwdMREresampNIR)
y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMMN <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey Mean Data NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NIR band')+
  ylab('Reflectance MRE NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMMN)

#MRE NDVI
x <- as.vector(MeanFwdMREresampNDVI)
y <- as.vector(TRM_Mean_MRE_FootprintSpectralonNDVI$TRM_1_MRE_2lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMMVI <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey Mean Data NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NDVI band')+
  ylab('Reflectance MRE NDVI Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMMVI)

ggsave(
  pMMVI,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/MRE_Mean_NDVI.png",
  width = 16,
  height = 25,
  units = "cm"
)

#----21 Mean Sequoia Reflectance vs Mean Tramway data------

#SEq Green

x <- as.vector(MeanFwdSeqresampGreen)
y <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMSG <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey Mean Data Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Green band')+
  ylab('Reflectance Sequoia Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMSG)

#SEq Red

x <- as.vector(MeanFwdSeqresampRed)
y <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$red)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMSR <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey Mean Data Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia Red band')+
  ylab('Reflectance Sequoia Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMSR)

#SEq RedEdge

x <- as.vector(MeanFwdSeqresampRedEdge)
y <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$redEdge)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMSRE <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey Mean Data RedEdge Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia RedEdge band')+
  ylab('Reflectance Sequoia RedEdge Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMSRE)

#SEq NIR

x <- as.vector(MeanFwdSeqresampNIR)
y <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$NIR)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMSN <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey Mean Data NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NIR band')+
  ylab('Reflectance Sequoia NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMSN)

#SEq NDVI

x <- as.vector(MeanFwdSeqresampNDVI)
y <- as.vector(TRM_Mean_SEQ_FootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pMSVI <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey Mean Data NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NDVI')+
  ylab('Reflectance Sequoia NIR NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMSVI)

ggsave(
  pMSVI,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/SEQ_Mean_NDVI.png",
  width = 16,
  height = 25,
  units = "cm"
)

#----22 Panel arrangements of Plots----


PlotMREMEAN <-grid.arrange(pMMB, pMMG, pMMR, pMMRE,pMMN,pMMVI, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotMREMEAN,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/MRE_Mean_Surveys_all bands.png",
  width = 16,
  height = 25,
  units = "cm"
)

PlotSEQMEAN <-grid.arrange(pMSG, pMSR, pMSRE,pMSN,pMSVI, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotSEQMEAN,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/SEQ_Mean_Surveys_all bands.png",
  width = 16,
  height = 25,
  units = "cm"
)

#----23 SEQ vs MRE Mean Data-------

# Plot SEQ Mean vs MRE Mean Green
{
  x <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$green)
  y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$green)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pGreen <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n  with MRE  Green")+
    #theme(aspect.ratio=1)+
    xlab('Reflectance (Green) Sequoia')+
    ylab('Reflectance (Green) MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pGreen)
}
# Plot SEQ vs MRE  Red
{
  x <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$red)
  y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$red)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pRed <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n  with MRE Red")+
    #theme(aspect.ratio=1)+
    xlab('Reflectance (Red) Sequoia')+
    ylab('Reflectance (Red) MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pRed)
}

# Plot SEQ vs MRE  RedEdge
{
  x <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$redEdge)
  y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$redEdge)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pRedEdge <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n  with MRE Rededge")+
    #theme(aspect.ratio=1)+
    xlab('Reflectance (RedEdge) Sequoia')+
    ylab('Reflectance (RedEdge) MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pRedEdge)
}

# Plot SEQ vs MRE  NIR
{
  x <- as.vector(TRM_Mean_SEQ_FootprintSpectralonReflectance$NIR)
  y <- as.vector(TRM_Mean_MRE_FootprintSpectralonReflectance$NIR)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pNIR <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n  with MRE NIR")+
    #theme(aspect.ratio=1)+
    xlab('Reflectance (NIR) Sequoia')+
    ylab('Reflectance (NIR) MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pNIR)
}
# Plot SEQ vs MRE  NDVI
{
  x <- as.vector(TRM_Mean_SEQ_FootprintSpectralonNDVI$TRM_1_SEQ_3lines_SPE_index_ndvi)
  y <- as.vector(TRM_Mean_MRE_FootprintSpectralonNDVI$TRM_1_MRE_2lines_SPE_index_ndvi)
  df <- data.frame(x = x, y = y,
                   d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  pNDVI <- ggplot(df) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    
    geom_abline(intercept = 0, slope = 1, col='grey' ) +
    ggtitle("Comparison of  Sequoia \n  with MRE NDVI")+
    #theme(aspect.ratio=1)+
    xlab('NDVI Sequoia')+
    ylab('NDVI MRE')+
    #coord_equal(ratio=1)
    coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
  plot(pNDVI)
}
PlotSEQvsMRE <-grid.arrange(pGreen, pRed, pRedEdge,pNIR,pNDVI, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotSEQvsMRE,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/SEQ_VS_MRE_Mean_Surveys_all bands_V2.png",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  pNDVI,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/SEQ_VS_MRE_Mean_Surveys_NDVI.png",
  width = 16,
  height = 25,
  units = "cm"
)

#24----Seq vs Seq Reproducibility ------

#TRM1 SEQ vs TRM2 SEQ Green

x <- as.vector(TRM_1_seqFootprintSpectralonReflectance$green)
y <- as.vector(TRM_2_seqFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pseq1v2green <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM2 Green")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v2green)


#TRM1 SEQ vs TRM3 SEQ Green

x <- as.vector(TRM_1_seqFootprintSpectralonReflectance$green)
y <- as.vector(TRM_3_seqFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pseq1v3green <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM3 Green")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v3green)


x <- as.vector(TRM_2_seqFootprintSpectralonReflectance$green)
y <- as.vector(TRM_3_seqFootprintSpectralonReflectance$green)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

pseq2v3green <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=3.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of  Sequoia TRM2 \n  with Sequoia TRM3 Green")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 2')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq2v3green)