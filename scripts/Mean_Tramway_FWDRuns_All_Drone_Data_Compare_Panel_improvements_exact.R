###Analyse All Drone data from Are1, TRM1, TRM2, TRM3 and all Tramway passes

#----0. Library-----
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
library(sf)
library(exactextractr)
library(writexl)  
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
tramwayFootprintsShapes <- read_sf(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData/Footprints', layer = "TramwayMeasurementFootprintShapesNew")
tramwayROI <- read_sf(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData', layer = "TramwayROI")

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
#SEQ exact_extract
TRM_1_seqFootprintSpectralonReflectance <- exact_extract(TRM_1_seqSpectralonReflStackCrop,tramwayFootprintsShapes,"mean")
names(TRM_1_seqFootprintSpectralonReflectance) <- c('T1S_green','T1S_red','T1S_redEdge','T1S_NIR')
Main_Footprint_DF <-bind_cols(tramwayFootprintsShapes,TRM_1_seqFootprintSpectralonReflectance)
TRM_1_seqFootprintSpectralonNDVI <- exact_extract(TRM_1_seqSpectralonNDVICrop,tramwayFootprintsShapes,"mean")
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, T1S_NDVI = TRM_1_seqFootprintSpectralonNDVI)


TRM_2_seqFootprintSpectralonReflectance <- exact_extract(TRM_2_seqSpectralonReflStackCrop,tramwayFootprintsShapes,"mean")
names(TRM_2_seqFootprintSpectralonReflectance) <- c('T2S_green','T2S_red','T2S_redEdge','T2S_NIR')
Main_Footprint_DF <-bind_cols(Main_Footprint_DF,TRM_2_seqFootprintSpectralonReflectance)
TRM_2_seqFootprintSpectralonNDVI <- exact_extract(TRM_2_seqSpectralonNDVICrop,tramwayFootprintsShapes,"mean")
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, T2S_NDVI = TRM_2_seqFootprintSpectralonNDVI)

TRM_3_seqFootprintSpectralonReflectance <- exact_extract(TRM_3_seqSpectralonReflStackCrop,tramwayFootprintsShapes,"mean")
names(TRM_3_seqFootprintSpectralonReflectance) <- c('T3S_green','T3S_red','T3S_redEdge','T3S_NIR')
Main_Footprint_DF <-bind_cols(Main_Footprint_DF,TRM_3_seqFootprintSpectralonReflectance)
TRM_3_seqFootprintSpectralonNDVI <- exact_extract(TRM_3_seqSpectralonNDVICrop,tramwayFootprintsShapes,"mean")
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, T3S_NDVI = TRM_3_seqFootprintSpectralonNDVI)


# Average Sequoia reflectance for Footprint

Main_Footprint_DF$TMS_NDVI<- (Main_Footprint_DF$T1S_NDVI+Main_Footprint_DF$T2S_NDVI+Main_Footprint_DF$T3S_NDVI)/3
Main_Footprint_DF$TMS_green<- (Main_Footprint_DF$T1S_green+Main_Footprint_DF$T2S_green+Main_Footprint_DF$T3S_green)/3
Main_Footprint_DF$TMS_red<- (Main_Footprint_DF$T1S_red+Main_Footprint_DF$T2S_red+Main_Footprint_DF$T3S_red)/3
Main_Footprint_DF$TMS_redEdge<- (Main_Footprint_DF$T1S_redEdge+Main_Footprint_DF$T2S_redEdge+Main_Footprint_DF$T3S_redEdge)/3
Main_Footprint_DF$TMS_NIR<- (Main_Footprint_DF$T1S_NIR+Main_Footprint_DF$T2S_NIR+Main_Footprint_DF$T3S_NIR)/3


#MRE exact_extract

TRM_1_MREFootprintSpectralonReflectance <- exact_extract(TRM_1_MRESpectralonReflStackCrop,tramwayFootprintsShapes,"mean")
names(TRM_1_MREFootprintSpectralonReflectance) <- c('T1M_blue','T1M_green','T1M_red','T1M_redEdge','T1M_NIR')
Main_Footprint_DF <-bind_cols(Main_Footprint_DF,TRM_1_MREFootprintSpectralonReflectance)
TRM_1_MREFootprintSpectralonNDVI <- exact_extract(TRM_1_MRESpectralonNDVICrop,tramwayFootprintsShapes,"mean")
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, T1M_NDVI = TRM_1_MREFootprintSpectralonNDVI)


TRM_2_MREFootprintSpectralonReflectance <- exact_extract(TRM_2_MRESpectralonReflStackCrop,tramwayFootprintsShapes,"mean")
names(TRM_2_MREFootprintSpectralonReflectance) <- c('T2M_blue','T2M_green','T2M_red','T2M_redEdge','T2M_NIR')
Main_Footprint_DF <-bind_cols(Main_Footprint_DF,TRM_2_MREFootprintSpectralonReflectance)
TRM_2_MREFootprintSpectralonNDVI <- exact_extract(TRM_2_MRESpectralonNDVICrop,tramwayFootprintsShapes,"mean")
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, T2M_NDVI = TRM_2_MREFootprintSpectralonNDVI)

TRM_3_MREFootprintSpectralonReflectance <- exact_extract(TRM_3_MRESpectralonReflStackCrop,tramwayFootprintsShapes,"mean")
names(TRM_3_MREFootprintSpectralonReflectance) <- c('T3M_blue','T3M_green','T3M_red','T3M_redEdge','T3M_NIR')
Main_Footprint_DF <-bind_cols(Main_Footprint_DF,TRM_3_MREFootprintSpectralonReflectance)
TRM_3_MREFootprintSpectralonNDVI <- exact_extract(TRM_3_MRESpectralonNDVICrop,tramwayFootprintsShapes,"mean")
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, T3M_NDVI = TRM_3_MREFootprintSpectralonNDVI)

#Average MRE Reflectance for Footprint

Main_Footprint_DF$TMM_NDVI<- (Main_Footprint_DF$T1M_NDVI+Main_Footprint_DF$T2M_NDVI+Main_Footprint_DF$T3M_NDVI)/3
Main_Footprint_DF$TMM_blue<- (Main_Footprint_DF$T1M_blue+Main_Footprint_DF$T2M_blue+Main_Footprint_DF$T3M_blue)/3
Main_Footprint_DF$TMM_green<- (Main_Footprint_DF$T1M_green+Main_Footprint_DF$T2M_green+Main_Footprint_DF$T3M_green)/3
Main_Footprint_DF$TMM_red<- (Main_Footprint_DF$T1M_red+Main_Footprint_DF$T2M_red+Main_Footprint_DF$T3M_red)/3
Main_Footprint_DF$TMM_redEdge<- (Main_Footprint_DF$T1M_redEdge+Main_Footprint_DF$T2M_redEdge+Main_Footprint_DF$T3M_redEdge)/3
Main_Footprint_DF$TMM_NIR<- (Main_Footprint_DF$T1M_NIR+Main_Footprint_DF$T2M_NIR+Main_Footprint_DF$T3M_NIR)/3

#----7.5 Append Tramway data to tibble and export------
Main_Footprint_DF <-bind_cols(Main_Footprint_DF,MeanFwdSeqresampdf)
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, MeanFwdSeqresampNDVI = MeanFwdSeqresampNDVI)

Main_Footprint_DF <-bind_cols(Main_Footprint_DF,MeanFwdMREresampdf)
Main_Footprint_DF <- dplyr::mutate(Main_Footprint_DF, MeanFwdMREresampNDVI = MeanFwdMREresampNDVI)

write.csv(Main_Footprint_DF, "E:/Glenn/Tramway_Rcode/output_data/Main_Footprint_DF" )
write_xlsx(Main_Footprint_DF, "E:/Glenn/Tramway_Rcode/output_data/Main_Footprint_DF.xlsx" )

#-----8. Tramway Data Only Plots--------


# Sequoia Surveys reproducibility 
plot(1:110, FirstRunFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$T1S_green,col='black')
lines(1:110, Main_Footprint_DF$T2S_green,col='red')
lines(1:110, Main_Footprint_DF$T3S_green,col='blue')
#lines(1:110, SecondRunBkdSeqresampNDVI,col='green')
##lines(1:110, ThirdRunFwdSeqresampNDVI,col='grey')
#lines(1:110, ThirdRunBkdSeqresampNDVI,col='yellow')
legend(20, 0.4, legend=c("Sequoia TRM_1", "Sequioa TRM_2", "Sequoia TRM_3"),
       lty=c(1,1),col=c('black','red', 'blue'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title(" Sequoia green band")



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

# Plot Tramway Runscomparison of resampling

x <- as.vector(MeanFwdSeqresampNDVI)
y <- as.vector(MeanFwdMREresampNDVI)
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

resamp_compare <- ggplot(df) +
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
  ggtitle("Comparison of NDVI from Tramway Data \n resampled for Sequoia and MRE sensor band widths ")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Measurements resampled for Sequioa \n sensor band widths NDVI')+
  ylab('Tramway Measurements resampled for MRE \n sensor band widths NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(resamp_compare)

ggsave(
  resamp_compare,
  filename = "E:/glenn/Tramway_Rcode/figures/plots/Tramway_data_compare_resampling.png",
  width = 10,
  height =10,
  units = "cm"
)


#-----9. TRM 1 SEQ Drone Survey and Tramway Data Plots Mean Forward Runs --------

#SEQ NDVI VS TRAMWAY NDVI PLOT
plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdSeqresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T1S_NDVI,col='red')
legend(0, 0.4, legend=c("Tramway Spectrometer NDVI (resampled for Sequoia bandwidth)", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("Parrot Sequoia NDVI and Tramway Data Mean NDVI")

#MRE NDVI VS TRAMWAY NDVI PLOT
plot(1:110, MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, MeanFwdMREresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T1M_NDVI, col='blue')
legend(0, 0.45, legend=c("Tramway Spectrometer NDVI (resampled for MRE bandwidth)", "MRE NDVI"),
       lty=c(1,1),col=c('black','blue'),box.lty=0, y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("MicaSense RedEdge NDVI and Tramway Data Mean NDVI")



#TRM 1 SEQ NDVI VS TRM 1 MRE NDVI PLOT
plot(1:110, MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$T1M_NDVI ,col= 'blue')
lines(1:110, Main_Footprint_DF$T1S_NDVI ,col='red')
#lines(1:110, SecondRunFwdSeqresampNDVI,col='green')
legend(20, 0.4, legend=c("MRE NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('blue','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 SEQ Drone Survey and  TRM1 MRE Drone Survey")



# Plot Tramway Mean Fwd Sequoia TRM1 Green Band

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampGreen)
y <- as.vector(Main_Footprint_DF$T1S_green)
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

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRed)
y <- as.vector(Main_Footprint_DF$T1S_red)
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
plot(p1sr)

# Plot Tramway Meant data compared Sequoia TRM1 RedEdge Band

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRedEdge)
y <- as.vector(Main_Footprint_DF$T1S_redEdge)
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
plot (p1sre)

#Plot Tramway Mean Data Compared with Sequoia TRM1 NIR Band

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNIR)
y <- as.vector(Main_Footprint_DF$T1S_NIR)
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
plot(p1sni)

#Plot Tramway Mean Data compared with Sequoia TRM1 NDVI

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNDVI)
y <- as.vector(Main_Footprint_DF$T1S_NDVI)
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
plot(p1snv)
#----10. TRM 1 MRE Drone Survey and Tramway Mean Forward Runs------
{
#MRE NDVI VS TRAMWAY NDVI PLOT (mean fwd)
plot(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T1M_NDVI ,col='red')
legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM1 MRE Drone Survey - Tramway Mean Data")

# Plot Tramway Mean Data compared with MRE TRM1 Blue Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampBlue)
y <- as.vector(Main_Footprint_DF$T1M_blue)
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
plot(p1mb)

# Plot Tramway Mean Data Compared with  MRE TRM1 Green Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampGreen)
y <- as.vector(Main_Footprint_DF$T1M_green)
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
plot(p1mg)

# Plot Tramway Mean Data compared with MRE TRM1 Red Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRed)
y <- as.vector(Main_Footprint_DF$T1M_red)
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
plot(p1mr)

# Plot Tramway Mean Data compared with MRE TRM1 RedEdge Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRedEdge)
y <- as.vector(Main_Footprint_DF$T1M_redEdge)
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
plot(p1mre)

#Plot Tramway Mean Data Compared with MRE TRM1 NIR Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNIR)
y <- as.vector(Main_Footprint_DF$T1M_NIR)
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
plot(p1mni)

#Plot Tramway Mean Data compared with MRE TRM1 NDVI

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNDVI)
y <- as.vector(Main_Footprint_DF$T1M_NDVI)
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
plot(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$T2M_NDVI ,col= 'black')
lines(1:110, Main_Footprint_DF$T2S_NDVI,col='red')
#lines(1:110, SecondRunFwdSeqresampNDVI,col='green')
legend(20, 0.4, legend=c("MRE NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM2 SEQ compared with TRM2 MRE ")

#TRM 2 SEQ Spectralon Mean Forward Tramway
plot(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T2S_NDVI,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM2 Sequioa Drone Survey - Tramway Mean Data")


# Plot Tramway Mean Data compared with Sequoia TRM2 Green Band

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampGreen)
y <- as.vector(Main_Footprint_DF$T2S_green)
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
plot(p2sg)

# Plot Tramway Mean Data compared with Sequoia TRM2 Red Band

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRed)
y <- as.vector(Main_Footprint_DF$T2S_red)
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
plot(p2sr)

# Plot Tramway Mean Data compared with Sequoia TRM2 RedEdge Band

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRedEdge)
y <- as.vector(Main_Footprint_DF$T2S_redEdge)
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
plot(p2sre)

#Plot Tramway Mean Data compared with Sequoia TRM2 NIR Band

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNIR)
y <- as.vector(Main_Footprint_DF$T2S_NIR)
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
plot(p2sni)

#Plot Tramway Mean Data with Sequoia TRM2 NDVI

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNDVI)
y <- as.vector(Main_Footprint_DF$T2S_NDVI)
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
plot(p2snv)
}

#-----12. TRM 2 MRE survey and Tramway Mean Data plots-----
{
#TRM 2 MRE Spectralon vs Mean Tramway Data
plot(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T2M_NDVI,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM2 MRE Compared with Tramway Mean Data")

# Plot Tramway Mean Data MRE TRM2 Blue Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampBlue)
y <- as.vector(Main_Footprint_DF$T2M_blue)
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
plot(p2mb)

# Plot Tramway Mean Data vs MRE TRM2 Green Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampGreen)
y <- as.vector(Main_Footprint_DF$T2M_green)
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
plot(p2mg)

# Plot Tramway Mean Data vs MRE TRM2 Red Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRed)
y <- as.vector(Main_Footprint_DF$T2M_red)
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
plot(p2mr)

# Plot Tramway Mean Data vs MRE TRM2 RedEdge Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRedEdge)
y <- as.vector(Main_Footprint_DF$T2M_redEdge)
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
plot(p2mre)

#Plot Tramway Mean Data vs MRE TRM2 NIR Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNIR)
y <- as.vector(Main_Footprint_DF$T2M_NIR)
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
plot(p2mni)

#Plot Tramway Mean Data vs MRE TRM2 NDVI

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNDVI)
y <- as.vector(Main_Footprint_DF$T2M_NDVI)
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
  plot(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
  lines(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,col='black')
  lines(1:110, Main_Footprint_DF$T3S_NDVI,col='red')
  legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
         lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
  title("TRM3 Sequioa Drone Survey - Tramway Mean Data")
  
  
  # Plot Tramway Mean data vs Sequoia TRM3 Green Band
  
  x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampGreen)
  y <- as.vector(Main_Footprint_DF$T3S_green)
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
  plot(p3sg)
  
  # Plot Tramway Mean data vs Sequoia TRM3 Red Band
  
  x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRed)
  y <- as.vector(Main_Footprint_DF$T3S_red)
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
  plot(p3sr)
  
  # Plot Tramway Mean Data vs Sequoia TRM3 RedEdge Band
  
  x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRedEdge)
  y <- as.vector(Main_Footprint_DF$T3S_redEdge)
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
  plot(p3sre)
  
  #Plot Tramway Mean data vs Sequoia TRM3 NIR Band
  
  x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNIR)
  y <- as.vector(Main_Footprint_DF$T3S_NIR)
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
  plot(p3sni)
  
  #Plot Tramway Mean Data vs Sequoia TRM3 NDVI
  
  x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNDVI)
  y <- as.vector(Main_Footprint_DF$T3S_NDVI)
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
plot(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T3M_NDVI,col='red')
legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("TRM3 MRE Spectralon - Tramway Mean Data")

# Plot Tramway Mean data MRE TRM3 Blue Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampBlue)
y <- as.vector(Main_Footprint_DF$T3M_blue)
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
plot(p3mb)

# Plot Tramway Mean data vs MRE TRM3 Green Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampGreen)
y <- as.vector(Main_Footprint_DF$T3M_green)
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
plot(p3mg)

ggsave(
  p3mg,
  filename = "figures/plots/test.png",
  width = 6,
  height = 6,
  units = "cm"
)

# Plot Tramway Mean Data vs MRE TRM3 Red Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRed)
y <- as.vector(Main_Footprint_DF$T3M_red)
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
plot(p3mr)

# Plot Tramway Mean data vs MRE TRM3 RedEdge Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRedEdge)
y <- as.vector(Main_Footprint_DF$T3M_redEdge)
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
plot(p3mre)

#Plot Tramway Mean data vs MRE TRM3 NIR Band

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNIR)
y <- as.vector(Main_Footprint_DF$T3M_NIR)
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
plot(p3mni)

#Plot Tramway Mean data vs MRE TRM3 NDVI

x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNDVI)
y <- as.vector(Main_Footprint_DF$T3M_NDVI)
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


# Following section has been commented out as decision was taken to only compare tramway focused drone surveys TRM1,2 and 3
# and not include the Area wide survey in the comparison with tramway data as it is not at the same spatial resolution
# and was included in the study for the purposes of comparison with Sentinel-2 data not tramway hyperspectral data.
# This commented section has not been updated with the exactextract function - if you want to run this code for the area use
# the /scripts/Mean_Tramway_FWDRuns_All_Drone_Data_Compare_Panel_improvements.R script

{
# #----15. ARE 1 Sequoia Drone Survey and Tramway Mean Data plots -----
# #ARE 1 SEQ vs Mean Data
# {
# plot(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
# lines(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,col='black')
# lines(1:110, ARE_1_seqFootprintSpectralonNDVI$ARE_1_SEQ_2lines_SPE_index_ndvi,col='red')
# legend(20, 0.4, legend=c("Tramway Spectrometer NDVI", "Sequoia NDVI"),
#        lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
# title("ARE1 Sequoia Drone Survey - Tramway Mean Data")
# 
# 
# # Plot Tramway Mean Data vs Sequoia ARE1 Green Band
# 
# x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampGreen)
# y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$green)
# df <- data.frame(x = x, y = y,
#                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# # Calculate Total Least Squares Regression (extracted from base-R PCA function)
# pca <- prcomp(~x+y,df)
# tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
# tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
# equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
# 
# # Compute the Lin's  correlation concordance coefficient
# ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
# ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
# 
# 
# MADval <- mean(abs(x-y))
# MADrel <- MADval/mean(x)*100
# lmres <- lm(y~x)
# r2val <- summary(lmres)$r.squared
# 
# pasg <- ggplot(df) +
#   geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#   geom_point(aes(x, y), alpha=0.3, size = 1) +
#   geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#   geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#   #theme(text = element_text(size=20))+
#   scale_color_identity() +
#   theme_fancy() +
#   
#   geom_abline(intercept = 0, slope = 1, col='grey' ) +
#   ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey ARE1 Green Band")+
#   #theme(aspect.ratio=1)+
#   xlab('Tramway Relectance resampled for Sequoia Green band')+
#   ylab('Reflectance Sequioa Green Band')+
#   #coord_equal(ratio=1)
#   coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
# #plot(p)
# 
# # Plot Tramway Mean data vs Sequoia ARE1 Red Band
# 
# x <- as.vector(MeanFwdSeqresampRed)
# y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$red)
# df <- data.frame(x = x, y = y,
#                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# # Calculate Total Least Squares Regression (extracted from base-R PCA function)
# pca <- prcomp(~x+y,df)
# tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
# tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
# equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
# 
# # Compute the Lin's  correlation concordance coefficient
# ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
# ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
# 
# 
# MADval <- mean(abs(x-y))
# MADrel <- MADval/mean(x)*100
# lmres <- lm(y~x)
# r2val <- summary(lmres)$r.squared
# 
# pasr <- ggplot(df) +
#   geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#   geom_point(aes(x, y), alpha=0.3, size = 1) +
#   geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#   geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#   #theme(text = element_text(size=20))+
#   scale_color_identity() +
#   theme_fancy() +
#   
#   geom_abline(intercept = 0, slope = 1, col='grey' ) +
#   ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n ARE1 Red Band")+
#   #theme(aspect.ratio=1)+
#   xlab('Tramway Relectance resampled for Sequoia Red band')+
#   ylab('Reflectance Sequioa Red Band')+
#   #coord_equal(ratio=1)
#   coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
# #plot(p)
# 
# # Plot Tramway Run 1 Bkd Sequoia ARE1 RedEdge Band
# 
# x <- as.vector(MeanFwdSeqresampRedEdge)
# y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$redEdge)
# df <- data.frame(x = x, y = y,
#                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# # Calculate Total Least Squares Regression (extracted from base-R PCA function)
# pca <- prcomp(~x+y,df)
# tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
# tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
# equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
# 
# # Compute the Lin's  correlation concordance coefficient
# ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
# ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
# 
# 
# MADval <- mean(abs(x-y))
# MADrel <- MADval/mean(x)*100
# lmres <- lm(y~x)
# r2val <- summary(lmres)$r.squared
# 
# pasre <- ggplot(df) +
#   geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#   geom_point(aes(x, y), alpha=0.3, size = 1) +
#   geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#   geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#   #theme(text = element_text(size=20))+
#   scale_color_identity() +
#   theme_fancy() +
#   
#   geom_abline(intercept = 0, slope = 1, col='grey' ) +
#   ggtitle("Comparison of Tramway Mean Data with Sequoia Survey \n ARE1 RedEdge Band")+
#   #theme(aspect.ratio=1)+
#   xlab('Tramway Relectance resampled for Sequoia RedEdge band')+
#   ylab('Reflectance Sequioa RedEdge Band')+
#   #coord_equal(ratio=1)
#   coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
# #plot(p)
# 
# #Plot Tramway Mean data vs Sequoia ARE1 NIR Band
# 
# x <- as.vector(MeanFwdSeqresampNIR)
# y <- as.vector(ARE_1_seqFootprintSpectralonReflectance$NIR)
# df <- data.frame(x = x, y = y,
#                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# # Calculate Total Least Squares Regression (extracted from base-R PCA function)
# pca <- prcomp(~x+y,df)
# tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
# tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
# equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
# 
# # Compute the Lin's  correlation concordance coefficient
# ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
# ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
# 
# 
# MADval <- mean(abs(x-y))
# MADrel <- MADval/mean(x)*100
# lmres <- lm(y~x)
# r2val <- summary(lmres)$r.squared
# 
# pasni <- ggplot(df) +
#   geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#   geom_point(aes(x, y), alpha=0.3, size = 1) +
#   geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#   geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#   #theme(text = element_text(size=20))+
#   scale_color_identity() +
#   theme_fancy() +
#   
#   geom_abline(intercept = 0, slope = 1, col='grey' ) +
#   ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey ARE1 NIR Band")+
#   #theme(aspect.ratio=1)+
#   xlab('Tramway Relectance resampled for Sequoia NIR band')+
#   ylab('Reflectance Sequioa NIR Band')+
#   #coord_equal(ratio=1)
#   coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
# #plot(p)
# 
# #Plot Tramway Mean Data vs Sequoia ARE1 NDVI
# 
# x <- as.vector(MeanFwdSeqresampNDVI)
# y <- as.vector(ARE_1_seqFootprintSpectralonNDVI$ARE_1_SEQ_2lines_SPE_index_ndvi)
# df <- data.frame(x = x, y = y,
#                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# # Calculate Total Least Squares Regression (extracted from base-R PCA function)
# pca <- prcomp(~x+y,df)
# tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
# tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
# equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
# 
# # Compute the Lin's  correlation concordance coefficient
# ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
# ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
# 
# 
# MADval <- mean(abs(x-y))
# MADrel <- MADval/mean(x)*100
# lmres <- lm(y~x)
# r2val <- summary(lmres)$r.squared
# 
# pasnv <- ggplot(df) +
#   geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#   geom_point(aes(x, y), alpha=0.3, size = 1) +
#   geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#   geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#   #theme(text = element_text(size=20))+
#   scale_color_identity() +
#   theme_fancy() +
#   
#   geom_abline(intercept = 0, slope = 1,col='grey' ) +
#   ggtitle("Comparison of Tramway Mean data \n with Sequoia Survey ARE1 NDVI")+
#   #theme(aspect.ratio=1)+
#   xlab('Tramway Relectance resampled for Sequoia NDVI')+
#   ylab('Sequoia NDVI')+
#   #coord_equal(ratio=1)
#   coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
# plot(pasnv)
# }
# 
# #----16. ARE 1 MRE Drone Survey and Tramway Mean Data plots -----
# {
#   
#   plot(1:110, MeanFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
#   lines(1:110, MeanFwdMREresampNDVI,col='black')
#   lines(1:110, ARE_1_MREFootprintNDVI$ARE_1_MRE_2lines_SPE_index_ndvi ,col='red')
#   legend(20, 0.6, legend=c("Tramway Spectrometer NDVI", "MRE NDVI"),
#          lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
#   title("ARE1 MRE Spectralon - Tramway Mean Data")
#   
#   # Plot Tramway Run 1 Bkd MRE ARE1 Blue Band
#   
#   x <- as.vector(MeanFwdMREresampBlue)
#   y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$blue)
#   df <- data.frame(x = x, y = y,
#                    d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
#   # Calculate Total Least Squares Regression (extracted from base-R PCA function)
#   pca <- prcomp(~x+y,df)
#   tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
#   tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
#   equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
#   
#   # Compute the Lin's  correlation concordance coefficient
#   ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#   ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
#   
#   
#   MADval <- mean(abs(x-y))
#   MADrel <- MADval/mean(x)*100
#   lmres <- lm(y~x)
#   r2val <- summary(lmres)$r.squared
#   
#   pamb <- ggplot(df) +
#     geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#     geom_point(aes(x, y), alpha=0.3, size = 1) +
#     geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#     geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#     #theme(text = element_text(size=20))+
#     scale_color_identity() +
#     theme_fancy() +
#     
#     geom_abline(intercept = 0, slope = 1, col='grey' ) +
#     ggtitle("Comparison of Tramway Mean Data with MRE \n Survey ARE1 Blue Band")+
#     #theme(aspect.ratio=1)+
#     xlab('Tramway Relectance resampled for MRE Blue band')+
#     ylab('Reflectance MRE Blue Band')+
#     #coord_equal(ratio=1)
#     coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#   #plot(p)
#   
#   # Plot Tramway Mean Data vs MRE ARE1 Green Band
#   
#   x <- as.vector(MeanFwdMREresampGreen)
#   y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$green)
#   df <- data.frame(x = x, y = y,
#                    d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
#   # Calculate Total Least Squares Regression (extracted from base-R PCA function)
#   pca <- prcomp(~x+y,df)
#   tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
#   tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
#   equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
#   
#   # Compute the Lin's  correlation concordance coefficient
#   ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#   ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
#   
#   
#   MADval <- mean(abs(x-y))
#   MADrel <- MADval/mean(x)*100
#   lmres <- lm(y~x)
#   r2val <- summary(lmres)$r.squared
#   
#   pamg <- ggplot(df) +
#     geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#     geom_point(aes(x, y), alpha=0.3, size = 1) +
#     geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#     geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#     #theme(text = element_text(size=20))+
#     scale_color_identity() +
#     theme_fancy() +
#     
#     geom_abline(intercept = 0, slope = 1, col='grey' ) +
#     ggtitle("Comparison of Tramway Mean Data with MRE \n Survey ARE1 Green Band")+
#     #theme(aspect.ratio=1)+
#     xlab('Tramway Relectance resampled for MRE Green band')+
#     ylab('Reflectance MRE Green Band')+
#     #coord_equal(ratio=1)
#     coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#   #plot(p)
#   
#   # Plot Tramway Mean data vs MRE ARE1 Red Band
#   
#   x <- as.vector(MeanFwdMREresampRed)
#   y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$red)
#   df <- data.frame(x = x, y = y,
#                    d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
#   # Calculate Total Least Squares Regression (extracted from base-R PCA function)
#   pca <- prcomp(~x+y,df)
#   tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
#   tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
#   equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
#   
#   # Compute the Lin's  correlation concordance coefficient
#   ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#   ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
#   
#   
#   MADval <- mean(abs(x-y))
#   MADrel <- MADval/mean(x)*100
#   lmres <- lm(y~x)
#   r2val <- summary(lmres)$r.squared
#   
#   pamr <- ggplot(df) +
#     geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#     geom_point(aes(x, y), alpha=0.3, size = 1) +
#     geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#     geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#     #theme(text = element_text(size=20))+
#     scale_color_identity() +
#     theme_fancy() +
#     
#     geom_abline(intercept = 0, slope = 1, col='grey' ) +
#     ggtitle("Comparison of Tramway Mean Data with MRE Survey \n ARE1 Red Band")+
#     #theme(aspect.ratio=1)+
#     xlab('Tramway Relectance resampled for MRE Red band')+
#     ylab('Reflectance MRE Red Band')+
#     #coord_equal(ratio=1)
#     coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#   #plot(p)
#   
#   # Plot Tramway Mean Data vs MRE ARE1 RedEdge Band
#   
#   x <- as.vector(MeanFwdMREresampRedEdge)
#   y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$redEdge)
#   df <- data.frame(x = x, y = y,
#                    d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
#   # Calculate Total Least Squares Regression (extracted from base-R PCA function)
#   pca <- prcomp(~x+y,df)
#   tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
#   tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
#   equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
#   
#   # Compute the Lin's  correlation concordance coefficient
#   ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#   ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
#   
#   
#   MADval <- mean(abs(x-y))
#   MADrel <- MADval/mean(x)*100
#   lmres <- lm(y~x)
#   r2val <- summary(lmres)$r.squared
#   
#   pamre <- ggplot(df) +
#     geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#     geom_point(aes(x, y), alpha=0.3, size = 1) +
#     geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#     geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#     #theme(text = element_text(size=20))+
#     scale_color_identity() +
#     theme_fancy() +
#     
#     geom_abline(intercept = 0, slope = 1, col='grey' ) +
#     ggtitle("Comparison of Tramway Mean Data with MRE Survey \n ARE1 RedEdge Band")+
#     #theme(aspect.ratio=1)+
#     xlab('Tramway Relectance resampled for MRE RedEdge band')+
#     ylab('Reflectance MRE RedEdge Band')+
#     #coord_equal(ratio=1)
#     coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#   #plot(p)
#   
#   #Plot Tramway Mean Data vs MRE ARE1 NIR Band
#   
#   x <- as.vector(MeanFwdMREresampNIR)
#   y <- as.vector(ARE_1_MREFootprintSpectralonReflectance$NIR)
#   df <- data.frame(x = x, y = y,
#                    d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
#   # Calculate Total Least Squares Regression (extracted from base-R PCA function)
#   pca <- prcomp(~x+y,df)
#   tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
#   tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
#   equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
#   
#   # Compute the Lin's  correlation concordance coefficient
#   ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#   ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
#   
#   
#   MADval <- mean(abs(x-y))
#   MADrel <- MADval/mean(x)*100
#   lmres <- lm(y~x)
#   r2val <- summary(lmres)$r.squared
#   
#   pamni <- ggplot(df) +
#     geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#     geom_point(aes(x, y), alpha=0.3, size = 1) +
#     geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#     geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#     #theme(text = element_text(size=20))+
#     scale_color_identity() +
#     theme_fancy() +
#     
#     geom_abline(intercept = 0, slope = 1, col='grey' ) +
#     ggtitle("Comparison of Tramway Mean Data with MRE \n Survey ARE1 NIR Band")+
#     #theme(aspect.ratio=1)+
#     xlab('Tramway Relectance resampled for MRE NIR band')+
#     ylab('Reflectance MRE NIR Band')+
#     #coord_equal(ratio=1)
#     coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#   #plot(p)
#   
#   #Plot Tramway Mean data vs  MRE ARE1 NDVI
#   
#   x <- as.vector(MeanFwdMREresampNDVI)
#   y <- as.vector(ARE_1_MREFootprintNDVI$ARE_1_MRE_2lines_SPE_index_ndvi)
#   df <- data.frame(x = x, y = y,
#                    d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
#   # Calculate Total Least Squares Regression (extracted from base-R PCA function)
#   pca <- prcomp(~x+y,df)
#   tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
#   tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
#   equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
#   
#   # Compute the Lin's  correlation concordance coefficient
#   ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#   ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
#   
#   
#   MADval <- mean(abs(x-y))
#   MADrel <- MADval/mean(x)*100
#   lmres <- lm(y~x)
#   r2val <- summary(lmres)$r.squared
#   
#   pamnv <- ggplot(df) +
#     geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
#     geom_point(aes(x, y), alpha=0.3, size = 1) +
#     geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#     geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#     geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
#     #theme(text = element_text(size=20))+
#     scale_color_identity() +
#     theme_fancy() +
#     
#     geom_abline(intercept = 0, slope = 1,col='grey' ) +
#     ggtitle("Comparison of Tramway Mean Data \n with MRE Survey ARE1 NDVI")+
#     #theme(aspect.ratio=1)+
#     xlab('Tramway Relectance resampled for MRE NDVI')+
#     ylab('MRE NDVI')+
#     #coord_equal(ratio=1)
#     coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
#   plot(pamnv)
# 

}

#----17. Panel arrangement of Plots-----
PlotTRM1_SEQ <- grid.arrange(p1sg, p1sr, p1sre, p1sni,p1snv, nrow = 3)#Plots of TRM1 Sequoia survey
PlotTRM1_MRE <-grid.arrange(p1mb,p1mg, p1mr, p1mre, p1mni,p1mnv, nrow = 3)#Plots of TRM1 MRE Survey
PlotTRM2_SEQ <-grid.arrange(p2sg, p2sr, p2sre, p2sni,p2snv, nrow = 3)#Plots of TRM2 Sequoia survey
PlotTRM2_MRE <-grid.arrange(p2mb,p2mg, p2mr, p2mre, p2mni,p2mnv, nrow = 3)#Plots of TRM2 MRE Survey
PlotTRM3_SEQ <- grid.arrange(p3sg, p3sr, p3sre, p3sni,p3snv, nrow = 3)#Plots of TRM3 Sequoia survey
PlotTRM3_MRE <-grid.arrange(p3mb,p3mg, p3mr, p3mre, p3mni,p3mnv, nrow = 3)#Plots of TRM3 MRE Survey
#PlotARE1_SEQ <-grid.arrange(pasg, pasr, pasre, pasni,pasnv, nrow = 3)#Plots of ARE1 Sequoia survey
#PlotARE1_MRE <-grid.arrange(pamb,pamg, pamr, pamre, pamni,pamnv, nrow = 3)#Plots of ARE1 MRE Survey
PlotNDVI <-grid.arrange(p1snv,p1mnv,p2snv, p2mnv, p3snv,p3mnv, nrow = 3)#Plots of NDVI all Surveys
PlotGreen<-grid.arrange(p1sg,p1mg,p2sg, p2mg, p3sg,p3mg,nrow = 3)#Plots of Green Band all Surveys
PlotRed<-grid.arrange(p1sr,p1mr,p2sr, p2mr, p3sr,p3mr, nrow = 3)#Plots of Red Band all Surveys
PlotRedEdge<-grid.arrange(p1sre,p1mre,p2sre, p2mre, p3sre,p3mre, nrow = 3)#Plots of RedEdge Band all Surveys
PlotNIR<-grid.arrange(p1sni,p1mni,p2sni, p2mni, p3sni,p3mni, nrow = 3)#Plots of NIR Band all Surveys
PlotBlue<-grid.arrange(p1mb,p2mb,p3mb, nrow = 2)#Plots of Blue Band all Surveys
# GGSAVE above plots
{
  ggsave(
    PlotTRM1_SEQ,
    filename = "figures/plots/PlotTRM1_SEQ.png",
    width = 16,
    height = 25,
    units = "cm"
  )  
  ggsave(
    PlotTRM1_MRE,
    filename = "figures/plots/PlotTRM1_MRE.png",
    width = 16,
    height = 25,
    units = "cm"
  )  
  ggsave(
    PlotTRM2_SEQ,
    filename = "figures/plots/PlotTRM2_SEQ.png",
    width = 16,
    height = 25,
    units = "cm"
  )
  ggsave(
    PlotTRM2_MRE,
    filename = "figures/plots/PlotTRM2_MRE.png",
    width = 6,
    height = 6,
    units = "cm"
  )
  ggsave(
    PlotTRM3_SEQ,
    filename = "figures/plots/PlotTRM3_SEQ.png",
    width = 16,
    height = 25,
    units = "cm"
  )
  ggsave(
    PlotNDVI,
    filename = "figures/plots/PlotNDVI_ALL_Surveys.png",
    width = 16,
    height = 25,
    units = "cm"
  )
  ggsave(
    PlotTRM3_MRE,
    filename = "figures/plots/PlotTRM3_MRE.png",
    width = 16,
    height = 25,
    units = "cm"
  )
  ggsave(
    PlotGreen,
    filename = "figures/plots/PlotGreen_ALL_Surveys.png",
    width = 6,
    height = 6,
    units = "cm"
  )
  ggsave(
    PlotRed,
    filename = "figures/plots/PlotRed_ALL_Surveys.png",
    width = 16,
    height = 25,
    units = "cm"
  )
  ggsave(
    PlotRedEdge,
    filename = "figures/plots/PlotredEdge_ALL_Surveys.png",
    width = 16,
    height = 25,
    units = "cm"
  ) 
  ggsave(
    PlotNIR,
    filename = "figures/plots/PlotNIR_ALL_Surveys.png",
    width = 6,
    height = 6,
    units = "cm"
  )
  ggsave(
    PlotBlue,
    filename = "figures/plots/PlotBlue_ALL_Surveys.png",
    width = 16,
    height = 25,
    units = "cm"
  )
}

plot(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T1M_NDVI ,col='red')
lines(1:110, Main_Footprint_DF$T2M_NDVI ,col='green')
lines(1:110, Main_Footprint_DF$T3M_NDVI ,col='blue')
#lines(1:110, ARE_1_MREFootprintNDVI$ARE_1_MRE_2lines_SPE_index_ndvi ,col='orange')

legend(20, 0.6, legend=c("Tramway Mean Data NDVI", "TRM_1 NDVI", "TRM_2 NDVI","TRM_3 NDVI"),
       lty=c(1,1),col=c('black','red', 'green','blue'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("NDVI MRE Surveys")

plot(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,ylim=c(0,0.6),type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$T1S_NDVI ,col='red')
lines(1:110, Main_Footprint_DF$T2S_NDVI ,col='green')
lines(1:110, Main_Footprint_DF$T3S_NDVI ,col='blue')
#lines(1:110, ARE_1_seqFootprintSpectralonNDVI$ARE_1_SEQ_2lines_SPE_index_ndvi ,col='orange')

legend(20, 0.6, legend=c("Tramway Mean Data NDVI", "TRM_1 NDVI", "TRM_2 NDVI","TRM_3 NDVI"),
       lty=c(1,1),col=c('black','red', 'green','blue'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
title("NDVI Sequoia Surveys")

#-----18. Save Plots------

ggsave(
  PlotNDVI,
  # filename = "/plots/test.pdf",
  filename = "figures/plots/NDVI_all_surveys_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotBlue,
  # filename = "/plots/test.pdf",
  filename = "figures/plots/Blue_all_surveys_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotGreen,
  # filename = "/plots/test.pdf",
  filename = "figures/plots/Green_all_surveys_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotRed,
  # filename = "/plots/test.pdf",
  filename = "figures/plots/Red_all_surveys_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  PlotRedEdge,
  # filename = "/plots/test.pdf",
  filename = "figures/plots/RedEdge_all_surveys_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  PlotNIR,
  # filename = "/plots/test.pdf",
  filename = "figures/plots/NIR_all_surveys_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  PlotTRM1_SEQ,
  filename = "figures/plots/TRM1_SEQ_All bands_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM2_SEQ,
  filename = "figures/plots/TRM2_SEQ_All bands_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM3_SEQ,
  filename = "figures/plots/TRM3_SEQ_All bands_M.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM1_MRE,
  filename = "figures/plots/TRM1_MRE_All bands_M.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM2_MRE,
  filename = "figures/plots/TRM2_MRE_All bands_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotTRM3_MRE,
  filename = "figures/plots/TRM3_MRE_All bands_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

#----19. SEQ Vs MRE TRM1 Survey----
# Plot SEQ TRM1 vs MRE TRM1 Green
{
x <- as.vector(Main_Footprint_DF$T1S_green)
y <- as.vector(Main_Footprint_DF$T1M_green)
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

}
# Plot SEQ TRM1 vs MRE TRM1 RedEdge
{
  x <- as.vector(Main_Footprint_DF$T1S_redEdge)
  y <- as.vector(Main_Footprint_DF$T1M_redEdge)
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
  x <- as.vector(Main_Footprint_DF$T1S_NIR)
  y <- as.vector(Main_Footprint_DF$T1M_NIR)
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
  x <- as.vector(Main_Footprint_DF$T1S_NDVI)
  y <- as.vector(Main_Footprint_DF$T1M_NDVI)
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
  filename = "figures/plots/TRM1_SEQvsMRE_M_SRF.pdf",
  width = 16,
  height = 25,
  units = "cm"
)


-# ----20 Mean MRE reflectance vs Mean Tramway Data-----

#MRE Blue
x <- as.vector(Main_Footprint_DF$MeanFwdMREresampBlue)
y <- as.vector(Main_Footprint_DF$TMM_blue)
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
x <- as.vector(Main_Footprint_DF$MeanFwdMREresampGreen)
y <- as.vector(Main_Footprint_DF$TMM_green)
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
x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRed)
y <- as.vector(Main_Footprint_DF$TMM_red)
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
x <- as.vector(Main_Footprint_DF$MeanFwdMREresampRedEdge)
y <- as.vector(Main_Footprint_DF$TMM_redEdge)
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
x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNIR)
y <- as.vector(Main_Footprint_DF$TMM_NIR)
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
x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNDVI)
y <- as.vector(Main_Footprint_DF$TMM_NDVI)
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
  filename = "figures/plots/MRE_Mean_NDVI.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

#----21 Mean Sequoia Reflectance vs Mean Tramway data------

#SEq Green

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampGreen)
y <- as.vector(Main_Footprint_DF$TMS_green)
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

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRed)
y <- as.vector(Main_Footprint_DF$TMS_red)
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

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampRedEdge)
y <- as.vector(Main_Footprint_DF$TMS_redEdge)
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

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNIR)
y <- as.vector(Main_Footprint_DF$TMS_NIR)
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

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNDVI)
y <- as.vector(Main_Footprint_DF$TMS_NDVI)
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
  ylab('Reflectance Sequoia NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMSVI)

ggsave(
  pMSVI,
  filename = "figures/plots/SEQ_Mean_NDVI.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

#----22 Panel arrangements of Plots----


PlotMREMEAN <-grid.arrange(pMMB, pMMG, pMMR, pMMRE,pMMN,pMMVI, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotMREMEAN,
  filename = "figures/plots/MRE_Mean_Surveys_all bands.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

PlotSEQMEAN <-grid.arrange(pMSG, pMSR, pMSRE,pMSN,pMSVI, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotSEQMEAN,
  filename = "figures/plots/SEQ_Mean_Surveys_all bands.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

#----23 SEQ vs MRE Mean Data-------

# Plot SEQ Mean vs MRE Mean Green
{
  x <- as.vector(Main_Footprint_DF$TMS_green)
  y <- as.vector(Main_Footprint_DF$TMM_green)
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
  x <- as.vector(Main_Footprint_DF$TMS_red)
  y <- as.vector(Main_Footprint_DF$TMM_red)
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
  x <- as.vector(Main_Footprint_DF$TMS_redEdge)
  y <- as.vector(Main_Footprint_DF$TMM_redEdge)
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
  x <- as.vector(Main_Footprint_DF$TMS_NIR)
  y <- as.vector(Main_Footprint_DF$TMM_NIR)
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
  x <- as.vector(Main_Footprint_DF$TMS_NDVI)
  y <- as.vector(Main_Footprint_DF$TMM_NDVI)
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
  filename = "figures/plots/SEQ_VS_MRE_Mean_Surveys_all bands_V2.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  pNDVI,
  filename = "figures/plots/SEQ_VS_MRE_Mean_Surveys_NDVI.pdf",
  width = 16,
  height = 25,
  units = "cm"
)

#----24 Seq vs Seq Reproducibility ------

#TRM1 SEQ vs TRM2 SEQ Green

x <- as.vector(Main_Footprint_DF$T1S_green)
y <- as.vector(Main_Footprint_DF$T2S_green)
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

x <- as.vector(Main_Footprint_DF$T1S_green)
y <- as.vector(Main_Footprint_DF$T3S_green)
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

#TRM2 SEQ vs TRM3 SEQ Green
x <- as.vector(Main_Footprint_DF$T2S_green)
y <- as.vector(Main_Footprint_DF$T3S_green)
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

#TRM1 SEQ vs TRM2 SEQ red

x <- as.vector(Main_Footprint_DF$T1S_red)
y <- as.vector(Main_Footprint_DF$T2S_red)
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

pseq1v2red <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM2 red")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v2red)


#TRM1 SEQ vs TRM3 SEQ red

x <- as.vector(Main_Footprint_DF$T1S_red)
y <- as.vector(Main_Footprint_DF$T3S_red)
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

pseq1v3red <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM3 red")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v3red)

#TRM2 SEQ vs TRM3 SEQ red
x <- as.vector(Main_Footprint_DF$T2S_red)
y <- as.vector(Main_Footprint_DF$T3S_red)
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

pseq2v3red <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM2 \n  with Sequoia TRM3 red")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 2')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq2v3red)


#TRM1 SEQ vs TRM2 SEQ RedEdge

x <- as.vector(Main_Footprint_DF$T1S_redEdge)
y <- as.vector(Main_Footprint_DF$T2S_redEdge)
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

pseq1v2redEdge <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM2 RedEdge")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v2redEdge)


#TRM1 SEQ vs TRM3 SEQ RedEdge

x <- as.vector(Main_Footprint_DF$T1S_redEdge)
y <- as.vector(Main_Footprint_DF$T3S_redEdge)
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

pseq1v3redEdge <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM3 RedEdge")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v3redEdge)

#TRM2 SEQ vs TRM3 SEQ redEdge
x <- as.vector(Main_Footprint_DF$T2S_redEdge)
y <- as.vector(Main_Footprint_DF$T3S_redEdge)
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

pseq2v3redEdge <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM2 \n  with Sequoia TRM3 RedEdge")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 2')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq2v3redEdge)


#TRM1 SEQ vs TRM2 SEQ NIR

x <- as.vector(Main_Footprint_DF$T1S_NIR)
y <- as.vector(Main_Footprint_DF$T2S_NIR)
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

pseq1v2NIR <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM2 NIR")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v2NIR)


#TRM1 SEQ vs TRM3 SEQ NIR

x <- as.vector(Main_Footprint_DF$T1S_NIR)
y <- as.vector(Main_Footprint_DF$T3S_NIR)
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

pseq1v3NIR <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM1 \n  with Sequoia TRM3 NIR")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 1')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq1v3NIR)

#TRM2 SEQ vs TRM3 SEQ NIR
x <- as.vector(Main_Footprint_DF$T2S_NIR)
y <- as.vector(Main_Footprint_DF$T3S_NIR)
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

pseq2v3NIR <- ggplot(df) +
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
  ggtitle("Comparison of  Sequoia TRM2 \n  with Sequoia TRM3 NIR")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance Sequoia TRM 2')+
  ylab('Reflectance Sequoia TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pseq2v3NIR)

#------panel arrangments and save-------
PlotSEQvSEQgr <-grid.arrange(pseq1v2green, pseq1v2red, pseq2v3green, pseq2v3red,pseq1v3green,pseq1v3red, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotSEQvSEQgr,
  filename = "figures/plots/PlotSEQvSEQgreen_red.png",
  width = 16,
  height = 25,
  units = "cm"
)

PlotSEQvSEQreNIR <-grid.arrange(pseq1v2redEdge, pseq1v2NIR, pseq2v3redEdge, pseq2v3NIR,pseq1v3redEdge,pseq1v3NIR, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotSEQvSEQreNIR,
  filename = "E:/glenn/Tramway_Rcode/figures/plots/PlotSEQvSEQredEdge_NIR.png",
  width = 16,
  height = 25,
  units = "cm"
)

#----25 MRE vs MRE Reproducibility ------

#TRM1 MRE vs TRM2 MRE blue

x <- as.vector(Main_Footprint_DF$T1M_blue)
y <- as.vector(Main_Footprint_DF$T2M_blue)
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

pMRE1v2blue <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM2 blue")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v2blue)


#TRM1 MRE vs TRM3 MRE blue

x <- as.vector(Main_Footprint_DF$T1M_blue)
y <- as.vector(Main_Footprint_DF$T3M_blue)
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

pMRE1v3blue <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM3 blue")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v3blue)

#TRM2 MRE vs TRM3 MRE blue
x <- as.vector(Main_Footprint_DF$T2M_blue)
y <- as.vector(Main_Footprint_DF$T3M_blue)
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

pMRE2v3blue <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM2 \n  with MRE TRM3 blue")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 2')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE2v3blue)


#TRM1 MRE vs TRM2 MRE Green

x <- as.vector(Main_Footprint_DF$T1M_green)
y <- as.vector(Main_Footprint_DF$T2M_green)
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

pMRE1v2green <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM2 Green")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v2green)


#TRM1 MRE vs TRM3 MRE Green

x <- as.vector(Main_Footprint_DF$T1M_green)
y <- as.vector(Main_Footprint_DF$T3M_green)
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

pMRE1v3green <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM3 Green")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v3green)

#TRM2 MRE vs TRM3 MRE Green
x <- as.vector(Main_Footprint_DF$T2M_green)
y <- as.vector(Main_Footprint_DF$T3M_green)
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

pMRE2v3green <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM2 \n  with MRE TRM3 Green")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 2')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE2v3green)

#TRM1 MRE vs TRM2 MRE red

x <- as.vector(Main_Footprint_DF$T1M_red)
y <- as.vector(Main_Footprint_DF$T2M_red)
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

pMRE1v2red <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM2 red")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v2red)


#TRM1 MRE vs TRM3 MRE red

x <- as.vector(Main_Footprint_DF$T1M_red)
y <- as.vector(Main_Footprint_DF$T3M_red)
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

pMRE1v3red <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM3 red")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v3red)

#TRM2 MRE vs TRM3 MRE red
x <- as.vector(Main_Footprint_DF$T2M_red)
y <- as.vector(Main_Footprint_DF$T3M_red)
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

pMRE2v3red <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM2 \n  with MRE TRM3 red")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 2')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE2v3red)


#TRM1 MRE vs TRM2 MRE RedEdge

x <- as.vector(Main_Footprint_DF$T1M_redEdge)
y <- as.vector(Main_Footprint_DF$T2M_redEdge)
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

pMRE1v2redEdge <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM2 RedEdge")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v2redEdge)


#TRM1 MRE vs TRM3 MRE RedEdge

x <- as.vector(Main_Footprint_DF$T1M_redEdge)
y <- as.vector(Main_Footprint_DF$T3M_redEdge)
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

pMRE1v3redEdge <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM3 RedEdge")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v3redEdge)

#TRM2 MRE vs TRM3 MRE redEdge
x <- as.vector(Main_Footprint_DF$T2M_redEdge)
y <- as.vector(Main_Footprint_DF$T3M_redEdge)
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

pMRE2v3redEdge <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM2 \n  with MRE TRM3 RedEdge")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 2')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE2v3redEdge)


#TRM1 MRE vs TRM2 MRE NIR

x <- as.vector(Main_Footprint_DF$T1M_NIR)
y <- as.vector(Main_Footprint_DF$T2M_NIR)
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

pMRE1v2NIR <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM2 NIR")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v2NIR)


#TRM1 MRE vs TRM3 MRE NIR

x <- as.vector(Main_Footprint_DF$T1M_NIR)
y <- as.vector(Main_Footprint_DF$T3M_NIR)
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

pMRE1v3NIR <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM1 \n  with MRE TRM3 NIR")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 1')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE1v3NIR)

#TRM2 MRE vs TRM3 MRE NIR
x <- as.vector(Main_Footprint_DF$T2M_NIR)
y <- as.vector(Main_Footprint_DF$T3M_NIR)
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

pMRE2v3NIR <- ggplot(df) +
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
  ggtitle("Comparison of  MRE TRM2 \n  with MRE TRM3 NIR")+
  #theme(aspect.ratio=1)+
  xlab('Reflectance MRE TRM 2')+
  ylab('Reflectance MRE TRM 3')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMRE2v3NIR)

#------panel arrangments and save-------
PlotMREvMREgr <-grid.arrange(pMRE1v2green, pMRE1v2red, pMRE2v3green, pMRE2v3red,pMRE1v3green,pMRE1v3red, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotMREvMREgr,
  filename = "figures/plots/PlotMREvMREgreen_red.png",
  width = 16,
  height = 25,
  units = "cm"
)

PlotMREvMREreNIR <-grid.arrange(pMRE1v2redEdge, pMRE1v2NIR, pMRE2v3redEdge, pMRE2v3NIR,pMRE1v3redEdge,pMRE1v3NIR, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotMREvMREreNIR,
  filename = "figures/plots/PlotMREvMREredEdge_NIR.png",
  width = 16,
  height = 25,
  units = "cm"
)

PlotMREvMREblue <-grid.arrange(pMRE1v2blue, pMRE2v3blue, pMRE1v3blue, nrow = 3)#Plots of  MRE Surveys Mean Data 

ggsave(
  PlotMREvMREblue,
  filename = "figures/plots/PlotMREvMREblue.png",
  width = 16,
  height = 25,
  units = "cm"
)

#----26. NDVI plot TRM1 SEQ vs MRE for Figure 4----

# Plot SEQ TRM1 vs MRE TRM1 NDVI
{
  x <- as.vector(Main_Footprint_DF$T1S_NDVI)
  y <- as.vector(Main_Footprint_DF$T1M_NDVI)
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
  
  pNDVITRM1b <- ggplot(df) +
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
  plot(pNDVITRM1b)
}

ggsave(
  pNDVITRM1b,
  filename = "figures/plots/SEQ_VS_MRE_TRM1_NDVI.png",
  width = 7,
  height = 7,
  units = "cm"
)
#----27.  SEQ vs MRE Mean NDVI Plot for Figure 5----
# Plot SEQ vs MRE  NDVI
{
  x <- as.vector(Main_Footprint_DF$TMS_NDVI)
  y <- as.vector(Main_Footprint_DF$TMM_NDVI)
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
  
  pNDVIb <- ggplot(df) +
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
  plot(pNDVIb)
}

ggsave(
  pNDVIb,
  filename = "figures/plots/SEQ_VS_MRE_Mean_NDVI.png",
  width = 16,
  height = 16,
  units = "cm"
)
#----28 Combined NDVI plot for figure 6----

## Plotting theme
theme_fancy_2 <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 6, color = "black"),
      axis.title = element_text(size = 6, color = "black"),
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

#MRE NDVI
x <- as.vector(Main_Footprint_DF$MeanFwdMREresampNDVI)
y <- as.vector(Main_Footprint_DF$TMM_NDVI)
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
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,2)),hjust='left',size=1.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=1.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=1.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=1.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy_2() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
# ggtitle("Comparison of Tramway Mean Data with MRE \n Survey Mean Data NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for MRE NDVI band')+
  ylab('Reflectance MRE NDVI Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMMVI)

ggsave(
  pMMVI,
  filename = "E:/glenn/Tramway_Rcode/figures/plots/MRE_Mean_NDVI_small.png",
  width = 6,
  height = 6,
  units = "cm"
)

#SEq NDVI

x <- as.vector(Main_Footprint_DF$MeanFwdSeqresampNDVI)
y <- as.vector(Main_Footprint_DF$TMS_NDVI)
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
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=1.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=1.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=1.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=1.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy_2() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
#  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey Mean Data NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance resampled for Sequoia NDVI')+
  ylab('Reflectance Sequoia NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pMSVI)

#ggsave(
 # pMSVI,
  #filename = "E:/glenn/Tramway_Rcode/figures/plots/SEQ_Mean_NDVI_small.png",
#   width = 6,
#   height = 6,
#   units = "cm"
# )

ggsave(
  pMSVI,
  filename = "figures/plots/SEQ_Mean_NDVI_small.png",
  width = 6,
  height = 6,
  units = "cm"
)

#Tramway length plot

#SEQ NDVI VS TRAMWAY NDVI PLOT
plot(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdSeqresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$TMS_NDVI ,col='red')
legend(0, 0.45, legend=c("Tramway Spectrometer NDVI (resampled for Sequoia bandwidth)", "Mean Sequoia NDVI"),
       lty=c(1,1),col=c('black','red'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
#title("Parrot Sequoia NDVI and Tramway Data Mean NDVI")

#MRE NDVI VS TRAMWAY NDVI PLOT
plot(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,type='n',xlab='[m]',ylab='NDVI')
lines(1:110, Main_Footprint_DF$MeanFwdMREresampNDVI,col='black')
lines(1:110, Main_Footprint_DF$TMM_NDVI , col='blue')
legend(0, 0.45, legend=c("Tramway Spectrometer NDVI (resampled for MRE bandwidth)", "Mean MRE NDVI"),
       lty=c(1,1),col=c('black','blue'),box.lty=0,y.intersp=1,x.intersp=1,bg="transparent",xpd=TRUE)
#title("MicaSense RedEdge NDVI and Tramway Data Mean NDVI")

#PlotcombinedNDVI <-  plot_grid(P2,P3,P13, align = "v", nrow = 1, rel_heights = c(0.33,0.33,0.33))