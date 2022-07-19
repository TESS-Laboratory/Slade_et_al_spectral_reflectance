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

#tramwayROI <- readOGR(dsn = 'D:/New_Mexico/JOR_Tramway_Project', layer = "TramwayROI")


#SEQUOIA stack

#SEQ Spectralon
seqGreenSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_SPE_transparent_reflectance_green.tif")
seqRedSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_SPE_transparent_reflectance_red.tif")
seqRedEdgeSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_SPE_transparent_reflectance_red edge.tif")
seqNIRSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_SPE_transparent_reflectance_NIR.tif")

seqSpectralonReflStack <- stack(seqGreenSpectralonRefl,seqRedSpectralonRefl,seqRedEdgeSpectralonRefl,seqNIRSpectralonRefl) 
#seqSpectralonReflStackCrop <- crop(seqSpectralonReflStack,tramwayROI)

#writeRaster(seqSpectralonReflStackCrop,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_2_SEQ_2lines/Spectralon/ARE_2_SEQ_2lines_SpectralonRefl_stackcrop.tif")
writeRaster(seqSpectralonReflStack,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_SpectralonRefl_stack.tif")

