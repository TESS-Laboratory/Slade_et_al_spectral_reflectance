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

MREBlueSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_SPE_transparent_reflectance_blue.tif")
MREGreenSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_SPE_transparent_reflectance_green.tif")
MRERedSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_SPE_transparent_reflectance_red.tif")
MRERedEdgeSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_SPE_transparent_reflectance_red edge.tif")
MRENIRSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_SPE_transparent_reflectance_NIR.tif")

MRESpectralonReflStack <- stack(MREBlueSpectralonRefl,MREGreenSpectralonRefl,MRERedSpectralonRefl,MRERedEdgeSpectralonRefl,MRENIRSpectralonRefl) 
#MRESpectralonReflStackCrop <- crop(MRESpectralonReflStack,tramwayROI)

#writeRaster(MRESpectralonReflStackcrop,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_SpectralonRefl_stackcrop.tif")
writeRaster(MRESpectralonReflStack,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_SpectralonRefl_stack.tif")


