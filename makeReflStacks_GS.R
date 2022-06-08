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

#SEQ ownPanel
seqGreenOwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ/ownPanel/ARE_1_SEQ_SEQ_CLEAN4_transparent_reflectance_green.tif")
seqRedOwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ/ownPanel/ARE_1_SEQ_SEQ_CLEAN4_transparent_reflectance_red.tif")
seqRedEdgeOwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ/ownPanel/ARE_1_SEQ_SEQ_CLEAN4_transparent_reflectance_red edge.tif")
seqNIROwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ/ownPanel/ARE_1_SEQ_SEQ_CLEAN4_transparent_reflectance_NIR.tif")

seqOwnPanelReflStack <- stack(seqGreenOwnPanelRefl,seqRedOwnPanelRefl,seqRedEdgeOwnPanelRefl,seqNIROwnPanelRefl) 
#seqOwnPanelReflStackCrop <- crop(seqOwnPanelReflStack,tramwayROI)

#writeRaster(seqOwnPanelReflStackCrop,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_ownPanelRefl_stackcrop.tif")
writeRaster(seqOwnPanelReflStack,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_ownPanelRefl_stack.tif")

#SEQ Spectralon
seqGreenSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SPE_transparent_reflectance_green.tif")
seqRedSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SPE_transparent_reflectance_red.tif")
seqRedEdgeSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SPE_transparent_reflectance_red edge.tif")
seqNIRSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SPE_transparent_reflectance_NIR.tif")

seqSpectralonReflStack <- stack(seqGreenSpectralonRefl,seqRedSpectralonRefl,seqRedEdgeSpectralonRefl,seqNIRSpectralonRefl) 
#seqSpectralonReflStackCrop <- crop(seqSpectralonReflStack,tramwayROI)

#writeRaster(seqSpectralonReflStackCrop,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SpectralonRefl_stackcrop.tif")
writeRaster(seqSpectralonReflStack,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SpectralonRefl_stack.tif")

#MRE stack

#MRE OWN PANEL

MREBlueOwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/ownPanel/ARE_1_MRE_MRE_transparent_reflectance_blue.tif")
MREGreenOwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/ownPanel/ARE_1_MRE_MRE_transparent_reflectance_green.tif")
MRERedOwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/ownPanel/ARE_1_MRE_MRE_transparent_reflectance_red.tif")
MRERedEdgeOwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/ownPanel/ARE_1_MRE_MRE_transparent_reflectance_red edge.tif")
MRENIROwnPanelRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/ownPanel/ARE_1_MRE_MRE_transparent_reflectance_NIR.tif")

MREOwnPanelReflStack <- stack(MREBlueOwnPanelRefl,MREGreenOwnPanelRefl,MRERedOwnPanelRefl,MRERedEdgeOwnPanelRefl,MRENIROwnPanelRefl) 
#MREOwnPanelReflStackCrop <- crop(MREOwnPanelReflStack,tramwayROI)

writeRaster(MREOwnPanelReflStack,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/ownPanel/ARE_1_MRE_ownPanelRefl_stack.tif")

#MRE Spectralon

MREBlueSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_blue.tif")
MREGreenSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_green.tif")
MRERedSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_red.tif")
MRERedEdgeSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_red edge.tif")
MRENIRSpectralonRefl <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_NIR.tif")

MRESpectralonReflStack <- stack(MREBlueSpectralonRefl,MREGreenSpectralonRefl,MRERedSpectralonRefl,MRERedEdgeSpectralonRefl,MRENIRSpectralonRefl) 
#MRESpectralonReflStackCrop <- crop(MRESpectralonReflStack,tramwayROI)

#writeRaster(MRESpectralonReflStackcrop,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SpectralonRefl_stackcrop.tif")
writeRaster(MRESpectralonReflStack,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SpectralonRefl_stack.tif")


