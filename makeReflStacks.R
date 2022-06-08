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

tramwayROI <- readOGR(dsn = 'D:/New_Mexico/JOR_Tramway_Project', layer = "TramwayROI")


#sequoia stack

#ownPanel
seqGreenOwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ownPanel/TRM_2_SEQ_proj2lines_transparent_reflectance_green.tif")
seqRedOwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ownPanel/TRM_2_SEQ_proj2lines_transparent_reflectance_red.tif")
seqRedEdgeOwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ownPanel/TRM_2_SEQ_proj2lines_transparent_reflectance_red edge.tif")
seqNIROwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ownPanel/TRM_2_SEQ_proj2lines_transparent_reflectance_NIR.tif")

seqOwnPanelReflStack <- stack(seqGreenOwnPanelRefl,seqRedOwnPanelRefl,seqRedEdgeOwnPanelRefl,seqNIROwnPanelRefl) 
seqOwnPanelReflStackCrop <- crop(seqOwnPanelReflStack,tramwayROI)

writeRaster(seqOwnPanelReflStackCrop,"D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/ownPanel/TRM_2_SEQ_proj2lines_ownPanelRefl_stackcrop.tif")

#Spectralon
seqGreenSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_proj2lines_transparent_reflectance_green.tif")
seqRedSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_proj2lines_transparent_reflectance_red.tif")
seqRedEdgeSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_proj2lines_transparent_reflectance_red edge.tif")
seqNIRSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_proj2lines_transparent_reflectance_NIR.tif")

seqSpectralonReflStack <- stack(seqGreenSpectralonRefl,seqRedSpectralonRefl,seqRedEdgeSpectralonRefl,seqNIRSpectralonRefl) 
seqSpectralonReflStackCrop <- crop(seqSpectralonReflStack,tramwayROI)

writeRaster(seqSpectralonReflStackCrop,"D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_SEQ/Spectralon/TRM_2_SEQ_proj2lines_SpectralonRefl_stackcrop.tif")

#RedEdge stack

MREBlueOwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ownPanel/TRM_2_MRE_proj2lines_transparent_reflectance_blue.tif")
MREGreenOwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ownPanel/TRM_2_MRE_proj2lines_transparent_reflectance_green.tif")
MRERedOwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ownPanel/TRM_2_MRE_proj2lines_transparent_reflectance_red.tif")
MRERedEdgeOwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ownPanel/TRM_2_MRE_proj2lines_transparent_reflectance_red edge.tif")
MRENIROwnPanelRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ownPanel/TRM_2_MRE_proj2lines_transparent_reflectance_NIR.tif")

MREOwnPanelReflStack <- stack(MREBlueOwnPanelRefl,MREGreenOwnPanelRefl,MRERedOwnPanelRefl,MRERedEdgeOwnPanelRefl,MRENIROwnPanelRefl) 
MREOwnPanelReflStackCrop <- crop(MREOwnPanelReflStack,tramwayROI)

writeRaster(MREOwnPanelReflStackCrop,"D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/ownPanel/TRM_2_MRE_proj2lines_ownPanelRefl_stackcrop.tif")

#Spectralon

MREBlueSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_blue.tif")
MREGreenSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_green.tif")
MRERedSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_red.tif")
MRERedEdgeSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_red edge.tif")
MRENIRSpectralonRefl <-  raster("D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_NIR.tif")

MRESpectralonReflStack <- stack(MREBlueSpectralonRefl,MREGreenSpectralonRefl,MRERedSpectralonRefl,MRERedEdgeSpectralonRefl,MRENIRSpectralonRefl) 
MRESpectralonReflStackCrop <- crop(MRESpectralonReflStack,tramwayROI)

writeRaster(MRESpectralonReflStackCrop,"D:/New_Mexico/JOR_Tramway_Project/ReflStacks/TRM_2_MRE/Spectralon/TRM_2_MRE_proj2lines_SpectralonRefl_stackcrop.tif")


