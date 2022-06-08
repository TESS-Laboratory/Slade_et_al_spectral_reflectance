#make raster reflectance stacks from Sentinel data

library(rgdal)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(MASS)
library(splines)
library(rgeos)

#Import shape files 

study_area_ROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Studyarea")


#Import Sentinel 2 bands and crop



S2_Blue_data <-  raster("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/Full Scenes/T13SCS_20200223T174321_B02_10m.jp2")
S2_Green_data <- raster("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/Full Scenes/T13SCS_20200223T174321_B03_10m.jp2")
S2_Red_data <-  raster("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/Full Scenes/T13SCS_20200223T174321_B04_10m.jp2")
S2_NIR_data <-  raster("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/Full Scenes/T13SCS_20200223T174321_B08_10m.jp2")

S2_Blue_data_crop <- crop(S2_Blue_data,study_area_ROI)
S2_Green_data_crop <- crop(S2_Green_data,study_area_ROI)
S2_Red_data_crop <- crop(S2_Red_data,study_area_ROI)
S2_NIR_data_crop <- crop(S2_NIR_data,study_area_ROI)


#Sentinel convert to Surface Reflection (0-1) and stack


fun <- function(x) { x / 10000 }
S2_Blue <- calc(S2_Blue_data_crop, fun)
S2_Green <- calc(S2_Green_data_crop, fun)
S2_Red <- calc(S2_Red_data_crop, fun)
S2_NIR <- calc(S2_NIR_data_crop, fun)


S2_2020_02_23_Stack_Reflectance <- stack(S2_Blue,S2_Green,S2_Red,S2_NIR) 
plot(S2_2020_02_23_Stack_Reflectance)

writeRaster(S2_2020_02_23_Stack_Reflectance,"E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/S2_2020_02_23_Stackcrop_Reflectance.tif")

