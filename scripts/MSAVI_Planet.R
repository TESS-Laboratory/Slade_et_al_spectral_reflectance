##Script for calculating SAVI, MSAVI2 and MTVI vegetation indices for Planet Image Data


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
  library(rasterVis)  # raster visualisation  
  library(sp)         # Spatial data processing           
  library(RStoolbox)  # Image analysis
  library(DescTools)

  
}
#----1. Read in shape files-----


study_area_ROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Studyarea")
PLANET_tinel_grid  <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "PLANET_tinel_10m_pixel_grid")

#----2. Read in images--------

#Import Planet banda

Planet <-  stack("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Planet/Planet_2022_02_23_clip.tif")

plot (Planet)
PLANET_BLUE <- Planet$blue
PLANET_GREEN <- Planet$green
PLANET_RED <- Planet$red
PLANET_NIR <- Planet$nir



#----3. Calculate SAVI,MSAVI, MSAVI2, MTVI-----


#----Planet-----


#Planet MSAVI

PLANET_MSAVI = PLANET_NIR + 0.5 - (0.5 * sqrt((2 * PLANET_NIR + 1)^2 - 8 * (PLANET_NIR - (2 * PLANET_RED))))

ggR(PLANET_MSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("PLANET_tinel 2 Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(PLANET_MSAVI,"E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Planet/PLANET_MSAVI.tif",overwrite=TRUE)


#PLANET_tinel NDVI
PLANET_NDVI = (PLANET_NIR - PLANET_RED)/(PLANET_NIR +PLANET_RED)

ggR(PLANET_NDVI, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", 
                       colours = c("red", "yellow", "green", "green4"))+
  ggtitle("PLANET_tinel 2 Normalized Difference Vegetation Index (NDVI)")

writeRaster(PLANET_NDVI,"E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Planet/PLANET_NDVI.tif", overwrite=TRUE)


#PLANET_tinel SAVI

L=0.5
PLANET_SAVI= (1 + L)*(PLANET_NIR - PLANET_RED)/(PLANET_NIR + PLANET_RED + L)

ggR(PLANET_SAVI, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("PLANET_tinel 2 Soil Adjusted Vegetation Index (SAVI)")

writeRaster(PLANET_SAVI,"E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Planet/PLANET_SAVI.tif", overwrite=TRUE)

#MSAVI2

PLANET_msavi2 = (2 * PLANET_NIR + 1 - sqrt( (2 * PLANET_NIR + 1)^2 - 8 * (PLANET_NIR - PLANET_RED) )) / 2 

ggR(PLANET_msavi2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("PLANET_tinel 2 MSAVI2")

writeRaster(PLANET_msavi2,"E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Planet/PLANET_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

PLANET_mtvi = 1.5 * (1.2 * (PLANET_NIR - PLANET_GREEN) - 2.5 * (PLANET_RED - PLANET_GREEN)) /  sqrt( (2 * PLANET_NIR + 1)^2 - (6 * PLANET_NIR - 5 * sqrt(PLANET_RED) - 0.5) )
ggR(PLANET_mtvi, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("PLANET_tinel 2 Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(PLANET_mtvi,"E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Planet/PLANET_MTVI.tif", overwrite=TRUE)

