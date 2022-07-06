##Script for calculating NDVI,SAVI,MSAVI2,MTVI2 for TRM Surveys for Sequoia and MRE
##Script then calculates an average and plots the data against Tramway data.


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
  library(patchwork) # this is Andy's favorite for multi-panel plots
  library(propagate) # required for predicting confidence intervals on ESD : Biomass plot
  library(ggpmisc) # for adding model parameters to plots
  library(gvlma) # Global Validation of Linear Models Assumptions
  library(polynom)
  library(nlstools)
  library(ggpubr) 
  library(DescTools)
  library(Metrics)
  library(hydroGOF) # The RMSE function in this package allows for na.rm parameter

  
}
#----1. Read in shape files-----

tramwayFootprintsShapes <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData/Footprints', layer = "TramwayMeasurementFootprintShapesNew")
study_area_ROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Studyarea")
Sentinel_grid  <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Sentinel_10m_pixel_grid")

#----2. Read in images--------
#SEQUOIA
RED1 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SPE_transparent_reflectance_red.tif")
NIR1 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SPE_transparent_reflectance_NIR.tif")
Green1 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SPE_transparent_reflectance_green.tif")

RED2 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/TRM_2_SEQ_proj2lines_transparent_reflectance_red.tif")
NIR2 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/TRM_2_SEQ_proj2lines_transparent_reflectance_NIR.tif")
Green2 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/TRM_2_SEQ_proj2lines_transparent_reflectance_green.tif")

RED3 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/TRM_3_SEQ_2lines_SPE_transparent_reflectance_red.tif")
NIR3 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/TRM_3_SEQ_2lines_SPE_transparent_reflectance_NIR.tif")
Green3 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/TRM_3_SEQ_2lines_SPE_transparent_reflectance_green.tif")


#MRE
MRERED1 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SPE_transparent_reflectance_red.tif")
MRENIR1 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SPE_transparent_reflectance_NIR.tif")
MREGreen1 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SPE_transparent_reflectance_green.tif")

MRERED2 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_red.tif")
MRENIR2 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_NIR.tif")
MREGreen2 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/TRM_2_MRE_proj2lines_transparent_reflectance_green.tif")

MRERED3 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/TRM_3_MRE_SPE_2lines_transparent_reflectance_red.tif")
MRENIR3 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/TRM_3_MRE_SPE_2lines_transparent_reflectance_NIR.tif")
MREGreen3 <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/TRM_3_MRE_SPE_2lines_transparent_reflectance_green.tif")


#----3. Calculate Indices-----
#----3.1 Calculate SEQ Indices----
#----Sequoia MSAVI----

#SEQuoia MSAVI Calculation TRM1
SEQMSAVI1 = NIR1 + 0.5 - (0.5 * sqrt((2 * NIR1 + 1)^2 - 8 * (NIR1 - (2 * RED1))))

ggR(SEQMSAVI1, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("TRM1 SEQ Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(SEQMSAVI1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/indices/TRM_1_SEQ_3lines_MSAVI.tif",overwrite=TRUE)

#SEQuoia MSAVI Calculation TRM2
SEQMSAVI2 = NIR2 + 0.5 - (0.5 * sqrt((2 * NIR2 + 1)^2 - 8 * (NIR2 - (2 * RED2))))

ggR(SEQMSAVI2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("TRM2 SEQ Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(SEQMSAVI2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/indices/TRM_2_SEQ_proj2lines_MSAVI.tif",overwrite=TRUE)

#SEQuoia MSAVI Calculation TRM3
SEQMSAVI3 = NIR3 + 0.5 - (0.5 * sqrt((2 * NIR3 + 1)^2 - 8 * (NIR3 - (2 * RED3))))

ggR(SEQMSAVI3, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("TRM3 SEQ Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(SEQMSAVI3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/indices/TRM_3_SEQ_2lines_MSAVI.tif",overwrite=TRUE)


#----Sequoia SAVI Calculation----

#SAVI TRM1
L=0.5
SEQSAVI1= (1 + L)*(NIR1 - RED1)/(NIR1 + RED1 + L)

ggR(SEQSAVI1, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM1 Sequoia Soil Adjusted Vegetation Index (SAVI)")

writeRaster(SEQSAVI1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/indices/TRM_1_SEQ_3lines_SAVI.tif", overwrite=TRUE)

#SAVI TRM2
L=0.5
SEQSAVI2= (1 + L)*(NIR2 - RED2)/(NIR2 + RED2 + L)

ggR(SEQSAVI2, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM2 Sequoia Soil Adjusted Vegetation Index (SAVI)")

writeRaster(SEQSAVI2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/indices/TRM_2_SEQ_proj2lines_SAVI.tif",overwrite=TRUE)


#SAVI TRM3
L=0.5
SEQSAVI3= (1 + L)*(NIR3 - RED3)/(NIR3 + RED3 + L)

ggR(SEQSAVI3, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM3 Sequoia Soil Adjusted Vegetation Index (SAVI)")

writeRaster(SEQSAVI3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/indices/TRM_3_SEQ_2lines_SAVI.tif",overwrite=TRUE)




#----Sequoia MSAVI2 Calculation-----

#msavi2 TRM1 Sequoia
SEQmsavi21 = (2 * NIR1 + 1 - sqrt( (2 * NIR1 + 1)^2 - 8 * (NIR1 - RED1) )) / 2 

ggR(SEQmsavi21, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM1 Sequoia MSAVI2")

writeRaster(SEQmsavi21,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/indices/TRM_1_SEQ_3lines_MSAVI2.tif", overwrite=TRUE)

#msavi2 TRM2 sequoia
SEQmsavi22 = (2 * NIR2 + 1 - sqrt( (2 * NIR2 + 1)^2 - 8 * (NIR2 - RED2) )) / 2 

ggR(SEQmsavi22, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM2 Sequoia MSAVI2")

writeRaster(SEQmsavi22,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/indices/TRM_2_SEQ_proj2lines_MSAVI2.tif",overwrite=TRUE)

#msavi2 TRM3 sequoia
SEQmsavi23 = (2 * NIR3 + 1 - sqrt( (2 * NIR3 + 1)^2 - 8 * (NIR3 - RED3) )) / 2 

ggR(SEQmsavi23, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM3 Sequoia MSAVI2")

writeRaster(SEQmsavi23,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/indices/TRM_3_SEQ_2lines_MSAVI2.tif",overwrite=TRUE)



#----SEQ Modified Triangular Vegetation Index 2 (MTVI) Calculation----
# TRM1 SEQ MTVI
SEQmtvi1 = 1.5 * (1.2 * (NIR1 - Green1) - 2.5 * (RED1 - Green1)) /  sqrt( (2 * NIR1 + 1)^2 - (6 * NIR1 - 5 * sqrt(RED1) - 0.5) )
ggR(SEQmtvi1, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 1 Sequoia Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(SEQmtvi1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/indices/TRM_1_SEQ_3lines_MTVI2.tif", overwrite=TRUE)

# TRM2 SEQ MTVI
SEQmtvi2 = 1.5 * (1.2 * (NIR2 - Green2) - 2.5 * (RED2 - Green2)) /  sqrt( (2 * NIR2 + 1)^2 - (6 * NIR2 - 5 * sqrt(RED2) - 0.5) )
ggR(SEQmtvi2, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 2 Sequoia Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(SEQmtvi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/indices/TRM_2_SEQ_proj2lines_MTVI2.tif",overwrite=TRUE)

# TRM3 SEQ MTVI
SEQmtvi3 = 1.5 * (1.2 * (NIR3 - Green3) - 2.5 * (RED3 - Green3)) /  sqrt( (2 * NIR3 + 1)^2 - (6 * NIR3 - 5 * sqrt(RED3) - 0.5) )
ggR(SEQmtvi3, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 3 Sequoia Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(SEQmtvi3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/indices/TRM_3_SEQ_2lines_MTVI2.tif",overwrite=TRUE)


#----SEQ Normalised Difference Vegetation Index (NDVI) Calculation----

#TRM 1 SEQ NDVI 
SEQndvi1 =  (NIR1 - RED1) / ( NIR1 + RED1)
ggR(SEQndvi1, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM1 Sequoia Normalised Difference Vegetation Index (NDVI)")

writeRaster(SEQndvi1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/indices/TRM_1_SEQ_3lines_NDVI_Rscript.tif", overwrite=TRUE)

#TRM 2 SEQ NDVI 
SEQndvi2 =  (NIR2 - RED2) / ( NIR2 + RED2)
ggR(SEQndvi2, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM2 Sequoia Normalised Difference Vegetation Index (NDVI)")

writeRaster(SEQndvi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_SEQ_2lines/Spectralon/indices/TRM_2_SEQ_proj2lines_NDVI_Rscript.tif",overwrite=TRUE)

#TRM 3 SEQ NDVI 
SEQndvi3 =  (NIR3 - RED3) / ( NIR3 + RED3)
ggR(SEQndvi3, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM3 Sequoia Normalised Difference Vegetation Index (NDVI)")

writeRaster(SEQndvi3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_SEQ_2lines/Spectralon/indices/TRM_3_SEQ_2lines_NDVI_Rscript.tif",overwrite=TRUE)



#--------3.2 Calculate MRE Indices--------

#----MRE MSAVI Calculation----

# TRM1 MRE MSAVI

MREMSAVI1 = MRENIR1 + 0.5 - (0.5 * sqrt((2 * MRENIR1 + 1)^2 - 8 * (MRENIR1 - (2 * MRERED1))))

ggR(MREMSAVI1, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("TRM1 MRE Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(MREMSAVI1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/indices/TRM_1_MRE_2lines_MSAVI.tif",overwrite=TRUE)

# TRM2 MRE MSAVI

MREMSAVI2 = MRENIR2 + 0.5 - (0.5 * sqrt((2 * MRENIR2 + 1)^2 - 8 * (MRENIR2 - (2 * MRERED2))))

ggR(MREMSAVI2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("TRM2 MRE Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(MREMSAVI2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/indices/TRM_2_MRE_2lines_MSAVI.tif",overwrite=TRUE)

# TRM3 MRE MSAVI

MREMSAVI3 = MRENIR3 + 0.5 - (0.5 * sqrt((2 * MRENIR3 + 1)^2 - 8 * (MRENIR3 - (2 * MRERED3))))

ggR(MREMSAVI3, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("TRM3 MRE Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(MREMSAVI3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/indices/TRM_3_MRE_2lines_MSAVI.tif",overwrite=TRUE)


#----MRE SAVI Calculation----

#TRM 1 MRE SAVI
L=0.5
MRESAVI1= (1 + L)*(MRENIR1 - MRERED1)/(MRENIR1 + MRERED1 + L)

ggR(MRESAVI1, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 1 MRE Soil Adjusted Vegetation Index (SAVI)")

writeRaster(MRESAVI1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/indices/TRM_1_MRE_2lines_SAVI.tif", overwrite=TRUE)

#TRM 2 MRE SAVI
L=0.5
MRESAVI2= (1 + L)*(MRENIR2 - MRERED2)/(MRENIR2 + MRERED2 + L)

ggR(MRESAVI2, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 2 MRE Soil Adjusted Vegetation Index (SAVI)")

writeRaster(MRESAVI2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/indices/TRM_2_MRE_2lines_SAVI.tif", overwrite=TRUE)

#TRM 3 MRE SAVI
L=0.5
MRESAVI3= (1 + L)*(MRENIR3 - MRERED3)/(MRENIR3 + MRERED3 + L)

ggR(MRESAVI3, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 3 MRE Soil Adjusted Vegetation Index (SAVI)")

writeRaster(MRESAVI3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/indices/TRM_3_MRE_2lines_SAVI.tif", overwrite=TRUE)



#----MRE MSAVI2 Calculation----
#TRM 1 MRE MSAVI2

MREmsavi21 = (2 * MRENIR1 + 1 - sqrt( (2 * MRENIR1 + 1)^2 - 8 * (MRENIR1 - MRERED1) )) / 2 

ggR(MREmsavi21, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM1 MRE MSAVI2")

writeRaster(MREmsavi21,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/indices/TRM_1_MRE_2lines_MSAVI2.tif", overwrite=TRUE)

#TRM 2 MRE MSAVI2

MREmsavi22 = (2 * MRENIR2 + 1 - sqrt( (2 * MRENIR2 + 1)^2 - 8 * (MRENIR2 - MRERED2) )) / 2 

ggR(MREmsavi22, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM2 MRE MSAVI2")

writeRaster(MREmsavi22,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/indices/TRM_2_MRE_2lines_MSAVI2.tif", overwrite=TRUE)

#TRM 3 MRE MSAVI2

MREmsavi23 = (2 * MRENIR3 + 1 - sqrt( (2 * MRENIR3 + 1)^2 - 8 * (MRENIR3 - MRERED3) )) / 2 

ggR(MREmsavi23, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM3 MRE MSAVI2")

writeRaster(MREmsavi23,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/indices/TRM_3_MRE_2lines_MSAVI2.tif", overwrite=TRUE)


#----MRE Modified Triangular Vegetation Index 2 (MTVI) Calculation----
#TRM 1 MTVI MRE
MREmtvi1 = 1.5 * (1.2 * (MRENIR1 - MREGreen1) - 2.5 * (MRERED1 - MREGreen1)) /  sqrt( (2 * MRENIR1 + 1)^2 - (6 * MRENIR1 - 5 * sqrt(MRERED1) - 0.5) )
ggR(MREmtvi1, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 1 MRE Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(MREmtvi1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/indices/TRM_1_MRE_2lines_MTVI.tif", overwrite=TRUE)

#TRM 2 MTVI MRE
MREmtvi2 = 1.5 * (1.2 * (MRENIR2 - MREGreen2) - 2.5 * (MRERED2 - MREGreen2)) /  sqrt( (2 * MRENIR2 + 1)^2 - (6 * MRENIR2 - 5 * sqrt(MRERED2) - 0.5) )
ggR(MREmtvi2, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 2 MRE Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(MREmtvi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/indices/TRM_2_MRE_2lines_MTVI.tif", overwrite=TRUE)

#TRM 3 MTVI MRE
MREmtvi3 = 1.5 * (1.2 * (MRENIR3 - MREGreen3) - 2.5 * (MRERED3 - MREGreen3)) /  sqrt( (2 * MRENIR3 + 1)^2 - (6 * MRENIR3 - 5 * sqrt(MRERED3) - 0.5) )
ggR(MREmtvi3, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 3 MRE Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(MREmtvi3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/indices/TRM_3_MRE_2lines_MTVI.tif", overwrite=TRUE)


#----MRE Normalised Difference Vegetation Index (NDVI) Calculation----
#TRM 1 MRE NDVI
MREndvi1 = (MRENIR1 - MRERED1) / (MRENIR1 + MRERED1)
ggR(MREndvi1, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 1 MRE Normalised Difference Vegetation Index (NDVI)")

writeRaster(MREndvi1,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/indices/TRM_1_MRE_2lines_NDVI_Rscript.tif", overwrite=TRUE)

#TRM 2 MRE NDVI
MREndvi2 = (MRENIR2 - MRERED2) / (MRENIR2 + MRERED2)
ggR(MREndvi2, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 2 MRE Normalised Difference Vegetation Index (NDVI)")

writeRaster(MREndvi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_2_MRE_2lines/Spectralon/indices/TRM_2_MRE_2lines_NDVI_Rscript.tif", overwrite=TRUE)

#TRM 3 MRE NDVI
MREndvi3 = (MRENIR3 - MRERED3) / (MRENIR3 + MRERED3)
ggR(MREndvi3, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("TRM 3 MRE Normalised Difference Vegetation Index (NDVI)")

writeRaster(MREndvi3,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_3_MRE_2lines/Spectralon/indices/TRM_3_MRE_2lines_NDVI_Rscript.tif", overwrite=TRUE)

#----4.Read in tramway data----
{
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

  MeanFwdSeqresampSpectra <- (FirstRunFwdSeqresampSpectra+SecondRunFwdSeqresampSpectra+ThirdRunFwdSeqresampSpectra)/3
  MeanFwdMREresampSpectra <-  (FirstRunFwdMREresampSpectra+SecondRunFwdMREresampSpectra+ThirdRunFwdMREresampSpectra)/3
  
  }

#Mean forwards Tramway resampled for SEQ
MeanFwdSeqresampGreen <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==550] 
MeanFwdSeqresampRed <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==660] 
MeanFwdSeqresampRedEdge <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==735] 
MeanFwdSeqresampNIR <- MeanFwdSeqresampSpectra$refl[MeanFwdSeqresampSpectra$wvl==790] 
MeanFwdSeqresampdf <- data.frame(MeanFwdSeqresampGreen,MeanFwdSeqresampRed,MeanFwdSeqresampRedEdge,MeanFwdSeqresampNIR)
MeanFwdSeqresampNDVI <- (MeanFwdSeqresampNIR-MeanFwdSeqresampRed)/(MeanFwdSeqresampNIR+MeanFwdSeqresampRed)
MeanFwdSeqresampMSAVI2 <- (2 * MeanFwdSeqresampNIR + 1 - sqrt( (2 * MeanFwdSeqresampNIR + 1)^2 - 8 * (MeanFwdSeqresampNIR - MeanFwdSeqresampRed) )) / 2 
MeanFwdSeqresampMTVI <- 1.5 * (1.2 * (MeanFwdSeqresampNIR - MeanFwdSeqresampGreen) - 2.5 * (MeanFwdSeqresampRed - MeanFwdSeqresampGreen)) /  sqrt( (2 * MeanFwdSeqresampNIR + 1)^2 - (6 * MeanFwdSeqresampNIR - 5 * sqrt(MeanFwdSeqresampRed) - 0.5) )
MeanFwdSeqresampSAVI <- (1 + 0.5)*(MeanFwdSeqresampNIR - MeanFwdSeqresampRed)/(MeanFwdSeqresampNIR + MeanFwdSeqresampRed + 0.5)
MeanFwdSeqresampMSAVI <- MeanFwdSeqresampNIR + 0.5 - (0.5 * sqrt((2 * MeanFwdSeqresampNIR + 1)^2 - 8 * (MeanFwdSeqresampNIR - (2 * MeanFwdSeqresampRed))))

#Mean forwards Tramway resampled for MRE
MeanFwdMREresampBlue <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==475] 
MeanFwdMREresampGreen <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==560] 
MeanFwdMREresampRed <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==668] 
MeanFwdMREresampRedEdge <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==717] 
MeanFwdMREresampNIR <- MeanFwdMREresampSpectra$refl[MeanFwdMREresampSpectra$wvl==840] 
MeanFwdMREresampdf <- data.frame(MeanFwdMREresampBlue,MeanFwdMREresampGreen,MeanFwdMREresampRed,MeanFwdMREresampRedEdge,MeanFwdMREresampNIR)
MeanFwdMREresampNDVI <- (MeanFwdMREresampNIR-MeanFwdMREresampRed)/(MeanFwdMREresampNIR+MeanFwdMREresampRed)
MeanFwdMREresampMSAVI2 <- (2 * MeanFwdMREresampNIR + 1 - sqrt( (2 * MeanFwdMREresampNIR + 1)^2 - 8 * (MeanFwdMREresampNIR - MeanFwdMREresampRed) )) / 2 
MeanFwdMREresampMTVI <- 1.5 * (1.2 * (MeanFwdMREresampNIR - MeanFwdMREresampGreen) - 2.5 * (MeanFwdMREresampRed - MeanFwdMREresampGreen)) /  sqrt( (2 * MeanFwdMREresampNIR + 1)^2 - (6 * MeanFwdMREresampNIR - 5 * sqrt(MeanFwdMREresampRed) - 0.5) )
MeanFwdMREresampSAVI <- (1 + 0.5)*(MeanFwdMREresampNIR - MeanFwdMREresampRed)/(MeanFwdMREresampNIR + MeanFwdMREresampRed + 0.5)
MeanFwdMREresampMSAVI <- MeanFwdMREresampNIR + 0.5 - (0.5 * sqrt((2 * MeanFwdMREresampNIR + 1)^2 - 8 * (MeanFwdMREresampNIR - (2 * MeanFwdMREresampRed))))

#----5. Extract Image Data ------

#Extract TRM 1 Indices data for footprints
TRM_1_SEQFootprintMSAVI2 <- extract(SEQmsavi21,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintMSAVI2 <- extract(MREmsavi21,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintMSAVI <- extract(SEQMSAVI1,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintMSAVI <- extract(MREMSAVI1,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintSAVI <- extract(SEQSAVI1,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintSAVI <- extract(MRESAVI1,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintMTVI <- extract(SEQmtvi1,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintMTVI <- extract(MREmtvi1,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintNDVI <- extract(SEQndvi1,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintNDVI <- extract(MREndvi1,tramwayFootprintsShapes,fun=mean,df=TRUE)

#Extract TRM 2 Indices data for footprints
TRM_2_SEQFootprintMSAVI2 <- extract(SEQmsavi22,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_2_MREFootprintMSAVI2 <- extract(MREmsavi22,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_2_SEQFootprintMSAVI <- extract(SEQMSAVI2,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_2_MREFootprintMSAVI <- extract(MREMSAVI2,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_2_SEQFootprintSAVI <- extract(SEQSAVI2,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_2_MREFootprintSAVI <- extract(MRESAVI2,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_2_SEQFootprintMTVI <- extract(SEQmtvi2,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_2_MREFootprintMTVI <- extract(MREmtvi2,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_2_SEQFootprintNDVI <- extract(SEQndvi2,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_2_MREFootprintNDVI <- extract(MREndvi2,tramwayFootprintsShapes,fun=mean,df=TRUE)

#Extract TRM 3 Indices data for footprints
TRM_3_SEQFootprintMSAVI2 <- extract(SEQmsavi23,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_3_MREFootprintMSAVI2 <- extract(MREmsavi23,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_3_SEQFootprintMSAVI <- extract(SEQMSAVI3,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_3_MREFootprintMSAVI <- extract(MREMSAVI3,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_3_SEQFootprintSAVI <- extract(SEQSAVI3,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_3_MREFootprintSAVI <- extract(MRESAVI3,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_3_SEQFootprintMTVI <- extract(SEQmtvi3,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_3_MREFootprintMTVI <- extract(MREmtvi3,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_3_SEQFootprintNDVI <- extract(SEQndvi3,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_3_MREFootprintNDVI <- extract(MREndvi3,tramwayFootprintsShapes,fun=mean,df=TRUE)

#----5.1 Mean Extracted data for surveys
#Mean NDVI
MeanSEQFootprintNDVI <- (TRM_1_SEQFootprintNDVI  + TRM_2_SEQFootprintNDVI + TRM_3_SEQFootprintNDVI)/3
MeanMREFootprintNDVI <-  (TRM_1_MREFootprintNDVI  + TRM_2_MREFootprintNDVI + TRM_3_MREFootprintNDVI)/3
#Mean SAVI
MeanSEQFootprintSAVI <- (TRM_1_SEQFootprintSAVI  + TRM_2_SEQFootprintSAVI + TRM_3_SEQFootprintSAVI)/3
MeanMREFootprintSAVI <-  (TRM_1_MREFootprintSAVI  + TRM_2_MREFootprintSAVI + TRM_3_MREFootprintSAVI)/3
#Mean MSAVI
MeanSEQFootprintMSAVI <- (TRM_1_SEQFootprintMSAVI  + TRM_2_SEQFootprintMSAVI + TRM_3_SEQFootprintMSAVI)/3
MeanMREFootprintMSAVI <-  (TRM_1_MREFootprintMSAVI  + TRM_2_MREFootprintMSAVI + TRM_3_MREFootprintMSAVI)/3
#Mean MSAVI2
MeanSEQFootprintMSAVI2 <- (TRM_1_SEQFootprintMSAVI2  + TRM_2_SEQFootprintMSAVI2 + TRM_3_SEQFootprintMSAVI2)/3
MeanMREFootprintMSAVI2 <-  (TRM_1_MREFootprintMSAVI2  + TRM_2_MREFootprintMSAVI2 + TRM_3_MREFootprintMSAVI2)/3
#Mean MTVI2
MeanSEQFootprintMTVI <- (TRM_1_SEQFootprintMTVI  + TRM_2_SEQFootprintMTVI + TRM_3_SEQFootprintMTVI)/3
MeanMREFootprintMTVI <-  (TRM_1_MREFootprintMTVI  + TRM_2_MREFootprintMTVI + TRM_3_MREFootprintMTVI)/3

#-----6. Plots--------
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



#-----6a Plot Tramway Mean data Sequoia MSAVI2-------


# Assign axis
x <- as.vector(MeanFwdSeqresampMSAVI2)
y <- as.vector(MeanSEQFootprintMSAVI2$layer)
# Make Data Frame
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
psmsavi2 <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Surveys Mean MSAVI2")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for Sequoia) MSAVI2')+
  ylab('Sequioa MSAVI2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(psmsavi2)

#-----6b Plot Tramway Mean data MRE TRM1 MSAVI2-------


# Assign axis
x <- as.vector(MeanFwdMREresampMSAVI2)
y <- as.vector(TRM_1_MREFootprintMSAVI2$layer)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
pmmsavi2 <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey MSAVI2")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for MRE) MSAVI2')+
  ylab('MRE MSAVI2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pmmsavi2)

#-----6c Plot Tramway Mean data MRE TRM1 SAVI-------


# Assign axis
x <- as.vector(MeanFwdMREresampSAVI)
y <- as.vector(TRM_1_MREFootprintSAVI$layer)
# Make Data Frame
df3 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
pmsavi <- ggplot(df3) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey SAVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for MRE) SAVI')+
  ylab('MRE SAVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pmsavi)

#-----6d Plot Tramway Mean data MRE TRM1 MSAVI-------


# Assign axis
x <- as.vector(MeanFwdMREresampMSAVI)
y <- as.vector(TRM_1_MREFootprintMSAVI$layer)
# Make Data Frame
df4 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
pmmsavi <- ggplot(df4) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey MSAVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for MRE) MSAVI')+
  ylab('MRE MSAVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.5,0.1),ylim=c(0,0.5))
plot(pmmsavi)

#-----6e Plot Tramway Mean data MRE TRM1 NDVI-------


# Assign axis
x <- as.vector(MeanFwdMREresampNDVI)
y <- as.vector(TRM_1_MREFootprintNDVI$layer)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
pmndvi <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for MRE) NDVI')+
  ylab('MRE NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pmndvi)

#-----6f Plot Tramway Mean data MRE TRM1 MTVI-------


# Assign axis
x <- as.vector(MeanFwdMREresampMTVI)
y <- as.vector(TRM_1_MREFootprintMTVI$layer)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
pmmtvi <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey MTVI2")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for MRE) MTVI2')+
  ylab('MRE MTVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.1,0.5),ylim=c(-0.1,0.5))
plot(pmmtvi)

#-----6g Plot Tramway Mean data SEQ TRM1 MTVI-------


# Assign axis
x <- as.vector(MeanFwdSeqresampMTVI)
y <- as.vector(TRM_1_SEQFootprintMTVI$layer)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
psmtvi <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey MTVI2")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for SEQ) MTVI2')+
  ylab('SEQ MTVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.1,0.5),ylim=c(-0.1,0.5))
plot(psmtvi)

#-----6h Plot Tramway Mean data SEQ TRM1 NDVI-------


# Assign axis
x <- as.vector(MeanFwdSeqresampNDVI)
y <- as.vector(TRM_1_SEQFootprintNDVI$layer)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
psndvi <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for SEQ) NDVI')+
  ylab('SEQ NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(psndvi)
#-----6i Plot Tramway Mean data SEQ TRM1 MSAVI-------


# Assign axis
x <- as.vector(MeanFwdSeqresampMSAVI)
y <- as.vector(TRM_1_SEQFootprintMSAVI$layer)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
psmsavi <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey MSAVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for SEQ) MSAVI')+
  ylab('SEQ MSAVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.40,0.1),ylim=c(-0.2,0.5))
plot(psmsavi)

#-----6j Plot Tramway Mean data SEQ TRM1 SAVI-------


# Assign axis
x <- as.vector(MeanFwdSeqresampSAVI)
y <- as.vector(TRM_1_SEQFootprintSAVI$layer)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
pssavi <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey SAVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for SEQ) SAVI')+
  ylab('SEQ SAVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(pssavi)

#----7. Panel arranged plots------

PlotTRM1_SEQ <- grid.arrange(psndvi, pssavi, psmsavi2, psmtvi, nrow = 2)#Plots of TRM1 Sequoia survey
PlotTRM1_MRE <-grid.arrange(pmndvi, pmsavi, pmmsavi2, pmmtvi, nrow = 2)#Plots of TRM1 MRE Survey

#----8. Save Panel Plots

ggsave(
  PlotTRM1_SEQ,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/SEQ_all_VI_M.png",
  width = 16,
  height = 25,
  units = "cm"
)

ggsave(
  PlotTRM1_MRE,
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/MRE_all_VI_M.png",
  width = 16,
  height = 25,
  units = "cm"
)