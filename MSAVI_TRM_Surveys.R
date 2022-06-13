##Script for calculating MSAVI


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

RED <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SPE_transparent_reflectance_red.tif")
NIR <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SPE_transparent_reflectance_NIR.tif")
Green <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SPE_transparent_reflectance_green.tif")

MRERED <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SPE_transparent_reflectance_red.tif")
MRENIR <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SPE_transparent_reflectance_NIR.tif")
MREGreen <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SPE_transparent_reflectance_green.tif")


#----3. Calculate MSAVI and SAVI-----

#----Sequoia----

#SEQuoia MSAVI Calculation
SEQMSAVI = NIR + 0.5 - (0.5 * sqrt((2 * NIR + 1)^2 - 8 * (NIR - (2 * RED))))

ggR(SEQMSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("SEQ Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(SEQMSAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_MSAVI.tif",overwrite=TRUE)


#Sequoia SAVI Calculation

L=0.5
SEQSAVI= (1 + L)*(NIR - RED)/(NIR + RED + L)

ggR(SEQSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sequoia Soil Adjusted Vegetation Index (SAVI)")

writeRaster(SEQSAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_SAVI.tif", overwrite=TRUE)

#Sequoia MSAVI2 Calculation


SEQmsavi2 = (2 * NIR + 1 - sqrt( (2 * NIR + 1)^2 - 8 * (NIR - RED) )) / 2 

ggR(SEQmsavi2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sequoia MSAVI2")

writeRaster(SEQmsavi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_MSAVI2.tif", overwrite=TRUE)

#SEQ Modified Triangular Vegetation Index 2 (MTVI) Calculation

SEQmtvi = 1.5 * (1.2 * (NIR - Green) - 2.5 * (RED - Green)) /  sqrt( (2 * NIR + 1)^2 - (6 * NIR - 5 * sqrt(RED) - 0.5) )
ggR(SEQmtvi, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sequoia Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(SEQmtvi,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_MTVI.tif", overwrite=TRUE)


#SEQ Normalised Difference Vegetation Index 2 (NDVI) Calculation

SEQndvi =  (NIR - RED) / ( NIR + RED)
ggR(SEQndvi, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sequoia Normalised Difference Vegetation Index 2 (NDVI)")

writeRaster(SEQndvi,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_3lines/Spectralon/TRM_1_SEQ_3lines_NDVI_Rscript.tif", overwrite=TRUE)




####--------MRE--------

#MRE MSAVI Calculation
MREMSAVI = MRENIR + 0.5 - (0.5 * sqrt((2 * MRENIR + 1)^2 - 8 * (MRENIR - (2 * MRERED))))

ggR(MREMSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("MRE Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(MREMSAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_MSAVI.tif",overwrite=TRUE)


#MRE SAVI Calculation

L=0.5
MRESAVI= (1 + L)*(MRENIR - MRERED)/(MRENIR + MRERED + L)

ggR(MRESAVI, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("MRESoil Adjusted Vegetation Index (SAVI)")

writeRaster(MRESAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SAVI.tif", overwrite=TRUE)

#MRE MSAVI2 Calculation


MREmsavi2 = (2 * MRENIR + 1 - sqrt( (2 * MRENIR + 1)^2 - 8 * (MRENIR - MRERED) )) / 2 

ggR(MREmsavi2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("MREMSAVI2")

writeRaster(MREmsavi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_MSAVI2.tif", overwrite=TRUE)

#MRE Modified Triangular Vegetation Index 2 (MTVI) Calculation

MREmtvi = 1.5 * (1.2 * (MRENIR - MREGreen) - 2.5 * (MRERED - MREGreen)) /  sqrt( (2 * MRENIR + 1)^2 - (6 * MRENIR - 5 * sqrt(MRERED) - 0.5) )
ggR(MREmtvi, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("MRE Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(MREmtvi,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_MTVI.tif", overwrite=TRUE)

#MRE Normalised Difference Vegetation Index 2 (NDVI) Calculation

MREndvi = (MRENIR - MRERED) / (MRENIR + MRERED)
ggR(MREndvi, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("MRE Normalised Difference Vegetation Index (NDVI)")

writeRaster(MREndvi,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_NDVI_Rscript.tif", overwrite=TRUE)


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

#Mean forwards SEQ
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

#Mean forwards MRE
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

TRM_1_SEQFootprintMSAVI2 <- extract(SEQmsavi2,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintMSAVI2 <- extract(MREmsavi2,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintMSAVI <- extract(SEQMSAVI,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintMSAVI <- extract(MREMSAVI,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintSAVI <- extract(SEQSAVI,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintSAVI <- extract(MRESAVI,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintMTVI <- extract(SEQmtvi,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintMTVI <- extract(MREmtvi,tramwayFootprintsShapes,fun=mean,df=TRUE)

TRM_1_SEQFootprintNDVI <- extract(SEQndvi,tramwayFootprintsShapes,fun=mean,df=TRUE)
TRM_1_MREFootprintNDVI <- extract(MREndvi,tramwayFootprintsShapes,fun=mean,df=TRUE)


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



#-----6a Plot Tramway Mean data Sequoia TRM1 MSAVI2-------


# Assign axis
x <- as.vector(MeanFwdSeqresampMSAVI2)
y <- as.vector(TRM_1_SEQFootprintMSAVI2$layer)
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
  ggtitle("Comparison of Tramway Mean Data with Sequoia \n Survey TRM1 MSAVI2")+
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
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 MSAVI2")+
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
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 SAVI")+
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
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 MSAVI")+
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
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 NDVI")+
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
  ggtitle("Comparison of Tramway Mean Data with MRE \n Survey TRM1 MTVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for MRE) MTVI')+
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
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey TRM1 MTVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for SEQ) MTVI')+
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
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey TRM1 NDVI")+
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
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey TRM1 MSAVI")+
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
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey TRM1 SAVI")+
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