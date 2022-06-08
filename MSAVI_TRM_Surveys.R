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
}


#Second run forwards SEQ
SecondRunFwdSeqresampGreen <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==550] 
SecondRunFwdSeqresampRed <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==660] 
SecondRunFwdSeqresampRedEdge <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==735] 
SecondRunFwdSeqresampNIR <- SecondRunFwdSeqresampSpectra$refl[SecondRunFwdSeqresampSpectra$wvl==790] 
SecondRunFwdSeqresampdf <- data.frame(SecondRunFwdSeqresampGreen,SecondRunFwdSeqresampRed,SecondRunFwdSeqresampRedEdge,SecondRunFwdSeqresampNIR)
SecondRunFwdSeqresampNDVI <- (SecondRunFwdSeqresampNIR-SecondRunFwdSeqresampRed)/(SecondRunFwdSeqresampNIR+SecondRunFwdSeqresampRed)
SecondRunFwdSeqresampMSAVI2 <- (2 * SecondRunFwdSeqresampNIR + 1 - sqrt( (2 * SecondRunFwdSeqresampNIR + 1)^2 - 8 * (SecondRunFwdSeqresampNIR - SecondRunFwdSeqresampRed) )) / 2 
SecondRunFwdSeqresampMTVI <- 1.5 * (1.2 * (SecondRunFwdSeqresampNIR - SecondRunFwdSeqresampGreen) - 2.5 * (SecondRunFwdSeqresampRed - SecondRunFwdSeqresampGreen)) /  sqrt( (2 * SecondRunFwdSeqresampNIR + 1)^2 - (6 * SecondRunFwdSeqresampNIR - 5 * sqrt(SecondRunFwdSeqresampRed) - 0.5) )
SecondRunFwdSeqresampSAVI <- (1 + 0.5)*(SecondRunFwdSeqresampNIR - SecondRunFwdSeqresampRed)/(SecondRunFwdSeqresampNIR + SecondRunFwdSeqresampRed + 0.5)
SecondRunFwdSeqresampMSAVI <- SecondRunFwdSeqresampNIR + 0.5 - (0.5 * sqrt((2 * SecondRunFwdSeqresampNIR + 1)^2 - 8 * (SecondRunFwdSeqresampNIR - (2 * SecondRunFwdSeqresampRed))))

#Second run forwards MRE
SecondRunFwdMREresampBlue <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==475] 
SecondRunFwdMREresampGreen <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==560] 
SecondRunFwdMREresampRed <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==668] 
SecondRunFwdMREresampRedEdge <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==717] 
SecondRunFwdMREresampNIR <- SecondRunFwdMREresampSpectra$refl[SecondRunFwdMREresampSpectra$wvl==840] 
SecondRunFwdMREresampdf <- data.frame(SecondRunFwdMREresampBlue,SecondRunFwdMREresampGreen,SecondRunFwdMREresampRed,SecondRunFwdMREresampRedEdge,SecondRunFwdMREresampNIR)
SecondRunFwdMREresampNDVI <- (SecondRunFwdMREresampNIR-SecondRunFwdMREresampRed)/(SecondRunFwdMREresampNIR+SecondRunFwdMREresampRed)
SecondRunFwdMREresampMSAVI2 <- (2 * SecondRunFwdMREresampNIR + 1 - sqrt( (2 * SecondRunFwdMREresampNIR + 1)^2 - 8 * (SecondRunFwdMREresampNIR - SecondRunFwdMREresampRed) )) / 2 
SecondRunFwdMREresampMTVI <- 1.5 * (1.2 * (SecondRunFwdMREresampNIR - SecondRunFwdMREresampGreen) - 2.5 * (SecondRunFwdMREresampRed - SecondRunFwdMREresampGreen)) /  sqrt( (2 * SecondRunFwdMREresampNIR + 1)^2 - (6 * SecondRunFwdMREresampNIR - 5 * sqrt(SecondRunFwdMREresampRed) - 0.5) )
SecondRunFwdMREresampSAVI <- (1 + 0.5)*(SecondRunFwdMREresampNIR - SecondRunFwdMREresampRed)/(SecondRunFwdMREresampNIR + SecondRunFwdMREresampRed + 0.5)
SecondRunFwdMREresampMSAVI <- SecondRunFwdMREresampNIR + 0.5 - (0.5 * sqrt((2 * SecondRunFwdMREresampNIR + 1)^2 - 8 * (SecondRunFwdMREresampNIR - (2 * SecondRunFwdMREresampRed))))

#----5. Extract Image Data ------

TRM_1_SEQFootprintMSAVI2 <- extract(SEQmsavi2,tramwayFootprintsShapes,fun=mean,df=TRUE)

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



# Plot Tramway Run 2 FWd Sequoia TRM1 MSAVI2

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(SecondRunFwdSeqresampMSAVI2, TRM_1_SEQFootprintMSAVI2$layer, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)

df2 <- data.frame(SecondRunFwdSeqresampMSAVI2,TRM_1_SEQFootprintMSAVI2)# prcomp needs data frame ?

pca <- prcomp(~df$SecondRunFwdSeqresampMSAVI2, data = df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Assign axis
x <- as.vector(SecondRunFwdSeqresampMSAVI2)
y <- as.vector(TRM_1_SEQFootprintMSAVI2$layer)

df3 <- data.frame(x = x, y = y)

#df <- data.frame(x = x, y = y,
#                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
pca <- prcomp(~x,data=df3)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")



# Calculate OLS
MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
p <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
#  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Run 2 Forwards with Sequoia \n Survey TRM1 MSAVI2")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for Sequoia) MSAVI2')+
  ylab('Sequioa MSAVI2')+
    #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p)

