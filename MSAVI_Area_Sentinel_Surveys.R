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

  
}
#----1. Read in shape files-----


study_area_ROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Studyarea")
Sentinel_grid  <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Sentinel_10m_pixel_grid")

#----2. Read in images--------

RED <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SPE_transparent_reflectance_red.tif")
NIR <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SPE_transparent_reflectance_NIR.tif")
Green <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SPE_transparent_reflectance_green.tif")

MRERED <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_red.tif")
MRENIR <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_NIR.tif")
MREGreen <-  raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SPE_transparent_reflectance_green.tif")

SEQ_VC <-raster ("E:/Glenn/Tramway Experiment/Processed/DroneData/Products/ARE_1_SEQ_VC_185.tif") # Rough Fraction vegetation cover for first look at comparison


#Import Sentinel 2 bands and crop and convert reflectance

S2_Red_data <-  raster("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/Full Scenes/T13SCS_20200223T174321_B04_10m.jp2")
S2_NIR_data <-  raster("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/Full Scenes/T13SCS_20200223T174321_B08_10m.jp2")
S2_Green_data <- raster("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/Full Scenes/T13SCS_20200223T174321_B03_10m.jp2")

S2_Red_data_crop <- crop(S2_Red_data,study_area_ROI)
S2_NIR_data_crop <- crop(S2_NIR_data,study_area_ROI)
S2_Green_data_crop <- crop(S2_Green_data,study_area_ROI)


#Sentinel convert to Surface Reflection (0-1) and stack


fun <- function(x) { x / 10000 }
S2_RED <- calc(S2_Red_data_crop, fun)
S2_NIR <- calc(S2_NIR_data_crop, fun)
S2_Green <- calc(S2_Green_data_crop, fun)


#----3. Calculate MSAVI and SAVI-----

#----Sequoia----

#SEQuoia MSAVI Calculation
MSAVI = NIR + 0.5 - (0.5 * sqrt((2 * NIR + 1)^2 - 8 * (NIR - (2 * RED))))

ggR(MSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("SEQ Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(MSAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_MSAVI.tif",overwrite=TRUE)


#Sequoia SAVI

L=0.5
SEQSAVI= (1 + L)*(NIR - RED)/(NIR + RED + L)

ggR(SEQSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sequoia Soil Adjusted Vegetation Index (SAVI)")

writeRaster(SEQSAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SAVI.tif", overwrite=TRUE)

#Sequoia MSAVI2


SEQmsavi2 = (2 * NIR + 1 - sqrt( (2 * NIR + 1)^2 - 8 * (NIR - RED) )) / 2 

ggR(SEQmsavi2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sequoia MSAVI2")

writeRaster(SEQmsavi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_MSAVI2.tif", overwrite=TRUE)

#SEQ Modified Triangular Vegetation Index 2 (MTVI)

SEQmtvi = 1.5 * (1.2 * (NIR - Green) - 2.5 * (RED - Green)) /  sqrt( (2 * NIR + 1)^2 - (6 * NIR - 5 * sqrt(RED) - 0.5) )
ggR(SEQmtvi, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sequoia Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(SEQmtvi,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_MTVI.tif", overwrite=TRUE)





#-----MRE--------

#MRE MSAVI

MREMSAVI = MRENIR + 0.5 - (0.5 * sqrt((2 * MRENIR + 1)^2 - 8 * (MRENIR - (2 * MRERED))))

ggR(MREMSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("MRE Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(MREMSAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_MSAVI.tif",overwrite=TRUE)

#MRE SAVI

L=0.5
MRESAVI= (1 + L)*(MRENIR - MRERED)/(MRENIR + MRERED + L)

ggR(MRESAVI, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("MRE Soil Adjusted Vegetation Index (SAVI)")

writeRaster(MRESAVI,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SAVI.tif", overwrite=TRUE)


#MRE MSAVI2

MREmsavi2 = (2 * MRENIR + 1 - sqrt( (2 * MRENIR + 1)^2 - 8 * (MRENIR - MRERED) )) / 2 

ggR(MREmsavi2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("MRE MSAVI2")

writeRaster(MREmsavi2,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_MSAVI2.tif", overwrite=TRUE)

# MRE MTVI

MREmtvi = 1.5 * (1.2 * (MRENIR - MREGreen) - 2.5 * (MRERED - MREGreen)) /  sqrt( (2 * MRENIR + 1)^2 - (6 * MRENIR - 5 * sqrt(MRERED) - 0.5) )
ggR(MREmtvi, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("MRE Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(MREmtvi,"E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_MTVI.tif", overwrite=TRUE)


#----SENTINEL 2-----


#Sentinel 2 MSAVI

SENMSAVI = S2_NIR + 0.5 - (0.5 * sqrt((2 * S2_NIR + 1)^2 - 8 * (S2_NIR - (2 * S2_RED))))

ggR(SENMSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI", colours = c("red", "yellow", "green", "blue"))+
  ggtitle("Sentinel 2 Modified Soil Adjusted Vegetation Index (MSAVI)")

writeRaster(SENMSAVI,"E:/Glenn/Tramway Experiment/Processed/Satellite_Data/sentinel_20200223_crop_MSAVI.tif",overwrite=TRUE)


#Sentinel NDVI
SENNDVI = (S2_NIR - S2_RED)/(S2_NIR +S2_RED)

ggR(SENNDVI, geom_raster = TRUE) +
  scale_fill_gradientn("NDVI", 
                       colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sentinel 2 Normalized Difference Vegetation Index (NDVI)")

writeRaster(SENNDVI,"E:/Glenn/Tramway Experiment/Processed/Satellite_Data/sentinel_20200223_crop_NDVI.tif", overwrite=TRUE)


#Sentinel SAVI

L=0.5
SENSAVI= (1 + L)*(S2_NIR - S2_RED)/(S2_NIR + S2_RED + L)

ggR(SENSAVI, geom_raster = TRUE) +
  scale_fill_gradientn("SAVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sentinel 2 Soil Adjusted Vegetation Index (SAVI)")

writeRaster(SENSAVI,"E:/Glenn/Tramway Experiment/Processed/Satellite_Data/sentinel_20200223_crop_SAVI.tif", overwrite=TRUE)

#MSAVI2

SENmsavi2 = (2 * S2_NIR + 1 - sqrt( (2 * S2_NIR + 1)^2 - 8 * (S2_NIR - S2_RED) )) / 2 

ggR(SENmsavi2, geom_raster = TRUE) +
  scale_fill_gradientn("MSAVI2", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sentinel 2 MSAVI2")

writeRaster(SENmsavi2,"E:/Glenn/Tramway Experiment/Processed/Satellite_Data/sentinel_20200223_crop_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

SENmtvi = 1.5 * (1.2 * (S2_NIR - S2_Green) - 2.5 * (S2_RED - S2_Green)) /  sqrt( (2 * S2_NIR + 1)^2 - (6 * S2_NIR - 5 * sqrt(S2_RED) - 0.5) )
ggR(SENmtvi, geom_raster = TRUE) +
  scale_fill_gradientn("MTVI", colours = c("red", "yellow", "green", "green4"))+
  ggtitle("Sentinel 2 Modified Triangular Vegetation Index 2 (MTVI)")

writeRaster(SENmtvi,"E:/Glenn/Tramway Experiment/Processed/Satellite_Data/sentinel_20200223_crop_MTVI.tif", overwrite=TRUE)

#----Extract Data on sentinel Grid
#Extract MTVI
ARE_1_MRE_MTVI_Grid <- extract(MREmtvi,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(ARE_1_MRE_MTVI_Grid) <- c('location','MTVI')

ARE_1_SEQ_MTVI_Grid <- extract(SEQmtvi,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(ARE_1_SEQ_MTVI_Grid) <- c('location','MTVI')

SEN_MTVI_Grid <- extract(SENmtvi,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(SEN_MTVI_Grid) <- c('location','MTVI')

SEQ_VC_Grid <- extract(SEQ_VC,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)

# Extract MSAVI2

ARE_1_MRE_MSAVI2_Grid <- extract(MREmsavi2,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(ARE_1_MRE_MSAVI2_Grid) <- c('location','msavi2')

ARE_1_SEQ_MSAVI2_Grid <- extract(SEQmsavi2,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(ARE_1_SEQ_MSAVI2_Grid) <- c('location','msavi2')

SEN_MSAVI2_Grid <- extract(SENmsavi2,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(SEN_MSAVI2_Grid) <- c('location','msavi2')



## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
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
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
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

#---- MTVI PLots-----
#-----Plot FVC vs Sentinel 2 MTVI
x <- as.vector((SEN_MTVI_Grid$MTVI)*10)
y <- as.vector(SEQ_VC_Grid$ARE_1_SEQ_VC_185)
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

FVCmtvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=3.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=3.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of FVC \n with Sentinel 2 MTVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel 2 MTVI (x 10)')+
  ylab('FVC (% cover/100)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.2,0.2),ylim=c(0,0.5))
plot(FVCmtvi)

#-----Plot FVC vs SEQ MTVI
x <- as.vector((ARE_1_SEQ_MTVI_Grid$MTVI)*10)
y <- as.vector(SEQ_VC_Grid$ARE_1_SEQ_VC_185)
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

FVCSEQmtvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=3.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=3.5)+
   #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of FVC \n with SEquoia MTVI - 10m grid")+
  #theme(aspect.ratio=1)+
  xlab('Sequoia MTVI (x10)')+
  ylab('FVC (% cover/100)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.4,0.2),ylim=c(0,0.5))
plot(FVCSEQmtvi)

#-----Plot FVC vs MRE MTVI
x <- as.vector((ARE_1_MRE_MTVI_Grid$MTVI)*10)
y <- as.vector(SEQ_VC_Grid$ARE_1_SEQ_VC_185)
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

FVCMREmtvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=3.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=3.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of FVC \n with MRE MTVI - 10m grid")+
  #theme(aspect.ratio=1)+
  xlab('MRE MTVI (x10)')+
  ylab('FVC (% cover/100)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.2,0.4),ylim=c(0,0.5))
plot(FVCMREmtvi)

#--------MSAVI2 PLots-----------

#-----Plot FVC vs Sentinel MSAVI2
x <- as.vector((SEN_MSAVI2_Grid$msavi2))
y <- as.vector(SEQ_VC_Grid$ARE_1_SEQ_VC_185)
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

FVCSENMSAVI2 <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=3.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=3.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of FVC \n with Sentinel MSAVI 2 - 10m grid")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel MSAVI2)')+
  ylab('FVC (% cover/100)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.4,0.2),ylim=c(0,0.5))
plot(FVCSENMSAVI2)
