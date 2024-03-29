##Script for comparing Sentinel-2 and drone reflectance data


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
  library(DescTools)  
}
#----1. Read in shape files-----


study_area_ROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Studyarea")
Sentinel_grid  <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Sentinel_10m_pixel_grid")

#----2. Read in images--------

ARE_1_MRESpectralonReflStack <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/ARE_1_MRE_SpectralonRefl_stack.tif")
ARE_1_MRESpectralonReflStack_AreaCrop <- crop(ARE_1_MRESpectralonReflStack,study_area_ROI)
ARE_1_MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/indices/ARE_1_MRE_SPE_index_ndvi.tif")
ARE_1_MRESpectralonNDVI_AreaCrop<- crop(ARE_1_MRESpectralonNDVI,study_area_ROI)

SEN_2022_02_23_stackcrop <- stack("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/S2_2020_02_23_Stackcrop_Reflectance.tif")

ARE_1_SeqSpectralonReflStack <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/ARE_1_SEQ_SpectralonRefl_stack.tif")
ARE_1_SeqSpectralonReflStack_AreaCrop <- crop(ARE_1_SeqSpectralonReflStack,study_area_ROI)
ARE_1_SeqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/indices/ARE_1_SEQ_SPE_index_ndvi.tif")
ARE_1_SeqSpectralonNDVI_AreaCrop<- crop(ARE_1_SeqSpectralonNDVI,study_area_ROI)

SEQ_VC <-raster ("E:/Glenn/Tramway Experiment/Processed/DroneData/Products/ARE_1_SEQ_VC_185.tif")
SEN_NDVI <-raster ("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/sentinel_2022_02_23_NDVI.tif")

#-----3. Resample drone image to resolution of Sentinel-2 image------
# not used at present as extracting data from high resolution drone images
#ARE_1_MRE_Resampled <- resample(ARE_1_MRESpectralonReflStack_AreaCrop, SEN_2022_02_23_stackcrop,method="bilinear", filename="E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/Resampled/AREA_1_MRE_Resampled_10m",format="GTiff")
#ARE_1_SEQ_Resampled <- resample(ARE_1_SeqSpectralonReflStack_AreaCrop, SEN_2022_02_23_stackcrop,method="bilinear", filename="E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/Resampled/AREA_1_SEQ_Resampled_10m",format="GTiff")
#ARE_1_MRE_Resampled_NDVI <- resample(ARE_1_MRESpectralonNDVI_AreaCrop, SEN_2022_02_23_stackcrop,method="bilinear", filename="E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/Resampled/AREA_1_MRE_NDVI_Resampled_10m",format="GTiff")
#ARE_1_SEQ_Resampled_NDVI <- resample(ARE_1_SeqSpectralonNDVI_AreaCrop, SEN_2022_02_23_stackcrop,method="bilinear", filename="E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/Resampled/AREA_1_SEQ_NDVI_Resampled_10m",format="GTiff")


#----5. Extract band reflectance data for sentinel grid from high resolution drone images----

ARE_1_SEQ_Grid_Reflectance <- extract(ARE_1_SeqSpectralonReflStack_AreaCrop,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(ARE_1_SEQ_Grid_Reflectance) <- c('location','green','red','redEdge','NIR')
ARE_1_SEQ_Grid_Reflectance_NDVI <- extract(ARE_1_SeqSpectralonNDVI_AreaCrop,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)


SEQ_VC_Grid <- extract(SEQ_VC,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)

ARE_1_MRE_Grid_Reflectance <- extract(ARE_1_MRESpectralonReflStack_AreaCrop,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(ARE_1_MRE_Grid_Reflectance) <- c('location','blue','green','red','redEdge','NIR')
ARE_1_MRE_Grid_Reflectance_NDVI <- extract(ARE_1_MRESpectralonNDVI_AreaCrop,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)

Sentinel_Grid_Reflectance <- extract(SEN_2022_02_23_stackcrop,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)
names(Sentinel_Grid_Reflectance) <- c('location','blue','green','red','NIR')

#Sentinel_Grid_Reflectance_NDVI <-((Sentinel_Grid_Reflectance$NIR-Sentinel_Grid_Reflectance$red)/(Sentinel_Grid_Reflectance$NIR+Sentinel_Grid_Reflectance$red))
#ARE_1_MRE_Grid_Reflectance_NDVI <- ((ARE_1_MRE_Grid_Reflectance$NIR-ARE_1_MRE_Grid_Reflectance$red)/(ARE_1_MRE_Grid_Reflectance$NIR+ARE_1_MRE_Grid_Reflectance$red))
#ARE_1_SEQ_Grid_Reflectance_NDVI <- ((ARE_1_SEQ_Grid_Reflectance$NIR-ARE_1_SEQ_Grid_Reflectance$red)/(ARE_1_SEQ_Grid_Reflectance$NIR+ARE_1_SEQ_Grid_Reflectance$red))
Sentinel_raster_ndvi <-extract(SEN_NDVI,Sentinel_grid,fun=mean,df=TRUE,na.rm=TRUE)

#----6. Plots----------

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

#-----Plot MRE Blue vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$blue)
y <- as.vector(ARE_1_MRE_Grid_Reflectance$blue)
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

senmb <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE ARE_1 Survey \n with Sentinel-2 Blue Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Blue band')+
  ylab('Reflectance MRE Blue Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(senmb)

#-----Plot MRE Green vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$green)
y <- as.vector(ARE_1_MRE_Grid_Reflectance$green)
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

senmg <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE ARE_1 Survey \n with Sentinel-2 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Green band')+
  ylab('Reflectance MRE Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(senmg)

#-----Plot MRE Red vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$red)
y <- as.vector(ARE_1_MRE_Grid_Reflectance$red)
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

senmr <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE ARE_1 Survey \n with Sentinel-2 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Red band')+
  ylab('Reflectance MRE Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(senmr)

#-----Plot MRE NIR vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$NIR)
y <- as.vector(ARE_1_MRE_Grid_Reflectance$NIR)
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

senmni <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE ARE_1 Survey \n with Sentinel-2 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NIR band')+
  ylab('Reflectance MRE NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(senmni)

#-----Plot SEQ NIR vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$NIR)
y <- as.vector(ARE_1_SEQ_Grid_Reflectance$NIR)
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

sensni <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ ARE_1 Survey \n with Sentinel-2 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NIR band')+
  ylab('Reflectance SEQ NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensni)

#-----Plot SEQ Red vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$red)
y <- as.vector(ARE_1_SEQ_Grid_Reflectance$red)
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

sensr <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ ARE_1 Survey \n with Sentinel-2 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Red band')+
  ylab('Reflectance SEQ Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensr)

#-----Plot SEQ Green vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$green)
y <- as.vector(ARE_1_SEQ_Grid_Reflectance$green)
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

sensg <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ ARE_1 Survey \n with Sentinel-2 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Green band')+
  ylab('Reflectance SEQ Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensg)

#-----Plot SEQ NDVI vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance_NDVI)
y <- as.vector(ARE_1_SEQ_Grid_Reflectance_NDVI)
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

sensvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ ARE_1 Survey \n with Sentinel-2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NDVI')+
  ylab('SEQ NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensvi)

#-----Plot MRE NDVI vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance_NDVI)
y <- as.vector(ARE_1_MRE_Grid_Reflectance_NDVI)
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

senmvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE ARE_1 Survey \n with Sentinel-2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NDVI')+
  ylab('MRE NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(senmvi)

#-----Plot FVC vs Sentinel-2 NDVI
x <- as.vector(Sentinel_Grid_Reflectance_NDVI)
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

FVCvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.3,y=0.2),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.3,y=0.17),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.3,y=0.14),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.3,y=0.11),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of FVC \n with Sentinel-2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NDVI')+
  ylab('FVC')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(FVCvi)


#-----Plot FVC vs Sentinel-2 NDVI
x <- as.vector(Sentinel_raster_ndvi$sentinel_2022_02_23_NDVI)
y <- as.vector(SEQ_VC_Grid$ARE_1_SEQ_VC_185)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))



FVCvi2 <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.3,y=0.2),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.3,y=0.17),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.3,y=0.14),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.3,y=0.11),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of FVC \n with Sentinel-2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NDVI')+
  ylab('FVC (% cover/100)')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(FVCvi2)

#-----Plot SEQ NDVI vs Sentinel-2 V2
x <- as.vector(Sentinel_raster_ndvi$sentinel_2022_02_23_NDVI)
y <- as.vector(ARE_1_SEQ_Grid_Reflectance_NDVI$ARE_1_SEQ_SPE_index_ndvi)
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

sensvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.37),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.34),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.31),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ ARE_1 Survey \n with Sentinel-2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NDVI')+
  ylab('SEQ NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensvi)




#----7. Panel arrangement of Plots-----

grid.arrange(sensg,sensr,sensni,sensvi, nrow = 2)#Plots of ARE1 Sequoia survey
grid.arrange(senmb,senmg,senmr,senmni,senmvi, nrow = 3)#Plots of ARE1 MRE Survey

PlotARE_1_SEQ <-grid.arrange(sensg,sensr,sensni,sensvi, nrow = 2)#Plots of ARE1 Sequoia survey
PlotARE_1_MRE <-grid.arrange(senmb,senmg,senmr,senmni,senmvi, nrow = 3)#Plots of ARE1 MRE Survey


#-----8. Save Plots------

ggsave(
  PlotARE_1_MRE,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/Sentinel/ARE_1_MRE.png",
  width = 16,
  height = 25,
  units = "cm"
)
ggsave(
  PlotARE_1_SEQ,
  # filename = "/plots/test.png",
  filename = "E:/glenn/Tramway Experiment/Processed/Plots/Sentinel/ARE_1_SEQ.png",
  width = 16,
  height = 25,
  units = "cm"
)