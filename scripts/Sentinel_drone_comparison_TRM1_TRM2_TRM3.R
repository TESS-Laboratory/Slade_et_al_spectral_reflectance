##Script for comparing Sentinel-2 and drone reflectance data
## TRM1,TRM2 and TRM3 Surveys
#Smaller TRMa surveys encompassing 64 Sentinel-2 10m pixels


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
  
}
#----1. Read in shape files-----


Sentinel_grid  <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Tramway_10m_grid")

#----2. Read in images--------

TRM_1_MRESpectralonReflStack <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_MRE_2lines/Spectralon/TRM_1_MRE_2lines_SpectralonRefl_stack.tif")

SEN_2022_02_23_stackcrop <- stack("E:/Glenn/Tramway Experiment/Raw Data/Satellite_Data/Sentinel_2/S2_2020_02_23_Stackcrop_Reflectance.tif")

TRM_1_SeqSpectralonReflStack <- stack("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/TRM_1_SEQ_2lines/Spectralon/TRM_1_SEQ_2lines_SpectralonRefl_stack.tif")




#-----3. Resample drone image to resolution of Sentinel-2 image

#TRM_1_MRE_Resampled <- resample(TRM_1_MRESpectralonReflStack_TRMaCrop, SEN_2022_02_23_stackcrop,method="bilinear", filename="E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/Resampled/TRMA_1_MRE_Resampled_10m",format="GTiff")
#TRM_1_SEQ_Resampled <- resample(TRM_1_SeqSpectralonReflStack_TRMaCrop, SEN_2022_02_23_stackcrop,method="bilinear", filename="E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/Resampled/TRMA_1_SEQ_Resampled_10m",format="GTiff")

{
  #--------4. Linns Correlatiom -----
  # Need to get resampled image data and sentinel data into DF to work with following
  # Compute the Lin's correlation concordance coefficient
  #
  #ccc_result <- CCC(TRM_1_SEQ_Resampled, SEN_2022_02_23_stackcrop, ci = "z-transform",
  #                  
  #                  conf.level = 0.95)
  #
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
}

#----5. Extract band reflectance data for sentinel grid

TRM_1_SEQ_Grid_Reflectance <- extract(TRM_1_SeqSpectralonReflStack,Sentinel_grid,fun=mean,df=TRUE)
names(TRM_1_SEQ_Grid_Reflectance) <- c('location','green','red','redEdge','NIR')

TRM_1_MRE_Grid_Reflectance <- extract(TRM_1_MRESpectralonReflStack,Sentinel_grid,fun=mean,df=TRUE)
names(TRM_1_MRE_Grid_Reflectance) <- c('location','blue','green','red','redEdge','NIR')

Sentinel_Grid_Reflectance <- extract(SEN_2022_02_23_stackcrop,Sentinel_grid,fun=mean,df=TRUE)
names(Sentinel_Grid_Reflectance) <- c('location','blue','green','red','NIR')

Sentinel_Grid_Reflectance_NDVI <-((Sentinel_Grid_Reflectance$NIR-Sentinel_Grid_Reflectance$red)/(Sentinel_Grid_Reflectance$NIR+Sentinel_Grid_Reflectance$red))
TRM_1_MRE_Grid_Reflectance_NDVI <- ((TRM_1_MRE_Grid_Reflectance$NIR-TRM_1_MRE_Grid_Reflectance$red)/(TRM_1_MRE_Grid_Reflectance$NIR+TRM_1_MRE_Grid_Reflectance$red))
TRM_1_SEQ_Grid_Reflectance_NDVI <- ((TRM_1_SEQ_Grid_Reflectance$NIR-TRM_1_SEQ_Grid_Reflectance$red)/(TRM_1_SEQ_Grid_Reflectance$NIR+TRM_1_SEQ_Grid_Reflectance$red))


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
        fill = "transpTRMnt",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

#-----Plot MRE Blue vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$blue)
y <- as.vector(TRM_1_MRE_Grid_Reflectance$blue)
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
r2val <- summary(lmres)$r.squTRMd

senmb <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.2),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.17),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.13),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.11),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE TRM_1 Survey \n with Sentinel-2 Blue Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Blue band')+
  ylab('Reflectance MRE Blue Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.2),ylim=c(0,0.2))
plot(senmb)

#-----Plot MRE Green vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$green)
y <- as.vector(TRM_1_MRE_Grid_Reflectance$green)
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
r2val <- summary(lmres)$r.squTRMd

senmg <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.2),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.17),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.13),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.11),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE TRM_1 Survey \n with Sentinel-2 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Green band')+
  ylab('Reflectance MRE Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.2),ylim=c(0,0.2))
plot(senmg)

#-----Plot MRE Red vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$red)
y <- as.vector(TRM_1_MRE_Grid_Reflectance$red)
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
r2val <- summary(lmres)$r.squTRMd

senmr <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE TRM_1 Survey \n with Sentinel-2 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Red band')+
  ylab('Reflectance MRE Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.3),ylim=c(0,0.3))
plot(senmr)

#-----Plot MRE NIR vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$NIR)
y <- as.vector(TRM_1_MRE_Grid_Reflectance$NIR)
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
r2val <- summary(lmres)$r.squTRMd

senmni <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE TRM_1 Survey \n with Sentinel-2 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NIR band')+
  ylab('Reflectance MRE NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(senmni)

#-----Plot SEQ NIR vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$NIR)
y <- as.vector(TRM_1_SEQ_Grid_Reflectance$NIR)
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
r2val <- summary(lmres)$r.squTRMd

sensni <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ TRM_1 Survey \n with Sentinel-2 NIR Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NIR band')+
  ylab('Reflectance SEQ NIR Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensni)

#-----Plot SEQ Red vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$red)
y <- as.vector(TRM_1_SEQ_Grid_Reflectance$red)
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
r2val <- summary(lmres)$r.squTRMd

sensr <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ TRM_1 Survey \n with Sentinel-2 Red Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Red band')+
  ylab('Reflectance SEQ Red Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensr)

#-----Plot SEQ Green vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance$green)
y <- as.vector(TRM_1_SEQ_Grid_Reflectance$green)
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
r2val <- summary(lmres)$r.squTRMd

sensg <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ TRM_1 Survey \n with Sentinel-2 Green Band")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 Green band')+
  ylab('Reflectance SEQ Green Band')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensg)

#-----Plot SEQ NDVI vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance_NDVI)
y <- as.vector(TRM_1_SEQ_Grid_Reflectance_NDVI)
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
r2val <- summary(lmres)$r.squTRMd

sensvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of SEQ TRM_1 Survey \n with Sentinel-2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NDVI')+
  ylab('SEQ NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(sensvi)

#-----Plot MRE NDVI vs Sentinel-2
x <- as.vector(Sentinel_Grid_Reflectance_NDVI)
y <- as.vector(TRM_1_MRE_Grid_Reflectance_NDVI)
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
r2val <- summary(lmres)$r.squTRMd

senmvi <- ggplot(df) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.27),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.23),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.21),label=equation,hjust='left', size=2.5)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of MRE TRM_1 Survey \n with Sentinel-2 NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Sentinel-2 NDVI')+
  ylab('MRE NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.4),ylim=c(0,0.4))
plot(senmvi)

