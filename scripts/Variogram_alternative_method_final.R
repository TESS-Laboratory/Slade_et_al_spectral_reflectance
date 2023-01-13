#NDVI Variogram script
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
  library("usdm")
  library (gstat)
  library (terra)
  library(sf)
  library(tictoc)
}
#----------1.Theme--------

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

#----------2.Read in Area ROI shape files------
AreaROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/Shapefiles', layer = "Area_ROI_2")

##-----3. Read in NDVI, crop NDVI-----


ARE_1_seqSpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_SEQ/Spectralon/indices/ARE_1_SEQ_SPE_index_ndvi.tif")
ARE_1_seqSpectralonNDVIC <- crop(ARE_1_seqSpectralonNDVI,AreaROI)
ARE_1_seqSpectralonNDVICrop <- mask(ARE_1_seqSpectralonNDVIC,AreaROI)
plot(ARE_1_seqSpectralonNDVI)
plot(ARE_1_seqSpectralonNDVIC)
plot (ARE_1_seqSpectralonNDVICrop)

#writeRaster(ARE_1_seqSpectralonNDVICrop,"E:/Glenn/Temp/ARE_1_seqSpectralonNDVICrop.tif", overwrite=TRUE)

ARE_1_MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/indices/ARE_1_MRE_SPE_index_ndvi.tif")
ARE_1_MRESpectralonNDVICrop <- crop(ARE_1_MRESpectralonNDVI,AreaROI)
plot (ARE_1_MRESpectralonNDVICrop)
#----4. Variogram-----


# resampling  needed for SEQ
r <- ARE_1_seqSpectralonNDVICrop
r[is.na(r[])] <- 0 
sampled <- terra::spatSample(rast(r),size = 1000000, xy=T, method = 'regular')
coordinates(sampled) = ~x+y
{tic()
  VAR <-variogram(ARE_1_SEQ_SPE_index_ndvi~x+y, sampled ,width = 0.05, cutoff = 15)
  toc()}
Fit <- fit.variogram(VAR, vgm("Sph"))
plot(VAR)
plot(Fit,cutoff=15)

# resampling  needed for MRE
r1 <- ARE_1_MRESpectralonNDVICrop
r1[is.na(r1[])] <- 0 
sampled1 <- terra::spatSample(rast(r1),size = 1000000, xy=T, method = 'regular')
coordinates(sampled1) = ~x+y
{tic()
  VAR1 <-variogram(ARE_1_MRE_SPE_index_ndvi~x+y, sampled1 ,width = 0.05, cutoff = 15)
  toc()}
Fit1 <- fit.variogram(VAR1, vgm("Sph"))
plot(VAR1)
plot(Fit1,cutoff=15)


