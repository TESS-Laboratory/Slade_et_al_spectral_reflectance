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
ARE_1_seqSpectralonNDVICrop <- crop(ARE_1_seqSpectralonNDVI,AreaROI)

ARE_1_MRESpectralonNDVI <- raster("E:/Glenn/Tramway Experiment/Processed/DroneData/ReflStacks/ARE_1_MRE/Spectralon/indices/ARE_1_MRE_SPE_index_ndvi.tif")
ARE_1_MRESpectralonNDVICrop <- crop(ARE_1_MRESpectralonNDVI,AreaROI)

#----4. Variogram-----

MRE_NDVI.var <- Variogram(ARE_1_MRESpectralonNDVICrop,  size=100) 

plot(MRE_NDVI.var)



