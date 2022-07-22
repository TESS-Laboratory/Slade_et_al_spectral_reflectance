##Test script for looking at Tramway reflectance - raw bands - not working at presents 

###Analyse All Tramway Runs bands 
# 
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

#----------1.Theme--------

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

#----------2.Read in Tramway footprint and ROI shape files------
tramwayFootprintsShapes <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData/Footprints', layer = "TramwayMeasurementFootprintShapesNew")
tramwayROI <- readOGR(dsn = 'E:/Glenn/Tramway Experiment/Processed/TramwayData', layer = "TramwayROI")

