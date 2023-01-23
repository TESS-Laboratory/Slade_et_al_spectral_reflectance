###Analyse Variogram

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
  library(reshape2)
  library(cowplot)
  
}
#----------1.Theme--------

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 11, color = "black"),
      axis.title = element_text(size = 11, color = "black"),
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

#----2. read in csv-----

DFVAR <- read.csv('E:/Glenn/Slade_et_al_spectral_reflectance/output_data/variogram_SEQ.csv')
#DFVAR3 <- read.csv('C:/workspace/Tramway/Variogram/variogram_MRE_SEQ.csv')
DFVAR2<- read.csv('E:/Glenn/Slade_et_al_spectral_reflectance/output_data/variogram_MRE.csv')

#df5 <- melt (DFVAR2 ,  id.vars = 'Distance', variable.name = 'series')
#----3. plot-------



# Assign plot to a variable
surveys_plot <- ggplot(data = DFVAR,
                       mapping = aes(x = distance, y = SEQ))

# Draw the plot
GS <-surveys_plot +
  geom_point( colour ='red')+  theme_fancy() +
  #  ggtitle("Variogram")+
  #theme(aspect.ratio=1)+
  xlab('Distance (m)')+
  ylab('Semivariance')+
  geom_point(data = DFVAR2, mapping = aes(x=distance, y=MRE), colour='blue')+
  annotate ("text",  x = 10.5, y = 0.009, label = "MicaSense RedEdge", size=3)+
  annotate ("text",  x = 10, y = 0.008, label = "Parrot Sequoia", size=3)+
  annotate("point", x =8.5, y=0.009, colour = "blue", size=2.5)+
  annotate("point", x =8.5,  y = 0.008,  colour = "red", size=2.5)



GS


ggsave2(
  GS,
  filename = "E:/Glenn/Slade_et_al_spectral_reflectance/figures/variogram2.jpg",
  width = 16,
  height = 8,
  units = "cm"
)

