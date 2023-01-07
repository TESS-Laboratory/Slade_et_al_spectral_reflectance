###Plot Sensor Response Curves

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
  library(reshape2)
  library(cowplot)
  
  
   }
#----------1.Theme--------

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 7, color = "black"),
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
#----2. Read in Data-------

SEQ_Response_Curve <- read_csv("E:/Glenn/Tramway Experiment/Raw data/DroneData/Sequoia_Response_2nm.csv")
MRE_Response_Curve <- read_csv("E:/Glenn/Tramway Experiment/Raw data/DroneData/M_RSRs_normalized_2nm.csv")
Combined_Response_Curve <- read_csv("E:/Glenn/Tramway Experiment/Raw data/DroneData/Sequoia_MRE_Response.csv")
Combined_Response_Curve_Sen <- read_csv("E:/Glenn/Tramway Experiment/Raw data/DroneData/Sequoia_MRE_Sentinel_Response.csv")
Response_Curve_Sen <- read_csv("E:/Glenn/Tramway Experiment/Raw data/Satellite_Data/Sentinel_2/Sentinel_Response_v3.csv")

#-----3. Plots-----------


df <- melt(SEQ_Response_Curve ,  id.vars = 'Wavelength', variable.name = 'series')
df2 <- melt (MRE_Response_Curve ,  id.vars = 'Wavelength', variable.name = 'series')
df3 <- melt (Combined_Response_Curve ,  id.vars = 'Wavelength', variable.name = 'series')
df4 <- melt (Combined_Response_Curve_Sen ,  id.vars = 'Wavelength', variable.name = 'series')
df5 <- melt (Response_Curve_Sen ,  id.vars = 'Wavelength', variable.name = 'series')


# Sequoia Spectral Response Curve - all black
P2 <-ggplot(df, aes(Wavelength, value)) +
  geom_line(aes(colour = series))+ theme_fancy ()+scale_color_manual(values = c(SEQ_Green = 'red',
                                                                                SEQ_Red = 'red', SEQ_RedEdge = 'red', SEQ_NIR = 'red'))+
  ylab ('Reflectance [HCRF]\n Response')+ xlab('Wavelength nm')+labs(colour='Sequoia Sensor')+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
    theme(legend.position="none")+
  annotate ("text",  x = 405, y = 0.9, label = "Parrot \n Sequoia", size=2.5)+
  annotate("segment", x =430, xend = 455, y = 0.89, yend = 0.89, colour = "red", size=0.35)

plot (P2)

# MRE Spectral Response Curve
P3 <-ggplot(df2, aes(Wavelength, value)) +
  geom_line(aes(colour = series))+ theme_fancy ()+scale_color_manual(values = c(MRE_Blue='black',MRE_Green= 'black',
                                                                                 MRE_Red = 'black', MRE_RedEdge = 'black', MRE_NIR = 'black'))+
  ylab ('Reflectance[HCRF]\n Response')+ xlab('Wavelength nm')+labs(colour='MicaSense Red Edge Sensor')+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
  theme(legend.position="none")+
  annotate ("text",  x = 405, y = 0.9, label = "MicaSense \n RedEdge", size=2.5)+
  annotate("segment", x =430, xend = 455, y = .89, yend = .89, colour = "black", size=0.35)

plot (P3)

# Combined Spectral Response Curves
P4 <-ggplot(df3, aes(Wavelength, value)) +
  geom_line(aes(colour = series))+ theme_fancy ()+scale_color_manual(values = c(MRE_Blue='black',MRE_Green= 'black',
                                                                                MRE_Red = 'black', MRE_RedEdge = 'black', MRE_NIR = 'black', SEQ_Green='red', SEQ_Red= 'red',SEQ_RedEdge='red',SEQ_NIR='red'))+
  ylab ('Reflectance[HCRF]\n Response')+ xlab('Wavelength nm')+labs(colour='MicaSense Red Edge Sensor')+theme(legend.position="none")

plot (P4)


# Combined Spectral Response Curves
P5 <-ggplot(df3, aes(Wavelength, value)) +
  geom_line(aes(colour = series))+ theme_fancy ()+scale_color_manual(values = c(MRE_Blue='blue',MRE_Green= 'green',
                                                                                MRE_Red = 'red', MRE_RedEdge = 'orange', MRE_NIR = 'black',SEQ_Green='darkolivegreen', SEQ_Red= 'tomato3',SEQ_RedEdge='chocolate1',SEQ_NIR='grey70'))+
  ylab ('Reflectance [HCRF] Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
  annotate("rect", xmin = 465, xmax = 485, ymin = -0.1, ymax = -0.02,colour ='blue' ,size =0.35 ,fill ='blue',  alpha = .2)+
  annotate ("text",  x = 475, y = -0.12, label = "MRE Blue", size=2)+
  annotate("rect", xmin = 550, xmax = 570, ymin = -0.1, ymax = -0.02,colour ='green' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 560, y = -0.12, label = "MRE Green", size=2)+
  annotate("rect", xmin = 658, xmax = 678, ymin = -0.1, ymax = -0.02,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 668, y = -0.12, label = "MRE Red", size=2)+
  annotate("rect", xmin = 707, xmax = 727, ymin = -0.1, ymax = -0.02,colour ='orange' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 717, y = -0.12, label = "MRE RedEdge", size=2)+
  annotate("rect", xmin = 820, xmax = 860, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 840, y = -0.12, label = "MRE NIR", size=2)+
  annotate("rect", xmin = 530, xmax = 570, ymin = -0.25, ymax = -0.17,colour ='green' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 550, y = -0.27, label = "SEQ Green", size=2)+
  annotate("rect", xmin = 640, xmax = 680, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 660, y = -0.27, label = "SEQ Red", size=2)+
  annotate("rect", xmin = 730, xmax = 740, ymin = -0.25, ymax = -0.17,colour ='orange' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 735, y = -0.27, label = "SEQ RedEdge", size=2)+
  annotate("rect", xmin = 770, xmax = 810, ymin = -0.25, ymax = -0.17,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 790, y = -0.27, label = "SEQ NIR", size=2)+
  annotate ("text",  x = 415, y = -0.07, label = "MicaSense \n RedEdge", size=3)+
  annotate ("text",  x = 415, y = -0.18, label = "Sequoia", size=3)+
  theme(
    legend.position = c(.15, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

plot (P5)

# Combined Spectral Response Curves
P6 <-ggplot(df3, aes(Wavelength, value)) +
  geom_line(aes(colour = series), size =0.55)+ theme_fancy ()+scale_color_manual(values = c(MRE_Blue='black',MRE_Green= 'black',
                                                                                MRE_Red = 'black', MRE_RedEdge = 'black', MRE_NIR = 'black',SEQ_Green='red', SEQ_Red= 'red',SEQ_RedEdge='red',SEQ_NIR='red'))+
  ylab ('Reflectance [HCRF] Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
  #ylim (-0.3,1)+ 
  scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
  annotate("rect", xmin = 465, xmax = 485, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='blue',  alpha = .2)+
  annotate ("text",  x = 475, y = -0.12, label = "MRE Blue", size=2)+
  annotate("rect", xmin = 550, xmax = 570, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 560, y = -0.12, label = "MRE Green", size=2)+
  annotate("rect", xmin = 658, xmax = 678, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 668, y = -0.12, label = "MRE Red", size=2)+
  annotate("rect", xmin = 707, xmax = 727, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 717, y = -0.12, label = "MRE RedEdge", size=2)+
  annotate("rect", xmin = 820, xmax = 860, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 840, y = -0.12, label = "MRE NIR", size=2)+
  annotate("rect", xmin = 530, xmax = 570, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 550, y = -0.27, label = "SEQ Green", size=2)+
  annotate("rect", xmin = 640, xmax = 680, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 660, y = -0.27, label = "SEQ Red", size=2)+
  annotate("rect", xmin = 730, xmax = 740, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 735, y = -0.27, label = "SEQ RedEdge", size=2)+
  annotate("rect", xmin = 770, xmax = 810, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 790, y = -0.27, label = "SEQ NIR", size=2)+
  annotate ("text",  x = 415, y = -0.07, label = "MicaSense \n RedEdge", size=3)+
  annotate ("text",  x = 415, y = -0.18, label = "Sequoia", size=3)+
  annotate ("text",  x = 410, y = 1, label = "MicaSense \n RedEdge", size=3)+
  annotate ("text",  x = 410, y = 0.9, label = "Sequoia", size=3)+
  annotate("segment", x =430, xend = 460, y = 1, yend = 1, colour = "black", size=0.55)+
  annotate("segment", x =430, xend = 460, y = 0.89, yend = 0.89, colour = "red", size=0.55)+
  
  
  
  
  
  theme(legend.position="none")+ ggtitle("Sensor Response Curves for Sequoia and MicaSense RedEdge") + 
  theme(plot.title = element_text(lineheight=1, size =12, face="bold"))

plot (P6)

# Combined Spectral Response Curves for Sequoia, MRE and Sentinel-2
{
P7 <-ggplot(df4, aes(Wavelength, value)) +
  geom_line(aes(colour = series), size =0.55)+ theme_fancy ()+scale_color_manual(values = c(MRE_Blue='black',MRE_Green= 'black',
                                                                                            MRE_Red = 'black', MRE_RedEdge = 'black', MRE_NIR = 'black',SEQ_Green='red', SEQ_Red= 'red',SEQ_RedEdge='red',SEQ_NIR='red',S2_Blue = 'blue', S2_Green = 'blue', S2_Red ='blue',S2_NIR='blue', S2_B5 = 'blue', S2_B6 = 'blue', S2_B7 = 'blue', Footprint_10='green'))+
  ylab ('Reflectance [HCRF] Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
  #ylim (-0.3,1)+ 
  scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
  annotate("rect", xmin = 465, xmax = 485, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='blue',  alpha = .2)+
  annotate ("text",  x = 475, y = -0.12, label = "MRE Blue", size=2)+
  annotate("rect", xmin = 550, xmax = 570, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 560, y = -0.12, label = "MRE Green", size=2)+
  annotate("rect", xmin = 658, xmax = 678, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 668, y = -0.12, label = "MRE Red", size=2)+
  annotate("rect", xmin = 707, xmax = 727, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 717, y = -0.12, label = "MRE RedEdge", size=2)+
  annotate("rect", xmin = 820, xmax = 860, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 840, y = -0.12, label = "MRE NIR", size=2)+
  annotate("rect", xmin = 530, xmax = 570, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 550, y = -0.27, label = "SEQ Green", size=2)+
  annotate("rect", xmin = 640, xmax = 680, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 660, y = -0.27, label = "SEQ Red", size=2)+
  annotate("rect", xmin = 730, xmax = 740, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 735, y = -0.27, label = "SEQ RedEdge", size=2)+
  annotate("rect", xmin = 770, xmax = 810, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 790, y = -0.27, label = "SEQ NIR", size=2)+
  annotate ("text",  x = 415, y = -0.07, label = "MicaSense \n RedEdge", size=3)+
  annotate ("text",  x = 415, y = -0.19, label = "Sequoia", size=3)+
  annotate ("text",  x = 405, y = 1, label = "MicaSense \n RedEdge", size=3)+
  annotate ("text",  x = 405, y = 0.9, label = "Sequoia", size=3)+
  annotate ("text",  x = 405, y = 0.8, label = "Sentinel-2", size=3)+
  annotate("segment", x =425, xend = 455, y = 1, yend = 1, colour = "black", size=0.55)+
  annotate("segment", x =425, xend = 455, y = 0.89, yend = 0.89, colour = "red", size=0.55)+
  annotate("segment", x =425, xend = 455, y = 0.79, yend = 0.79, colour = "blue", size=0.55)+
  annotate ("text",  x = 405, y = 0.7, label = "Creosote \n Bush", size=3)+
  annotate("segment", x =425, xend = 455, y = 0.69, yend = 0.69, colour = "green", size=0.55)+
  annotate("rect", xmin = 459, xmax = 524, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='blue',  alpha = .2)+
  annotate ("text",  x = 492, y = -0.4, label = "S2 Blue", size=2)+
    annotate("rect", xmin = 541, xmax = 577, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='green',  alpha = .2)+
    annotate ("text",  x = 560, y = -0.4, label = "S2 Green", size=2)+
    annotate("rect", xmin = 649, xmax = 680, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='red',  alpha = .2)+
    annotate ("text",  x = 665, y = -0.4, label = "S2 Red", size=2)+ 
    annotate("rect", xmin = 695, xmax = 711, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 702, y = -0.4, label = "S2 RE B5", size=2)+   
    annotate("rect", xmin = 731, xmax = 746, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 739, y = -0.4, label = "S2 RE B6", size=2)+   
    annotate("rect", xmin = 770, xmax = 790, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 780, y = -0.4, label = "S2 RE B7", size=2)+
    annotate("rect", xmin = 780, xmax = 886, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='black',  alpha = .2)+
    annotate ("text",  x = 833, y = -0.4, label = "S2 NIR", size=2)+
    annotate ("text",  x = 415, y = -0.34, label = "Sentinel-2", size=3)+
  theme(legend.position="none")+ ggtitle("Sensor Response Curves for Sentinel-2, Sequoia and MicaSense RedEdge") + 
  theme(plot.title = element_text(lineheight=1, size =12, face="bold"))
}
plot (P7)



# revised Sequoia and MicaSense Plot
{
P8 <-ggplot(df3, aes(Wavelength, value)) +
  geom_line(aes(colour = series), size =0.55)+ theme_fancy ()+scale_color_manual(values = c(MRE_Blue='black',MRE_Green= 'black',
                                                                                            MRE_Red = 'black', MRE_RedEdge = 'black', MRE_NIR = 'black',SEQ_Green='red', SEQ_Red= 'red',SEQ_RedEdge='red',SEQ_NIR='red',S2_Blue = 'blue', S2_Green = 'blue', S2_Red ='blue',S2_NIR='blue', S2_B5 = 'blue', S2_B6 = 'blue', S2_B7 = 'blue', Footprint_10='green'))+
  ylab ('Reflectance [HCRF] Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
  #ylim (-0.3,1)+ 
  scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
  annotate("rect", xmin = 465, xmax = 485, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='blue',  alpha = .2)+
  annotate ("text",  x = 475, y = -0.12, label = "MRE Blue", size=2)+
  annotate("rect", xmin = 550, xmax = 570, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 560, y = -0.12, label = "MRE Green", size=2)+
  annotate("rect", xmin = 658, xmax = 678, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 668, y = -0.12, label = "MRE Red", size=2)+
  annotate("rect", xmin = 707, xmax = 727, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 717, y = -0.12, label = "MRE RedEdge", size=2)+
  annotate("rect", xmin = 820, xmax = 860, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 840, y = -0.12, label = "MRE NIR", size=2)+
  annotate("rect", xmin = 530, xmax = 570, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='green',  alpha = .2)+
  annotate ("text",  x = 550, y = -0.27, label = "SEQ Green", size=2)+
  annotate("rect", xmin = 640, xmax = 680, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
  annotate ("text",  x = 660, y = -0.27, label = "SEQ Red", size=2)+
  annotate("rect", xmin = 730, xmax = 740, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='orange',  alpha = .2)+
  annotate ("text",  x = 735, y = -0.27, label = "SEQ RedEdge", size=2)+
  annotate("rect", xmin = 770, xmax = 810, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='black',  alpha = .2)+
  annotate ("text",  x = 790, y = -0.27, label = "SEQ NIR", size=2)+
  annotate ("text",  x = 415, y = -0.07, label = "MicaSense \n RedEdge", size=3)+
  annotate ("text",  x = 415, y = -0.19, label = "Sequoia", size=3)+
  annotate ("text",  x = 405, y = 1, label = "MicaSense \n RedEdge", size=3)+
  annotate ("text",  x = 405, y = 0.9, label = "Sequoia", size=3)+
  annotate("segment", x =425, xend = 455, y = 1, yend = 1, colour = "black", size=0.55)+
  annotate("segment", x =425, xend = 455, y = 0.89, yend = 0.89, colour = "red", size=0.55)+
    annotate("rect", xmin = 459, xmax = 524, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='blue',  alpha = .2)+
    annotate ("text",  x = 492, y = -0.4, label = "S2 Blue", size=2)+
    annotate("rect", xmin = 541, xmax = 577, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='green',  alpha = .2)+
    annotate ("text",  x = 560, y = -0.4, label = "S2 Green", size=2)+
    annotate("rect", xmin = 649, xmax = 680, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='red',  alpha = .2)+
    annotate ("text",  x = 665, y = -0.4, label = "S2 Red", size=2)+ 
    annotate("rect", xmin = 695, xmax = 711, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 702, y = -0.4, label = "S2 RE B5", size=2)+   
    annotate("rect", xmin = 731, xmax = 746, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 739, y = -0.4, label = "S2 RE B6", size=2)+   
    annotate("rect", xmin = 770, xmax = 790, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 780, y = -0.4, label = "S2 RE B7", size=2)+
    annotate("rect", xmin = 780, xmax = 886, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='black',  alpha = .2)+
    annotate ("text",  x = 833, y = -0.4, label = "S2 NIR", size=2)+
  annotate ("text",  x = 415, y = -0.34, label = "Sentinel-2", size=3)+
  theme(legend.position="none")+ ggtitle("Sensor Response Curves for Sequoia and MicaSense RedEdge") + 
  theme(plot.title = element_text(lineheight=1, size =12, face="bold"))
}
  plot (P8)
  
# Combined  Sentinel-2 Sensor response 
  {
    P9 <-ggplot(df5, aes(Wavelength, value)) +
      geom_line(aes(colour = series), size =0.55)+ theme_fancy ()+scale_color_manual(values = c(S2_Blue = 'blue', S2_Green = 'blue', S2_Red ='blue',S2_NIR='blue', S2_B5 = 'blue', S2_B6 = 'blue', S2_B7 = 'blue', Footprint_10='green'))+
      ylab ('Reflectance [HCRF] Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
      #ylim (-0.3,1)+ 
      scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
      annotate("rect", xmin = 465, xmax = 485, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='blue',  alpha = .2)+
      annotate ("text",  x = 475, y = -0.12, label = "MRE Blue", size=2)+
      annotate("rect", xmin = 550, xmax = 570, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='green',  alpha = .2)+
      annotate ("text",  x = 560, y = -0.12, label = "MRE Green", size=2)+
      annotate("rect", xmin = 658, xmax = 678, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='red',  alpha = .2)+
      annotate ("text",  x = 668, y = -0.12, label = "MRE Red", size=2)+
      annotate("rect", xmin = 707, xmax = 727, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 717, y = -0.12, label = "MRE RedEdge", size=2)+
      annotate("rect", xmin = 820, xmax = 860, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
      annotate ("text",  x = 840, y = -0.12, label = "MRE NIR", size=2)+
      annotate("rect", xmin = 530, xmax = 570, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='green',  alpha = .2)+
      annotate ("text",  x = 550, y = -0.27, label = "SEQ Green", size=2)+
      annotate("rect", xmin = 640, xmax = 680, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
      annotate ("text",  x = 660, y = -0.27, label = "SEQ Red", size=2)+
      annotate("rect", xmin = 730, xmax = 740, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 735, y = -0.27, label = "SEQ RedEdge", size=2)+
      annotate("rect", xmin = 770, xmax = 810, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='black',  alpha = .2)+
      annotate ("text",  x = 790, y = -0.27, label = "SEQ NIR", size=2)+
      annotate ("text",  x = 415, y = -0.07, label = "MicaSense \n RedEdge", size=3)+
      annotate ("text",  x = 415, y = -0.19, label = "Sequoia", size=3)+
      annotate ("text",  x = 405, y = 0.8, label = "Sentinel-2", size=3)+
      annotate("segment", x =425, xend = 455, y = 0.79, yend = 0.79, colour = "blue", size=0.55)+
      annotate ("text",  x = 405, y = 0.7, label = "Creosote \n Bush", size=3)+
      annotate("segment", x =425, xend = 455, y = 0.69, yend = 0.69, colour = "green", size=0.55)+
      annotate("rect", xmin = 459, xmax = 524, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='blue',  alpha = .2)+
      annotate ("text",  x = 492, y = -0.4, label = "S2 Blue", size=2)+
      annotate("rect", xmin = 541, xmax = 577, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='green',  alpha = .2)+
      annotate ("text",  x = 560, y = -0.4, label = "S2 Green", size=2)+
      annotate("rect", xmin = 649, xmax = 680, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='red',  alpha = .2)+
      annotate ("text",  x = 665, y = -0.4, label = "S2 Red", size=2)+ 
      annotate("rect", xmin = 695, xmax = 711, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 702, y = -0.4, label = "S2 RE B5", size=2)+   
      annotate("rect", xmin = 731, xmax = 746, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 739, y = -0.4, label = "S2 RE B6", size=2)+   
      annotate("rect", xmin = 770, xmax = 790, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 780, y = -0.4, label = "S2 RE B7", size=2)+
      annotate("rect", xmin = 780, xmax = 886, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='black',  alpha = .2)+
      annotate ("text",  x = 833, y = -0.4, label = "S2 NIR", size=2)+
      annotate ("text",  x = 415, y = -0.34, label = "Sentinel-2", size=3)+
      theme(legend.position="none")+ ggtitle("Sensor Response Curve for Sentinel-2") + 
      theme(plot.title = element_text(lineheight=1, size =12, face="bold"))
  }
  plot (P9) 
 
  
  
  # Revised Sentinel-2 Sensor response 
  {
    P10 <-ggplot(df5, aes(Wavelength, value)) +
      geom_line(aes(colour = series), size =0.35)+ theme_fancy ()+scale_color_manual(values = c(S2_Blue = 'blue', S2_Green = 'blue', S2_Red ='blue',S2_NIR='blue', S2_B5 = 'blue', S2_B6 = 'blue', S2_B7 = 'blue'))+
      ylab ('Reflectance [HCRF] Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
      #ylim (-0.3,1)+ 
      scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
      annotate("rect", xmin = 465, xmax = 485, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='blue',  alpha = .2)+
      annotate ("text",  x = 475, y = -0.12, label = "MRE Blue", size=2)+
      annotate("rect", xmin = 550, xmax = 570, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='green',  alpha = .2)+
      annotate ("text",  x = 560, y = -0.12, label = "MRE Green", size=2)+
      annotate("rect", xmin = 658, xmax = 678, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='red',  alpha = .2)+
      annotate ("text",  x = 668, y = -0.12, label = "MRE Red", size=2)+
      annotate("rect", xmin = 707, xmax = 727, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 717, y = -0.12, label = "MRE RedEdge", size=2)+
      annotate("rect", xmin = 820, xmax = 860, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
      annotate ("text",  x = 840, y = -0.12, label = "MRE NIR", size=2)+
      annotate("rect", xmin = 530, xmax = 570, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='green',  alpha = .2)+
      annotate ("text",  x = 550, y = -0.27, label = "SEQ Green", size=2)+
      annotate("rect", xmin = 640, xmax = 680, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
      annotate ("text",  x = 660, y = -0.27, label = "SEQ Red", size=2)+
      annotate("rect", xmin = 730, xmax = 740, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 735, y = -0.27, label = "SEQ RedEdge", size=2)+
      annotate("rect", xmin = 770, xmax = 810, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='black',  alpha = .2)+
      annotate ("text",  x = 790, y = -0.27, label = "SEQ NIR", size=2)+
      annotate ("text",  x = 415, y = -0.07, label = "MicaSense \n RedEdge", size=2.5)+
      annotate ("text",  x = 415, y = -0.19, label = "Sequoia", size=2.5)+
      annotate ("text",  x = 405, y = 0.8, label = "Sentinel-2", size=2.5)+
      annotate("segment", x =430, xend = 455, y = 0.79, yend = 0.79, colour = "blue", size=0.35)+
#      annotate ("text",  x = 405, y = 0.7, label = "Creosote \n Bush", size=2.5)+
#      annotate("segment", x =425, xend = 455, y = 0.69, yend = 0.69, colour = "green", size=0.55)+
      annotate("rect", xmin = 459, xmax = 524, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='blue',  alpha = .2)+
      annotate ("text",  x = 492, y = -0.4, label = "S2 Blue", size=2)+
      annotate("rect", xmin = 541, xmax = 577, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='green',  alpha = .2)+
      annotate ("text",  x = 560, y = -0.4, label = "S2 Green", size=2)+
      annotate("rect", xmin = 649, xmax = 680, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='red',  alpha = .2)+
      annotate ("text",  x = 665, y = -0.4, label = "S2 Red", size=2)+ 
      annotate("rect", xmin = 695, xmax = 711, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 702, y = -0.4, label = "S2 RE B5", size=2)+   
      annotate("rect", xmin = 731, xmax = 746, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 739, y = -0.4, label = "S2 RE B6", size=2)+   
      annotate("rect", xmin = 770, xmax = 790, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
      annotate ("text",  x = 780, y = -0.4, label = "S2 RE B7", size=2)+
      annotate("rect", xmin = 780, xmax = 886, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='black',  alpha = .2)+
      annotate ("text",  x = 833, y = -0.4, label = "S2 NIR", size=2)+
      annotate ("text",  x = 415, y = -0.34, label = "Sentinel-2", size=2.5)+
      theme(legend.position="none")
  }
  plot (P10) 
 
  
  # revised MRE SEQ plot 
  {
  P11 <-ggplot(df3, aes(Wavelength, value)) +
    geom_line(aes(colour = series), size =0.35)+ theme_fancy ()+scale_color_manual(values = c(MRE_Blue='black',MRE_Green= 'black',
                                                                                              MRE_Red = 'black', MRE_RedEdge = 'black', MRE_NIR = 'black',SEQ_Green='red', SEQ_Red= 'red',SEQ_RedEdge='red',SEQ_NIR='red',S2_Blue = 'blue', S2_Green = 'blue', S2_Red ='blue',S2_NIR='blue', S2_B5 = 'blue', S2_B6 = 'blue', S2_B7 = 'blue', Footprint_10='green'))+
    ylab ('Reflectance [HCRF] Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
      annotate ("text",  x = 405, y = 0.9, label = "MicaSense \n RedEdge", size=2.5)+
      annotate ("text",  x = 405, y = 0.8, label = "Sequoia", size=2.5)+
      annotate("segment", x =430, xend = 455, y = .89, yend = .89, colour = "black", size=0.35)+
      annotate("segment", x =430, xend = 455, y = 0.79, yend = 0.79, colour = "red", size=0.35)+
    #ylim (-0.3,1)+ 
    scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+theme(legend.position="none")
  
  }
  plot (P11)
  {
    
    #Annotation only
  P12 <-ggplot(df5, aes(Wavelength, value)) + theme_fancy ()+
  scale_y_continuous(breaks = seq(-0.5, 0, by = 1))+   
    scale_x_continuous(breaks = seq(400, 900, by = 50))+
    annotate("rect", xmin = 465, xmax = 485, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='blue',  alpha = .2)+
    annotate ("text",  x = 475, y = -0.12, label = "MRE Blue", size=2)+
    annotate("rect", xmin = 550, xmax = 570, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='green',  alpha = .2)+
    annotate ("text",  x = 560, y = -0.12, label = "MRE Green", size=2)+
    annotate("rect", xmin = 658, xmax = 678, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='red',  alpha = .2)+
    annotate ("text",  x = 668, y = -0.12, label = "MRE Red", size=2)+
    annotate("rect", xmin = 707, xmax = 727, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 717, y = -0.12, label = "MRE RedEdge", size=2)+
    annotate("rect", xmin = 820, xmax = 860, ymin = -0.1, ymax = -0.02,colour ='black' ,size =0.35 ,fill ='black',  alpha = .2)+
    annotate ("text",  x = 840, y = -0.12, label = "MRE NIR", size=2)+
    annotate("rect", xmin = 530, xmax = 570, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='green',  alpha = .2)+
    annotate ("text",  x = 550, y = -0.27, label = "PS Green", size=2)+
    annotate("rect", xmin = 640, xmax = 680, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='red',  alpha = .2)+
    annotate ("text",  x = 660, y = -0.27, label = "PS Red", size=2)+
    annotate("rect", xmin = 730, xmax = 740, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 735, y = -0.27, label = "PS RedEdge", size=2)+
    annotate("rect", xmin = 770, xmax = 810, ymin = -0.25, ymax = -0.17,colour ='red' ,size =0.35 ,fill ='black',  alpha = .2)+
    annotate ("text",  x = 790, y = -0.27, label = "PS NIR", size=2)+
    annotate ("text",  x = 430, y = -0.07, label = "MicaSense \n (MRE)", size=2.25)+
    annotate ("text",  x = 435, y = -0.21, label = "Parrot \n Sequoia (PS) ", size=2.25)+
    #annotate ("text",  x = 405, y = 0.8, label = "Sentinel-2 (S2) ", size=2.5)+
   # annotate("segment", x =430, xend = 455, y = 0.79, yend = 0.79, colour = "blue", size=0.35)+
    #      annotate ("text",  x = 405, y = 0.7, label = "Creosote \n Bush", size=2.5)+
    #      annotate("segment", x =425, xend = 455, y = 0.69, yend = 0.69, colour = "green", size=0.55)+
    annotate("rect", xmin = 459, xmax = 524, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='blue',  alpha = .2)+
    annotate ("text",  x = 492, y = -0.4, label = "S2 Blue", size=2)+
    annotate("rect", xmin = 541, xmax = 577, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='green',  alpha = .2)+
    annotate ("text",  x = 560, y = -0.4, label = "S2 Green", size=2)+
    annotate("rect", xmin = 649, xmax = 680, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='red',  alpha = .2)+
    annotate ("text",  x = 665, y = -0.4, label = "S2 Red", size=2)+ 
    annotate("rect", xmin = 695, xmax = 711, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 702, y = -0.4, label = "S2 RE B5", size=2)+   
    annotate("rect", xmin = 731, xmax = 746, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 739, y = -0.4, label = "S2 RE B6", size=2)+   
    annotate("rect", xmin = 770, xmax = 790, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='orange',  alpha = .2)+
    annotate ("text",  x = 780, y = -0.4, label = "S2 RE B7", size=2)+
    annotate("rect", xmin = 780, xmax = 886, ymin = -0.38, ymax = -0.3,colour ='blue' ,size =0.35 ,fill ='black',  alpha = .2)+
    annotate ("text",  x = 833, y = -0.4, label = "S2 NIR", size=2)+
    annotate ("text",  x = 430, y = -0.34, label = "Sentinel-2", size=2.25)+
    theme(legend.position="none")+
     theme(
        plot.title = element_blank(),
        axis.title.y = element_blank())
  }
  plot (P12)
  
  # Revised Sentinel-2 Sensor response V2
  {
    P13 <-ggplot(df5, aes(Wavelength, value)) +
      geom_line(aes(colour = series), size =0.35)+ theme_fancy ()+scale_color_manual(values = c(S2_Blue = 'blue', S2_Green = 'blue', S2_Red ='blue',S2_NIR='blue', S2_B5 = 'blue', S2_B6 = 'blue', S2_B7 = 'blue'))+
      ylab ('Reflectance[HCRF]\n Response')+ xlab('Wavelength nm')+labs(colour='Sensors')+
      #ylim (-0.3,1)+ 
      scale_y_continuous(breaks = seq(0, 1, by = .2))+   scale_x_continuous(breaks = seq(400, 900, by = 50))+
      annotate ("text",  x = 405, y = 0.8, label = "Sentinel-2", size=2.5)+
      annotate("segment", x =430, xend = 455, y = 0.79, yend = 0.79, colour = "blue", size=0.35)+
        theme(legend.position="none")
  }
  plot (P13) 
  

  ggsave(
    P2,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Sequoia_black.pdf",
    width = 25,
    height = 16,
    units = "cm"
  ) 
  ggsave(
    P3,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/MRE_coloured.pdf",
    width = 25,
    height = 16,
    units = "cm"
  )
  ggsave(
    P4,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Sequoia_MRE_black.pdf",
    width = 25,
    height = 16,
    units = "cm"
  )

  ggsave(
    P5,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Sequoia_MRE_coloured_annotated.pdf",
    width = 25,
    height = 16,
    units = "cm"
  )
  
  
  ggsave(
    P6,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Sequoia_MRE_black_annotated.pdf",
    width = 25,
    height = 16,
    units = "cm"
  )
  
  ggsave(
    P7,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/combined_annotated.pdf",
    width = 25,
    height = 16,
    units = "cm"
  )
  
  ggsave(
    P8,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/MRE_SEQ_annotated_sentinel.pdf",
    width = 25,
    height = 16,
    units = "cm"
  )
  
  ggsave(
    P9,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Sentinel_annotated.pdf",
    width = 25,
    height = 16,
    units = "cm"
  )

  PlotRESponse <-grid.arrange(P11, P10, nrow = 2)#Plots of  MRE Surveys Mean Data 
  
  
  ggsave(
    PlotRESponse,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Combined_panel_response.pdf",
    width = 16,
    height = 25,
    units = "cm",
  )
  
  PlotRESponse2 <-  plot_grid(P11, P10, align = "v", nrow = 2, rel_heights = c(0.75, 1))
  ggsave(
    PlotRESponse2,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Combined_panel_response2.pdf",
    width = 17,
    height = 20,
    units = "cm",
  )
  
  ggsave(
    PlotRESponse2,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Combined_panel_response2.png",
    width = 17,
    height = 20,
    units = "cm",
  )
  
  PlotRESponse3 <-  plot_grid(P2,P3, P10, align = "v", nrow = 3, rel_heights = c(0.3,0.3,0.4))
  ggsave(
    PlotRESponse3,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Combined_panel_response3.png",
    width = 17,
    height = 18,
    units = "cm",
  )
  
  PlotRESponse4 <-  plot_grid(P2,P3,P13,P12, align = "v", nrow = 4, rel_heights = c(0.25,0.25,0.25,0.25))
  ggsave(
    PlotRESponse4,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Combined_panel_response3.png",
    width = 17,
    height = 17,
    units = "cm",
  )
  
  PlotRESponse5 <-  plot_grid(P2,P3,P13, align = "v", nrow = 3, rel_heights = c(0.33,0.33,0.33))
  ggsave(
    PlotRESponse5,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Combined_panel_response4.png",
    width = 16,
    height = 12,
    units = "cm",
  )
  
  ggsave(
    P12,
    filename = "E:/glenn/Tramway_Rcode/figures/plots/sensor_response_curves/Annotated.png",
    width = 16,
    height = 4,
    units = "cm",
  )