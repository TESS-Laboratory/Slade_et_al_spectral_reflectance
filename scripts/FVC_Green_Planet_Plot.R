###Analyse FVC vs Planet data

#----0. Library-----
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
  library(sf)
  library(exactextractr)
  library(writexl)  
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

# Read in CSV file for Planet Grid FVC and NDVI and Green

Fvc_ndvi <- read.csv ("E:/Glenn/Tramway Experiment/Processed/RF/FVC_NDVI_Green_Harmonised_Planet.csv")

#FVC vs Green Planet

x <- as.vector(Fvc_ndvi$X_Harmomnis)
y <- as.vector(Fvc_ndvi$FVC_2/100)
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

P1 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.05,y=1),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.05,y=.95),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.05,y=.9),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.05,y=.85),label=equation,hjust='left', size=3.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of FVC \n  with Planet NDVI")+
  #theme(aspect.ratio=1)+
  xlab('NDVI')+
  ylab('FVC (1 = 100 %) ')
  #coord_equal(ratio=1)
 #coord_fixed(xlim=c(0,0.2),ylim=c(0,55))
plot(P1)

ggsave(
  P1,
  filename = "E:/Glenn/Slade_et_al_spectral_reflectance/figures/plots/FVC_vs_Green_Harmonised_Planet.png",
  width = 10,
  height = 10,
  units = "cm"
)


