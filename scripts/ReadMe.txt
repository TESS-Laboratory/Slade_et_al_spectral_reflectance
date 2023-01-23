This Repo contains R scripts used in the preparation and analysis of Hyperspectral data, Drone Image Data and Satellite Image data for the following manuscript :

Title:
Optical reflectance across spatial scales – An inter-comparison of transect-based hyperspectral, drone, and satellite reflectance data for dry season rangeland.

Contributors: 
Glenn Slade1*, Dominic Fawcett1, Karen Anderson2, Andrew M. Cunliffe1, Richard E. Brazier 1, Kamal Nyaupane4, Marguerite Mauritz3, Sergio Vargas4.

Affiliation:
*Corresponding Author 
1 Department of Geography, Faculty of Environment, Science and Economy, University of Exeter
2 Environment and Sustainability Institute, University of Exeter, Penryn Campus, TR109FE
3 Biological Sciences, The University of Texas at El Paso, 500 W University Ave., El Paso, TX 79968, USA University of Texas at El Paso
4 Environmental Science and Engineering Program, The University of Texas at El Paso, 500 W University Ave., El Paso, TX 79968, USA University of Texas at El Paso

Submitted to: Drone Systems and Applications



Scripts Folder contains the following files:

Spectral_Response_Curves: Reads in csv files for the spectral response curves for MicaSense RedEdge, Parrot Sequoia and Sentinel-2 sensors
			and produces different combinations of plots and annotations.
Mean_Tramway_FWDRuns_All_Drone_Data_Compare_Panel_improvements_exact: This folder reads the drone image data and Tramway hyperspectral data and extracts
			data and uses the exact_extract function to creat a dataframe of values from each drone survey. Comparison plots
			are then made for combinations of drone data and trawmay data. Data frame is exported to outputs folder as csv and xsls files
reasmpleTramwaySpectra: Used to resample Hyperspectral data to the MRE and PS band wavelengths
MSAVI_TRM_Mean_Surveys: This script reads in drone image data and hyperspectral data and  calculates NDVI, MSAVI2, SAVI and MTVI vegetation indices and then uses the exaxt_extract function to 
			extract data for each footprint to crate a data frame of vegetation indices for each survey and tramway data. Plots of comaprisons
			are the generated
readNMspectra: 		Script for processing Ocean Optics Flame spectrometer
ELMcalibration:		Script for calibration ELM
Variogram:		Reads in Area surveys for MRE and PS and performs a variogram analysis 
tramwaySpecIntercomparisons:  Script compares results from image data calibrated with different calibration panel types.
Sentinel_drone_comparison_v2:  Reads in Sentinel-2 and drone Area image data and extracts data based on the 10m pixel grid of the 
			Sentinel-2 image.  Comparison plots of different bands are the generated

Sentinel_drone_comparison_20m_RedEdge: Reads in Sentinel-2 data and adds the additional extraction of data on the 20m pixel grid of the RedEdge B5,B6 and B7
			and compares the RedEdge PS and MRE bands to the nearest Sentinel-2 bands
MSAVI_Area_Sentinel_Surveys: Reads in Area drone image data and Sentinel 2 data - calculates NDVI, SAVI, MSAVI2 and MTVI images and extracts
			data based on the 10m Sentinel-2 grid.  Comparison plots are the generated.

Folders:  

Old_script_versions: Old versions of scripts 
Reflectance_stacks_scripts: Scripts that stack individual reflectance image data in each band into a multilayered image stack for use in
			Analysis scripts.