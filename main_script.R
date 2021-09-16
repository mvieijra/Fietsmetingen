# THIS IS A SCRIPT THAT CAN BE USED TO PRODUCE TEMPERATURE CHARTS BASED ON MOBILE BICYCLE MEASUREMENTS

# Load packages that are required to run the program
library(numbers)
library(plyr)
library(devtools)
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(SDMTools)
library(tibble)
library(GISTools)
library(maps)
library(base)
library(prettymapr)
library(postGIStools)
library(proj4)
library(rgdal)
library(sf)
library(foreach)
library(doParallel)
library(tcltk2)
library(spex)
library(fasterize)
library(Hmisc)
library(PerformanceAnalytics)
library(plotly)
library(dplyr)
library(geosphere)
library(stringr)
library(lubridate)
library(pspline)
library(reshape2)
library(rasterVis)
library(doMC)
library(viridis)
library(tmap)
library(leaflet)
library(rpart)
library(rpart.plot)
library(plotrix)
library(fields)
library(tidyr)
library(nlstools)
library(plot.matrix)
library(easynls)
library(calibrate)

# Make a connection to the script with all the necessary functions
source("/home/michiel/Documenten/VLINDER/Programs/functions_script.R")

#---------------------------------------------GENERAL SETTINGS-----------------------------------------------------------#

print("General settings are loaded")
rotate = FALSE
BBK = TRUE   # If TRUE the land cover fractions are calculated based on the BBK land cover map of Flanders
ESM = FALSE   # If TRUE the land cover fractions are calculated based on the ESM land cover map of Europe
temp_inertia_bool = TRUE    # If TRUE the temperature is corrected for thermal inertia
temp_decline_bool = TRUE    # If TRUE the temperature is corrected for temperature decline
vlinder_decline = TRUE   # If TRUE the temperature decline correction is based on the VLINDER stations
mocca_decline = TRUE   # If TRUE the temperature decline correction is based on the MOCCA stations
station_decline = FALSE   # If TRUE the temperature decline correction is based on the specified set of weather stations (not MOCCA or VLINDER)
vlinder_chart = TRUE   # If TRUE temperature charts are made based on a temperature decline correction based on the VLINDER stations
mocca_chart = TRUE   # If TRUE temperature charts are made based on a temperature decline correction based on the MOCCA stations
station_chart = FALSE   # If TRUE temperature charts are made based on a temperature decline correction based on the set of weather stations (not MOCCA or VLINDER)
vlinder_visualisation = FALSE   # If TRUE visualisations of the temperatures of the route are made based on a temperature decline correction based on the VLINDER stations
mocca_visualisation = FALSE   # If TRUE visualisations of the temperatures of the route are made based on a temperature decline correction based on the MOCCA stations
station_visualisation = FALSE   # If TRUE visualisations of the temperatures of the route are made based on a temperature decline correction based on the set of weather stations (not MOCCA or VLINDER)

if (BBK == FALSE & ESM == FALSE){
  stop("At least one land cover map (either BBK or ESM) should be used to do the analysis", call. = FALSE)
}
if (temp_inertia_bool == FALSE & temp_decline_bool == TRUE){
  stop("If temp_decline_bool = TRUE then temp_inertia_bool should also be TRUE", call. = FALSE)
}

if (vlinder_decline == FALSE & vlinder_chart == TRUE){
  stop("If vlinder_chart = TRUE then vlinder_decline should also be TRUE", call. = FALSE)
}

if (mocca_decline == FALSE & mocca_chart == TRUE){
  stop("If mocca_chart = TRUE then mocca_decline should also be TRUE", call. = FALSE)
}

if (station_decline == FALSE & station_chart == TRUE){
  stop("If station_chart = TRUE then station_decline should also be TRUE", call. = FALSE)
}

if (vlinder_decline == FALSE & vlinder_visualisation == TRUE){
  stop("If vlinder_visualisation = TRUE then vlinder_decline should also be TRUE", call. = FALSE)
}

if (mocca_decline == FALSE & mocca_visualisation == TRUE){
  stop("If mocca_visualisation = TRUE then mocca_decline should also be TRUE", call. = FALSE)
}

if (station_decline == FALSE & station_visualisation == TRUE){
  stop("If station_visualisation = TRUE then station_decline should also be TRUE", call. = FALSE)
}

if (mocca_decline == TRUE & temp_decline_bool == FALSE){
  stop("If mocca_decline = TRUE then temp_decline_bool should also be TRUE", call. = FALSE)
}

if (vlinder_decline == TRUE & temp_decline_bool == FALSE){
  stop("If vlinder_decline = TRUE then temp_decline_bool should also be TRUE", call. = FALSE)
}

if (station_decline == TRUE & temp_decline_bool == FALSE){
  stop("If station_decline = TRUE then temp_decline_bool should also be TRUE", call. = FALSE)
}

# Specify the number of cores that will be used for calculation of the land cover fractions
cores = 3
registerDoMC(cores)

# Specify the distances for which circular buffers have to be created (in meter)
buffer_distances = c(10, 50, 100, 200, 300, 400, 500)	

# Specify the land cover variables that will be used to predict the temperatures of the gridpoints of the temperature chart
# These variables are given as input to the linear model from which the temperature chart is created
model_variables = c("impervious200", "green200", "water200")

# Create a vector with all distances that are appearing in the land cover input variables for the linear model (model variables)
buffer_distances_chart = c(200)

step_distance = 200   # The distance between two gridpoints (in meter)
if (mod(1000, step_distance) != 0){
  stop("1000 should be divisible by the step distance", call. = FALSE)
}

seconds_per_interval = 1800   # The time interval for which temperature charts are made (in seconds)
if (mod(seconds_per_interval, 10) != 0){
  stop("The number of seconds of the time interval should be divisible by 10 ('seconds_per_interval' variable)", call. = FALSE)
}

# The distance that is added to the four extremity points of the route (north, east, south and west) to set the scale of the temperature chart
# MAKE SURE THAT THE EXTENT OF THE CHART IS SUFFICIENTLY LARGE SUCH THAT THE LOCATIONS OF THE WEATHER STATIONS 
# USED FOR VALIDATION OF THE CHART TEMPERATURES FIT WELL INTO THE CHART AND ARE NOT AT THE BORDER
extension_of_chart = 3500   

# Specify a string of the date of the measurements (will be used in some plot titles)
datestring = "06-09-2021"

datafile_measurements = "/home/michiel/Documenten/VLINDER/Measurements/Gent_metingen.csv"   # Directory of the datafile with the measurements
BBK_directory = "/home/michiel/Documenten/VLINDER/Landcover_maps/BBK_Flanders2015.tif"   # Directory of the BBK land cover map of Flanders
ESM_directory = "/home/michiel/Documenten/VLINDER/Landcover_maps/ESM2012_Flanders.tif"   # Directory of the ESM land cover map of Europe (cut out for the region of Flanders)

# Directory of the folder with the datafiles used for the thermal inertia correction
# MAKE SURE THE INERTIA FILES IN THIS FOLDER ARE IN TXT-FORMAT
inertiafiles_directory = "/home/michiel/Documenten/VLINDER/Inertia_files/"

# Directories of the datafiles with the temperature measurements of the MOCCA stations during the measurement campaign
# PUT HERE THE DATAFILES OF THE MOCCA STATIONS THAT SHOULD BE TAKEN INTO ACCOUNT FOR THE TEMPERATURE DECLINE CORRECTION BASED ON THE MOCCA STATIONS
hondafile = "/home/michiel/Documenten/VLINDER/Station_data/Mocca_Honda.csv"
mellefile = "/home/michiel/Documenten/VLINDER/Station_data/Mocca_Melle.csv"
plantentuinfile = "/home/michiel/Documenten/VLINDER/Station_data/Mocca_plantentuin.csv"
sint_bavofile = "/home/michiel/Documenten/VLINDER/Station_data/Mocca_sint_bavo.csv"
wondelgemfile = "/home/michiel/Documenten/VLINDER/Station_data/Mocca_wondelgem.csv"

# Directories of the datafiles with the temperature measurements of the VLINDER stations during the measurement campaign
# PUT HERE THE DATAFILES OF THE VLINDER STATIONS THAT SHOULD BE TAKEN INTO ACCOUNT FOR THE TEMPERATURE DECLINE CORRECTION BASED ON THE VLINDER STATIONS
file1 = "/home/michiel/Documenten/VLINDER/Station_data/Vlinder_01_06-09-2021.csv"
file2 = "/home/michiel/Documenten/VLINDER/Station_data/Vlinder_02_06-09-2021.csv"
file3 = "/home/michiel/Documenten/VLINDER/Station_data/Vlinder_27_06-09-2021.csv"
file4 = "/home/michiel/Documenten/VLINDER/Station_data/Vlinder_28_06-09-2021.csv"
file5 = "/home/michiel/Documenten/VLINDER/Station_data/Vlinder_29_06-09-2021.csv"
file6 = "/home/michiel/Documenten/VLINDER/Station_data/Vlinder_71_06-09-2021.csv"

# Create a vector with the names of the MOCCA stations and a vector with the directories of the MOCCA datafiles 
# Only the MOCCA stations that will be used for the temperature decline correction should be included
# MAKE SURE THAT THE ORDER OF THE NAMES CORRESPONDS TO THE ORDER OF THE DIRECTORIES
mocca_path_list = c(hondafile, mellefile, plantentuinfile, sint_bavofile, wondelgemfile)
mocca_name_list = c("Honda", "Melle", "Botanic garden", "Sint-Bavo", "Wondelgem")

# Create a vector with the names of the VLINDER stations and a vector with the directories of the VLINDER datafiles
# Only the VLINDER stations that will be used for the temperature decline correction should be included
# MAKE SURE THAT THE ORDER OF THE NAMES CORRESPONDS TO THE ORDER OF THE DIRECTORIES
vlinder_path_list = c(file2, file3, file4, file5, file1)
vlinder_name_list = c("UGent", "Gent Kunstinstituut", "Gentbrugge", "Oostakker", "Melle")

# Create a dataframe with the locations of all MOCCA stations
mocca_locations = data.frame(c("Botanic garden", "Melle", "Sint-Bavo", "Wondelgem", "Honda", "Provinciehuis"), c(3.72247, 3.815744, 3.732, 3.702875, 3.749, 3.7277), c(51.0357, 50.98043, 51.052, 51.084, 51.109, 51.05120))
colnames(mocca_locations) = c("Name", "X_WGS_GPS", "Y_WGS_GPS")

# Create a dataframe with the locations of a large number of VLINDER stations 
vlinder_locations = data.frame(c("Melle", "UGent", "Turnhout city", "Turnhout farm", "Gent Watersportbaan", "Bonheiden", "Mechelen North", "Mechelen Lindepoort", "Aalst Heuvelpark", "Aalst city", "Antw. Linkeroever", "Antw. Zoo", "Antw. ITG", "Zandvliet", "Asse", "Beveren", "Oudsbergen", "Bree", "Brussel Royal Palace", "Brussel Cathedral", "De Haan", "Diksmuide", "Sint-Laureins", "Eeklo", "Evergem", "Geel", "Gent Ottogracht", "Gentbrugge", "Oostakker", "Antw. Eilandje", "Heist-op-den-Berg rural", "Heist-op-den-Berg city", "Herentals", "Ieper city", "Keerbergen", "Koekelberg", "Kontich", "Lier Zimmertoren", "Lommel rural", "Londerzeel", "Maaseik", "Maldegem market", "Menen", "Nieuwpoort", "Oudenaarde Donk", "Oudenaarde Schelde", "Pelt", "Sint-Niklaas Ster", "Sint-Niklaas city", "Sint-Truiden", "Gingelom", "Tielt city", "Tielt Poelberg", "Tienen", "De Panne", "Vorselaar", "Zelzate city", "Zelzate harbour", "Melle_2", "Ieper Vesten", "Maldegem Courtmanslaan", "Geraardsbergen", "Mechelen Nekkerspoel", "Mechelen Dijle", "Herzele", "Ninove", "Zottegem", "Lier Donkhoeve", "Lommel city", "Langemark-Poelkapelle", "Gent observatory"), 
                               c(3.815763, 3.709695, 4.952112, 4.93469, 3.6751, 4.5163, 4.465868, 4.477398, 4.075781, 4.034961, 4.381624, 4.423266, 4.398711, 4.315013, 4.1926, 4.293436, 5.6134583, 5.656769, 4.363656, 4.357877, 2.991917, 2.8562197, 3.580151, 3.572062, 3.708611, 4.997889, 3.728, 3.791667, 3.789441, 4.417377, 4.685974, 4.722222, 4.8135, 2.882751, 4.659717, 4.308056, 4.44967, 4.569839, 5.348093, 4.255669, 5.81812, 3.445343, 3.115786, 2.752535, 3.588439, 3.586094, 5.391508, 4.189688, 4.145906, 5.188869, 5.226407, 3.328104, 3.358833, 4.940457, 2.560609, 4.7705625, 3.814217, 3.803448, 3.815763, 2.88305556, 3.452833, 3.891167, 4.4941, 4.4825, 3.8834, 4.0221, 3.8084, 4.5433, 5.3139, 2.952222, 3.72449), 
                               c(50.980438, 51.022379, 51.324609, 51.335635, 51.0527, 51.0271, 51.0437, 51.02813, 50.927375, 50.941617, 51.222486, 51.217295, 51.212093, 51.350618, 50.9353, 51.26685, 51.0652694, 51.136244, 50.841182, 50.847033, 51.260389, 50.9895009, 51.260578, 51.167015, 51.15472, 51.161992, 51.05808, 51.0435, 51.088643, 51.238839, 51.051216, 51.08056, 51.19283, 50.8509999, 51.00311, 50.86514, 51.13478, 51.128779, 51.204142, 51.028222, 51.10144, 51.208354, 50.796269, 51.129473, 50.842522, 50.820251, 51.22313, 51.166678, 51.164983, 50.817338, 50.743256, 50.997417, 50.979972, 50.810032, 51.07774, 51.2010841, 51.19893, 51.207515, 50.980438, 50.84638889, 51.20875, 50.77327, 51.0317, 51.0229, 50.8844, 50.8368, 50.8631, 51.1362, 51.2312, 50.951944, 51.04573))
colnames(vlinder_locations) = c("Name", "X_WGS_GPS", "Y_WGS_GPS")

# Check if the names in the set of MOCCA and VLINDER stations that are taken into account are valid
names_check(mocca_locations, mocca_name_list)
names_check(vlinder_locations, vlinder_name_list)

# Create dataframes with the locations of the MOCCA and VLINDER stations that are taken into account
used_mocca_locations = mocca_locations[which(mocca_locations$Name %in% mocca_name_list),]
used_vlinder_locations = vlinder_locations[which(vlinder_locations$Name %in% vlinder_name_list),]

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#

# IF IT IS DESIRED TO CORRECT FOR TEMPERATURE DECLINE BASED ON TEMPERATURE MEASUREMENTS OF WEATHER STATIONS
# THAT DO NOT BELONG TO THE MOCCA OR VLINDER NETWORK, DO THE FOLLOWING:

# PUT HERE THE DIRECTORIES OF THE DATAFILES WITH THE TEMPERATURE MEASUREMENTS OF THE WEATHER STATIONS 
# THAT SHOULD BE TAKEN INTO ACCOUNT FOR THE TEMPERATURE DECLINE CORRECTION
# MAKE SURE THAT THE FILES HAVE THE FOLLOWING FORMAT: FIRST COLUMN THE DATE (IN UTC, IN FORMAT YEAR-MONTH-DAY HOUR:MINUTE:SECOND), SECOND COLUMN THE MEASURED TEMPERATURE OF THE PARTICULAR WEATHER STATION
# THE TWO COLUMNS SHOULD BE SEPARATED BY A "," SIGN WITH NO SPACINGS AND THE COLUMNS MAY NOT HAVE NAMES AT THE TOP
# ASSIGN EACH DIRECTORY TO A VARIABLE NAME



# CREATE A VECTOR OF THESE VARIABLE NAMES OF THE DIRECTORIES
station_path_list = c()
# CREATE A VECTOR WITH THE NAMES OF THE WEATHER STATIONS (NAMES SHOULD BE STRINGS)
station_name_list = c()
# MAKE SURE THAT THE ORDER OF THE NAMES CORRESPONDS TO THE ORDER OF THE DIRECTORIES

# CREATE A DATAFRAME WITH THE LOCATIONS OF THE WEATHER STATIONS
# THE FORMAT OF THE DATAFRAME SHOULD BE: (NAMES OF STATIONS, X WGS COORDINATES, Y WGS COORDINATES)
# MAKE SURE THAT THE NAMES ARE THE SAME AS IN THE 'STATION_NAME_LIST' VECTOR
locations_of_stations = data.frame(c(), c(), c())

# UNCOMMENT THIS WHEN FILLING IN THE DATAFRAME
#colnames(locations_of_stations) = c("Name", "X_WGS_GPS", "Y_WGS_GPS")


# Check if the names of the specified set of weather stations (not MOCCA or VLINDER) that are taken into account are valid
# UNCOMMENT THIS WHEN FILLING IN THE NAMES OF THE WEATHER STATIONS (NOT MOCCA OR VLINDER)
#names_check(locations_of_stations, station_name_list)

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#

# Create a dataframe with WGS84 coordinates of locations of weather stations that will be used for validating the temperatures of the temperature chart
# Create a column with the names of the weather stations as well
WGS_dataframe_loc = data.frame("X_WGS_GPS" = c(used_vlinder_locations$X_WGS_GPS), "Y_WGS_GPS" = c(used_vlinder_locations$Y_WGS_GPS), "names" = c("MELLE", "UGENT", "GENT KUNSTINSTITUUT", "GENTBRUGGE", "OOSTAKKER"))

# Convert the dataframe of WGS84 coordinates to a list of Lambert-72 coordinates of the weather stations that will be used for validating the temperatures of the temperature chart
final_locations_list_lambert = WGS_to_lambert(WGS_dataframe_loc)

# Convert the dataframe of WGS84 coordinates to a list of LAEA Europe coordinates of the weather stations that will be used for validating the temperatures of the temperature chart
final_locations_list_laea = WGS_to_laea(WGS_dataframe_loc)

# Calculate the thickness (number of gridcells) of the outer boundary of the temperature chart that will be removed
number_of_boundaries_removed = round(1000/(1.5*step_distance))
print(number_of_boundaries_removed)

extension_of_chart = extension_of_chart + step_distance*number_of_boundaries_removed


#------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------READ OUT SECTION-----------------------------------------------------#

print("Read out section")

# Create a dataframe from the measured data that is stored in a csv-file and convert the coordinates from (D)DDMM.MMMM format to numerical format
# The coordinates are also transformed from WGS84 (= GPS coordinate system) to Lambert-72 system (used in the BBK land cover map of Flanders)
dataframe_measurements_BBK = reading_data_BBK(datafile_measurements) 

# Create a dataframe from the measured data that is stored in a csv-file and convert the coordinates from (D)DDMM.MMMM format to numerical format
# The coordinates are also transformed from WGS84 (= GPS coordinate system) to LAEA Europe system (used in the ESM land cover map of Europe)
dataframe_measurements_ESM = reading_data_ESM(datafile_measurements) 


print("Data is correctly read out")

#------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------LAND COVER SECTION-------------------------------------------------------------#

print("Land cover section")


# Create circular buffers for all distances specified in the general settings (based on the BBK land cover map of Flanders)
buffers_dataframe_BBK = creating_buffers_BBK(dataframe_measurements_BBK, buffer_distances)

# Build the BBK small land cover map
print("Building the BBK small land cover map of the environment of the route")

small_landcover_map_BBK = creating_small_landcover_map_BBK(BBK_directory, buffers_dataframe_BBK, buffer_distances)

# Lower the resolution of the small land cover map
small_landcover_map_low_resolution_BBK = aggregate_blocks(small_landcover_map_BBK, res_factor=10)
dataframe_landcover_map_BBK = as.data.frame(small_landcover_map_low_resolution_BBK, xy = TRUE)

print("The small land cover map is built")
print("Calculating the land cover fractions of the measured datapoints (based on the BBK land cover map of Flanders)")

# Calculate the land cover fractions for all measured datapoints (based on the BBK land cover map)
landcover_dataframe_BBK = calculate_landcover_BBK(buffers_dataframe_BBK, buffer_distances, small_landcover_map_BBK, cores)

# Calculate the green and impervious land cover fractions for all measured datapoints
landcover_dataframe_BBK = calculate_combined_landcover(landcover_dataframe_BBK, buffer_distances)

print("Land cover fractions are calculated")

# Create circular buffers for all distances specified in the general settings (based on the ESM land cover map of Europe)
buffers_dataframe_ESM = creating_buffers_ESM(dataframe_measurements_ESM, buffer_distances)

# Build the ESM small land cover map
print("Building the ESM small land cover map of the environment of the route")

small_landcover_map_ESM = creating_small_landcover_map_ESM(ESM_directory, buffers_dataframe_ESM, buffer_distances)

# Lower the resolution of the small land cover map
small_landcover_map_low_resolution_ESM = aggregate_blocks(small_landcover_map_ESM, res_factor=10)
dataframe_landcover_map_ESM = as.data.frame(small_landcover_map_low_resolution_ESM, xy = TRUE)

print("The small land cover map is built")
print("Calculating the land cover fractions of the measured datapoints (based on the ESM land cover map of Europe)")

# Calculate the land cover fractions for all measured datapoints (based on the ESM land cover map)
landcover_dataframe_ESM = calculate_landcover_ESM(buffers_dataframe_ESM, buffer_distances, small_landcover_map_ESM, cores)

print("Land cover fractions are calculated")


#------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------VISUALISATION OF ROUTE------------------------------------------------------#

if (BBK){
  print("Visualising the route on the BBK land cover map of Flanders")
  
  visualisation_route_measured_temp_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, FALSE, FALSE)
  visualisation_route_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK)
  print("The route is visualised")
}

if (ESM){
  # Manipulate the dataframe of the ESM small land cover map such that it is ready to plot
  dataframe_landcover_map_ESM = dataframe_landcover_map_ESM_setup(dataframe_landcover_map_ESM)
  
  print("Visualising the route on the ESM land cover map of Europe")
  
  visualisation_route_measured_temp_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, FALSE, FALSE)
  visualisation_route_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM)
  print("The route is visualised")
}

#--------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------TEMPERATURE CORRECTION SECTION----------------------------------------------------#

# Apply a correction for thermal inertia if selected
if (temp_inertia_bool){
  
  print("Thermal inertia section")
  
  if (BBK){
    # Specify the degrees of freedom used for smoothing of the temperature derivative (for thermal inertia correction)
    degrees_of_freedom = round(NROW(landcover_dataframe_BBK)/3)
    
    # Correct the temperatures for thermal inertia (for the BBK land cover map of Flanders)
    print("Correcting temperature for thermal inertia (for the BBK land cover map of Flanders)")
    thermal_inertia_corrected_temp = thermal_inertia_correction(landcover_dataframe_BBK, inertiafiles_directory, degrees_of_freedom)
    landcover_dataframe_BBK$measured_temp = landcover_dataframe_BBK$temperature
    landcover_dataframe_BBK$temperature = thermal_inertia_corrected_temp
    landcover_dataframe_BBK$thermal_inertia = landcover_dataframe_BBK$temperature
    
    visualisation_route_measured_temp_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, TRUE, FALSE)
    visualisation_route_thermal_inertia_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, FALSE)
  }
  
  if (ESM){
    # Specify the degrees of freedom used for smoothing of the temperature derivative (for thermal inertia correction)
    degrees_of_freedom = round(NROW(landcover_dataframe_ESM)/3)
    
    # Correct the temperatures for thermal inertia (for the ESM land cover map of Europe)
    print("Correcting temperature for thermal inertia (for the ESM land cover map of Europe)")
    thermal_inertia_corrected_temp = thermal_inertia_correction(landcover_dataframe_ESM, inertiafiles_directory, degrees_of_freedom)
    landcover_dataframe_ESM$measured_temp = landcover_dataframe_ESM$temperature
    landcover_dataframe_ESM$temperature = thermal_inertia_corrected_temp
    landcover_dataframe_ESM$thermal_inertia = landcover_dataframe_ESM$temperature
    
    visualisation_route_measured_temp_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, TRUE, FALSE)
    visualisation_route_thermal_inertia_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, FALSE)
  }
  
  print("Temperature is corrected for thermal inertia")
}

# Apply a correction for temperature decline if selected
if(temp_decline_bool){
  
  print("Temperature decline section")
  
  if (vlinder_decline == TRUE | mocca_decline == TRUE){
    
    if (BBK){
      print("Calculating the land cover fractions of the MOCCA stations (based on the BBK land cover map of Flanders)")
      
      # Calculate the land cover fractions for the MOCCA stations that are of interest based on the BBK land cover map of Flanders
      mocca_landcover_BBK = landcover_mocca_BBK(buffer_distances, used_mocca_locations, BBK_directory, cores) 
      mocca_landcover_BBK = calculate_combined_landcover(mocca_landcover_BBK, buffer_distances) 
      
      print("The land cover fractions of the MOCCA stations are calculated")
      
      print("Calculating the land cover fractions of the VLINDER stations (based on the BBK land cover map of Flanders)")
      
      # Calculate the land cover fractions for the VLINDER stations that are of interest based on the BBK land cover map of Flanders
      vlinder_landcover_BBK = landcover_vlinder_BBK(buffer_distances, used_vlinder_locations, BBK_directory, cores)
      vlinder_landcover_BBK = calculate_combined_landcover(vlinder_landcover_BBK, buffer_distances) 
      
      print("The land cover fractions of the VLINDER stations are calculated")
    }
    
    if (ESM){
      print("Calculating the land cover fractions of the MOCCA stations (based on the ESM land cover map of Europe)")
      
      # Calculate the land cover fractions for the MOCCA stations that are of interest based on the ESM land cover map of Europe
      mocca_landcover_ESM = landcover_mocca_ESM(buffer_distances, used_mocca_locations, ESM_directory, cores) 
      
      print("The land cover fractions of the MOCCA stations are calculated")
      
      print("Calculating the land cover fractions of the VLINDER stations (based on the ESM land cover map of Europe)")
      
      # Calculate the land cover fractions for the VLINDER stations that are of interest based on the ESM land cover map of Europe
      vlinder_landcover_ESM = landcover_vlinder_ESM(buffer_distances, used_vlinder_locations, ESM_directory, cores)
      
      print("The land cover fractions of the VLINDER stations are calculated")
    }
  }
  
  if (station_decline){
    if (BBK){
      print("Calculating the land cover fractions of the specified set of weather stations (not MOCCA or VLINDER) based on the BBK land cover map of Flanders")
      
      # Calculate the land cover fractions for the specified set of weather stations (not MOCCA or VLINDER) based on the BBK land cover map of Flanders
      station_landcover_BBK = landcover_weather_stations_BBK(buffer_distances, locations_of_stations, BBK_directory, cores)
      station_landcover_BBK = calculate_combined_landcover(station_landcover_BBK, buffer_distances) 
      
      print("The land cover fractions of the weather stations are calculated")
    }
    
    if (ESM){
      print("Calculating the land cover fractions of the specified set of weather stations (not MOCCA or VLINDER) based on the ESM land cover map of Europe")
      
      # Calculate the land cover fractions for the specified set of weather stations (not MOCCA or VLINDER) based on the ESM land cover map of Europe
      station_landcover_ESM = landcover_weather_stations_ESM(buffer_distances, locations_of_stations, ESM_directory, cores)
      
      print("The land cover fractions of the weather stations are calculated")
    }
  }
  
  if (vlinder_chart == TRUE | mocca_chart == TRUE | station_chart == TRUE){
    
    # Specify the number of gridpoints in the horizontal and vertical directions
    number_horizontal_steps = ceil((max(landcover_dataframe_BBK$X) - min(landcover_dataframe_BBK$X) + (2*extension_of_chart))/step_distance)
    number_vertical_steps = ceil((max(landcover_dataframe_BBK$Y) - min(landcover_dataframe_BBK$Y) + (2*extension_of_chart))/step_distance)
    
    print(number_horizontal_steps)
    print(number_vertical_steps)
    # Calculate the model variables for the gridpoints of the temperature chart
    # The land cover fractions of the gridpoints of the temperature chart are calculated based on the BBK land cover map of Flanders
    # The indices of the gridpoints where the weather stations used for validation of the chart temperatures are located are calculated as well
    chart_variables_loc_indices_BBK = chart_variables_and_locations(landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, step_distance, extension_of_chart, buffer_distances_chart, final_locations_list_lambert, WGS_dataframe_loc$names, model_variables, "BBK")
    chart_variables_BBK = chart_variables_loc_indices_BBK$chart_variables
    indices_of_locations_BBK = chart_variables_loc_indices_BBK$indices_of_locations
    
    # Create a dataframe with the Lambert-72 coordinates of the gridpoints of the temperature chart
    chart_dataframe_BBK = chart_variables_loc_indices_BBK$chart_dataframe
    
    stations_validation_names_BBK = chart_variables_loc_indices_BBK$validation_names
    stations_validation_names_BBK = as.character(stations_validation_names_BBK)
    remove_locations_boundary(indices_of_locations_BBK, number_horizontal_steps, number_vertical_steps, number_of_boundaries_removed, stations_validation_names_BBK)
    print(class(stations_validation_names_BBK))
    print(stations_validation_names_BBK)
    
    # Calculate the model variables for the gridpoints of the temperature chart
    # The land cover fractions of the gridpoints of the temperature chart are calculated based on the ESM land cover map of Europe
    # The indices of the gridpoints where the weather stations used for validation of the chart temperatures are located are calculated as well
    chart_variables_loc_indices_ESM = chart_variables_and_locations(landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, step_distance, extension_of_chart, buffer_distances_chart, final_locations_list_laea, WGS_dataframe_loc$names, model_variables, "ESM")
    chart_variables_ESM = chart_variables_loc_indices_ESM$chart_variables
    indices_of_locations_ESM = chart_variables_loc_indices_ESM$indices_of_locations
    
    # Create a dataframe with the LAEA Europe coordinates of the gridpoints of the temperature chart
    chart_dataframe_ESM = chart_variables_loc_indices_ESM$chart_dataframe
    
    stations_validation_names_ESM = chart_variables_loc_indices_ESM$validation_names
    stations_validation_names_ESM = as.character(stations_validation_names_ESM)
    print(class(stations_validation_names_ESM))
    print(stations_validation_names_ESM)
    
  }
  if (mocca_decline){
    
    if (BBK){
      print("Correcting the temperatures based on the MOCCA stations (based on the BBK land cover map of Flanders)")
      
      # Calculate the temperature residuals and smoothed temperature residuals for the specified MOCCA stations 
      mocca_smoothed_residual_dataframe = mocca_smoothed_residuals(landcover_dataframe_BBK, mocca_path_list, mocca_name_list, 1)
      # Plot the temperature residuals and smoothed temperature residuals of the specified MOCCA stations
      residu_plot_mocca(mocca_smoothed_residual_dataframe, mocca_name_list)
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of VLINDER stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corrected_temperatures_vlinder_BBK = temp_decline_corr_vlinder(landcover_dataframe_BBK, vlinder_landcover_BBK, vlinder_path_list, vlinder_name_list, buffer_distances, 1, FALSE, "BBK")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of MOCCA stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corrected_temperatures_mocca_BBK = temp_decline_corr_mocca(landcover_dataframe_BBK, mocca_landcover_BBK, mocca_path_list, mocca_name_list, buffer_distances, 1, TRUE, "BBK")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of MOCCA stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are not smoothed
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corr_temp_mocca_no_smoothing_BBK = temp_decline_corr_mocca_no_smoothing(landcover_dataframe_BBK, mocca_landcover_BBK, mocca_path_list, mocca_name_list, buffer_distances, 1)
      
      # Create a dataframe with: the date of the measurements, the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of MOCCA stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature and the temperatures corrected for thermal inertia but not corrected for temperature decline
      # The temperature decline correction is based on the BBK land cover map of Flanders
      dataframe_temperatures_mocca_BBK = data.frame("date" = landcover_dataframe_BBK$fulldate, "vlinder_corrected" = corrected_temperatures_vlinder_BBK, "mocca_corrected_no_smoothing" = corr_temp_mocca_no_smoothing_BBK, "mocca_corrected" = corrected_temperatures_mocca_BBK, "measured" = landcover_dataframe_BBK$measured_temp, "inertia_corrected" = landcover_dataframe_BBK$thermal_inertia)
      
      # Create a plot of the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of MOCCA stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (MOCCA corrected) predicted by a linear model with as variables the model variables
      # The temperature decline correction is based on the BBK land cover map of Flanders
      dataframe_temperatures_mocca_BBK = plot_temp_evolutions_mocca(landcover_dataframe_BBK, dataframe_temperatures_mocca_BBK, model_variables, corrected_temperatures_mocca_BBK, "BBK")
      
      # Calculate the temperatures corrected for temperature decline at different times in the time range of the measurements
      # The begin time of the measurements is always included and the time interval is specified in the general settings by the 'seconds_per_interval' variable
      # These will also be the times for which temperature charts will be made. The temperature decline correction is based on the set of MOCCA stations
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corr_temp_timeseries_mocca_BBK = temp_decline_timeseries_mocca(landcover_dataframe_BBK, seconds_per_interval, mocca_landcover_BBK, mocca_path_list, mocca_name_list, buffer_distances, "BBK")
      plot_decline_timeseries(corr_temp_timeseries_mocca_BBK, landcover_dataframe_BBK, "MOCCA")
      print("The temperatures are corrected for temperature decline based on the MOCCA stations")
    }
    
    if (ESM){
      print("Correcting the temperatures based on the MOCCA stations (based on the ESM land cover map of Europe)")
      
      # Calculate the temperature residuals and smoothed temperature residuals for the specified MOCCA stations 
      mocca_smoothed_residual_dataframe = mocca_smoothed_residuals(landcover_dataframe_ESM, mocca_path_list, mocca_name_list, 1)
      # Plot the temperature residuals and smoothed temperature residuals of the specified MOCCA stations
      residu_plot_mocca(mocca_smoothed_residual_dataframe, mocca_name_list)
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of VLINDER stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the ESM land cover map of Europe
      corrected_temperatures_vlinder_ESM = temp_decline_corr_vlinder(landcover_dataframe_ESM, vlinder_landcover_ESM, vlinder_path_list, vlinder_name_list, buffer_distances, 1, FALSE, "ESM")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of MOCCA stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the ESM land cover map of Europe
      corrected_temperatures_mocca_ESM = temp_decline_corr_mocca(landcover_dataframe_ESM, mocca_landcover_ESM, mocca_path_list, mocca_name_list, buffer_distances, 1, TRUE, "ESM")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of MOCCA stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are not smoothed
      # The temperature decline correction is based on the ESM land cover map of Europe
      corr_temp_mocca_no_smoothing_ESM = temp_decline_corr_mocca_no_smoothing(landcover_dataframe_ESM, mocca_landcover_ESM, mocca_path_list, mocca_name_list, buffer_distances, 1)
      
      # Create a dataframe with: the date of the measurements, the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of MOCCA stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature and the temperatures corrected for thermal inertia but not corrected for temperature decline
      # The temperature decline correction is based on the ESM land cover map of Europe
      dataframe_temperatures_mocca_ESM = data.frame("date" = landcover_dataframe_ESM$fulldate, "vlinder_corrected" = corrected_temperatures_vlinder_ESM, "mocca_corrected_no_smoothing" = corr_temp_mocca_no_smoothing_ESM, "mocca_corrected" = corrected_temperatures_mocca_ESM, "measured" = landcover_dataframe_ESM$measured_temp, "inertia_corrected" = landcover_dataframe_ESM$thermal_inertia)
      
      # Create a plot of the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of MOCCA stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (MOCCA corrected) predicted by a linear model with as variables the model variables
      # The temperature decline correction is based on the ESM land cover map of Europe
      dataframe_temperatures_mocca_ESM = plot_temp_evolutions_mocca(landcover_dataframe_ESM, dataframe_temperatures_mocca_ESM, model_variables, corrected_temperatures_mocca_ESM, "ESM")
      
      # Calculate the temperatures corrected for temperature decline at different times in the time range of the measurements
      # The begin time of the measurements is always included and the time interval is specified in the general settings by the 'seconds_per_interval' variable
      # These will also be the times for which temperature charts will be made. The temperature decline correction is based on the set of MOCCA stations
      # The temperature decline correction is based on the ESM land cover map of Europe
      corr_temp_timeseries_mocca_ESM = temp_decline_timeseries_mocca(landcover_dataframe_ESM, seconds_per_interval, mocca_landcover_ESM, mocca_path_list, mocca_name_list, buffer_distances, "ESM")
      
      print("the temperatures are corrected for temperature decline based on the MOCCA stations")
    }
    
    if (mocca_chart){
      
      if (BBK){
        print("Creating temperature charts based on temperatures corrected for temperature decline based on the MOCCA stations")
        print("The temperature decline correction is based on the BBK land cover map of Flanders")
        
        # Create temperature charts of the environment of the route that is used for the mobile bicycle measurement campaign 
        # The measurements of this campaign are used as input for the linear model with which the charts are made
        # The charts are made at specific reference times. The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The chart is made with as input the measured temperatures corrected for temperature decline based on the set of MOCCA stations and based on the BBK land cover map of Flanders
        charts_data_mocca_BBK = chart_timeseries(landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, model_variables, step_distance, corr_temp_timeseries_mocca_BBK, chart_variables_BBK)
        chart_data_BBK = create_temp_chart(charts_data_mocca_BBK, number_horizontal_steps, number_vertical_steps, seconds_per_interval, landcover_dataframe_BBK, step_distance, number_of_boundaries_removed, indices_of_locations_BBK, stations_validation_names_BBK, "mocca", "BBK", rotate)
        
        print("Creating temperature charts with the temperature residuals (begin temperature - temperature)")
        # Create temperature charts with the temperature residuals (begin temperature - temperature) for the different temperature charts
        create_chart_temp_res(chart_data_BBK, landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, seconds_per_interval, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, stations_validation_names_ESM, stations_validation_names_BBK, "mocca", "BBK", rotate)
        
        print("The temperature charts are created")
      }
      
      if (ESM){
        print("Creating temperature charts based on temperatures corrected for temperature decline based on the MOCCA stations")
        print("The temperature decline correction is based on the ESM land cover map of Europe")
        
        # Create temperature charts of the environment of the route that is used for the mobile bicycle measurement campaign 
        # The measurements of this campaign are used as input for the linear model with which the charts are made
        # The charts are made at specific reference times. The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The chart is made with as input the measured temperatures corrected for temperature decline based on the set of MOCCA stations and based on the ESM land cover map of Europe
        charts_data_mocca_ESM = chart_timeseries(landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, model_variables, step_distance, corr_temp_timeseries_mocca_ESM, chart_variables_ESM)
        chart_data_ESM = create_temp_chart_ESM(charts_data_mocca_ESM, landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, chart_dataframe_ESM, chart_dataframe_BBK, seconds_per_interval, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, step_distance, stations_validation_names_ESM, stations_validation_names_BBK, "mocca", rotate)
        
        print("Creating temperature charts with the temperature residuals (begin temperature - temperature)")
        # Create temperature charts with the temperature residuals (begin temperature - temperature) for the different temperature charts
        create_chart_temp_res(chart_data_ESM, landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, seconds_per_interval, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, stations_validation_names_ESM, stations_validation_names_BBK, "mocca", "ESM", rotate)
        
        print("The temperature charts are created")
      }
      
      if (ESM == TRUE & BBK == TRUE & rotate == TRUE){
        print("Creating temperature charts with the temperature differences between the BBK temperature chart and the ESM temperature chart (projected to the BBK temperature chart domain by bilinear interpollation)")
        
        # Create temperature charts with the temperature differences between the BBK temperature chart and the ESM temperature chart (projected to the BBK temperature chart domain by bilinear interpollation)
        create_diff_chart_BBK_ESM(chart_data_BBK, chart_data_ESM, chart_dataframe_BBK, chart_dataframe_ESM, landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, step_distance, seconds_per_interval, number_of_boundaries_removed, indices_of_locations_BBK, stations_validation_names_BBK, "mocca")
        
        print("The temperature charts are created")
      }
      
    }
    
    if (mocca_visualisation){
      
      if (BBK){
        print("Visualising the temperatures of the route corrected for temperature decline (MOCCA based) at different times")
        print("The temperature decline correction is based on the BBK land cover map of Flanders")
        
        visualisation_route_measured_temp_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, TRUE, TRUE, corr_temp_timeseries_mocca_BBK)
        visualisation_route_thermal_inertia_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, TRUE, corr_temp_timeseries_mocca_BBK)
        
        # Visualise the temperatures of the route corrected for temperature decline 
        # Different temperature decline corrections are used based on different reference datapoints
        # The result is a series of plots of the temperatures of the route at different reference times
        # The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The temperature decline correction is based on the set of MOCCA stations and based on the BBK land cover map of Flanders
        visualisation_route_timeseries_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, seconds_per_interval, corr_temp_timeseries_mocca_BBK, "mocca")
        
        print("The visualisation is done")
      }
      
      if (ESM){
        print("Visualising the temperatures of the route corrected for temperature decline (MOCCA based) at different times")
        print("The temperature decline correction is based on the ESM land cover map of Europe")
        
        visualisation_route_measured_temp_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, TRUE, TRUE, corr_temp_timeseries_mocca_ESM)
        visualisation_route_thermal_inertia_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, TRUE, corr_temp_timeseries_mocca_ESM)
        
        # Visualise the temperatures of the route corrected for temperature decline 
        # Different temperature decline corrections are used based on different reference datapoints
        # The result is a series of plots of the temperatures of the route at different reference times
        # The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The temperature decline correction is based on the set of MOCCA stations and based on the ESM land cover map of Europe
        visualisation_route_timeseries_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, seconds_per_interval, corr_temp_timeseries_mocca_ESM, "mocca")
        
        print("The visualisation is done")
      }
    }
  }
  
  if (vlinder_decline){
    
    if (BBK){
      print("Correcting the temperatures based on the VLINDER stations (based on the BBK land cover map of Flanders)")
      
      # Calculate the temperature residuals and smoothed temperature residuals for the specified VLINDER stations 
      vlinder_smoothed_residual_dataframe = vlinder_smoothed_residuals(landcover_dataframe_BBK, vlinder_path_list, vlinder_name_list, 1)
      # Plot the temperature residuals and smoothed temperature residuals of the specified MOCCA stations
      residu_plot_vlinder(vlinder_smoothed_residual_dataframe, vlinder_name_list)
      
      # Correct the temperature for temperature decline when one VLINDER station is removed from the specified set of VLINDER stations
      # Each station is removed alternately
      # The temperature decline correction is based on the BBK land cover map of Flanders
      dataframe_corr_temperatures_one_BBK = one_station_removed_vlinder(landcover_dataframe_BBK, vlinder_name_list, vlinder_path_list, vlinder_landcover_BBK, buffer_distances, "BBK")
      
      # Correct the temperature for temperature decline when multiple VLINDER stations are removed from the specified set of VLINDER stations
      # Each set of two stations is removed alternately
      # The temperature decline correction is based on the BBK land cover map of Flanders
      dataframe_corr_temperatures_two_BBK = multiple_stations_removed_vlinder(landcover_dataframe_BBK, vlinder_name_list, vlinder_path_list, vlinder_landcover_BBK, buffer_distances, 2, "BBK")
      
      # Create a plot of the evolution of the measured temperature, the temperature corrected for thermal inertia, the temperature corrected for temperature decline based on the whole set of VLINDER stations
      # and the temperature corrected for temperature decline when one VLINDER station is removed from the whole set of stations
      # The temperature decline correction is based on the BBK land cover map of Flanders
      plot_corr_temp_stations_removed_vlinder(landcover_dataframe_BBK, vlinder_landcover_BBK, vlinder_path_list, vlinder_name_list, buffer_distances, dataframe_corr_temperatures_one_BBK, 1, "BBK")
      
      # Create a plot of the evolution of the measured temperature, the temperature corrected for thermal inertia, the temperature corrected for temperature decline based on the whole set of VLINDER stations
      # and the temperature corrected for temperature decline when two VLINDER stations are removed from the whole set of stations
      # The temperature decline correction is based onthe BBK land cover map of Flanders
      plot_corr_temp_stations_removed_vlinder(landcover_dataframe_BBK, vlinder_landcover_BBK, vlinder_path_list, vlinder_name_list, buffer_distances, dataframe_corr_temperatures_two_BBK, 2, "BBK")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of VLINDER stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corrected_temperatures_vlinder_BBK = temp_decline_corr_vlinder(landcover_dataframe_BBK, vlinder_landcover_BBK, vlinder_path_list, vlinder_name_list, buffer_distances, 1, TRUE, "BBK")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of VLINDER stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are not smoothed
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corr_temp_vlinder_no_smoothing_BBK = temp_decline_corr_vlinder_no_smoothing(landcover_dataframe_BBK, vlinder_landcover_BBK, vlinder_path_list, vlinder_name_list, buffer_distances, 1)
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of MOCCA stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corrected_temperatures_mocca_BBK = temp_decline_corr_mocca(landcover_dataframe_BBK, mocca_landcover_BBK, mocca_path_list, mocca_name_list, buffer_distances, 1, FALSE, "BBK")
      
      # Create a dataframe with: the date of the measurements, the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of VLINDER stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature and the temperatures corrected for thermal inertia but not corrected for temperature decline
      # The temperature decline correction is based on the BBK land cover map of Flanders
      dataframe_temperatures_vlinder_BBK = data.frame("date" = landcover_dataframe_BBK$fulldate, "vlinder_corrected" = corrected_temperatures_vlinder_BBK, "vlinder_corrected_no_smoothing" = corr_temp_vlinder_no_smoothing_BBK, "mocca_corrected" = corrected_temperatures_mocca_BBK, "measured" = landcover_dataframe_BBK$measured_temp, "inertia_corrected" = landcover_dataframe_BBK$thermal_inertia)
      
      # Create a plot of the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of VLINDER stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (VLINDER corrected) predicted by a linear model with as variables the model variables
      # The temperature decline correction is based on the BBK land cover map of Flanders
      dataframe_temperatures_vlinder_BBK = plot_temp_evolutions_vlinder(landcover_dataframe_BBK, dataframe_temperatures_vlinder_BBK, model_variables, corrected_temperatures_vlinder_BBK, "BBK")
      
      # Calculate the temperatures corrected for temperature decline at different times in the time range of the measurements
      # The begin time of the measurements is always included and the time interval is specified in the general settings by the 'seconds_per_interval' variable
      # These will also be the times for which temperature charts will be made. The temperature decline correction is based on the set of VLINDER stations
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corr_temp_timeseries_vlinder_BBK = temp_decline_timeseries_vlinder(landcover_dataframe_BBK, seconds_per_interval, vlinder_landcover_BBK, vlinder_path_list, vlinder_name_list, buffer_distances, "BBK")
      plot_decline_timeseries(corr_temp_timeseries_vlinder_BBK, landcover_dataframe_BBK, "VLINDER")
      print("The temperatures are corrected for temperature decline based on the VLINDER stations")
    }
    
    if (ESM){
      print("Correcting the temperatures based on the VLINDER stations (based on the ESM land cover map of Europe)")
      
      # Calculate the temperature residuals and smoothed temperature residuals for the specified VLINDER stations 
      vlinder_smoothed_residual_dataframe = vlinder_smoothed_residuals(landcover_dataframe_ESM, vlinder_path_list, vlinder_name_list, 1)
      # Plot the temperature residuals and smoothed temperature residuals of the specified MOCCA stations
      residu_plot_vlinder(vlinder_smoothed_residual_dataframe, vlinder_name_list)
      
      # Correct the temperature for temperature decline when one VLINDER station is removed from the specified set of VLINDER stations
      # Each station is removed alternately
      # The temperature decline correction is based on the ESM land cover map of Europe
      dataframe_corr_temperatures_one_ESM = one_station_removed_vlinder(landcover_dataframe_ESM, vlinder_name_list, vlinder_path_list, vlinder_landcover_ESM, buffer_distances, "ESM")
      
      # Correct the temperature for temperature decline when multiple VLINDER stations are removed from the specified set of VLINDER stations
      # Each set of two stations is removed alternately
      # The temperature decline correction is based on the ESM land cover map of Europe
      dataframe_corr_temperatures_two_ESM = multiple_stations_removed_vlinder(landcover_dataframe_ESM, vlinder_name_list, vlinder_path_list, vlinder_landcover_ESM, buffer_distances, 2, "ESM")
      
      # Create a plot of the evolution of the measured temperature, the temperature corrected for thermal inertia, the temperature corrected for temperature decline based on the whole set of VLINDER stations
      # and the temperature corrected for temperature decline when one VLINDER station is removed from the whole set of stations
      # The temperature decline correction is based on the ESM land cover map of Europe
      plot_corr_temp_stations_removed_vlinder(landcover_dataframe_ESM, vlinder_landcover_ESM, vlinder_path_list, vlinder_name_list, buffer_distances, dataframe_corr_temperatures_one_ESM, 1, "ESM")
      
      # Create a plot of the evolution of the measured temperature, the temperature corrected for thermal inertia, the temperature corrected for temperature decline based on the whole set of VLINDER stations
      # and the temperature corrected for temperature decline when two VLINDER stations are removed from the whole set of stations
      # The temperature decline correction is based on the ESM land cover map of Europe
      plot_corr_temp_stations_removed_vlinder(landcover_dataframe_ESM, vlinder_landcover_ESM, vlinder_path_list, vlinder_name_list, buffer_distances, dataframe_corr_temperatures_two_ESM, 2, "ESM")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of VLINDER stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the ESM land cover map of Europe
      corrected_temperatures_vlinder_ESM = temp_decline_corr_vlinder(landcover_dataframe_ESM, vlinder_landcover_ESM, vlinder_path_list, vlinder_name_list, buffer_distances, 1, TRUE, "ESM")
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of VLINDER stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are not smoothed
      # The temperature decline correction is based on the ESM land cover map of Europe
      corr_temp_vlinder_no_smoothing_ESM = temp_decline_corr_vlinder_no_smoothing(landcover_dataframe_ESM, vlinder_landcover_ESM, vlinder_path_list, vlinder_name_list, buffer_distances, 1)
      
      # Create a vector of temperatures that are corrected for temperature decline based on the specified set of MOCCA stations
      # The temperatures are refered to the begin time of the measurements and the final temperature residuals are smoothed
      # The temperature decline correction is based on the ESM land cover map of Europe
      corrected_temperatures_mocca_ESM = temp_decline_corr_mocca(landcover_dataframe_ESM, mocca_landcover_ESM, mocca_path_list, mocca_name_list, buffer_distances, 1, FALSE, "ESM")
      
      # Create a dataframe with: the date of the measurements, the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of VLINDER stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature and the temperatures corrected for thermal inertia but not corrected for temperature decline
      # The temperature decline correction is based on the ESM land cover map of Europe
      dataframe_temperatures_vlinder_ESM = data.frame("date" = landcover_dataframe_ESM$fulldate, "vlinder_corrected" = corrected_temperatures_vlinder_ESM, "vlinder_corrected_no_smoothing" = corr_temp_vlinder_no_smoothing_ESM, "mocca_corrected" = corrected_temperatures_mocca_ESM, "measured" = landcover_dataframe_ESM$measured_temp, "inertia_corrected" = landcover_dataframe_ESM$thermal_inertia)
      
      # Create a plot of the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
      # the temperatures corrected for temperature decline based on the set of VLINDER stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
      # the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (VLINDER corrected) predicted by a linear model with as variables the model variables
      # The temperature decline correction is based on the ESM land cover map of Europe
      dataframe_temperatures_vlinder_ESM = plot_temp_evolutions_vlinder(landcover_dataframe_ESM, dataframe_temperatures_vlinder_ESM, model_variables, corrected_temperatures_vlinder_ESM, "ESM")
      
      # Calculate the temperatures corrected for temperature decline at different times in the time range of the measurements
      # The begin time of the measurements is always included and the time interval is specified in the general settings by the 'seconds_per_interval' variable
      # These will also be the times for which temperature charts will be made. The temperature decline correction is based on the set of VLINDER stations
      # The temperature decline correction is based on the ESM land cover map of Europe
      corr_temp_timeseries_vlinder_ESM = temp_decline_timeseries_vlinder(landcover_dataframe_ESM, seconds_per_interval, vlinder_landcover_ESM, vlinder_path_list, vlinder_name_list, buffer_distances, "ESM")
      
      print("The temperatures are corrected for temperature decline based on the VLINDER stations")
    }
    
    if (vlinder_chart){
      
      if (BBK){
        print("Creating temperature charts based on temperatures corrected for temperature decline based on the VLINDER stations")
        print("The temperature decline correction is based on the BBK land cover map of Flanders")
        
        # Create temperature charts of the environment of the route that is used for the mobile bicycle measurement campaign 
        # The measurements of this campaign are used as input for the linear model with which the charts are made
        # The charts are made at specific reference times. The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The chart is made with as input the measured temperatures corrected for temperature decline based on the set of VLINDER stations and based on the BBK land cover map of Flanders
        charts_data_vlinder_BBK = chart_timeseries(landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, model_variables, step_distance, corr_temp_timeseries_vlinder_BBK, chart_variables_BBK)
        chart_data_BBK = create_temp_chart(charts_data_vlinder_BBK, number_horizontal_steps, number_vertical_steps, seconds_per_interval, landcover_dataframe_BBK, step_distance, number_of_boundaries_removed, indices_of_locations_BBK, stations_validation_names_BBK, "vlinder", "BBK", rotate)
        
        print("Creating temperature charts with the temperature residuals (begin temperature - temperature)")
        # Create temperature charts with the temperature residuals (begin temperature - temperature) for the different temperature charts
        create_chart_temp_res(chart_data_BBK, landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, seconds_per_interval, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, stations_validation_names_ESM, stations_validation_names_BBK, "vlinder", "BBK", rotate)
        
        print("The temperature charts are created")
      }
      
      if (ESM){
        print("Creating temperature charts based on temperatures corrected for temperature decline based on the VLINDER stations")
        print("The temperature decline correction is based on the ESM land cover map of Europe")
        
        # Create temperature charts of the environment of the route that is used for the mobile bicycle measurement campaign 
        # The measurements of this campaign are used as input for the linear model with which the charts are made
        # The charts are made at specific reference times. The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The chart is made with as input the measured temperatures corrected for temperature decline based on the set of VLINDER stations and based on the ESM land cover map of Europe
        charts_data_vlinder_ESM = chart_timeseries(landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, model_variables, step_distance, corr_temp_timeseries_vlinder_ESM, chart_variables_ESM)
        chart_data_ESM = create_temp_chart_ESM(charts_data_vlinder_ESM, landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, chart_dataframe_ESM, chart_dataframe_BBK, seconds_per_interval, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, step_distance, stations_validation_names_ESM, stations_validation_names_BBK, "vlinder", rotate)
        
        print("Creating temperature charts with the temperature residuals (begin temperature - temperature)")
        # Create temperature charts with the temperature residuals (begin temperature - temperature) for the different temperature charts
        create_chart_temp_res(chart_data_ESM, landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, seconds_per_interval, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, stations_validation_names_ESM, stations_validation_names_BBK, "vlinder", "ESM", rotate)
        
        print("The temperature charts are created")
      }
      
      if (ESM == TRUE & BBK == TRUE & rotate == TRUE){
        print("Creating temperature charts with the temperature differences between the BBK temperature chart and the ESM temperature chart (projected to the BBK temperature chart domain by bilinear interpollation)")
        
        # Create temperature charts with the temperature differences between the BBK temperature chart and the ESM temperature chart (projected to the BBK temperature chart domain by bilinear interpollation)
        create_diff_chart_BBK_ESM(chart_data_BBK, chart_data_ESM, chart_dataframe_BBK, chart_dataframe_ESM, landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, step_distance, seconds_per_interval, number_of_boundaries_removed, indices_of_locations_BBK, stations_validation_names_BBK, "vlinder")
        
        print("The temperature charts are created")
      }
      
    }
    
    if (vlinder_visualisation){
      
      if (BBK){
        print("Visualising the temperatures of the route corrected for temperature decline (VLINDER based) at different times")
        print("The temperature decline correction is based on the BBK land cover map of Flanders")
        
        visualisation_route_measured_temp_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, TRUE, TRUE, corr_temp_timeseries_vlinder_BBK)
        visualisation_route_thermal_inertia_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, TRUE, corr_temp_timeseries_vlinder_BBK)
        
        # Visualise the temperatures of the route corrected for temperature decline 
        # Different temperature decline corrections are used based on different reference datapoints
        # The result is a series of plots of the temperatures of the route at different reference times
        # The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The temperature decline correction is based on the set of VLINDER stations and based on the BBK land cover map of Flanders
        visualisation_route_timeseries_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, seconds_per_interval, corr_temp_timeseries_vlinder_BBK, "vlinder")
        
        print("The visualisation is done")
      }
      
      if (ESM){
        print("Visualising the temperatures of the route corrected for temperature decline (VLINDER based) at different times")
        print("The temperature decline correction is based on the ESM land cover map of Europe")
        
        visualisation_route_measured_temp_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, TRUE, TRUE, corr_temp_timeseries_vlinder_ESM)
        visualisation_route_thermal_inertia_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, TRUE, corr_temp_timeseries_vlinder_ESM)
        
        # Visualise the temperatures of the route corrected for temperature decline 
        # Different temperature decline corrections are used based on different reference datapoints
        # The result is a series of plots of the temperatures of the route at different reference times
        # The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The temperature decline correction is based on the set of VLINDER stations and based on the ESM land cover map of Europe
        visualisation_route_timeseries_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, seconds_per_interval, corr_temp_timeseries_vlinder_ESM, "vlinder")
        
        print("The visualisation is done")
      }
    }
  }
  
  if (station_decline){
    
    if (BBK){
      print("Correcting the temperatures based on the specified set of weather stations (not MOCCA or VLINDER) (based on the BBK land cover map of Flanders)")
      
      # Calculate the temperature residuals and smoothed temperature residuals for the specified set of weather stations (not MOCCA or VLINDER)
      station_smoothed_residual_dataframe = station_smoothed_residuals(landcover_dataframe_BBK, station_path_list, station_name_list, 1)
      # Plot the temperature residuals and smoothed temperature residuals of the specified set of weather stations (not MOCCA or VLINDER)
      residu_plot_stations(station_smoothed_residual_dataframe, station_name_list)
      
      # Calculate the temperatures corrected for temperature decline at different times in the time range of the measurements
      # The begin time of the measurements is always included and the time interval is specified in the general settings by the 'seconds_per_interval' variable
      # These will also be the times for which temperature charts will be made. The temperature decline correction is based on the specified set of weather stations (not MOCCA or VLINDER)
      # The temperature decline correction is based on the BBK land cover map of Flanders
      corr_temp_timeseries_stations_BBK = temp_decline_timeseries_stations(landcover_dataframe_BBK, seconds_per_interval, station_landcover_BBK, station_path_list, station_name_list, buffer_distances, "BBK")
      
      print("The temperatures are corrected for temperature decline based on the set of weather stations (not MOCCA or VLINDER)")
    }
    
    if (ESM){
      print("Correcting the temperatures based on the specified set of weather stations (not MOCCA or VLINDER) (based on the ESM land cover map of Europe)")
      
      # Calculate the temperature residuals and smoothed temperature residuals for the specified set of weather stations (not MOCCA or VLINDER)
      station_smoothed_residual_dataframe = station_smoothed_residuals(landcover_dataframe_ESM, station_path_list, station_name_list, 1)
      # Plot the temperature residuals and smoothed temperature residuals of the specified set of weather stations (not MOCCA or VLINDER)
      residu_plot_stations(station_smoothed_residual_dataframe, station_name_list)
      
      # Calculate the temperatures corrected for temperature decline at different times in the time range of the measurements
      # The begin time of the measurements is always included and the time interval is specified in the general settings by the 'seconds_per_interval' variable
      # These will also be the times for which temperature charts will be made. The temperature decline correction is based on the specified set of weather stations (not MOCCA or VLINDER)
      # The temperature decline correction is based on the ESM land cover map of Europe
      corr_temp_timeseries_stations_ESM = temp_decline_timeseries_stations(landcover_dataframe_ESM, seconds_per_interval, station_landcover_ESM, station_path_list, station_name_list, buffer_distances, "ESM")
      
      print("The temperatures are corrected for temperature decline based on the set of weather stations (not MOCCA or VLINDER)")
    }
    
    if (station_chart){
      
      if (BBK){
        print("Creating temperature charts based on temperatures corrected for temperature decline based on the set of weather stations (not MOCCA or VLINDER)")
        print("The temperature decline correction is based on the BBK land cover map of Flanders")
        
        # Create temperature charts of the environment of the route that is used for the mobile bicycle measurement campaign 
        # The measurements of this campaign are used as input for the linear model with which the charts are made
        # The charts are made at specific reference times. The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The chart is made with as input the measured temperatures corrected for temperature decline based on the specified set of weather stations (not MOCCA or VLINDER) and based on the BBK land cover map of Flanders
        charts_data_stations_BBK = chart_timeseries(landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, model_variables, step_distance, corr_temp_timeseries_stations_BBK, chart_variables_BBK)
        chart_data_BBK = create_temp_chart(charts_data_stations_BBK, number_horizontal_steps, number_vertical_steps, seconds_per_interval, landcover_dataframe_BBK, step_distance, number_of_boundaries_removed, indices_of_locations_BBK, stations_validation_names_BBK, "weather_stations", "BBK", rotate)
        
        print("Creating temperature charts with the temperature residuals (begin temperature - temperature)")
        # Create temperature charts with the temperature residuals (begin temperature - temperature) for the different temperature charts
        create_chart_temp_res(chart_data_BBK, landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, seconds_per_interval, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, stations_validation_names_ESM, stations_validation_names_BBK, "weather_stations", "BBK", rotate)
        
        print("The temperature charts are created")
      }
      
      if (ESM){
        print("Creating temperature charts based on temperatures corrected for temperature decline based on the set of weather stations (not MOCCA or VLINDER)")
        print("The temperature decline correction is based on the ESM land cover map of Europe")
        
        # Create temperature charts of the environment of the route that is used for the mobile bicycle measurement campaign 
        # The measurements of this campaign are used as input for the linear model with which the charts are made
        # The charts are made at specific reference times. The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The chart is made with as input the measured temperatures corrected for temperature decline based on the specified set of weather stations (not MOCCA or VLINDER) and based on the ESM land cover map of Europe
        charts_data_stations_ESM = chart_timeseries(landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, model_variables, step_distance, corr_temp_timeseries_stations_ESM, chart_variables_ESM)
        chart_data_ESM = create_temp_chart_ESM(charts_data_stations_ESM, landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, chart_dataframe_ESM, chart_dataframe_BBK, seconds_per_interval, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, step_distance, stations_validation_names_ESM, stations_validation_names_BBK, "weather_stations", rotate)
        
        print("Creating temperature charts with the temperature residuals (begin temperature - temperature)")
        # Create temperature charts with the temperature residuals (begin temperature - temperature) for the different temperature charts
        create_chart_temp_res(chart_data_ESM, landcover_dataframe_ESM, number_horizontal_steps, number_vertical_steps, seconds_per_interval, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, stations_validation_names_ESM, stations_validation_names_BBK, "weather_stations", "ESM", rotate)
        
        print("The temperature charts are created")
      }
      
      if (ESM == TRUE & BBK == TRUE & rotate == TRUE){
        print("Creating temperature charts with the temperature differences between the BBK temperature chart and the ESM temperature chart (projected to the BBK temperature chart domain by bilinear interpollation)")
        
        # Create temperature charts with the temperature differences between the BBK temperature chart and the ESM temperature chart (projected to the BBK temperature chart domain by bilinear interpollation)
        create_diff_chart_BBK_ESM(chart_data_BBK, chart_data_ESM, chart_dataframe_BBK, chart_dataframe_ESM, landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, step_distance, seconds_per_interval, number_of_boundaries_removed, indices_of_locations_BBK, stations_validation_names_BBK, "weather_stations")
        
        print("The temperature charts are created")
      }
    }
    
    if (station_visualisation){
      
      if (BBK){
        print("Visualising the temperatures of the route corrected for temperature decline (not MOCCA or VLINDER based) at different times")
        print("The temperature decline correction is based on the BBK land cover map of Flanders")
        
        visualisation_route_measured_temp_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, TRUE, TRUE, corr_temp_timeseries_stations_BBK)
        visualisation_route_thermal_inertia_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, TRUE, corr_temp_timeseries_stations_BBK)
        
        # Visualise the temperatures of the route corrected for temperature decline 
        # Different temperature decline corrections are used based on different reference datapoints
        # The result is a series of plots of the temperatures of the route at different reference times
        # The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The temperature decline correction is based on the set of specified weather stations (not MOCCA or VLINDER) and based on the BBK land cover map of Flanders
        visualisation_route_timeseries_BBK(landcover_dataframe_BBK, dataframe_landcover_map_BBK, seconds_per_interval, corr_temp_timeseries_stations_BBK, "weather_stations")
        
        print("The visualisation is done")
      }
      
      if (ESM){
        print("Visualising the temperatures of the route corrected for temperature decline (not MOCCA or VLINDER based) at different times")
        print("The temperature decline correction is based on the ESM land cover map of Europe")
        
        visualisation_route_measured_temp_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, TRUE, TRUE, corr_temp_timeseries_stations_ESM)
        visualisation_route_thermal_inertia_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, TRUE, corr_temp_timeseries_stations_ESM)
        
        # Visualise the temperatures of the route corrected for temperature decline 
        # Different temperature decline corrections are used based on different reference datapoints
        # The result is a series of plots of the temperatures of the route at different reference times
        # The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings
        # The temperature decline correction is based on the set of specified weather stations (not MOCCA or VLINDER) and based on the ESM land cover map of Europe
        visualisation_route_timeseries_ESM(landcover_dataframe_ESM, dataframe_landcover_map_ESM, seconds_per_interval, corr_temp_timeseries_stations_ESM, "weather_stations")
        
        print("The visualisation is done")
      }
    }
  }
  
  if (mocca_decline && vlinder_decline){
    
    plot_decline_timeseries_diff(corr_temp_timeseries_mocca_BBK, corr_temp_timeseries_vlinder_BBK, landcover_dataframe_BBK)
    if (mocca_chart && vlinder_chart){
      create_diff_chart_mocca_vlinder(charts_data_mocca_BBK, charts_data_vlinder_BBK, landcover_dataframe_BBK, number_horizontal_steps, number_vertical_steps, number_of_boundaries_removed, step_distance, seconds_per_interval, indices_of_locations_BBK, stations_validation_names_BBK, "BBK")
    }
  }
  
}

print("Program is done")

