# THIS SCRIPT CONTAINS THE FUNCTIONS THAT ARE USED TO PRODUCE TEMPERATURE CHARTS BASED ON MOBILE BICYCLE MEASUREMENTS

#-------------------------------------INITIALIZATION FUNCTION----------------------------------------------#

# Function that checks if the names in a vector are appearing in a dataframe, if not the program stops
names_check = function(dataframe, name_vector){
  
  number_wrong_names = 0
  for (name in name_vector){
    if (!(name %in% dataframe$Name)){
      print(paste0(name, " is not a valid station"))
      number_wrong_names = number_wrong_names + 1
    }
  }
  if (number_wrong_names != 0){
    stop(call. = FALSE)
  }
}

#----------------------------------------------------------------------------------------------------------#

#----------------------------------------READ OUT FUNCTION-------------------------------------------------#

# Function that creates a dataframe from the measured data stored in a csv-file and that converts the coordinates from (D)DDMM.MMMM format to numerical format
# The coordinates are also transformed from WGS84 (= GPS coordinate system) to Lambert-72 system (used in the BBK land cover map of Flanders)
reading_data_BBK = function(datafile){
  
  # Read the data from a csv-datafile and give every datapoint a name (number)
  dataframe_measurements = fread(datafile, sep=",", dec=".")
  
  # Give every column a characteristic name and add a column with the number of the observation
  names(dataframe_measurements) = c("date", "time", "Y_WGS_GPS", "X_WGS_GPS", "temperature", "pressure", "humidity")
  dataframe_measurements$Name = seq(1, NROW(dataframe_measurements), 1)
  
  # Convert the (D)DDMM.MMMM format to numerical format
  degreeslist_Y = as.numeric(substr(dataframe_measurements$Y_WGS_GPS, 1, 2))
  degreeslist_X = as.numeric(substr(dataframe_measurements$X_WGS_GPS, 1, 1))
  minutelist_Y= as.numeric(substr(dataframe_measurements$Y_WGS_GPS, 3, 8))
  minutelist_X = as.numeric(substr(dataframe_measurements$X_WGS_GPS, 2, 8))
  dataframe_measurements$Y_WGS_GPS = degreeslist_Y + (minutelist_Y/60)
  dataframe_measurements$X_WGS_GPS = degreeslist_X + (minutelist_X/60)
  
  # Create a column with date + time
  dataframe_measurements$fulldate = with(dataframe_measurements, paste(dataframe_measurements$date, dataframe_measurements$time))
  dataframe_measurements$fulldate = as.POSIXct(dataframe_measurements$fulldate, format="%d/%m/%Y %H:%M:%S", tz=Sys.timezone(), origin)
  
  # Transform the coordinates from WGS84 system to Lambert-72 system (EPSG:31370)
  dataframe_measurements = coordinate_projection_lambert(dataframe_measurements)
  return(dataframe_measurements)
}

# Function that creates a dataframe from the measured data stored in a csv-file and that converts the coordinates from (D)DDMM.MMMM format to numerical format
# The coordinates are also transformed from WGS84 (= GPS coordinate system) to LAEA Europe system (used in the ESM land cover map of Europe)
reading_data_ESM = function(datafile){
  
  # Read the data from a csv-datafile and give every datapoint a name (number)
  dataframe_measurements = fread(datafile, sep=",", dec=".")
  
  # Give every column a characteristic name and add a column with the number of the observation
  names(dataframe_measurements) = c("date", "time", "Y_WGS_GPS", "X_WGS_GPS", "temperature", "pressure", "humidity")
  dataframe_measurements$Name = seq(1, NROW(dataframe_measurements), 1)
  
  # Convert the (D)DDMM.MMMM format to numerical format
  degreeslist_Y = as.numeric(substr(dataframe_measurements$Y_WGS_GPS, 1, 2))
  degreeslist_X = as.numeric(substr(dataframe_measurements$X_WGS_GPS, 1, 1))
  minutelist_Y= as.numeric(substr(dataframe_measurements$Y_WGS_GPS, 3, 8))
  minutelist_X = as.numeric(substr(dataframe_measurements$X_WGS_GPS, 2, 8))
  dataframe_measurements$Y_WGS_GPS = degreeslist_Y + (minutelist_Y/60)
  dataframe_measurements$X_WGS_GPS = degreeslist_X + (minutelist_X/60)
  
  # Create a column with date + time
  dataframe_measurements$fulldate = with(dataframe_measurements, paste(dataframe_measurements$date, dataframe_measurements$time))
  dataframe_measurements$fulldate = as.POSIXct(dataframe_measurements$fulldate, format="%d/%m/%Y %H:%M:%S", tz=Sys.timezone(), origin)
  
  # Transform the coordinates from WGS84 system to LAEA Europe system (EPSG:3035)
  dataframe_measurements = coordinate_projection_laea(dataframe_measurements)
  return(dataframe_measurements)
}

#---------------------------------------------------------------------------------------------------------#

#--------------------------------------------COORDINATE FUNCTIONS------------------------------------------#

# Function that transforms the coordinates from WGS84 (= GPS coordinate system) to Lambert-72 system (used in the BBK land cover map of Flanders)
coordinate_projection_lambert = function(dataframe){
  
  # Setting existing coordinate as lat-long system
  cord.dec = SpatialPoints(cbind(dataframe$X_WGS_GPS, dataframe$Y_WGS_GPS), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  # Transform the coordinates to Lambert_72 system using EPSG:31370
  cord.UTM = spTransform(cord.dec, CRS("+init=epsg:31370"))
  
  # Extract the Lambert-72 coordinates
  dataframe$X_lambert = cord.UTM$coords.x1
  dataframe$Y_lambert = cord.UTM$coords.x2
  
  return(dataframe)
}

# Function that transforms the coordinates from WGS84 (= GPS coordinate system) to LAEA Europe system (used in the ESM land cover map of Europe)
coordinate_projection_laea = function(dataframe){
  
  # Setting existing coordinate as lat-long system
  cord.dec = SpatialPoints(cbind(dataframe$X_WGS_GPS, dataframe$Y_WGS_GPS), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  # Transform the coordinates to LAEA Europe system using EPSG:3035
  cord.UTM = spTransform(cord.dec, CRS("+init=epsg:3035"))
  
  # Extract the LAEA Europe coordinates
  dataframe$X_laea = cord.UTM$coords.x1
  dataframe$Y_laea = cord.UTM$coords.x2
  
  return(dataframe)
}

# Function that transforms a dataframe of WGS84 coordinates to a list of Lambert-72 coordinates (used in the BBK land cover map of Flanders)
WGS_to_lambert = function(dataframe_WGS){
  
  # Transform the WGS84 coordinates in the dataframe to Lambert-72 coordinates 
  dataframe_lambert = coordinate_projection_lambert(dataframe_WGS)
  
  # Initialize a list to store the Lambert-72 coordinates of the dataframe
  list_lambert_coords = vector(mode = "list", length = length(dataframe_lambert$X_lambert))
  
  # Fill the list with the Lambert-72 coordinates of the dataframe
  for (row in 1:length(dataframe_lambert$X_lambert)){
    list_lambert_coords[[row]] = c(dataframe_lambert$X_lambert[row], dataframe_lambert$Y_lambert[row])
  }
  return(list_lambert_coords)
}

# Function that transforms a dataframe of WGS84 coordinates to a list of LAEA Europe coordinates (used in the ESM land cover map of Europe)
WGS_to_laea = function(dataframe_WGS){
  
  # Transform the WGS84 coordinates in the dataframe to LAEA Europe coordinates
  dataframe_laea = coordinate_projection_laea(dataframe_WGS)
  
  # Initialize a list to store the LAEA Europe coordinates of the dataframe
  list_laea_coords = vector(mode = "list", length = length(dataframe_laea$X_laea))
  
  # Fill the list with the LAEA Europe coordinates of the dataframe
  for (row in 1:length(dataframe_laea$X_laea)){
    list_laea_coords[[row]] = c(dataframe_laea$X_laea[row], dataframe_laea$Y_laea[row], row)
  }
  return(list_laea_coords)
}

#-----------------------------------------------------------------------------------------------------------#

#------------------------------------LAND COVER FUNCTIONS--------------------------------------------------#

# Function that creates circular buffers for different distances (based on the BBK land cover map of Flanders)
creating_buffers_BBK = function(dataframe_measurements, distance_vector){
  
  # Create a geometry with the right projection (EPSG:31370)
  points.sf.lambert = st_as_sf(dataframe_measurements, coords = c("X_lambert", "Y_lambert"), crs = 31370, agr = "constant")
  
  # Create buffers for each distance in the vector of distances specified in the general settings
  for (index in 1:length(distance_vector)) {
    
    # Create the buffer and add a column with the distance of the buffer
    buffer = st_buffer(points.sf.lambert, distance_vector[index], nQuadSegs = 200)
    buffer$distance = distance_vector[index]
    
    # Add Lambert-72 coordinates to the buffer 
    buffer$X = dataframe_measurements$X_lambert
    buffer$Y = dataframe_measurements$Y_lambert	
    
    # Create a dataframe with all buffers
    if (index == 1) {
      buffers = buffer
    } 
    else {
      buffers = rbind(buffers, buffer)
    }
    print(paste0("Buffers created for radius: ", distance_vector[index], " m (based on BBK land cover map)"))
  }
  return(buffers)
}

# Function that creates circular buffers for different distances (based on the ESM land cover map of Europe)
creating_buffers_ESM = function(dataframe_measurements, distance_vector){
  
  # Create a geometry with the right projection (EPSG:3035)
  points.sf.laea = st_as_sf(dataframe_measurements, coords = c("X_laea", "Y_laea"), crs = 3035, agr = "constant")
  
  # Create buffers for each distance in the vector of distances specified in the general settings
  for (index in 1:length(distance_vector)) {
    
    # Create the buffer and add a column with the distance of the buffer
    buffer = st_buffer(points.sf.laea, distance_vector[index], nQuadSegs = 200)
    buffer$distance = distance_vector[index]
    
    # Add LAEA Europe coordinates to the buffer 
    buffer$X = dataframe_measurements$X_laea
    buffer$Y = dataframe_measurements$Y_laea
    
    # Create a dataframe with all buffers
    if (index == 1) {
      buffers = buffer
    } 
    else {
      buffers = rbind(buffers, buffer)
    }
    print(paste0("Buffers created for radius: ", distance_vector[index], " m (based on ESM land cover map)"))
  }
  return(buffers)
}

# Function that creates a small land cover map based on the BBK land cover map of Flanders (used to calculate the land cover fractions)
creating_small_landcover_map_BBK = function(BBK_path, buffer_dataframe, distance_vector){
  
  # Create a RasterLayer object from the BBK land cover map
  BBK_map = raster(BBK_path)   
  
  # Define the limits of the small land cover map
  X_minimum = min(buffer_dataframe$X) - max(distance_vector) - 100.
  X_maximum = max(buffer_dataframe$X) + max(distance_vector) + 100.
  Y_minimum = min(buffer_dataframe$Y) - max(distance_vector) - 100.
  Y_maximum = max(buffer_dataframe$Y) + max(distance_vector) + 100.
  
  # Calculate the time it takes for the program to create the small land cover map
  start_time = Sys.time()
  chart = as(extent(X_minimum, X_maximum, Y_minimum, Y_maximum), 'SpatialPolygons')
  crs(chart) = "+init=epsg:31370"
  small_landcover_map = crop(BBK_map, chart)
  end_time = Sys.time()
  duration = end_time-start_time
  print(paste0("Time to make the BBK small land cover map: ", duration, " seconds"))
  return(small_landcover_map)
}

# Function that creates a small land cover map based on the ESM land cover map of Europe (used to calculate the land cover fractions)
creating_small_landcover_map_ESM = function(ESM_path, buffer_dataframe, distance_vector){
  
  # Create a RasterLayer object from the ESM land cover map
  ESM_map = raster(ESM_path)   
  
  # Define the limits of the small land cover map
  X_minimum = min(buffer_dataframe$X) - max(distance_vector) - 100.
  X_maximum = max(buffer_dataframe$X) + max(distance_vector) + 100.
  Y_minimum = min(buffer_dataframe$Y) - max(distance_vector) - 100.
  Y_maximum = max(buffer_dataframe$Y) + max(distance_vector) + 100.
  
  # Calculate the time it takes for the program to create the small land cover map
  start_time = Sys.time()
  chart = as(extent(X_minimum, X_maximum, Y_minimum, Y_maximum), 'SpatialPolygons')
  crs(chart) = "+init=epsg:3035"
  small_landcover_map = crop(ESM_map, chart)
  end_time = Sys.time()
  duration = end_time-start_time
  print(paste0("Time to make the ESM small land cover map: ", duration, " seconds"))
  return(small_landcover_map)
}

# Function that changes the resolution of a RasterLayer object by a specified factor 
# This is done by splitting the data in blocks for memory consuming reasons
aggregate_blocks = function(raster_object, res_factor, fun=mean, filename=''){
  
  # Split the input raster in blocks
  blocks_data = blockSize(raster_object)                   # Find automatic blocks
  blocks_data$startrow = seq(1, nrow(raster_object), res_factor)        # Calculate the starting row of each block
  blocks_data$nrows = diff(c(blocks_data$startrow,(nrow(raster_object)+1)))   # Calculate the number of rows of each block
  blocks_data$endrow = blocks_data$startrow + blocks_data$nrows - 1          # Calculate the ending row of each block
  blocks_data$n = length(blocks_data$startrow)                 # number of blocks
  
  # Write the output raster
  raster_out = raster_object                            # Get raster basic structure
  res(raster_out) = res(raster_out)*res_factor                 # Set the resolution of the output raster
  raster_out = writeStart(raster_out, filename=filename)
  
  # Split the output raster in blocks
  blocks_data_out = blockSize(raster_out)                          # Find automatic blocks
  blocks_data_out$row = seq(1, nrow(raster_out), by=1)  
  
  # Loop the blocks defined above for aggregating
  for (j in 1:nrow(raster_out)) {
    # print(paste(j, "out of", bs$n))
    
    # Crop the input raster using the block
    ext = extent(raster_object, r1=blocks_data$startrow[j], r2=blocks_data$endrow[j], c1=1, c2=ncol(raster_object))
    sub_raster = crop(raster_object, ext)
    
    # Aggregate pixel values for the cropped input raster
    agg_values = aggregate(sub_raster, res_factor, fun=fun)
    agg_values = getValues(agg_values)
    
    # Write the values of the pixel aggregation to the output raster
    writeValues(raster_out, agg_values, blocks_data_out$row[j])
    
  }
  raster_out = writeStop(raster_out)           # Stop writing to output raster
}

# Function that calculates the land cover fractions
# The land cover fractions of fourteen land cover types are calculated based on the BBK land cover map of Flanders
calculate_landcover_BBK = function(buffer_dataframe, distance_vector, small_landcover_map, cores){
  
  # Create a list of the observation numbers
  pointlist = unique(buffer_dataframe$Name)
  
  # Sort the vector of buffer distances in increasing order
  distance_vector = sort(distance_vector, decreasing = FALSE)
  
  # Make a cluster of cores used to calculate the land cover fractions
  cl = makeCluster(cores)
  clusterExport(cl=cl, list("small_landcover_map", "buffer_dataframe", "distance_vector", "pointlist"), envir = environment())
  
  # Calculate the land cover fractions for each datapoint (for all buffers)
  landcoverlist = foreach (j = 1:length(pointlist), .export=c("small_landcover_map", "buffer_dataframe", "distance_vector", "pointlist"), .combine = rbind, .packages = c("raster", "fasterize", "sf")) %dopar% {
    datapoint_number = pointlist[j]
    
    # Collection of all buffers belonging to a certain datapoint
    buffer_collection = buffer_dataframe[which(buffer_dataframe$Name == datapoint_number),] 
    classtat.results = data.frame()
    
    # Crop the small land cover map to the extent of the largest buffer of the collection of all buffers (largest buffer map)
    biggest_geometry_point = buffer_collection[which(buffer_collection$distance == max(buffer_collection$distance)),]
    largest_buffer_map = raster::crop(small_landcover_map, extent(biggest_geometry_point))
    
    # Initialize a dataframe for the land cover fractions of the datapoint
    landcover_datapoint = data.frame(impervious=numeric(), green=numeric(), water=numeric())
    
    # Calculate the land cover fractions of all buffers belonging to the datapoint
    for (distance in distance_vector){
      
      # Only for the smallest buffer of the collection of all buffers
      if (distance == min(distance_vector)){
        smallest_buffer = buffer_collection[which(buffer_collection$distance == distance),]   # Extract the smallest buffer from the collection of all buffers 
        smallest_buffer_map = raster::crop(largest_buffer_map, extent(smallest_buffer))   # Crop the largest buffer map to the extent of the smallest buffer (smallest buffer map)
        cliptest = raster::mask(smallest_buffer_map, smallest_buffer)
        fr = fasterize(smallest_buffer, smallest_buffer_map)   # This makes a raster of the sf geometry type   
        area_raster = raster::mask(x=smallest_buffer_map, mask=fr)
        small_geometry = smallest_buffer
      } 
      
      # For all buffers of the collection of buffers except the smallest buffer
      else {
        current_buffer = buffer_collection[which(buffer_collection$distance == distance),]   # Extract the current buffer from the collection of all buffers 
        
        # Subtract the area of the previous buffer (which is smaller than the current buffer) from the current buffer area (a thorus shape is left)
        thorus_geometry = st_difference(current_buffer, small_geometry)
        thorus_map = raster::crop(largest_buffer_map, extent(thorus_geometry))   # Crop the largest buffer map to the extent of the thorus shape
        cliptest = raster::mask(thorus_map, thorus_geometry) 
        fr = fasterize(thorus_geometry, thorus_map)   # This makes a raster of the sf geometry type   
        area_raster = raster::mask(x=thorus_map, mask=fr)
        small_geometry = current_buffer
      }
      
      # Calculate the fourteen different land cover fractions
      # This is done by building a frequency table containing the number of occurences for each of the fourteen different land cover types in the geometry and normalizing this table
      raster_vector_format = table(as.vector(area_raster))   # Create the frequency table   
      
      frequency_table = prop.table(raster_vector_format)   # Normalize the frequency table
      
      # Extract the fourteen different land cover fractions based on the frequency table 
      building = as.numeric(frequency_table["1"])
      building[is.na(building)] = 0
      road = as.numeric(frequency_table["2"])
      road[is.na(road)] = 0
      rest_impervious = as.numeric(frequency_table["3"])
      rest_impervious[is.na(rest_impervious)] = 0
      rail_road = as.numeric(frequency_table["4"])
      rail_road[is.na(rail_road)] = 0
      water = as.numeric(frequency_table["5"])
      water[is.na(water)] = 0
      rest_non_impervious = as.numeric(frequency_table["6"])
      rest_non_impervious[is.na(rest_non_impervious)] = 0
      crop_land = as.numeric(frequency_table["7"])
      crop_land[is.na(crop_land)] = 0
      grass_shrub = as.numeric(frequency_table["8"])
      grass_shrub[is.na(grass_shrub)] = 0
      
      tree = as.numeric(frequency_table["9"])
      tree[is.na(tree)] = 0
      grass_shrub_agriculture = as.numeric(frequency_table["10"])
      grass_shrub_agriculture[is.na(grass_shrub_agriculture)] = 0
      grass_shrub_road = as.numeric(frequency_table["11"])
      grass_shrub_road[is.na(grass_shrub_road)] = 0
      trees_road = as.numeric(frequency_table["12"])
      trees_road[is.na(trees_road)] = 0
      grass_shrub_water = as.numeric(frequency_table["13"])
      grass_shrub_water[is.na(grass_shrub_water)] = 0
      trees_water = as.numeric(frequency_table["14"])
      trees_water[is.na(trees_water)] = 0
      
      watertest = sum(water, grass_shrub_water)
      water = watertest - grass_shrub_water
      
      if (distance == min(distance_vector)){
        # Create a vector with the fourteen land cover fractions that are calculated (for the smallest buffer)
        landcover_buffer = cbind(building, road, rest_impervious, rail_road, water, rest_non_impervious, crop_land, grass_shrub, tree, grass_shrub_agriculture, grass_shrub_road, trees_road, grass_shrub_water, trees_water)
      }
      
      else{
        # Create a vector with the fourteen land cover fractions calculated (for all buffers except the smallest buffer)
        thorus_landcover_buffer = cbind(building, road, rest_impervious, rail_road, water, rest_non_impervious, crop_land, grass_shrub, tree, grass_shrub_agriculture, grass_shrub_road, trees_road, grass_shrub_water, trees_water)   # The land cover fractions for the thorus shape
        circle_landcover_buffer = (thorus_landcover_buffer * distance^2) + (previous_buffer_landcover * previous_distance^2)   # Calculate the land cover fractions for the whole buffer (multiply by the square of the distance to scale the land cover fractions by area)
        landcover_buffer = prop.table(circle_landcover_buffer)   # Normalize the land cover fractions of the whole buffer  
      }
      
      landcover_buffer = as.data.frame(landcover_buffer)
      previous_buffer_landcover = landcover_buffer
      previous_distance = distance
      
      # Store the land cover fractions for all buffers of a certain datapoint in a dataframe 
      # Each row represents the land cover fractions for a certain buffer distance
      landcover_datapoint = rbind(landcover_datapoint, landcover_buffer)
    }
    
    return(landcover_datapoint)
  }
  
  buffer_dataframe = arrange(buffer_dataframe, buffer_dataframe$Name)
  landcover_dataframe = cbind(as.data.frame(buffer_dataframe), as.data.frame(landcoverlist)) 
  
  # The format of the land cover dataframe is such that the land cover fractions of a certain datapoint are displayed in multiple rows (one row for each buffer distance)
  
  # Create a vector with the names of the fourteen land cover types
  landcover_classes = c('building', 'road', 'rest_impervious', 'rail_road', 'water', 'rest_non_impervious', 'crop_land', 'grass_shrub', 'tree', 'grass_shrub_agriculture', 'grass_shrub_road', 'trees_road', 'grass_shrub_water', 'trees_water')
  
  # Change the format of the land cover dataframe such that the land cover fractions of a certain datapoint are displayed in one row with multiple columns indicating the land cover fractions for different buffer distances
  not_landcover_data = landcover_dataframe[which(landcover_dataframe$distance == distance_vector[1]), !(names(landcover_dataframe) %in% landcover_classes)]
  final_landcover_dataframe = data.frame(not_landcover_data)
  for (distance in distance_vector) {
    landcover_data = landcover_dataframe[which(landcover_dataframe$distance == distance), names(landcover_dataframe) %in% landcover_classes]
    distance_string = as.character(distance)
    landcover_colnames = paste(landcover_classes, distance_string, sep="")
    colnames(landcover_data) = landcover_colnames
    final_landcover_dataframe = cbind(final_landcover_dataframe, landcover_data)
  }
  
  return(final_landcover_dataframe)
}

# Function that calculates the land cover fractions
# The impervious, green and water land cover fractions are calculated based on the ESM land cover map of Europe
calculate_landcover_ESM = function(buffer_dataframe, distance_vector, small_landcover_map, cores){
  
  # Create a list of the observation numbers
  pointlist = unique(buffer_dataframe$Name)
  
  # Sort the vector of buffer distances in increasing order
  distance_vector = sort(distance_vector, decreasing = FALSE)
  
  # Make a cluster of cores used to calculate the land cover fractions
  cl = makeCluster(cores)
  clusterExport(cl=cl, list("small_landcover_map", "buffer_dataframe", "distance_vector", "pointlist"), envir = environment())
  
  # Calculate the land cover fractions for each datapoint (for all buffers)
  landcoverlist = foreach (j = 1:length(pointlist), .export=c("small_landcover_map", "buffer_dataframe", "distance_vector", "pointlist"), .combine = rbind, .packages = c("raster", "fasterize", "sf")) %dopar% {
    datapoint_number = pointlist[j]
    
    # Collection of all buffers belonging to a certain datapoint
    buffer_collection = buffer_dataframe[which(buffer_dataframe$Name == datapoint_number),] 
    classtat.results = data.frame()
    
    # Crop the small land cover map to the extent of the largest buffer of the collection of all buffers (largest buffer map)
    biggest_geometry_point = buffer_collection[which(buffer_collection$distance == max(buffer_collection$distance)),]
    largest_buffer_map = raster::crop(small_landcover_map, extent(biggest_geometry_point))
    
    # Initialize a dataframe for the land cover fractions of the datapoint
    landcover_datapoint = data.frame(impervious=numeric(), green=numeric(), water=numeric())
    
    # Calculate the land cover fractions of all buffers belonging to the datapoint
    for (distance in distance_vector){
      
      # Only for the smallest buffer of the collection of all buffers
      if (distance == min(distance_vector)){
        smallest_buffer = buffer_collection[which(buffer_collection$distance == distance),]   # Extract the smallest buffer from the collection of all buffers 
        smallest_buffer_map = raster::crop(largest_buffer_map, extent(smallest_buffer))   # Crop the largest buffer map to the extent of the smallest buffer (smallest buffer map)
        cliptest = raster::mask(smallest_buffer_map, smallest_buffer)
        fr = fasterize(smallest_buffer, smallest_buffer_map)   # This makes a raster of the sf geometry type   
        area_raster = raster::mask(x=smallest_buffer_map, mask=fr)
        small_geometry = smallest_buffer
      } 
      
      # For all buffers of the collection of buffers except the smallest buffer
      else {
        current_buffer = buffer_collection[which(buffer_collection$distance == distance),]   # Extract the current buffer from the collection of all buffers 
        
        # Subtract the area of the previous buffer (which is smaller than the current buffer) from the current buffer area (a thorus shape is left)
        thorus_geometry = st_difference(current_buffer, small_geometry)
        thorus_map = raster::crop(largest_buffer_map, extent(thorus_geometry))   # Crop the largest buffer map to the extent of the thorus shape
        cliptest = raster::mask(thorus_map, thorus_geometry) 
        fr = fasterize(thorus_geometry, thorus_map)   # This makes a raster of the sf geometry type   
        area_raster = raster::mask(x=thorus_map, mask=fr)
        small_geometry = current_buffer
      }
      
      # Calculate the impervious, green and water land cover fractions
      # This is done by building a frequency table containing the number of occurences for each different land cover type in the geometry and normalizing this table
      raster_vector_format = table(as.vector(area_raster))   # Create the frequency table   
      
      # Extract the different land cover fractions of interest based on the frequency table
      BUbuildings = prop.table(raster_vector_format)["50"]
      BUbuildings[is.na(BUbuildings)] = 0
      BUstreet_greenNDVI = prop.table(raster_vector_format)["45"]
      BUstreet_greenNDVI[is.na(BUstreet_greenNDVI)] = 0
      BUgreenUA = prop.table(raster_vector_format)["41"]
      BUgreenUA[is.na(BUgreenUA)] = 0
      BUgreenNDVI = prop.table(raster_vector_format)["40"]
      BUgreenNDVI[is.na(BUgreenNDVI)] = 0
      BUstreets = prop.table(raster_vector_format)["35"]
      BUstreets[is.na(BUstreets)] = 0
      BUopen_space = prop.table(raster_vector_format)["30"]
      BUopen_space[is.na(BUopen_space)] = 0
      NBUstreet_greenNDVI = prop.table(raster_vector_format)["25"]
      NBUstreet_greenNDVI[is.na(NBUstreet_greenNDVI)] = 0
      NBUgreenNDVI = prop.table(raster_vector_format)["20"]
      NBUgreenNDVI[is.na(NBUgreenNDVI)] = 0
      NBUstreets = prop.table(raster_vector_format)["15"]
      NBUstreets[is.na(NBUstreets)] = 0
      NBUopen_space = prop.table(raster_vector_format)["10"]
      NBUopen_space[is.na(NBUopen_space)] = 0
      railways = prop.table(raster_vector_format)["2"]
      railways[is.na(railways)] = 0
      water = prop.table(raster_vector_format)["1"]
      water[is.na(water)] = 0
      
      # Calculate the impervious and green land cover fractions by aggregating land cover classes
      impervious = sum(railways,NBUstreets,(BUopen_space*0.5),BUstreets,BUbuildings, na.rm = TRUE)
      green = sum(NBUopen_space,NBUgreenNDVI,NBUstreet_greenNDVI,(BUopen_space*0.5),BUgreenNDVI,BUgreenUA,BUstreet_greenNDVI, na.rm = TRUE)
      
      if (distance == min(distance_vector)){
        # Create a vector with the impervious, green and water land cover fractions calculated (for the smallest buffer)
        landcover_buffer = cbind(impervious, green, water)
      }
      
      else{
        # Create a vector with the impervious, green and water land cover fractions calculated (for all buffers except the smallest buffer)
        thorus_landcover_buffer = cbind(impervious, green, water)   # The land cover fractions for the thorus shape
        circle_landcover_buffer = (thorus_landcover_buffer * distance^2) + (previous_buffer_landcover * previous_distance^2)   # Calculate the land cover fractions for the whole buffer (multiply by the square of the distance to scale the land cover fractions by area)
        landcover_buffer = prop.table(circle_landcover_buffer)   # Normalize the land cover fractions of the whole buffer  
      }
      
      landcover_buffer = as.data.frame(landcover_buffer)
      previous_buffer_landcover = landcover_buffer
      previous_distance = distance
      
      # Store the land cover fractions for all buffers of a certain datapoint in a dataframe 
      # Each row represents the land cover fractions for a certain buffer distance
      landcover_datapoint = rbind(landcover_datapoint, landcover_buffer)
    }
    
    return(landcover_datapoint)
  }
  
  buffer_dataframe = arrange(buffer_dataframe, buffer_dataframe$Name)
  landcover_dataframe = cbind(as.data.frame(buffer_dataframe), as.data.frame(landcoverlist)) 
  
  # The format of the land cover dataframe is such that the land cover fractions of a certain datapoint are displayed in multiple rows (one row for each buffer distance)
  
  # Create a vector with the names of the land cover types
  landcover_classes = c('impervious', 'green', 'water')
  
  # Change the format of the land cover dataframe such that the land cover fractions of a certain datapoint are displayed in one row with multiple columns indicating the land cover fractions for different buffer distances
  not_landcover_data = landcover_dataframe[which(landcover_dataframe$distance == distance_vector[1]), !(names(landcover_dataframe) %in% landcover_classes)]
  final_landcover_dataframe = data.frame(not_landcover_data)
  for (distance in distance_vector) {
    landcover_data = landcover_dataframe[which(landcover_dataframe$distance == distance), names(landcover_dataframe) %in% landcover_classes]
    distance_string = as.character(distance)
    landcover_colnames = paste(landcover_classes, distance_string, sep="")
    colnames(landcover_data) = landcover_colnames
    final_landcover_dataframe = cbind(final_landcover_dataframe, landcover_data)
  }
  
  return(final_landcover_dataframe)
}

# Function that calculates the green and impervious land cover fractions
calculate_combined_landcover = function(landcover_dataframe, distance_vector){
  
  # Define which land cover types belong to the 'impervious' class and the 'green' class
  impervious_variables = c('building', 'road', 'rest_impervious', 'rail_road')
  green_variables = c('tree', 'rest_non_impervious', 'grass_shrub', 'crop_land', 'grass_shrub_agriculture', 'grass_shrub_road', 'grass_shrub_water', 'trees_water', 'trees_road')
  
  # Add extra columns to the land cover dataframe with the impervious and green land cover fractions for each buffer distance
  for (distance in distance_vector){
    distance_string = as.character(distance)
    impervious_colname = paste0("impervious", distance_string)
    green_colname = paste0("green", distance_string)
    
    impervious_columns = paste(impervious_variables, distance_string, sep="")
    green_columns = paste(green_variables, distance_string, sep="")
    
    # Calculate the impervious and green land cover fractions as the sum of the land cover fractions for the specific land cover types belonging to the impervious and green classes
    landcover_dataframe[impervious_colname] = rowSums(landcover_dataframe[, impervious_columns])
    landcover_dataframe[green_colname] = rowSums(landcover_dataframe[, green_columns])
  }
  
  return(landcover_dataframe)
}

# Function that calculates the land cover fractions for the specified VLINDER stations and that visualises the location of these VLINDER stations
# The land cover fractions are calculated based on the BBK land cover map of Flanders
landcover_vlinder_BBK = function(distance_vector, vlinder_locations, BBK_directory, cores){
  
  # Transform the coordinates from WGS84 system to Lambert-72 system and create buffers for the VLINDER stations 
  vlinder_locations = coordinate_projection_lambert(vlinder_locations)
  vlinderbuffers = creating_buffers_BBK(vlinder_locations, distance_vector)
  
  # Build a small land cover map for the VLINDER stations (based on the BBK land cover map)
  print("Building the BBK small land cover map of the environment of the VLINDER stations")
  
  vlinder_landcover_map = creating_small_landcover_map_BBK(BBK_directory, vlinderbuffers, distance_vector)
  vlinder_landcover_map_low_resolution = aggregate_blocks(vlinder_landcover_map, res_factor=10)   # Lower the resolution of the small land cover map 
  
  print("The small land cover map is built")
  
  # Calculate the land cover fractions for the VLINDER stations (based on the BBK land cover map)
  vlinder_landcover_dataframe = calculate_landcover_BBK(vlinderbuffers, distance_vector, vlinder_landcover_map, cores)
  
  # Visualise the location of the VLINDER stations
  vlinder_landcover_map_dataframe = as.data.frame(vlinder_landcover_map_low_resolution, xy = TRUE)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_vlinder_stations_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = vlinder_landcover_map_dataframe, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.37) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust = 0))
  plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest_impervious", "Rail_road", "Water", "Rest_non_impervious", "Crop_land", "Grass_shrub", "Tree", "Grass_shrub_agriculture", "Grass_shrub_road", "Trees_road", "Grass_shrub_water", "Trees_water"), name = "Land cover type")                                                                                                    
  plot3 = plot2 + geom_point(data = vlinderbuffers, aes(x = X, y = Y, col = Name), shape = 18, size = 4) + ggtitle("BBK land cover map of the environment of the VLINDER stations") + guides(col=guide_legend(title="Station name"))
  print(plot3)
  dev.off()
  
  return(vlinder_landcover_dataframe)
}

# Function that calculates the land cover fractions for the specified VLINDER stations and that visualises the location of these VLINDER stations
# The land cover fractions are calculated based on the ESM land cover map of Europe
landcover_vlinder_ESM = function(distance_vector, vlinder_locations, ESM_directory, cores){
  
  # Transform the coordinates from WGS84 system to LAEA Europe system and create buffers for the VLINDER stations 
  vlinder_locations = coordinate_projection_laea(vlinder_locations)
  vlinderbuffers = creating_buffers_ESM(vlinder_locations, distance_vector)
  
  # Build a small land cover map for the VLINDER stations (based on the ESM land cover map)
  print("Building the ESM small land cover map of the environment of the VLINDER stations")
  
  vlinder_landcover_map = creating_small_landcover_map_ESM(ESM_directory, vlinderbuffers, distance_vector)
  vlinder_landcover_map_low_resolution = aggregate_blocks(vlinder_landcover_map, res_factor=10)   # Lower the resolution of the small land cover map 
  
  print("The small land cover map is built")
  
  # Calculate the land cover fractions for the VLINDER stations (based on the ESM land cover map)
  vlinder_landcover_dataframe = calculate_landcover_ESM(vlinderbuffers, distance_vector, vlinder_landcover_map, cores)
  
  # Visualise the location of the VLINDER stations
  vlinder_landcover_map_dataframe = as.data.frame(vlinder_landcover_map_low_resolution, xy = TRUE)
  
  # Manipulate the dataframe of the ESM small land cover map such that it is ready to plot
  vlinder_landcover_map_dataframe = dataframe_landcover_map_ESM_setup(vlinder_landcover_map_dataframe)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_vlinder_stations_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = vlinder_landcover_map_dataframe, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.37) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust = 0))
  plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
  plot3 = plot2 + geom_point(data = vlinderbuffers, aes(x = X, y = Y, col = Name), shape = 18, size = 4) + ggtitle("ESM land cover map of the environment of the VLINDER stations") + guides(col=guide_legend(title="Station name"))                                                                                                    
  print(plot3)
  dev.off()
  
  return(vlinder_landcover_dataframe)
}

# Function that calculates the land cover fractions for the specified MOCCA stations and that visualises the location of these MOCCA stations
# The land cover fractions are calculated based on the BBK land cover map of Flanders
landcover_mocca_BBK = function(distance_vector, mocca_locations, BBK_directory, cores){
  
  # Transform the coordinates from WGS84 system to Lambert-72 system and create buffers for the six MOCCA stations
  mocca_locations = coordinate_projection_lambert(mocca_locations)
  moccabuffers = creating_buffers_BBK(mocca_locations, distance_vector)
  
  # Build a small land cover map for the six MOCCA stations (based on the BBK land cover map) 
  print("Building the BBK small land cover map of the environment of the MOCCA stations")
  
  mocca_landcover_map = creating_small_landcover_map_BBK(BBK_directory, moccabuffers, distance_vector)
  mocca_landcover_map_low_resolution = aggregate_blocks(mocca_landcover_map, res_factor=10)   # Lower the resolution of the small land cover map   
  
  print("The small land cover map is built")
  
  # Calculate the land cover fractions for the six MOCCA stations (based on the BBK land cover map)
  mocca_landcover_dataframe = calculate_landcover_BBK(moccabuffers, distance_vector, mocca_landcover_map, cores)
  
  # Visualise the location of the six MOCCA stations
  mocca_landcover_map_dataframe = as.data.frame(mocca_landcover_map_low_resolution, xy = TRUE)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_mocca_stations_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = mocca_landcover_map_dataframe, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.37) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust = 0))
  plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest_impervious", "Rail_road", "Water", "Rest_non_impervious", "Crop_land", "Grass_shrub", "Tree", "Grass_shrub_agriculture", "Grass_shrub_road", "Trees_road", "Grass_shrub_water", "Trees_water"), name = "Land cover type")                                                                                                    
  plot3 = plot2 + geom_point(data = moccabuffers, aes(x = X, y = Y, col = Name), shape = 18, size = 4) + ggtitle("BBK land cover map of the environment of the MOCCA stations") + guides(col=guide_legend(title="Station name"))
  print(plot3)
  dev.off()
  
  return(mocca_landcover_dataframe)
}

# Function that calculates the land cover fractions for the specified MOCCA stations and that visualises the location of these MOCCA stations
# The land cover fractions are calculated based on the ESM land cover map of Europe
landcover_mocca_ESM = function(distance_vector, mocca_locations, ESM_directory, cores){
  
  # Transform the coordinates from WGS84 system to LAEA Europe system and create buffers for the six MOCCA stations
  mocca_locations = coordinate_projection_laea(mocca_locations)
  moccabuffers = creating_buffers_ESM(mocca_locations, distance_vector)
  
  # Build a small land cover map for the six MOCCA stations (based on the ESM land cover map)
  print("Building the ESM small land cover map of the environment of the MOCCA stations")
  
  mocca_landcover_map = creating_small_landcover_map_ESM(ESM_directory, moccabuffers, distance_vector)
  mocca_landcover_map_low_resolution = aggregate_blocks(mocca_landcover_map, res_factor=10)   # Lower the resolution of the small land cover map   
  
  print("The small land cover map is built")
  
  # Calculate the land cover fractions for the six MOCCA stations (based on the ESM land cover map)
  mocca_landcover_dataframe = calculate_landcover_ESM(moccabuffers, distance_vector, mocca_landcover_map, cores)
  
  # Visualise the location of the six MOCCA stations
  mocca_landcover_map_dataframe = as.data.frame(mocca_landcover_map_low_resolution, xy = TRUE)
  
  # Manipulate the dataframe of the ESM small land cover map such that it is ready to plot
  mocca_landcover_map_dataframe = dataframe_landcover_map_ESM_setup(mocca_landcover_map_dataframe)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_mocca_stations_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = mocca_landcover_map_dataframe, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.37) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust = 0))
  plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
  plot3 = plot2 + geom_point(data = moccabuffers, aes(x = X, y = Y, col = Name), shape = 18, size = 4) + ggtitle("ESM land cover map of the environment of the MOCCA stations") + guides(col=guide_legend(title="Station name"))
  print(plot3)
  dev.off()
  
  return(mocca_landcover_dataframe)
}

# Function that calculates the land cover fractions for the specified set of weather stations (not MOCCA or VLINDER) and that visualises the location of these stations
# The land cover fractions are calculated based on the BBK land cover map of Flanders
landcover_weather_stations_BBK = function(distance_vector, station_locations, BBK_directory, cores){
  
  # Transform the coordinates from WGS84 system to Lambert-72 system and create buffers for the weather stations
  station_locations = coordinate_projection_lambert(station_locations)
  stationbuffers = creating_buffers_BBK(station_locations, distance_vector)
  
  # Build a small land cover map for the weather stations (based on the BBK land cover map)
  print("Building the BBK small land cover map of the environment of the set of weather stations (not MOCCA or VLINDER)")
  
  station_landcover_map = creating_small_landcover_map_BBK(BBK_directory, stationbuffers, distance_vector)
  station_landcover_map_low_resolution = aggregate_blocks(station_landcover_map, res_factor=10)   # Lower the resolution of the small land cover map   
  
  print("The small land cover map is built")
  
  # Calculate the land cover fractions for the weather stations (based on the BBK land cover map)
  station_landcover_dataframe = calculate_landcover_BBK(stationbuffers, distance_vector, station_landcover_map, cores)
  
  # Visualise the location of the weather stations
  station_landcover_map_dataframe = as.data.frame(station_landcover_map_low_resolution, xy = TRUE)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_weather_stations_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = station_landcover_map_dataframe, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.37) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust = 0))
  plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest_impervious", "Rail_road", "Water", "Rest_non_impervious", "Crop_land", "Grass_shrub", "Tree", "Grass_shrub_agriculture", "Grass_shrub_road", "Trees_road", "Grass_shrub_water", "Trees_water"), name = "Land cover type")                                                                                                    
  plot3 = plot2 + geom_point(data = stationbuffers, aes(x = X, y = Y, col = Name), shape = 18, size = 4) + ggtitle("BBK land cover map of the environment of \n the weather stations (not MOCCA or VLINDER)") + guides(col=guide_legend(title="Station name"))
  print(plot3)
  dev.off()
  
  return(station_landcover_dataframe)
}

# Function that calculates the land cover fractions for the specified set of weather stations (not MOCCA or VLINDER) and that visualises the location of these stations
# The land cover fractions are calculated based on the ESM land cover map of Europe
landcover_weather_stations_ESM = function(distance_vector, station_locations, ESM_directory, cores){
  
  # Transform the coordinates from WGS84 system to LAEA Europe system and create buffers for the weather stations
  station_locations = coordinate_projection_laea(station_locations)
  stationbuffers = creating_buffers_ESM(station_locations, distance_vector)
  
  # Build a small land cover map for the weather stations (based on the ESM land cover map)
  print("Building the ESM small land cover map of the environment of the set of weather stations (not MOCCA or VLINDER)")
  
  station_landcover_map = creating_small_landcover_map_ESM(ESM_directory, stationbuffers, distance_vector)
  station_landcover_map_low_resolution = aggregate_blocks(station_landcover_map, res_factor=10)   # Lower the resolution of the small land cover map   
  
  print("The small land cover map is built")
  
  # Calculate the land cover fractions for the weather stations (based on the ESM land cover map)
  station_landcover_dataframe = calculate_landcover_ESM(stationbuffers, distance_vector, station_landcover_map, cores)
  
  # Visualise the location of the weather stations
  station_landcover_map_dataframe = as.data.frame(station_landcover_map_low_resolution, xy = TRUE)
  
  # Manipulate the dataframe of the ESM small land cover map such that it is ready to plot
  station_landcover_map_dataframe = dataframe_landcover_map_ESM_setup(station_landcover_map_dataframe)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_weather_stations_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = station_landcover_map_dataframe, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.37) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust = 0))
  plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
  plot3 = plot2 + geom_point(data = stationbuffers, aes(x = X, y = Y, col = Name), shape = 18, size = 4) + ggtitle("ESM land cover map of the environment of \n the weather stations (not MOCCA or VLINDER)") + guides(col=guide_legend(title="Station name"))
  print(plot3)
  dev.off()
  
  return(station_landcover_dataframe)
}

#------------------------------------------------------------------------------------------------------------#

#---------------------------------------TEMPERATURE CORRECTION FUNCTIONS-----------------------------------#

# Function that corrects the measured temperature for thermal inertia
thermal_inertia_correction = function(landcover_dataframe, inertia_files_directory, degrees_of_freedom){
  
  print(paste0("Degrees of freedom used for smoothing of the temperature derivative: ", degrees_of_freedom))
  
  # Create a vector of datafiles used for the thermal inertia correction
  # Each file gives a value of tau which is a measure for the speed of change of the temperature of the sensor when there is a considerable temperature difference
  inertia_files = c("temp_inertia_1", "temp_inertia_2", "temp_inertia_3", "temp_inertia_4", "temp_inertia_5", "temp_inertia_6", "temp_inertia_7")
  
  # Function that calculates the derivative and smoothed derivative of temperature
  temperature_derivative = function(dataframe, degrees_of_freedom){
    
    # Calculate the derivative of temperature and the smoothed derivative of temperature 
    temp_derivative_data = predict(sm.spline(dataframe$fulldate, dataframe$temperature), dataframe$fulldate, 1)
    temp_derivative = temp_derivative_data[,1]
    derivative_dataframe = data.frame("date" = dataframe$fulldate, "temp_derivative" = temp_derivative)
    smoothed_temp_derivative_data = smooth.spline(derivative_dataframe$date, derivative_dataframe$temp_derivative, df = degrees_of_freedom)
    derivative_dataframe$smooth = smoothed_temp_derivative_data$y
    return(derivative_dataframe) 
  }
  
  # Calculate the derivative and smoothed derivative of temperature for the measured datapoints
  temp_derivative_dataframe = temperature_derivative(landcover_dataframe, degrees_of_freedom)
  smoothed_derivative = temp_derivative_dataframe$smooth
  
  # Plot the derivative of temperature and the smoothed derivative of temperature vs. time
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temperature_derivative_", datestring, ".pdf"))
  plot = ggplot(data = temp_derivative_dataframe, aes(x = date, color = variable)) + geom_line(aes(y = temp_derivative_dataframe$temp_derivative, col='Derivative of temperature')) 
  plot2 = plot + geom_line(aes(y = temp_derivative_dataframe$smooth, col='Smoothed derivative of temperature'), size = 1) + labs(x='Time (in UTC)', y="dT/dt") + scale_color_manual(values = c("Derivative of temperature" = "blue", "Smoothed derivative of temperature" = "orange")) + ggtitle(paste0("Smoothing of the temperature derivative at: ", datestring, " \n (degrees of freedom used: ", degrees_of_freedom, ")")) + theme(plot.title = element_text(size = 15, hjust = 0), axis.title=element_text(size=17), axis.text=element_text(size=12.3), legend.title=element_blank(), legend.text=element_text(size=13.5), legend.position = "top")
  print(plot2)
  dev.off()
  
  
  # Function that calculates tau via a fit of a non-linear model T ~ a + b*(1 - exp(-t*c)) to the data in the inertia datafiles
  calculation_tau = function(inertia_datafile, inertia_files_directory){
    
    # Read the data from the inertia datafile 
    path_inertia_data = paste0(inertia_files_directory, inertia_datafile, ".txt")
    inertia_data = fread(path_inertia_data, sep=",", dec=".")
    names(inertia_data) = c("date", "time", "XWGS", "YWGS", "temperature", "pressure", "humidity")
    inertia_data$fulldate = paste(inertia_data$date, inertia_data$time)
    inertia_data$fulldate = as.POSIXct(inertia_data$fulldate, format="%d/%m/%Y %H:%M:%S", tz=Sys.timezone())
    
    # Create a dataframe with the measured temperature and seconds passed since begin of the signal
    inertia_data$seconds = as.numeric(difftime(inertia_data$fulldate, inertia_data$fulldate[1], units = "secs"))
    temp = inertia_data$temperature
    secs = inertia_data$seconds
    temp_evolution_dataframe = data.frame(secs, temp)
    
    # Use a non-linear model T ~ a + b*(1 - exp(-t*c)) to fit the parameters a, b and c
    fit = nlsfit(data = temp_evolution_dataframe, model = 12, start = c(a = max(inertia_data$temperature), b = min(inertia_data$temperature), c = 0.01))
    
    # Extract the best fit coefficients a, b and c
    coefficient_list = as.numeric(unlist(fit[2])) 
    a = coefficient_list[1]
    b = coefficient_list[2]
    c = coefficient_list[3]
    
    # Create a function T ~ a + b*(1 - exp(-t*c)) with the best coefficients a, b and c
    model_function = function(temp_evolution_dataframe, parameter_list){
      a = as.numeric(parameter_list[1])
      b = as.numeric(parameter_list[2])
      c = as.numeric(parameter_list[3])
      seconds = as.numeric(temp_evolution_dataframe[1])
      temperature = a + b * (1 - exp(-c * seconds))
      return(temperature)
    }
    
    # Apply the function with the best coefficients to the data in the inertia datafile to estimate the temperatures (= modelled temperature response)
    temp_evolution_dataframe$model = apply(temp_evolution_dataframe, 1, model_function, parameter_list = c(a,b,c))
    
    # Plot the modelled and measured temperature response
    pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/response_", inertia_datafile, ".pdf"))
    plot = ggplot(data = temp_evolution_dataframe, aes(x = secs, color = variable)) + geom_line(aes(y = temp_evolution_dataframe$temp, col='Measured temperature response'), size = 1) 
    plot2 = plot + geom_line(aes(y = temp_evolution_dataframe$model, col='Modelled temperature response'), size = 1) + labs(x='Time (s)', y="Temperature (°C)") + scale_color_manual(values = c("Measured temperature response" = "blue", "Modelled temperature response" = "orange")) + ggtitle(bquote("Temperature response (cooling profile) with " ~ tau == .(1/c) ~ " s")) + theme(plot.title = element_text(size = 17, hjust = 0), axis.title=element_text(size=17), axis.text=element_text(size=17), legend.title=element_blank(), legend.text=element_text(size=13), legend.position = "top")
    print(plot2)
    dev.off()
    
    
    # Calculate tau which is a measure for the speed of change of the temperature of the sensor when there is a considerable temperature difference
    tau = 1/c
    
    return(tau)
  }
  
  # Create a vector used for different values of tau (for each inertia datafile)
  tauvector = c()
  
  # Calculate tau for each inertia datafile and put the values in a vector 
  for (inertia_file in inertia_files){
    tau = calculation_tau(inertia_file, inertia_files_directory)
    tauvector = c(tauvector, tau)
  }
  print(c("Values of tau: ", paste0(tauvector, " s")))
  
  # Create one tau by taking the mean of all the elements in the vector of tau values
  finaltau = mean(tauvector)
  print(paste0("Final tau value: ", finaltau, " s"))
  
  # Calculate the temperature corrected for thermal inertia and store it as a column in the landcover dataframe
  landcover_dataframe$thermal_inertia_corrected = landcover_dataframe$temperature + (finaltau * smoothed_derivative)
  
  # Plot the temperature (uncorrected for thermal inertia) and temperature (corrected for thermal inertia) vs. time
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/inertia_correction_", datestring, ".pdf"))
  plot = ggplot(data = landcover_dataframe) + geom_line(aes(x = landcover_dataframe$fulldate, y = landcover_dataframe$thermal_inertia_corrected, col = 'Corrected temperature'))
  plot2 = plot + geom_line(aes(x = landcover_dataframe$fulldate, y = landcover_dataframe$temperature, col = 'Uncorrected temperature'))  + labs(x = 'Time (in UTC)', y = "Temperature (°C)") + scale_color_manual(values = c("Uncorrected temperature" = "blue", "Corrected temperature" = "orange")) + ggtitle(paste0("Thermal inertia correction at: ", datestring)) + theme(plot.title = element_text(size = 17, hjust = 0), axis.title=element_text(size=17), axis.text=element_text(size=17), legend.title = element_blank(), legend.text=element_text(size=13), legend.position = "top")
  print(plot2)
  dev.off()
  
  return(landcover_dataframe$thermal_inertia_corrected)
}

# Function that calculates the temperature residuals and smoothed temperature residuals for a number of MOCCA stations (based on the active temperature)
mocca_smoothed_residuals = function(landcover_dataframe, mocca_directory_list, mocca_name_list, number_reference){
  
  # Calculate the start time and end time of the measurements
  starttime = min(landcover_dataframe$fulldate)
  endtime = max(landcover_dataframe$fulldate)
  
  # Define columnnames for the MOCCA data
  column_names_mocca = c("date", "time", "active_temp", "passive_temp", "humidity", "max_wind", "average_wind", "wind_direction", "variance_wind_speed", "precipitation")
  
  # Create a dataframe for the residuals and smoothed residuals of the MOCCA stations. The time of the measurements is already stored in the dataframe
  mocca_residual_dataframe = data.frame("date" = landcover_dataframe$fulldate)
  
  # Calculate the temperature residuals and smoothed temperature residuals for all specified MOCCA stations
  index=1
  while (index <= length(mocca_name_list)){
    
    # Read the data from the MOCCA datafile
    moccadata = read.table(mocca_directory_list[index], header = FALSE, sep = ';', skip = 1)
    names(moccadata) = column_names_mocca
    
    # Create a column with date + time
    moccadata$fulldate = with(moccadata, paste(moccadata$date, moccadata$time))
    moccadata$fulldate = as.POSIXct(moccadata$fulldate, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone(), origin)
    moccadata$fulldate = as.POSIXct(moccadata$fulldate, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    
    # Only retain the MOCCA datapoints that are relevant (same time interval as the measured datapoints)
    mocca_subset = subset(moccadata, moccadata$fulldate < (endtime + 300) & moccadata$fulldate > (starttime - 300))
    
    # Interpolation of the temperatures of the MOCCA datapoints to the exact same times as the measured datapoints of the route
    mocca_interpolated = approx(x = mocca_subset$fulldate, y = mocca_subset$active_temp, xout = unique(landcover_dataframe$fulldate))
    interpolated_temp = as.numeric(unlist(mocca_interpolated[2]))
    
    # Add MOCCA interpolated temperatures to the dataframe together with the temperature residuals and smoothed temperature residuals
    mocca_residual_dataframe = add_column(mocca_residual_dataframe, !!(mocca_name_list[index]) := interpolated_temp )
    mocca_residual_dataframe = add_column(mocca_residual_dataframe, !!(paste0('residu_', mocca_name_list[index])) := mocca_residual_dataframe[number_reference,2+(index-1)*3]-mocca_residual_dataframe[,2+(index-1)*3])
    
    mocca_smoothed_residuals = smooth.spline(mocca_residual_dataframe$date, mocca_residual_dataframe[,index*3], df=5)
    smoothed_residuals = mocca_smoothed_residuals$y
    mocca_residual_dataframe = add_column(mocca_residual_dataframe, !!(paste0('residu_', mocca_name_list[index], '_smoothed')) := smoothed_residuals)
    
    index = index+1
  }
  
  return(mocca_residual_dataframe)
}

# Function that calculates the temperature residuals and smoothed temperature residuals for a number of VLINDER stations 
vlinder_smoothed_residuals = function(landcover_dataframe, vlinder_directory_list, vlinder_name_list, number_reference){
  
  # Calculate the start time and end time of the measurements
  starttime = min(landcover_dataframe$fulldate)
  endtime = max(landcover_dataframe$fulldate)
  
  # Define columnnames for the VLINDER data
  column_names_vlinder = c("date", "time", "temperature", "humidity", "pressure", "precipitation", "total_precipitation", "wind_direction", "wind_speed", "puff")
  
  # Create a dataframe for the residuals and smoothed residuals of the VLINDER stations. The time of the measurements is already stored in the dataframe
  vlinder_residual_dataframe = data.frame("date" = landcover_dataframe$fulldate)
  
  # Calculate the temperature residuals and smoothed temperature residuals for all specified VLINDER stations
  index=1
  while (index <= length(vlinder_name_list)){
    
    # Read the data from the VLINDER datafile
    vlinderdata = read.table(vlinder_directory_list[index], header = FALSE, sep = ';', skip = 1)
    names(vlinderdata) = column_names_vlinder
    
    # Create a column with date + time
    vlinderdata$fulldate = with(vlinderdata, paste(vlinderdata$date, vlinderdata$time))
    vlinderdata$fulldate = as.POSIXct(vlinderdata$fulldate, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone(), origin)
    vlinderdata$fulldate = as.POSIXct(vlinderdata$fulldate, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    
    # Only retain the VLINDER datapoints that are relevant (same time interval as the measured datapoints)
    vlinder_subset = subset(vlinderdata, vlinderdata$fulldate < (endtime + 300) & vlinderdata$fulldate > (starttime - 300))
    
    # Interpolation of the temperatures of the VLINDER datapoints to the exact same times as the measured datapoints of the route
    vlinder_interpolated = approx(x = vlinder_subset$fulldate, y = vlinder_subset$temperature, xout = unique(landcover_dataframe$fulldate))
    interpolated_temp = as.numeric(unlist(vlinder_interpolated[2]))
    
    
    # Add VLINDER interpolated temperatures to the dataframe together with the temperature residuals and smoothed temperature residuals
    vlinder_residual_dataframe = add_column(vlinder_residual_dataframe, !!(vlinder_name_list[index]) := interpolated_temp )
    vlinder_residual_dataframe = add_column(vlinder_residual_dataframe, !!(paste0('residu_', vlinder_name_list[index])) := vlinder_residual_dataframe[number_reference,2+(index-1)*3]-vlinder_residual_dataframe[,2+(index-1)*3])
    
    vlinder_smoothed_residuals = smooth.spline(vlinder_residual_dataframe$date, vlinder_residual_dataframe[,index*3], df=5)
    smoothed_residuals = vlinder_smoothed_residuals$y
    vlinder_residual_dataframe = add_column(vlinder_residual_dataframe, !!(paste0('residu_', vlinder_name_list[index], '_smoothed')) := smoothed_residuals)
    
    index = index+1
  }
  
  return(vlinder_residual_dataframe)
}

# Function that calculates the temperature residuals and smoothed temperature residuals for a number of weather stations (not MOCCA or VLINDER)
station_smoothed_residuals = function(landcover_dataframe, station_directory_list, station_name_list, number_reference){
  
  # Calculate the start time and end time of the measurements
  starttime = min(landcover_dataframe$fulldate)
  endtime = max(landcover_dataframe$fulldate)
  
  # Define columnnames for the data of the weather stations
  column_names_station = c("fulldate", "temperature")
  
  # Create a dataframe for the residuals and smoothed residuals of the weather stations. The time of the measurements is already stored in the dataframe
  station_residual_dataframe = data.frame("date" = landcover_dataframe$fulldate)
  
  # Calculate the temperature residuals and smoothed temperature residuals for all specified weather stations
  index=1
  while (index <= length(station_name_list)){
    
    # Read the data from the datafile of the weather station
    stationdata = read.table(station_directory_list[index], header = FALSE, sep = ';', skip = 1)
    names(stationdata) = column_names_station
    stationdata$date = as.POSIXct(stationdata$fulldate, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    
    # Only retain the datapoints that are relevant (same time interval as the measured datapoints)
    station_subset = subset(stationdata, stationdata$date < (endtime+300) & stationdata$date > (starttime-300))
    
    # Interpolation of the temperatures of the station datapoints to the exact same times as the measured datapoints of the route
    station_interpolated = approx(x = station_subset$date, y = station_subset$temperature, xout = unique(landcover_dataframe$fulldate))
    interpolated_temp = as.numeric(unlist(station_interpolated[2]))
    
    # Add the station interpolated temperatures to the dataframe together with the temperature residuals and smoothed temperature residuals
    station_residual_dataframe = add_column(station_residual_dataframe, !!(station_name_list[index]) := interpolated_temp )
    station_residual_dataframe = add_column(station_residual_dataframe, !!(paste0('residu_', station_name_list[index])) := station_residual_dataframe[number_reference,2+(index-1)*3]-station_residual_dataframe[,2+(index-1)*3])
    
    station_smoothed_residuals = smooth.spline(station_residual_dataframe$date, station_residual_dataframe[,index*3], df=5)
    smoothed_residuals = station_smoothed_residuals$y
    station_residual_dataframe = add_column(station_residual_dataframe, !!(paste0('residu_', station_name_list[index], '_smoothed')) := smoothed_residuals)
    
    index = index+1
  }
  
  return(station_residual_dataframe)
}

# Function that calculates the temperature residuals for the temperature decline
temperature_residu_cal = function(landcover_dataframe, stations_smoothed_residuals, stations_landcover, distance_vector, name_list){
  
  # Create a vector with the names of the land cover variables that are used to calculate the temperature residuals
  variable_list = c()
  for(distance in distance_vector){
    distance_string = as.character(distance)
    variables_distance = c(paste0('green', distance_string), paste0('impervious', distance_string))
    variable_list = append(variable_list, variables_distance)
  }
  
  # Function that calculates the likelihood between the land cover of a certain location and a certain weather station
  likelihood = function(landcover, stations_landcover) {
    landcover = as.numeric(landcover)
    stations_landcover = as.numeric(stations_landcover)
    weight = 1/abs(landcover-stations_landcover)
    weight = replace_na(weight, 9000)
    weight = sum(weight)
    return(weight)
  }
  
  # Extract the impervious and green land cover fractions for the measured datapoints 
  landcover_measurements = select(landcover_dataframe, variable_list)
  
  # Extract the impervious and green land cover fractions for the weather stations
  landcover_of_stations = select(stations_landcover, c('Name', variable_list))
  
  residual_dataframe = c()
  weights_dataframe = c()
  for (station in name_list){
    
    # Extract the smoothed temperature residuals for each station
    smoothed_residual = select(stations_smoothed_residuals, paste0("residu_", station, "_smoothed"))
    smoothed_residual = as.vector(unlist(smoothed_residual))
    residual_dataframe = cbind(residual_dataframe, smoothed_residual)
    
    # Calculate the weight factors for each station
    specific_landcover = subset(landcover_of_stations, Name == station) %>% select(-Name)
    weights = apply(landcover_measurements, 1, likelihood, stations_landcover = as.numeric(specific_landcover[1,]))
    weights_dataframe = cbind(weights_dataframe, weights)
  }
  residual_dataframe = as.data.frame(residual_dataframe)
  weights_dataframe = as.data.frame(weights_dataframe)
  
  # Function that calculates the temperature residuals as a weighted sum of the smoothed temperature residuals of the weather stations
  # The weights are the likelihood coefficients between the land cover of the measured datapoints and the weather stations
  calculate_temp_residual = function(dataframe){
    
    residuals = as.numeric(dataframe)[1:length(name_list)]   # The smoothed temperature residuals of the stations
    weights = as.numeric(dataframe)[(length(name_list)+1):(2*length(name_list))]   # The likelihood coefficients 
    temp_residu = weighted.mean(residuals, weights)
    return(temp_residu)
  }
  
  # Replace infinite weight values by 99999
  weights_dataframe[weights_dataframe == Inf] = 99999 
  
  # Create a dataframe with the smoothed temperature residuals of the stations together with the likelihood coefficients
  residual_weight_dataframe = data.frame(residual_dataframe, weights_dataframe)
  
  # Calculate the temperature residuals 
  temperature_residu = apply(residual_weight_dataframe, 1, calculate_temp_residual)
  
  return(temperature_residu)
}

# Function that calculates the smoothed temperature residuals of the measured datapoints that are used to correct for temperature decline
# The residuals are calculated based on either the BBK map or ESM map depending on the 'land_cover_map' variable
smoothing_temp_residu = function(landcover_dataframe, temperature_residual, make_plot, correction_type, land_cover_map){
  
  # Calculate the smoothed temperature residuals
  smoothed_residuals_list = smooth.spline(landcover_dataframe$fulldate, temperature_residual, df=5)
  smoothed_residuals = smoothed_residuals_list$y
  
  # Create a dataframe for plotting of the temperature residuals and smoothed temperature residuals of the measured datapoints
  plot_dataframe = data.frame(landcover_dataframe$fulldate, temperature_residual, smoothed_residuals)
  names(plot_dataframe) = c("date", "residual", "smoothed_residual")
  
  if (make_plot){
    
    # Plot of the temperature residuals and smoothed temperature residuals of the measured datapoints
    pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/smoothed_residuals_", correction_type, "_", land_cover_map, ".pdf"))
    plot = ggplot(data = plot_dataframe) + geom_line(aes(x = date, y = residual, color = "temperature_residual"), size = 0.35) + geom_line(aes(x = date, y = smoothed_residual, color = "smoothed_temperature_residual"), size = 1)
    plot2 = plot + scale_color_manual(values = c("temperature_residual" = "red", "smoothed_temperature_residual" = "blue"))
    plot3 = plot2 + ggtitle(paste0("Temperature residuals and smoothed temperature residuals at: ", datestring, " \n based on the ", land_cover_map, " land cover map")) + labs(x = 'Time (in UTC)', y = 'Temperature residual (°C)') + theme(plot.title = element_text(hjust = 0.5), legend.justification=c(1,1), legend.position=c(0.5,1), legend.title=element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15))
    print(plot3)
    dev.off()
  }
  
  return(smoothed_residuals)
}

# Function that corrects the temperatures for temperature decline based on a specific reference datapoint (reference time) and for a particular set of VLINDER stations
# The final temperature residuals are smoothed and then added to the uncorrected temperatures
temp_decline_corr_vlinder = function(landcover_dataframe, landcover_vlinder_stations, vlinder_directory_list, vlinder_name_list, distance_vector, reference_num, make_plot, land_cover_map){
  
  # Calculate the temperature residuals and smoothed temperature residuals for the set of VLINDER stations 
  smoothed_residual_dataframe = vlinder_smoothed_residuals(landcover_dataframe, vlinder_directory_list, vlinder_name_list, reference_num)
  
  # Calculate the temperature residuals based on the smoothed temperature residuals of the VLINDER stations and the similarity in land cover fractions between the datapoints and the VLINDER stations
  temperature_residu = temperature_residu_cal(landcover_dataframe, smoothed_residual_dataframe, landcover_vlinder_stations, distance_vector, vlinder_name_list)
  
  # Smooth the temperature residuals
  final_smoothed_residuals = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot, "vlinder", land_cover_map)
  
  # Correct the temperatures for temperature decline by adding the final smoothed temperature residuals
  corrected_temp = landcover_dataframe$temperature + final_smoothed_residuals
  
  return(corrected_temp)
}

# Function that corrects the temperatures for temperature decline based on a specific reference datapoint (reference time) and for a particular set of MOCCA stations
# The final temperature residuals are smoothed and then added to the uncorrected temperatures
temp_decline_corr_mocca = function(landcover_dataframe, landcover_mocca_stations, mocca_directory_list, mocca_name_list, distance_vector, reference_num, make_plot, land_cover_map){
  
  # Calculate the temperature residuals and smoothed temperature residuals for the set of MOCCA stations
  smoothed_residual_dataframe = mocca_smoothed_residuals(landcover_dataframe, mocca_directory_list, mocca_name_list, reference_num)
  
  # Calculate the temperature residuals based on the smoothed temperature residuals of the MOCCA stations and the similarity in land cover fractions between the datapoints and the MOCCA stations
  temperature_residu = temperature_residu_cal(landcover_dataframe, smoothed_residual_dataframe, landcover_mocca_stations, distance_vector, mocca_name_list)
  
  # Smooth the temperature residuals
  final_smoothed_residuals = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot, "mocca", land_cover_map)
  
  # Correct the temperatures for temperature decline by adding the final smoothed temperature residuals
  corrected_temp = landcover_dataframe$temperature + final_smoothed_residuals
  
  return(corrected_temp)
}

# Function that corrects the temperatures for temperature decline based on a specific reference datapoint (reference time) and for a particular set of weather stations (not MOCCA or VLINDER)
# The final temperature residuals are smoothed and then added to the uncorrected temperatures
temp_decline_corr_stations = function(landcover_dataframe, landcover_weather_stations, station_directory_list, station_name_list, distance_vector, reference_num, make_plot, land_cover_map){
  
  # Calculate the temperature residuals and smoothed temperature residuals for the set of weather stations
  smoothed_residual_dataframe = station_smoothed_residuals(landcover_dataframe, station_directory_list, station_name_list, reference_num)
  
  # Calculate the temperature residuals based on the smoothed temperature residuals of the specified weather stations and the similarity in land cover fractions between the datapoints and the weather stations
  temperature_residu = temperature_residu_cal(landcover_dataframe, smoothed_residual_dataframe, landcover_weather_stations, distance_vector, station_name_list)
  
  # Smooth the temperature residuals
  final_smoothed_residuals = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot, "weather_stations", land_cover_map)
  
  # Correct the temperatures for temperature decline by adding the final smoothed temperature residuals
  corrected_temp = landcover_dataframe$temperature + final_smoothed_residuals
  
  return(corrected_temp)
}

# Function that corrects the temperatures for temperature decline based on a specific reference datapoint (reference time) and for a particular set of VLINDER stations
# The final temperature residuals are not smoothed
temp_decline_corr_vlinder_no_smoothing = function(landcover_dataframe, landcover_vlinder_stations, vlinder_directory_list, vlinder_name_list, distance_vector, reference_num){
  
  # Calculate the temperature residuals and smoothed temperature residuals for the set of VLINDER stations
  smoothed_residual_dataframe = vlinder_smoothed_residuals(landcover_dataframe, vlinder_directory_list, vlinder_name_list, reference_num)
  
  # Calculate the temperature residuals based on the smoothed temperature residuals of the VLINDER stations and the similarity in land cover fractions between the datapoints and the VLINDER stations
  temperature_residu = temperature_residu_cal(landcover_dataframe, smoothed_residual_dataframe, landcover_vlinder_stations, distance_vector, vlinder_name_list)
  
  # Correct the temperatures for temperature decline by adding the final temperature residuals
  corrected_temp = landcover_dataframe$temperature + temperature_residu
  
  return(corrected_temp)
}

# Function that corrects the temperatures for temperature decline based on a specific reference datapoint (reference time) and for a particular set of MOCCA stations
# The final temperature residuals are not smoothed
temp_decline_corr_mocca_no_smoothing = function(landcover_dataframe, landcover_mocca_stations, mocca_directory_list, mocca_name_list, distance_vector, reference_num){
  
  # Calculate the temperature residuals and smoothed temperature residuals for the set of MOCCA stations
  smoothed_residual_dataframe = mocca_smoothed_residuals(landcover_dataframe, mocca_directory_list, mocca_name_list, reference_num)
  
  # Calculate the temperature residuals based on the smoothed temperature residuals of the MOCCA stations and the similarity in land cover fractions between the datapoints and the MOCCA stations
  temperature_residu = temperature_residu_cal(landcover_dataframe, smoothed_residual_dataframe, landcover_mocca_stations, distance_vector, mocca_name_list)
  
  # Correct the temperatures for temperature decline by adding the final temperature residuals
  corrected_temp = landcover_dataframe$temperature + temperature_residu
  
  return(corrected_temp)
}

# Function that corrects the temperatures for temperature decline based on a specific reference datapoint (reference time) and for a particular set of weather stations (not MOCCA or VLINDER)
# The final temperature residuals are not smoothed
temp_decline_corr_stations_no_smoothing = function(landcover_dataframe, landcover_weather_stations, station_directory_list, station_name_list, distance_vector, reference_num){
  
  # Calculate the temperature residuals and smoothed temperature residuals for the set of weather stations
  smoothed_residual_dataframe = station_smoothed_residuals(landcover_dataframe, station_directory_list, station_name_list, reference_num)
  
  # Calculate the temperature residuals based on the smoothed temperature residuals of the specified weather stations and the similarity in land cover fractions between the datapoints and the weather stations
  temperature_residu = temperature_residu_cal(landcover_dataframe, smoothed_residual_dataframe, landcover_weather_stations, distance_vector, station_name_list)
  
  # Correct the temperatures for temperature decline by adding the final temperature residuals
  corrected_temp = landcover_dataframe$temperature + temperature_residu
  
  return(corrected_temp)
}

# Function that predicts the measured (corrected or uncorrected) temperatures based on a linear model that is built with these temperatures as input together with variables in the dataframe
predict_temp_whole_data = function(variables_temp_data){
  
  # Build the linear model
  linear_model = lm(temperature ~ ., data = variables_temp_data)
  
  # Predict the measured temperatures based on the linear model
  predicted_temp = predict(linear_model, newdata = variables_temp_data)
  
  return(predicted_temp)
}

# Function that creates a plot of the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
# the temperatures corrected for temperature decline based on the set of VLINDER stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
# the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (VLINDER corrected) predicted by a linear model with as variables the model variables that are set in the general settings of the main script
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
plot_temp_evolutions_vlinder = function(landcover_dataframe, temperature_dataframe, model_variables, corr_temp_vlinder, land_cover_map){
  
  landcover_dataframe$temperature = corr_temp_vlinder
  
  # Create a dataframe with the model variables and temperature of the measurements (VLINDER corrected)
  data_variables_temp = data_variables(landcover_dataframe, model_variables)
  
  # Predict the measured temperatures (VLINDER corrected) based on a linear model with as variables the model variables
  predicted_temperatures = predict_temp_whole_data(data_variables_temp)
  temperature_dataframe = data.frame(temperature_dataframe, predicted_temperatures)
  
  # Plot the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
  # the temperatures corrected for temperature decline based on the set of VLINDER stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
  # the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (VLINDER corrected) predicted by a linear model with as variables the model variables
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temp_evolutions_vlinder_", land_cover_map, ".pdf"), width = 8)
  plot = ggplot(data = temperature_dataframe) + geom_line(aes(x = date, y = vlinder_corrected_no_smoothing, color = "VLINDER corrected no smoothing"), size = 0.4) + geom_line(aes(x = date, y = mocca_corrected, color = "MOCCA corrected"), size = 0.4)
  plot2 = plot + geom_line(aes(x = date, y = measured, color = "Measured (not corrected)"), size = 0.4) + geom_line(aes(x = date, y = inertia_corrected, color = "Inertia corrected (no decline)"), size = 0.4)
  plot3 = plot2 + geom_line(aes(x = date, y = predicted_temperatures, color = "Predicted temperatures (VLINDER based)"), size = 0.4) + geom_line(aes(x = date, y = vlinder_corrected, color = "VLINDER corrected"), size = 0.4) 
  plot4 = plot3 + scale_color_manual(values = c("VLINDER corrected no smoothing" = "green", "Measured (not corrected)" = "darkgoldenrod1", "Inertia corrected (no decline)" = "forestgreen", "Predicted temperatures (VLINDER based)" = "darkmagenta", "VLINDER corrected" = "red", "MOCCA corrected" = "blue"), breaks = c("Measured (not corrected)", "Inertia corrected (no decline)", "VLINDER corrected no smoothing", "VLINDER corrected", "Predicted temperatures (VLINDER based)", "MOCCA corrected")) 
  plot5 = plot4 + ggtitle(paste0("Temperature decline correction based on the ", land_cover_map, " land cover map \n Temperatures at: ", datestring)) + labs(x = 'Time (in UTC)', y = 'Temperature (°C)') + theme(plot.title = element_text(hjust = 0.5), legend.justification=c(1,1), legend.position="top", legend.title=element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15)) + guides(color=guide_legend(nrow=2,byrow=TRUE))
  print(plot5)
  dev.off()
  
  return(temperature_dataframe)
}

# Function that creates a plot of the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
# the temperatures corrected for temperature decline based on the set of MOCCA stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
# the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (MOCCA corrected) predicted by a linear model with as variables the model variables that are set in the general settings of the main script
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
plot_temp_evolutions_mocca = function(landcover_dataframe, temperature_dataframe, model_variables, corr_temp_mocca, land_cover_map){
  
  landcover_dataframe$temperature = corr_temp_mocca
  
  # Create a dataframe with the model variables and temperature of the measurements (MOCCA corrected)
  data_variables_temp = data_variables(landcover_dataframe, model_variables)
  
  # Predict the measured temperatures (MOCCA corrected) based on a linear model with as variables the model variables
  predicted_temperatures = predict_temp_whole_data(data_variables_temp)
  temperature_dataframe = data.frame(temperature_dataframe, predicted_temperatures)
  
  # Plot the evolution of: the temperatures corrected for temperature decline based on the set of VLINDER stations and smoothing of the final temperature residuals,
  # the temperatures corrected for temperature decline based on the set of MOCCA stations and no smoothing of the final temperature residuals, the temperatures corrected for temperature decline based on the set of MOCCA stations and smoothing of the final temperature residuals, 
  # the measured uncorrected temperature, the temperatures corrected for thermal inertia but not corrected for temperature decline and the temperatures (MOCCA corrected) predicted by a linear model with as variables the model variables
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temp_evolutions_mocca_", land_cover_map, ".pdf"), width = 8)
  plot = ggplot(data = temperature_dataframe) + geom_line(aes(x = date, y = vlinder_corrected, color = "VLINDER corrected (4b)"), size = 0.4) + geom_line(aes(x = date, y = mocca_corrected_no_smoothing, color = "MOCCA corrected no smoothing (3)"), size = 0.4) 
  plot2 = plot + geom_line(aes(x = date, y = measured, color = "Measured (not corrected) (1)"), size = 0.4) + geom_line(aes(x = date, y = inertia_corrected, color = "Inertia corrected (no decline) (2)"), size = 0.4)
  plot3 = plot2 + geom_line(aes(x = date, y = predicted_temperatures, color = "Predicted temperatures (MOCCA based) (5)"), size = 0.4) + geom_line(aes(x = date, y = mocca_corrected, color = "MOCCA corrected (4a)"), size = 0.4)
  plot4 = plot3 + scale_color_manual(values = c("MOCCA corrected (4a)" = "blue", "MOCCA corrected no smoothing (3)" = "green", "Measured (not corrected) (1)" = "darkgoldenrod1", "Inertia corrected (no decline) (2)" = "forestgreen", "Predicted temperatures (MOCCA based) (5)" = "darkmagenta", "VLINDER corrected (4b)" = "red"), breaks = c("Measured (not corrected) (1)", "Inertia corrected (no decline) (2)", "MOCCA corrected no smoothing (3)", "MOCCA corrected (4a)", "VLINDER corrected (4b)", "Predicted temperatures (MOCCA based) (5)")) 
  plot5 = plot4 + ggtitle(paste0("Temperature decline correction based on the ", land_cover_map, " land cover map \n Temperatures at: ", datestring)) + labs(x = 'Time (in UTC)', y = 'Temperature (°C)') + theme(plot.title = element_text(hjust = 0.5), legend.justification=c(1,1), legend.position="top", legend.title=element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15)) + guides(color=guide_legend(nrow=2,byrow=TRUE))
  print(plot5)
  dev.off()
  
  return(temperature_dataframe)
}

# Function that corrects the temperature for temperature decline when one VLINDER station is removed from the specified set of VLINDER stations
# Each station is removed alternately
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
one_station_removed_vlinder = function(landcover_dataframe, vlinder_name_list, vlinder_directory_list, landcover_of_vlinder, distance_vector, land_cover_map){
  
  # Remove each VLINDER station alternately and calculate the temperatures corrected for temperature decline
  for (index in 1:length(vlinder_name_list)){
    
    # Remove the specific station from the set of stations
    new_name_list = vlinder_name_list[-index]
    new_directory_list = vlinder_directory_list[-index]
    print(paste0(c("Correcting for temperature decline with VLINDER stations: ", new_name_list)))
    print(paste0("Temperature decline based on the ", land_cover_map, " land cover map"))
    
    # Calculate the temperatures corrected for temperature decline when the specific station is removed
    landcover_stations_data = landcover_of_vlinder[which(landcover_of_vlinder$Name %in% new_name_list),]
    residual_dataframe_stations = vlinder_smoothed_residuals(landcover_dataframe, new_directory_list, new_name_list, 1)
    temperature_residu = temperature_residu_cal(landcover_dataframe, residual_dataframe_stations, landcover_stations_data, distance_vector, new_name_list)
    smoothed_temperature_residu = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot = FALSE, "vlinder", land_cover_map)
    corrected_temperature = landcover_dataframe$temperature + smoothed_temperature_residu
    
    # Store the corrected temperatures in a dataframe
    if (index == 1){
      dataframe_corrected_temp = data.frame(corrected_temperature)
      names(dataframe_corrected_temp) = paste0("removed_", vlinder_name_list[index])
    }
    else{
      dataframe_corrected_temp = add_column(dataframe_corrected_temp, !!(paste0("removed_", vlinder_name_list[index])) := corrected_temperature)
    }
  }
  
  return(dataframe_corrected_temp)
}

# Function that corrects the temperature for temperature decline when one MOCCA station is removed from the specified set of MOCCA stations
# Each station is removed alternately
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
one_station_removed_mocca = function(landcover_dataframe, mocca_name_list, mocca_directory_list, landcover_of_mocca, distance_vector, land_cover_map){
  
  # Remove each MOCCA station alternately and calculate the temperatures corrected for temperature decline
  for (index in 1:length(mocca_name_list)){
    
    # Remove the specific station from the set of stations
    new_name_list = mocca_name_list[-index]
    new_directory_list = mocca_directory_list[-index]
    print(paste0(c("Correcting for temperature decline with MOCCA stations: ", new_name_list)))
    print(paste0("Temperature decline based on the ", land_cover_map, " land cover map"))
    
    # Calculate the temperatures corrected for temperature decline when the specific station is removed
    landcover_stations_data = landcover_of_mocca[which(landcover_of_mocca$Name %in% new_name_list),]
    residual_dataframe_stations = mocca_smoothed_residuals(landcover_dataframe, new_directory_list, new_name_list, 1)
    temperature_residu = temperature_residu_cal(landcover_dataframe, residual_dataframe_stations, landcover_stations_data, distance_vector, new_name_list)
    smoothed_temperature_residu = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot = FALSE, "mocca", land_cover_map)
    corrected_temperature = landcover_dataframe$temperature + smoothed_temperature_residu
    
    # Store the corrected temperatures in a dataframe
    if (index == 1){
      dataframe_corrected_temp = data.frame(corrected_temperature)
      names(dataframe_corrected_temp) = paste0("removed_", mocca_name_list[index])
    }
    else{
      dataframe_corrected_temp = add_column(dataframe_corrected_temp, !!(paste0("removed_", mocca_name_list[index])) := corrected_temperature)
    }
  }
  
  return(dataframe_corrected_temp)
}

# Function that corrects the temperature for temperature decline when one weather station is removed from the specified set of weather stations (not MOCCA or VLINDER)
# Each station is removed alternately
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
one_station_removed = function(landcover_dataframe, station_name_list, station_directory_list, landcover_of_stations, distance_vector, land_cover_map){
  
  # Remove each weather station alternately and calculate the temperatures corrected for temperature decline
  for (index in 1:length(station_name_list)){
    
    # Remove the specific station from the set of stations
    new_name_list = station_name_list[-index]
    new_directory_list = station_directory_list[-index]
    print(paste0(c("Correcting for temperature decline with weather stations: ", new_name_list)))
    print(paste0("Temperature decline based on the ", land_cover_map, " land cover map"))
    
    # Calculate the temperatures corrected for temperature decline when the specific station is removed
    landcover_stations_data = landcover_of_stations[which(landcover_of_stations$Name %in% new_name_list),]
    residual_dataframe_stations = station_smoothed_residuals(landcover_dataframe, new_directory_list, new_name_list, 1)
    temperature_residu = temperature_residu_cal(landcover_dataframe, residual_dataframe_stations, landcover_stations_data, distance_vector, new_name_list)
    smoothed_temperature_residu = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot = FALSE, "weather_stations", land_cover_map)
    corrected_temperature = landcover_dataframe$temperature + smoothed_temperature_residu
    
    # Store the corrected temperatures in a dataframe
    if (index == 1){
      dataframe_corrected_temp = data.frame(corrected_temperature)
      names(dataframe_corrected_temp) = paste0("removed_", station_name_list[index])
    }
    else{
      dataframe_corrected_temp = add_column(dataframe_corrected_temp, !!(paste0("removed_", station_name_list[index])) := corrected_temperature)
    }
  }
  
  return(dataframe_corrected_temp)
}

# Function that corrects the temperature for temperature decline when multiple VLINDER stations are removed from the specified set of VLINDER stations
# Each set of stations is removed alternately
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
multiple_stations_removed_vlinder = function(landcover_dataframe, vlinder_name_list, vlinder_directory_list, landcover_of_vlinder, distance_vector, number_removed_stations, land_cover_map){
  
  # Calculate all different combinations of VLINDER stations that will be removed 
  vector_station_numbers = seq(1, length(vlinder_name_list), by = 1)
  pairs_removed_stations = combn(vector_station_numbers, number_removed_stations)
  
  # Remove each set of stations alternately and calculate the temperatures corrected for temperature decline
  for (index in 1:length(pairs_removed_stations[1,])){
    
    # Remove the specific set of stations from the whole set of stations
    removed_pair = pairs_removed_stations[,index]
    new_name_list = vlinder_name_list[-removed_pair]
    new_directory_list = vlinder_directory_list[-removed_pair]
    print(paste0(c("Correcting for temperature decline with VLINDER stations: ", new_name_list)))
    print(paste0("Temperature decline based on the ", land_cover_map, " land cover map"))
    
    # Calculate the temperatures corrected for temperature decline when the specific set of stations is removed
    landcover_stations_data = landcover_of_vlinder[which(landcover_of_vlinder$Name %in% new_name_list),]
    residual_dataframe_stations = vlinder_smoothed_residuals(landcover_dataframe, new_directory_list, new_name_list, 1)
    temperature_residu = temperature_residu_cal(landcover_dataframe, residual_dataframe_stations, landcover_stations_data, distance_vector, new_name_list)
    smoothed_temperature_residu = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot = FALSE, "vlinder", land_cover_map)
    corrected_temperature = landcover_dataframe$temperature + smoothed_temperature_residu
    
    name_stations = "removed"
    for (station in removed_pair){
      name_stations = paste0(name_stations, "_", vlinder_name_list[station])
    }
    
    
    # Store the corrected temperatures in a dataframe
    if (index == 1){
      dataframe_corrected_temp = data.frame(corrected_temperature)
      names(dataframe_corrected_temp) = name_stations
    }
    else{
      dataframe_corrected_temp = add_column(dataframe_corrected_temp, !!(name_stations) := corrected_temperature)
    }
  }
  
  return(dataframe_corrected_temp)
}

# Function that corrects the temperature for temperature decline when multiple MOCCA stations are removed from the specified set of MOCCA stations
# Each set of stations is removed alternately
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
multiple_stations_removed_mocca = function(landcover_dataframe, mocca_name_list, mocca_directory_list, landcover_of_mocca, distance_vector, number_removed_stations, land_cover_map){
  
  # Calculate all different combinations of MOCCA stations that will be removed 
  vector_station_numbers = seq(1, length(mocca_name_list), by = 1)
  pairs_removed_stations = combn(vector_station_numbers, number_removed_stations)
  
  # Remove each set of stations alternately and calculate the temperatures corrected for temperature decline
  for (index in 1:length(pairs_removed_stations[1,])){
    
    # Remove the specific set of stations from the whole set of stations
    removed_pair = pairs_removed_stations[,index]
    new_name_list = mocca_name_list[-removed_pair]
    new_directory_list = mocca_directory_list[-removed_pair]
    print(paste0(c("Correcting for temperature decline with MOCCA stations: ", new_name_list)))
    print(paste0("Temperature decline based on the ", land_cover_map, " land cover map"))
    
    # Calculate the temperatures corrected for temperature decline when the specific set of stations is removed
    landcover_stations_data = landcover_of_mocca[which(landcover_of_mocca$Name %in% new_name_list),]
    residual_dataframe_stations = mocca_smoothed_residuals(landcover_dataframe, new_directory_list, new_name_list, 1)
    temperature_residu = temperature_residu_cal(landcover_dataframe, residual_dataframe_stations, landcover_stations_data, distance_vector, new_name_list)
    smoothed_temperature_residu = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot = FALSE, "mocca", land_cover_map)
    corrected_temperature = landcover_dataframe$temperature + smoothed_temperature_residu
    
    name_stations = "removed"
    for (station in removed_pair){
      name_stations = paste0(name_stations, "_", mocca_name_list[station])
    }
    
    # Store the corrected temperatures in a dataframe
    if (index == 1){
      dataframe_corrected_temp = data.frame(corrected_temperature)
      names(dataframe_corrected_temp) = name_stations
    }
    else{
      dataframe_corrected_temp = add_column(dataframe_corrected_temp, !!(name_stations) := corrected_temperature)
    }
  }
  
  return(dataframe_corrected_temp)
}

# Function that corrects the temperature for temperature decline when multiple weather stations are removed from the specified set of weather stations (not MOCCA or VLINDER)
# Each set of stations is removed alternately
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
multiple_stations_removed = function(landcover_dataframe, station_name_list, station_directory_list, landcover_of_stations, distance_vector, number_removed_stations, land_cover_map){
  
  # Calculate all different combinations of weather stations that will be removed 
  vector_station_numbers = seq(1, length(station_name_list), by = 1)
  pairs_removed_stations = combn(vector_station_numbers, number_removed_stations)
  
  # Remove each set of stations alternately and calculate the temperatures corrected for temperature decline
  for (index in 1:length(pairs_removed_stations[1,])){
    
    # Remove the specific set of stations from the whole set of stations
    removed_pair = pairs_removed_stations[,index]
    new_name_list = station_name_list[-removed_pair]
    new_directory_list = station_directory_list[-removed_pair]
    print(paste0(c("Correcting for temperature decline with weather stations: ", new_name_list)))
    print(paste0("Temperature decline based on the ", land_cover_map, " land cover map"))
    
    # Calculate the temperatures corrected for temperature decline when the specific set of stations is removed
    landcover_stations_data = landcover_of_station[which(landcover_of_station$Name %in% new_name_list),]
    residual_dataframe_stations = station_smoothed_residuals(landcover_dataframe, new_directory_list, new_name_list, 1)
    temperature_residu = temperature_residu_cal(landcover_dataframe, residual_dataframe_stations, landcover_stations_data, distance_vector, new_name_list)
    smoothed_temperature_residu = smoothing_temp_residu(landcover_dataframe, temperature_residu, make_plot = FALSE, "weather_stations", land_cover_map)
    corrected_temperature = landcover_dataframe$temperature + smoothed_temperature_residu
    
    name_stations = "removed"
    for (station in removed_pair){
      name_stations = paste0(name_stations, "_", station_name_list[station])
    }
    
    # Store the corrected temperatures in a dataframe
    if (index == 1){
      dataframe_corrected_temp = data.frame(corrected_temperature)
      names(dataframe_corrected_temp) = name_stations
    }
    else{
      dataframe_corrected_temp = add_column(dataframe_corrected_temp, !!(name_stations) := corrected_temperature)
    }
  }
  
  return(dataframe_corrected_temp)
}


# Function that plots the evolution of the measured temperature, the temperature corrected for thermal inertia, the temperature corrected for temperature decline based on the whole set of VLINDER stations
# and the temperature corrected for temperature decline when one or more VLINDER stations are removed from the whole set of stations
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
plot_corr_temp_stations_removed_vlinder = function(landcover_dataframe, landcover_of_vlinder, vlinder_directory_list, vlinder_name_list, distance_vector, dataframe_corrections, number_removed, land_cover_map){
  
  # Calculate the temperatures corrected for temperature decline based on the whole set of VLINDER stations
  temp_decline_corr_full_set = temp_decline_corr_vlinder(landcover_dataframe, landcover_of_vlinder, vlinder_directory_list, vlinder_name_list, distance_vector, 1, FALSE, land_cover_map)
  
  # Create a dataframe that will be used for the plot
  dataframe = data.frame("date" = landcover_dataframe$fulldate, "measured_temperature" = landcover_dataframe$measured_temp,  "inertia_corrected_no_decline" = landcover_dataframe$thermal_inertia, "decline_full_set" = temp_decline_corr_full_set)
  total_dataframe_corr_temperatures = cbind(dataframe_corrections, dataframe)
  
  # Calculate the number of variables that will be plotted
  number_variables = length(total_dataframe_corr_temperatures) - 1
  
  # Create the plot
  total_dataframe_corr_temperatures = melt(total_dataframe_corr_temperatures,  id.vars = 'date', variable.name = 'station')
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/vlinder_", number_removed, "_removed_", land_cover_map, ".pdf"))
  plot = ggplot(total_dataframe_corr_temperatures, aes(date, value)) + geom_line(aes(colour = station))
  plot2 = plot + ggtitle(paste0("Evolution of temperature corrected and uncorrected \n for temperature decline (VLINDER based) \n Temperature decline correction based on the ", land_cover_map, " land cover map")) + labs(x = 'Time (in UTC)', y = 'Temperature (°C)') + guides(color = guide_legend(nrow = ceil(number_variables/3), byrow = TRUE)) + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position = "top", axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot2)
  dev.off()
}

# Function that plots the evolution of the measured temperature, the temperature corrected for thermal inertia, the temperature corrected for temperature decline based on the whole set of MOCCA stations
# and the temperature corrected for temperature decline when one or more MOCCA stations are removed from the whole set of stations
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
plot_corr_temp_stations_removed_mocca = function(landcover_dataframe, landcover_of_mocca, mocca_directory_list, mocca_name_list, distance_vector, dataframe_corrections, number_removed, land_cover_map){
  
  # Calculate the temperatures corrected for temperature decline based on the whole set of MOCCA stations
  temp_decline_corr_full_set = temp_decline_corr_mocca(landcover_dataframe, landcover_of_mocca, mocca_directory_list, mocca_name_list, distance_vector, 1, FALSE, land_cover_map)
  
  # Create a dataframe that will be used for the plot
  dataframe = data.frame("date" = landcover_dataframe$fulldate, "measured_temperature" = landcover_dataframe$measured_temp,  "inertia_corrected_no_decline" = landcover_dataframe$thermal_inertia, "decline_full_set" = temp_decline_corr_full_set)
  total_dataframe_corr_temperatures = cbind(dataframe_corrections, dataframe)
  
  # Calculate the number of variables that will be plotted
  number_variables = length(total_dataframe_corr_temperatures) - 1
  
  # Create the plot
  total_dataframe_corr_temperatures = melt(total_dataframe_corr_temperatures,  id.vars = 'date', variable.name = 'station')
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/mocca_", number_removed, "_removed_", land_cover_map, ".pdf"))
  plot = ggplot(total_dataframe_corr_temperatures, aes(date, value)) + geom_line(aes(colour = station))
  plot2 = plot + ggtitle(paste0("Evolution of temperature corrected and uncorrected \n for temperature decline (MOCCA based) \n Temperature decline correction based on the ", land_cover_map, " land cover map")) + labs(x = 'Time (in UTC)', y = 'Temperature (°C)') + guides(color = guide_legend(nrow = ceil(number_variables/3), byrow = TRUE)) + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position = "top", axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot2)
  dev.off()
}

# Function that plots the evolution of the measured temperature, the temperature corrected for thermal inertia, the temperature corrected for temperature decline based on the whole set of weather stations (not MOCCA or VLINDER)
# and the temperature corrected for temperature decline when one or more weather stations are removed from the whole set of stations
# The temperature decline correction is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
plot_corr_temp_stations_removed = function(landcover_dataframe, landcover_of_stations, station_directory_list, station_name_list, distance_vector, dataframe_corrections, number_removed, land_cover_map){
  
  # Calculate the temperatures corrected for temperature decline based on the whole set of weather stations
  temp_decline_corr_full_set = temp_decline_corr_stations(landcover_dataframe, landcover_of_stations, station_directory_list, station_name_list, distance_vector, 1, FALSE, land_cover_map)
  
  # Create a dataframe that will be used for the plot
  dataframe = data.frame("date" = landcover_dataframe$fulldate, "measured_temperature" = landcover_dataframe$measured_temp,  "inertia_corrected_no_decline" = landcover_dataframe$thermal_inertia, "decline_full_set" = temp_decline_corr_full_set)
  total_dataframe_corr_temperatures = cbind(dataframe_corrections, dataframe)
  
  # Calculate the number of variables that will be plotted
  number_variables = length(total_dataframe_corr_temperatures) - 1
  
  # Create the plot
  total_dataframe_corr_temperatures = melt(total_dataframe_corr_temperatures,  id.vars = 'date', variable.name = 'station')
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/stations_", number_removed, "_removed_", land_cover_map, ".pdf"))
  plot = ggplot(total_dataframe_corr_temperatures, aes(date, value)) + geom_line(aes(colour = station))
  plot2 = plot + ggtitle(paste0("Evolution of temperature corrected and uncorrected \n for temperature decline (not MOCCA or VLINDER) \n Temperature decline correction based on the ", land_cover_map, " land cover map")) + labs(x = 'Time (in UTC)', y = 'Temperature (°C)') + guides(color = guide_legend(nrow = ceil(number_variables/3), byrow = TRUE)) + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position = "top", axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot2)
  dev.off()
}


# Function that calculates the temperatures corrected for temperature decline at different times in the time range of the measurements
# The begin time of the measurements is always included and the time interval is specified in the general settings in the main script by the 'seconds_per_interval' variable
# These will also be the times for which temperature charts will be made. The decline correction is based on the set of VLINDER stations
# The temperature decline correction is based on either the BBK map or ESM map land cover map of Flanders
temp_decline_timeseries_vlinder = function(landcover_dataframe, seconds_per_interval, landcover_vlinder_stations, vlinder_directory_list, vlinder_name_list, distance_vector, land_cover_map){
  
  # Calculate the number of observations between two consecutive reference datapoints for the temperature decline corrections 
  step = seconds_per_interval/10
  
  # Correct for temperature decline at specific reference times with specified time interval
  obs_number=1
  while (obs_number <= length(landcover_dataframe$fulldate)){
    print(paste0("Correcting for temperature decline at reference time: ", landcover_dataframe$fulldate[obs_number], " (VLINDER) based on the ", land_cover_map, " land cover map"))
    corrected_temperature = temp_decline_corr_vlinder(landcover_dataframe, landcover_vlinder_stations, vlinder_directory_list, vlinder_name_list, distance_vector, obs_number, FALSE, land_cover_map)
    if (obs_number == 1){
      dataframe_corr_temp_timeseries = data.frame(corrected_temperature)
      names(dataframe_corr_temp_timeseries) = paste0("corrected_", obs_number)
    }
    else{
      dataframe_corr_temp_timeseries = add_column(dataframe_corr_temp_timeseries, !!(paste0("corrected_", obs_number)) := corrected_temperature)
    }
    obs_number = obs_number + step
  }
  
  return(dataframe_corr_temp_timeseries)
}

# Function that calculates the temperatures corrected for temperature decline at different times in the time range of the measurements
# The begin time of the measurements is always included and the time interval is specified in the general settings in the main script by the 'seconds_per_interval' variable
# These will also be the times for which temperature charts will be made. The decline correction is based on the set of MOCCA stations
# The temperature decline correction is based on either the BBK map or ESM map land cover map of Flanders
temp_decline_timeseries_mocca = function(landcover_dataframe, seconds_per_interval, landcover_mocca_stations, mocca_directory_list, mocca_name_list, distance_vector, land_cover_map){
  
  # Calculate the number of observations between two consecutive reference datapoints for the temperature decline corrections 
  step = seconds_per_interval/10
  
  # Correct for temperature decline at specific reference times with specified time interval
  obs_number=1
  while (obs_number <= length(landcover_dataframe$fulldate)){
    print(paste0("Correcting for temperature decline at reference time: ", landcover_dataframe$fulldate[obs_number], " (MOCCA) based on the ", land_cover_map, " land cover map"))
    corrected_temperature = temp_decline_corr_mocca(landcover_dataframe, landcover_mocca_stations, mocca_directory_list, mocca_name_list, distance_vector, obs_number, FALSE, land_cover_map)
    if (obs_number == 1){
      dataframe_corr_temp_timeseries = data.frame(corrected_temperature)
      names(dataframe_corr_temp_timeseries) = paste0("corrected_", obs_number)
    }
    else{
      dataframe_corr_temp_timeseries = add_column(dataframe_corr_temp_timeseries, !!(paste0("corrected_", obs_number)) := corrected_temperature)
    }
    obs_number = obs_number + step
  }
  
  return(dataframe_corr_temp_timeseries)
}

# Function that calculates the temperatures corrected for temperature decline at different times in the time range of the measurements
# The begin time of the measurements is always included and the time interval is specified in the general settings in the main script by the 'seconds_per_interval' variable
# These will also be the times for which temperature charts will be made. The decline correction is based on the set of specified weather stations (not MOCCA or VLINDER)
# The temperature decline correction is based on either the BBK map or ESM map land cover map of Flanders
temp_decline_timeseries_stations = function(landcover_dataframe, seconds_per_interval, landcover_weather_stations, station_directory_list, station_name_list, distance_vector, land_cover_map){
  
  # Calculate the number of observations between two consecutive reference datapoints for the temperature decline corrections 
  step = seconds_per_interval/10
  
  # Correct for temperature decline at specific reference times with specified time interval
  obs_number=1
  while (obs_number <= length(landcover_dataframe$fulldate)){
    print(paste0("Correcting for temperature decline at reference time: ", landcover_dataframe$fulldate[obs_number], " (not MOCCA or VLINDER) based on the ", land_cover_map, " land cover map"))
    corrected_temperature = temp_decline_corr_stations(landcover_dataframe, landcover_weather_stations, station_directory_list, station_name_list, distance_vector, obs_number, FALSE, land_cover_map)
    if (obs_number == 1){
      dataframe_corr_temp_timeseries = data.frame(corrected_temperature)
      names(dataframe_corr_temp_timeseries) = paste0("corrected_", obs_number)
    }
    else{
      dataframe_corr_temp_timeseries = add_column(dataframe_corr_temp_timeseries, !!(paste0("corrected_", obs_number)) := corrected_temperature)
    }
    obs_number = obs_number + step
  }
  
  return(dataframe_corr_temp_timeseries)
}

# Function that plots a histogram of the temperature 
temperature_distribution = function(landcover_dataframe){
  
  # Plot the histogram
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temp_hist_", datestring, ".pdf"))
  plot = ggplot() + geom_histogram(data = landcover_dataframe, aes(x=temperature), binwidth=.5, colour = "black", fill = "red") + labs(x = "Temperature (°C)", y = "Number of counts") + ggtitle(paste0("Histogram of temperature at: ", datestring, "\n (temperature inertia correction = ", temp_inertia_bool, ") \n (temperature decline correction = ", temp_decline_bool, ")")) + theme(plot.title = element_text(size = 15, hjust = 0.5), axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot)
  dev.off()
}

# Function that creates a plot of the temperature residuals and smoothed temperature residuals of the VLINDER stations
residu_plot_vlinder = function(vlinder_smoothed_residuals, name_list_vlinder){
  
  # Create a dataframe that is used for the plot
  vlinder_smoothed_residuals = vlinder_smoothed_residuals[,!(names(vlinder_smoothed_residuals) %in% name_list_vlinder)]
  
  # Calculate the number of variables that will be plotted
  number_variables = length(vlinder_smoothed_residuals) - 1
  
  # Plot the temperature residuals and smoothed temperature residuals of the VLINDER stations
  vlinder_smoothed_residuals = melt(vlinder_smoothed_residuals,  id.vars = 'date', variable.name = 'station')
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temp_res_VLINDER_", datestring, ".pdf"), width = 9)
  plot = ggplot(vlinder_smoothed_residuals, aes(date, value)) + geom_line(aes(colour = station))                                                                             
  plot2 = plot + ggtitle(paste0("Temperature residuals for the VLINDER stations at: ", datestring)) + labs(x = 'Time (in UTC)', y = 'Temperature residual (°C)') + guides(color = guide_legend(nrow = ceil(number_variables/3), byrow = TRUE)) + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position = "top", axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot2)
  dev.off()
}

# Function that creates a plot of the temperature residuals and smoothed temperature residuals of the MOCCA stations
residu_plot_mocca = function(mocca_smoothed_residuals, name_list_mocca){
  
  # Create a dataframe that is used for the plot
  mocca_smoothed_residuals = mocca_smoothed_residuals[,!(names(mocca_smoothed_residuals) %in% name_list_mocca)]
  
  # Calculate the number of variables that will be plotted
  number_variables = length(mocca_smoothed_residuals) - 1
  
  # Plot the temperature residuals and smoothed temperature residuals of the MOCCA stations
  mocca_smoothed_residuals = melt(mocca_smoothed_residuals,  id.vars = 'date', variable.name = 'station')
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temp_res_MOCCA_", datestring, ".pdf"), width = 9)
  plot = ggplot(mocca_smoothed_residuals, aes(date, value)) + geom_line(aes(colour = station))                                                                                  
  plot2 = plot + ggtitle(paste0("Temperature residuals for the MOCCA stations at: ", datestring)) + labs(x = 'Time (in UTC)', y = 'Temperature residual (°C)') + guides(color = guide_legend(nrow = ceil(number_variables/3), byrow = TRUE)) + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position = "top", axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot2)
  dev.off()
}

# Function that creates a plot of the temperature residuals and smoothed temperature residuals of the specified weather stations (not MOCCA or VLINDER)
residu_plot_stations = function(station_smoothed_residuals, name_list_station){
  
  # Create a dataframe that is used for the plot
  station_smoothed_residuals = station_smoothed_residuals[,!(names(station_smoothed_residuals) %in% name_list_station)]
  
  # Calculate the number of variables that will be plotted
  number_variables = length(station_smoothed_residuals) - 1
  
  # Plot the temperature residuals and smoothed temperature residuals of the weather stations
  station_smoothed_residuals = melt(station_smoothed_residuals,  id.vars = 'date', variable.name = 'station')
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temp_res_stations_", datestring, ".pdf"), width = 9)
  plot = ggplot(station_smoothed_residuals, aes(date, value)) + geom_line(aes(colour = station))                                                                                  
  plot2 = plot + ggtitle(paste0("Temperature residuals for the weather stations (not MOCCA or VLINDER) at: ", datestring)) + labs(x = 'Time (in UTC)', y = 'Temperature residual (°C)') + guides(color = guide_legend(nrow = ceil(number_variables/3), byrow = TRUE)) + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position = "top", axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot2)
  dev.off()
}

plot_decline_timeseries = function(dataframe_timeseries, landcover_dataframe, correction_type){
  names_vector = names(dataframe_timeseries)
  print(names_vector)
  datevector = c()
  for (index in 1:length(names_vector)){
    number = as.numeric(str_sub(names_vector[index], start = 11))
    date = as.character(landcover_dataframe$fulldate[number])
    datevector = c(datevector, date)
    print(datevector)
    names(dataframe_timeseries) = datevector
  }
  
  dataframe_timeseries$fulldate = landcover_dataframe$fulldate
  melted = melt(dataframe_timeseries, id.vars="fulldate")
  print(melted)
  maximum = max(melted$value)
  minimum = min(melted$value)
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/temp_decline_timeseries_", correction_type, ".pdf"), , width = 8)
  plot1 = ggplot(data=melted, aes(x=fulldate, y=value, group=variable, color = variable)) + geom_line() + ggtitle(paste0("Temperature decline corrections at: ", datestring))
  plot2 = plot1 + labs(x = 'Time (in UTC)', y = 'Temperature (°C)')
  plot3 = plot2 + scale_y_continuous(limits = c(minimum, maximum)) + theme(plot.title = element_text(hjust = 0.5), legend.justification=c(1,1), legend.title=element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot3)
  dev.off()
}

plot_decline_timeseries_diff = function(dataframe_timeseries_mocca, dataframe_timeseries_vlinder, landcover_dataframe){
  for (index in 1:length(dataframe_timeseries_mocca)){
    column = dataframe_timeseries_mocca[,index] - dataframe_timeseries_vlinder[,index]
    print(column)
    if (index == 1){
      dataframe_timeseries_diff = data.frame(column)
    }
    else{
      dataframe_timeseries_diff = cbind(dataframe_timeseries_diff, column)
    }
  }
  
  names_vector = names(dataframe_timeseries_mocca)
  print(names_vector)
  datevector = c()
  for (index in 1:length(names_vector)){
    number = as.numeric(str_sub(names_vector[index], start = 11))
    date = as.character(landcover_dataframe$fulldate[number])
    datevector = c(datevector, date)
    print(datevector)
    names(dataframe_timeseries_diff) = datevector
  }
  print(dataframe_timeseries_diff)
  dataframe_timeseries_diff$fulldate = landcover_dataframe$fulldate
  melted = melt(dataframe_timeseries_diff, id.vars="fulldate")
  print(melted)
  maximum = max(melted$value)
  minimum = min(melted$value)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/temp_decline_timeseries_diff.pdf", , width = 8)
  plot1 = ggplot(data=melted, aes(x=fulldate, y=value, group=variable, color = variable)) + geom_line() + ggtitle(paste0("Temperature decline correction differences (MOCCA - VLINDER) at: ", datestring))
  plot2 = plot1 + labs(x = 'Time (in UTC)', y = 'Temperature (°C)')
  plot3 = plot2 + scale_y_continuous(limits = c(minimum, maximum)) + theme(plot.title = element_text(hjust = 0.5), legend.justification=c(1,1), legend.title=element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15))
  print(plot3)
  dev.off()
}


#------------------------------------------------------------------------------------------------------------#

#-----------------------------------------CHART FUNCTIONS----------------------------------------------------#

# Function that creates a dataframe with the coordinates (in Lambert-72 system) of the gridpoints of the temperature chart together with a number
making_chart_dataframe_BBK = function(dataframe, step_distance, number_horizontal_steps, number_vertical_steps, chart_extension){
  
  # Initialize a dataframe
  chart_dataframe = data.frame(c(), c(), c())
  
  # Initialize the starting point used for creating the grid
  point = c(min(dataframe$X) - chart_extension - step_distance, max(dataframe$Y) + chart_extension + step_distance, 0) 
  
  # Create a grid of equally spaced points. The distance between the gridpoints is specified by the 'step_distance' variable.
  # The coordinates of the gridpoints are stored in a dataframe together with a number. The grid covers the area of the route with an additional spacing that is given by the 'chart_extension' variable starting from the four extremity points of the route
  # The grid is created from the upper left corner to the bottom right corner
  for (step in 1:number_vertical_steps){
    point[2] = point[2] - step_distance
    for (step in 1:number_horizontal_steps){
      point[1] = point[1] + step_distance
      point[3] = point[3] + 1
      chart_dataframe = rbind(chart_dataframe, point)
    }
    point[1] = min(dataframe$X) - chart_extension - step_distance
  }
  
  names(chart_dataframe) = c("X_lambert", "Y_lambert", "Name")
  return(chart_dataframe)
}

# Function that creates a dataframe with the coordinates (in LAEA Europe system) of the gridpoints of the temperature chart together with a number
making_chart_dataframe_ESM = function(dataframe, step_distance, number_horizontal_steps, number_vertical_steps, chart_extension){
  
  # Initialize a dataframe
  chart_dataframe = data.frame(c(), c(), c())
  
  # Initialize the starting point used for creating the grid
  point = c(min(dataframe$X) - chart_extension - step_distance, max(dataframe$Y) + chart_extension + step_distance, 0) 
  
  # Create a grid of equally spaced points. The distance between the gridpoints is specified by the 'step_distance' variable.
  # The coordinates of the gridpoints are stored in a dataframe together with a number. The grid covers the area of the route with an additional spacing that is given by the 'chart_extension' variable starting from the four extremity points of the route
  # The grid is created from the upper left corner to the bottom right corner
  for (step in 1:number_vertical_steps){
    point[2] = point[2] - step_distance
    for (step in 1:number_horizontal_steps){
      point[1] = point[1] + step_distance
      point[3] = point[3] + 1
      chart_dataframe = rbind(chart_dataframe, point)
    }
    point[1] = min(dataframe$X) - chart_extension - step_distance
  }
  
  names(chart_dataframe) = c("X_laea", "Y_laea", "Name")
  return(chart_dataframe)
}

# Function that plots the small land cover map for the temperature chart (based on the BBK land cover map of Flanders)
plot_chart_small_landcover_BBK = function(chart_small_landcover){
  chart_landcover_map_low_resolution = aggregate_blocks(chart_small_landcover, res_factor=10)   # Lower the resolution of the small land cover map   
  
  # Plot the small land cover map for the temperature chart (based on the BBK land cover map)
  chart_dataframe_landcover_map = as.data.frame(chart_landcover_map_low_resolution, xy = TRUE)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/small_landcover_chart_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = chart_dataframe_landcover_map, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest_impervious", "Rail_road", "Water", "Rest_non_impervious", "Crop_land", "Grass_shrub", "Tree", "Grass_shrub_agriculture", "Grass_shrub_road", "Trees_road", "Grass_shrub_water", "Trees_water"), name = "Land cover type") + ggtitle("BBK small land cover map for temperature chart")
  print(plot2)
  dev.off()
}

# Function that plots the small land cover map for the temperature chart (based on the ESM land cover map of Europe)
plot_chart_small_landcover_ESM = function(chart_small_landcover){
  chart_landcover_map_low_resolution = aggregate_blocks(chart_small_landcover, res_factor=10)   # Lower the resolution of the small land cover map   
  
  # Plot the small land cover map for the temperature chart (based on the ESM land cover map)
  chart_dataframe_landcover_map = as.data.frame(chart_landcover_map_low_resolution, xy = TRUE)
  
  # Manipulate the dataframe of the ESM small land cover map such that it is ready to plot
  chart_dataframe_landcover_map = dataframe_landcover_map_ESM_setup(chart_dataframe_landcover_map)
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/small_landcover_chart_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = chart_dataframe_landcover_map, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type") + ggtitle("ESM small land cover map for temperature chart")
  print(plot2)
  dev.off()
}

# Function that removes weather stations used for validation of the chart temperatures from which the location doesn't fit in the temperature chart
remove_locations = function(dataframe_chart, location_list, stations_validation_names, step_distance){
  
  # Create a vector with the four extremity points of the temperature chart
  chart_extremities = c(min(dataframe_chart$X), max(dataframe_chart$X), min(dataframe_chart$Y), max(dataframe_chart$Y))
  
  # Initialize a vector for the indices of locations that should be removed
  removed_indices = c()
  
  # Check for each location if it fits in the temperature chart
  for (index in 1:length(location_list)){
    location = unlist(location_list[index])
    if (location[1] < (chart_extremities[1] - step_distance/2) | location[1] > (chart_extremities[2] + step_distance/2) | location[2] < (chart_extremities[3] - step_distance/2) | location[2] > (chart_extremities[4] + step_distance/2)){
      print(paste0("Location: ", stations_validation_names[index], " doesn't fit in the temperature chart"))
      removed_indices = c(removed_indices, index)
    }
  }
  
  # Remove locations that doesn't fit in the temperature chart
  if (length(removed_indices) > 0){
    location_list = location_list[-removed_indices]
    stations_validation_names = stations_validation_names[-removed_indices]
  }
  
  returnlist = list("locations" = location_list, "names" = stations_validation_names)
  return(returnlist)
}

remove_locations_boundary = function(indices_list, num_horizontal_steps, num_vertical_steps, num_boundaries_removed, stations_validation_names){
  vector_removed = c()
  for (index in 1:num_vertical_steps){
    for (number in 1:num_boundaries_removed){
      vector_removed = c(vector_removed, index*num_horizontal_steps - (number-1))
      vector_removed = c(vector_removed, (index-1) * num_horizontal_steps + number)
    }
  }
  vector_removed = c(vector_removed, 1:(num_horizontal_steps*num_boundaries_removed))
  vector_removed = c(vector_removed, (num_vertical_steps*num_horizontal_steps - (num_horizontal_steps*num_boundaries_removed)+1):(num_vertical_steps*num_horizontal_steps))
  vector_removed = unique(vector_removed)
  print(vector_removed)

  for (index in 1:length(indices_list$X_indices)){
    location_X = unlist(indices_list[1])
    location_Y = unlist(indices_list[2])
    print(location_X)
    print(location_Y)
    matrix_number = (location_Y[index] - 1) * num_horizontal_steps + location_X[index]
    print(matrix_number)
    if (matrix_number %in% vector_removed){
      print(paste0("Location: ", stations_validation_names[index], " doesn't fit in the temperature chart"))
    }
  }
}

# Function that calculates the numbers of the gridpoints where the weather stations used for validation of the chart temperatures are located
location_numbers_on_chart = function(chart_dataframe, locations_list, step_distance){
  
  # Initialize a vector for the numbers of the gridpoints where the weather stations are located
  vector_loc_numbers = c()
  
  # Calculate for each location the number of the gridpoint that is closest to the location
  row_number_location = 0
  for (location in locations_list){
    distance = 2*step_distance
    
    # Loop over all gridpoints
    for (row in 1:length(chart_dataframe$X)){
      # Calculate the distance between the location and the gridpoint
      new_distance = abs(location[1]-chart_dataframe$X[row]) + abs(location[2]-chart_dataframe$Y[row])
      if (new_distance < distance){
        distance = new_distance
        row_number_location = row
      }
      
    }
    vector_loc_numbers = c(vector_loc_numbers, row_number_location)
  }
  
  return(vector_loc_numbers)
}

# Function that calculates the indices of the gridpoints where the weather stations used for validation of the chart temperatures are located
matrix_indices_locations = function(vector_location_numbers, number_horizontal_steps, number_vertical_steps){
  
  # Initialize vectors for the X and Y indices of the gridpoints of the locations of the weather stations
  X_indices = c()
  Y_indices = c()
  
  # Transform the numbers of the gridpoints of the locations to pairs of indices
  for (loc_number in vector_location_numbers){
    row = floor(loc_number/number_horizontal_steps)
    if (mod(loc_number, number_horizontal_steps) != 0){
      row = row + 1
      column = mod(loc_number, number_horizontal_steps)
    }
    if (mod(loc_number, number_horizontal_steps) == 0){
      column = number_horizontal_steps
    }
    X_indices = c(X_indices, column)
    Y_indices = c(Y_indices, row)
  }
  Y_indices = abs(Y_indices - number_vertical_steps) + 1
  
  # Put the X and Y indices of the locations in a list
  indices_list = list("X_indices" = X_indices, "Y_indices" = Y_indices)
  return(indices_list)
}

# Function that creates a dataframe with the model variables for the gridpoints of the temperature chart
# These model variables are used to predict the temperatures of the gridpoints of the chart
chart_variables_selection =  function(chart_dataframe, model_variables){
  
  # Remove rows in the dataframe with NA values
  chart_dataframe = drop_na(chart_dataframe)
  
  # Select model variables and create a new dataframe with these variables 
  chart_variables_dataframe = chart_dataframe %>% dplyr::select(model_variables)
  chart_variables_dataframe = chart_variables_dataframe[!is.infinite(rowSums(chart_variables_dataframe)),]   # Remove rows with infinite values
  
  return(chart_variables_dataframe)
}

# Function that creates a dataframe with the model variables and temperature
data_variables =  function(dataframe, model_variables){
  
  # Remove rows in the dataframe with NA values
  dataframe = drop_na(dataframe)
  
  # Select model variables and create a new dataframe with these variables and temperature
  variables_dataframe = dataframe %>% dplyr::select(model_variables)
  variables_dataframe$temperature = dataframe$temperature
  variables_dataframe = variables_dataframe[!is.infinite(rowSums(variables_dataframe)),]   # Remove rows with infinite values
  
  return(variables_dataframe)
}

# Function that calculates the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
chart_selection = function(route_variables, chart_variables, max_distance){
  
  # Initialize a vector for the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
  positions = c()
  
  # Compare the land cover fractions of each gridpoint to the land cover fractions of the measured datapoints
  for (row in 1:length(chart_variables$temperature)){
    for (Row in 1:length(route_variables[,1])){
      
      # Calculate a measure for the similarity between the land cover of a gridpoint and a measured datapoint
      distancevector = route_variables[Row,] - chart_variables[row,1:length(chart_variables)-1]
      distance = sum(abs(distancevector))/length(route_variables)
      
      # If the similarity measure exceeds a particular specified limit, go to the next gridpoint
      if (distance < max_distance){
        break
      }
      
      # If the similarity measure doesn't exceed the particular specified limit for all datapoints, the position of that gridpoint is stored
      if (Row == length(route_variables[,1])){
        positions = c(positions, row)
      }
    }
  }
  
  return(positions)
}

# Function that predicts the temperatures of the gridpoints of the temperature chart based on a linear model with as variables the variables in 'model_variables'
# The linear model is built with temperatures corrected for temperature decline at a specific reference time specified by the 'reference_number' variable
# The positions of gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements are also calculated
pred_chart_temp_and_selection = function(landcover_dataframe, chart_landcover_data, model_variables, reference_number, corr_temp_timeseries, step_distance){
  
  # Extract the temperatures corrected for temperature decline at the specific reference time specified by the 'reference_number' variable
  corr_temperatures = corr_temp_timeseries[[paste0("corrected_", reference_number)]]
  landcover_dataframe$temperature = corr_temperatures
  
  # Create a dataframe with the model variables and temperature of the measurements
  data_variables_temp = data_variables(landcover_dataframe, model_variables)
  
  # Build a linear model based on the model variables and temperature of the measurements
  chart_linear_model = lm(temperature ~ ., data = data_variables_temp)
  
  # Predict the temperatures of the gridpoints of the temperature chart based on the linear model
  chart_temperatures = predict(chart_linear_model, newdata = chart_landcover_data)
  chart_landcover_data$temperature = chart_temperatures
  
  # Create a dataframe with only the model variables of the measurements
  data_variables_landcover = data_variables_temp[1:length(data_variables_temp) - 1]  
  if (reference_number == 1){
    # Calculate the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
    positions_no_temp = chart_selection(data_variables_landcover, chart_landcover_data, max_distance = 0.06)
  }
  else{
    positions_no_temp = 0
  }
  
  # Create a list with the temperatures of the gridpoints of the temperature chart and the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
  info = list("chart_temperatures" = chart_temperatures, "positions_no_temp" = positions_no_temp)
  return(info)
}

# Function that transforms a vector with certain values to a matrix of these values
making_chart = function(vector, number_horizontal_steps, number_vertical_steps){
  
  # Initialize an empty matrix with the specified number of rows and columns
  chart_matrix = matrix(nrow=number_vertical_steps, ncol=number_horizontal_steps)
  
  # Fill the matrix with the values that are in the vector 
  # The matrix is filled from the upper left corner to the bottom right corner
  for (rownumber in 1:number_vertical_steps){
    for (columnnumber in 1:number_horizontal_steps){
      number = (rownumber-1)*number_horizontal_steps + columnnumber
      value = vector[number]
      chart_matrix[rownumber, columnnumber] = value
    }
  }
  return(chart_matrix)
}

plot_chart_diff_mocca_vlinder = function(chart, dataframe, number_horizontal_steps, number_vertical_steps, step_distance, min_temp, max_temp, number_reference, indices_of_locations, stations_validation_names, land_cover_map){
  # Create axes with distance information for the temperature chart
  number_horizontal_steps = round_any(number_horizontal_steps-1, 1000/step_distance, f = floor)
  number_vertical_steps = round_any(number_vertical_steps-1, 1000/step_distance, f = floor)
  horizontal_labels = seq(from = 0, to = (step_distance * number_horizontal_steps)/1000, by = 1)
  horizontal_labels_positions = seq(from = 1, to = number_horizontal_steps+1 , by = 1000/step_distance)
  vertical_labels = seq(from = 0, to = (step_distance * number_vertical_steps)/1000, by = 1)
  vertical_labels_positions = seq(from = 1, to = number_vertical_steps+1, by = 1000/step_distance)
  
  # Create a color palette used for coloring the gridpoints of the temperature chart
  colors_chart = brewer.pal(9, "YlOrRd")
  
  # Plot the temperature chart
  png(paste0("/home/michiel/Documenten/Atmospheric_modeling/Produced_data/temp_chart_LINEAR_diff_mocca_vlinder_", number_reference, "_", land_cover_map, ".png"), width = 720, height = 720)   # TITLE OF PLOT MAY BE CHANGED DEPENDING ON THE REGION THE TEMPERATURE CHART IS COVERING
  chartplot = plot(chart, axis.col = NULL, axis.row = NULL, na.col = "gray60", xlab = "", ylab = "", main=paste0("Chart of Ghent region at: ", dataframe$fulldate[number_reference], " (in UTC) \n based on the ", land_cover_map, " land cover map"), key = list(side = 2, cex.axis = 0.8), col = colors_chart, breaks = seq(min_temp, max_temp, length.out=9)) + axis(side = 1, at = horizontal_labels_positions, labels = horizontal_labels) + mtext("Km", side = 1, line = 3) + axis(side = 4, at = vertical_labels_positions, labels = vertical_labels)
  # Plot the locations of the weather stations used for validation of the temperatures of the temperature chart
  points(x = indices_of_locations$X_indices, y = indices_of_locations$Y_indices, col = "black", pch = 5, cex = 100/max(number_horizontal_steps, number_vertical_steps), lwd = 4-(max(number_horizontal_steps, number_vertical_steps)*0.01))
  # Add an arrow to the plot that is indicating the 'north' direction
  addnortharrow(pos = "topright", cols = c("black", "white"), border = "black", text.col = "black", lwd = 3)
  # Add the names of the weather stations used for validation of the temperatures of the temperature chart
  textxy(X = indices_of_locations$X_indices, Y = indices_of_locations$Y_indices, labs = stations_validation_names, cex = 2-(max(number_horizontal_steps, number_vertical_steps)*0.005), offset = 0.6, col = "black")
  print(chartplot)
  dev.off()
  
}

# Function that plots the temperature charts 
plot_chart = function(chart, dataframe, number_horizontal_steps, number_vertical_steps, step_distance, number_reference, min_temp, max_temp, indices_of_locations, stations_validation_names, correction_type, land_cover_map, rotate){
  
  # Create axes with distance information for the temperature chart
  number_horizontal_steps = round_any(number_horizontal_steps-1, 1000/step_distance, f = floor)
  number_vertical_steps = round_any(number_vertical_steps-1, 1000/step_distance, f = floor)
  horizontal_labels = seq(from = 0, to = (step_distance * number_horizontal_steps)/1000, by = 1)
  horizontal_labels_positions = seq(from = 1, to = number_horizontal_steps+1 , by = 1000/step_distance)
  vertical_labels = seq(from = 0, to = (step_distance * number_vertical_steps)/1000, by = 1)
  vertical_labels_positions = seq(from = 1, to = number_vertical_steps+1, by = 1000/step_distance)
  
  # Create a color palette used for coloring the gridpoints of the temperature chart
  colors_chart = brewer.pal(9, "YlOrRd")
  
  if (rotate == TRUE & land_cover_map == "ESM"){
    # Plot the temperature chart
    png(paste0("/home/michiel/Documenten/Atmospheric_modeling/Produced_data/temp_chart_LINEAR_", correction_type, "_", number_reference, "_", land_cover_map, "_rotated.png"), width = 720, height = 720)   # TITLE OF PLOT MAY BE CHANGED DEPENDING ON THE REGION THE TEMPERATURE CHART IS COVERING
    chartplot = plot(chart, axis.col = NULL, axis.row = NULL, na.col = "gray60", xlab = "", ylab = "", main=paste0("Chart of Ghent region at: ", dataframe$fulldate[number_reference], " (in UTC) \n based on the ", land_cover_map, " land cover map"), key = list(side = 2, cex.axis = 0.8), col = colors_chart, breaks = seq(min_temp, max_temp, length.out=9)) + axis(side = 1, at = horizontal_labels_positions, labels = horizontal_labels) + mtext("Km", side = 1, line = 3) + axis(side = 4, at = vertical_labels_positions, labels = vertical_labels)
    # Plot the locations of the weather stations used for validation of the temperatures of the temperature chart
    points(x = indices_of_locations$X_indices, y = indices_of_locations$Y_indices, col = "black", pch = 5, cex = 100/max(number_horizontal_steps, number_vertical_steps), lwd = 4-(max(number_horizontal_steps, number_vertical_steps)*0.01))
    # Add an arrow to the plot that is indicating the 'north' direction
    addnortharrow(pos = "topright", cols = c("black", "white"), border = "black", text.col = "black", lwd = 3)
    # Add the names of the weather stations used for validation of the temperatures of the temperature chart
    textxy(X = indices_of_locations$X_indices, Y = indices_of_locations$Y_indices, labs = stations_validation_names, cex = 2-(max(number_horizontal_steps, number_vertical_steps)*0.005), offset = 0.6, col = "black")
    print(chartplot)
    dev.off()
  }
  
  else{
    # Plot the temperature chart
    png(paste0("/home/michiel/Documenten/Atmospheric_modeling/Produced_data/temp_chart_LINEAR_", correction_type, "_", number_reference, "_", land_cover_map, ".png"), width = 720, height = 720)   # TITLE OF PLOT MAY BE CHANGED DEPENDING ON THE REGION THE TEMPERATURE CHART IS COVERING
    chartplot = plot(chart, axis.col = NULL, axis.row = NULL, na.col = "gray60", xlab = "", ylab = "", main=paste0("Chart of Ghent region at: ", dataframe$fulldate[number_reference], " (in UTC) \n based on the ", land_cover_map, " land cover map"), key = list(side = 2, cex.axis = 0.8), col = colors_chart, breaks = seq(min_temp, max_temp, length.out=9)) + axis(side = 1, at = horizontal_labels_positions, labels = horizontal_labels) + mtext("Km", side = 1, line = 3) + axis(side = 4, at = vertical_labels_positions, labels = vertical_labels)
    # Plot the locations of the weather stations used for validation of the temperatures of the temperature chart
    points(x = indices_of_locations$X_indices, y = indices_of_locations$Y_indices, col = "black", pch = 5, cex = 100/max(number_horizontal_steps, number_vertical_steps), lwd = 4-(max(number_horizontal_steps, number_vertical_steps)*0.01))
    # Add an arrow to the plot that is indicating the 'north' direction
    addnortharrow(pos = "topright", cols = c("black", "white"), border = "black", text.col = "black", lwd = 3)
    # Add the names of the weather stations used for validation of the temperatures of the temperature chart
    textxy(X = indices_of_locations$X_indices, Y = indices_of_locations$Y_indices, labs = stations_validation_names, cex = 2-(max(number_horizontal_steps, number_vertical_steps)*0.005), offset = 0.6, col = "black")
    print(chartplot)
    dev.off()
  }
}

# Function that plots a temperature chart with the temperature residuals (begin temperature - temperature) at a particular time
plot_chart_temp_res = function(chart, dataframe, number_horizontal_steps, number_vertical_steps, step_distance, number_reference, min_temp, max_temp, indices_of_locations, stations_validation_names, correction_type, land_cover_map, rotate){
  
  # Create axes with distance information for the temperature chart
  number_horizontal_steps = round_any(number_horizontal_steps-1, 1000/step_distance, f = floor)
  number_vertical_steps = round_any(number_vertical_steps-1, 1000/step_distance, f = floor)
  horizontal_labels = seq(from = 0, to = (step_distance * number_horizontal_steps)/1000, by = 1)
  horizontal_labels_positions = seq(from = 1, to = number_horizontal_steps+1 , by = 1000/step_distance)
  vertical_labels = seq(from = 0, to = (step_distance * number_vertical_steps)/1000, by = 1)
  vertical_labels_positions = seq(from = 1, to = number_vertical_steps+1, by = 1000/step_distance)
  
  # Create a color palette used for coloring the gridpoints of the temperature chart
  colors_chart = rev(brewer.pal(11, "RdBu"))
  
  if (rotate == TRUE & land_cover_map == "ESM"){
    # Plot the temperature chart
    png(paste0("/home/michiel/Documenten/Atmospheric_modeling/Produced_data/chart_temp_res_LINEAR_", correction_type, "_", number_reference, "_", land_cover_map, "_rotated.png"), width = 720, height = 720)   # TITLE OF PLOT MAY BE CHANGED DEPENDING ON THE REGION THE TEMPERATURE CHART IS COVERING
    chartplot = plot(chart, axis.col = NULL, axis.row = NULL, na.col = "gray60", xlab = "", ylab = "", main=paste0("Chart of Antwerp region \n Temperature residuals at: ", dataframe$fulldate[number_reference], " (in UTC) \n based on the ", land_cover_map, " land cover map"), key = list(side = 2, cex.axis = 0.8), col = colors_chart, breaks = seq(min_temp, max_temp, length.out=11)) + axis(side = 1, at = horizontal_labels_positions, labels = horizontal_labels) + mtext("Km", side = 1, line = 3) + axis(side = 4, at = vertical_labels_positions, labels = vertical_labels)
    # Plot the locations of the weather stations used for validation of the temperatures of the temperature chart
    points(x = indices_of_locations$X_indices, y = indices_of_locations$Y_indices, col = "black", pch = 5, cex = 100/max(number_horizontal_steps, number_vertical_steps), lwd = 4-(max(number_horizontal_steps, number_vertical_steps)*0.01))
    # Add an arrow to the plot that is indicating the 'north' direction
    addnortharrow(pos = "topright", cols = c("black", "white"), border = "black", text.col = "black", lwd = 3)
    # Add the names of the weather stations used for validation of the temperatures of the temperature chart
    textxy(X = indices_of_locations$X_indices, Y = indices_of_locations$Y_indices, labs = stations_validation_names, cex = 2-(max(number_horizontal_steps, number_vertical_steps)*0.005), offset = 0.6, col = "black")
    print(chartplot)
    dev.off()
  }
  
  else{
    # Plot the temperature chart
    png(paste0("/home/michiel/Documenten/Atmospheric_modeling/Produced_data/chart_temp_res_LINEAR_", correction_type, "_", number_reference, "_", land_cover_map, ".png"), width = 720, height = 720)   # TITLE OF PLOT MAY BE CHANGED DEPENDING ON THE REGION THE TEMPERATURE CHART IS COVERING
    chartplot = plot(chart, axis.col = NULL, axis.row = NULL, na.col = "gray60", xlab = "", ylab = "", main=paste0("Chart of Antwerp region \n Temperature residuals at: ", dataframe$fulldate[number_reference], " (in UTC) \n based on the ", land_cover_map, " land cover map"), key = list(side = 2, cex.axis = 0.8), col = colors_chart, breaks = seq(min_temp, max_temp, length.out=11)) + axis(side = 1, at = horizontal_labels_positions, labels = horizontal_labels) + mtext("Km", side = 1, line = 3) + axis(side = 4, at = vertical_labels_positions, labels = vertical_labels)
    # Plot the locations of the weather stations used for validation of the temperatures of the temperature chart
    points(x = indices_of_locations$X_indices, y = indices_of_locations$Y_indices, col = "black", pch = 5, cex = 100/max(number_horizontal_steps, number_vertical_steps), lwd = 4-(max(number_horizontal_steps, number_vertical_steps)*0.01))
    # Add an arrow to the plot that is indicating the 'north' direction
    addnortharrow(pos = "topright", cols = c("black", "white"), border = "black", text.col = "black", lwd = 3)
    # Add the names of the weather stations used for validation of the temperatures of the temperature chart
    textxy(X = indices_of_locations$X_indices, Y = indices_of_locations$Y_indices, labs = stations_validation_names, cex = 2-(max(number_horizontal_steps, number_vertical_steps)*0.005), offset = 0.6, col = "black")
    print(chartplot)
    dev.off()
  }
}

# Function that plots a temperature chart with the temperature differences between the BBK temperature chart and the ESM temperature chart
plot_chart_diff = function(chart, dataframe, number_horizontal_steps, number_vertical_steps, step_distance, number_reference, min_temp, max_temp, indices_of_locations, stations_validation_names, correction_type, land_cover_map){
  
  # Create axes with distance information for the temperature chart
  number_horizontal_steps = round_any(number_horizontal_steps-1, 1000/step_distance, f = floor)
  number_vertical_steps = round_any(number_vertical_steps-1, 1000/step_distance, f = floor)
  horizontal_labels = seq(from = 0, to = (step_distance * number_horizontal_steps)/1000, by = 1)
  horizontal_labels_positions = seq(from = 1, to = number_horizontal_steps+1 , by = 1000/step_distance)
  vertical_labels = seq(from = 0, to = (step_distance * number_vertical_steps)/1000, by = 1)
  vertical_labels_positions = seq(from = 1, to = number_vertical_steps+1, by = 1000/step_distance)
  
  # Create a color palette used for coloring the gridpoints of the temperature chart
  colors_chart = rev(brewer.pal(11, "RdBu"))
  
  # Plot the temperature chart
  png(paste0("/home/michiel/Documenten/Atmospheric_modeling/Produced_data/chart_LINEAR_", correction_type, "_", number_reference, "_diff_", land_cover_map, ".png"), width = 720, height = 720)   # TITLE OF PLOT MAY BE CHANGED DEPENDING ON THE REGION THE TEMPERATURE CHART IS COVERING
  chartplot = plot(chart, axis.col = NULL, axis.row = NULL, na.col = "gray60", xlab = "", ylab = "", main=paste0("Chart of Antwerp region \n Temperature differences at: ", dataframe$fulldate[number_reference], " (in UTC) \n between using the BBK map and ESM map (BBK - ESM)"), key = list(side = 2, cex.axis = 0.8), col = colors_chart, breaks = seq(min_temp, max_temp, length.out=11)) + axis(side = 1, at = horizontal_labels_positions, labels = horizontal_labels) + mtext("Km", side = 1, line = 3) + axis(side = 4, at = vertical_labels_positions, labels = vertical_labels)
  # Plot the locations of the weather stations used for validation of the temperatures of the temperature chart
  points(x = indices_of_locations$X_indices, y = indices_of_locations$Y_indices, col = "black", pch = 5, cex = 100/max(number_horizontal_steps, number_vertical_steps), lwd = 4-(max(number_horizontal_steps, number_vertical_steps)*0.01))
  # Add an arrow to the plot that is indicating the 'north' direction
  addnortharrow(pos = "topright", cols = c("black", "white"), border = "black", text.col = "black", lwd = 3)
  # Add the names of the weather stations used for validation of the temperatures of the temperature chart
  textxy(X = indices_of_locations$X_indices, Y = indices_of_locations$Y_indices, labs = stations_validation_names, cex = 2-(max(number_horizontal_steps, number_vertical_steps)*0.005), offset = 0.6, col = "black")
  print(chartplot)
  dev.off()
}

# Function that creates a dataframe with the model variables for the gridpoints of the temperature chart
# The land cover fractions of the gridpoints of the temperature chart are calculated based on either the BBK map or ESM map depending on the 'land_cover_map' variable
# The indices of the gridpoints where the weather stations used for validation of the chart temperatures are located are calculated as well
chart_variables_and_locations = function(landcover_dataframe, num_horizontal_steps, num_vertical_steps, step_distance, extension_chart, buffer_distances_chart, loc_list, stations_validation_names, model_variables, land_cover_map){
  
  if (land_cover_map == "BBK"){
    
    # Create a dataframe with the coordinates (in Lambert-72 system) of the gridpoints of the temperature chart together with a number
    chart_dataframe = making_chart_dataframe_BBK(landcover_dataframe, step_distance, num_horizontal_steps, num_vertical_steps, extension_chart)   
    
    # Create buffers for the gridpoints of the temperature chart
    chart_bufferdata = creating_buffers_BBK(chart_dataframe, buffer_distances_chart)
    
    print("Building the BBK small land cover map for the temperature charts")
    
    # Create a small land cover map for the temperature chart (based on the BBK land cover map of Flanders)
    chart_small_landcover_map = creating_small_landcover_map_BBK(BBK_directory, chart_bufferdata, buffer_distances_chart)
    
    print("The small land cover map is built")
    
    plot_chart_small_landcover_BBK(chart_small_landcover_map)   # Plot the BBK small land cover map for the temperature chart
    
    print("Calculating the land cover fractions for the gridpoints of the temperature charts (based on the BBK land cover map of Flanders)")
    
    # Calculate land cover fractions for the gridpoints of the temperature chart (based on the BBK land cover map of Flanders)
    chart_landcover = calculate_landcover_BBK(chart_bufferdata, buffer_distances_chart, chart_small_landcover_map, cores)
    chart_total_landcover = calculate_combined_landcover(chart_landcover, buffer_distances_chart)
    
    print("The land cover fractions are calculated")
  }
  
  if (land_cover_map == "ESM"){
    
    # Create a dataframe with the coordinates (in LAEA Europe system) of the gridpoints of the temperature chart together with a number
    chart_dataframe = making_chart_dataframe_ESM(landcover_dataframe, step_distance, num_horizontal_steps, num_vertical_steps, extension_chart)   
    
    # Create buffers for the gridpoints of the temperature chart
    chart_bufferdata = creating_buffers_ESM(chart_dataframe, buffer_distances_chart)
    
    print("Building the ESM small land cover map for the temperature charts")
    
    # Create a small land cover map for the temperature chart (based on the ESM land cover map of Europe)
    chart_small_landcover_map = creating_small_landcover_map_ESM(ESM_directory, chart_bufferdata, buffer_distances_chart)
    
    print("The small land cover map is built")
    
    plot_chart_small_landcover_ESM(chart_small_landcover_map)   # Plot the ESM small land cover map for the temperature chart
    
    print("Calculating the land cover fractions for the gridpoints of the temperature charts (based on the ESM land cover map of Europe)")
    
    # Calculate land cover fractions for the gridpoints of the temperature chart (based on the ESM land cover map of Europe)
    chart_total_landcover = calculate_landcover_ESM(chart_bufferdata, buffer_distances_chart, chart_small_landcover_map, cores)
    
    print("The land cover fractions are calculated")
  }
  
  print("Removing locations that doesn't fit in the temperature charts")
  
  # Remove weather stations used for validation of the chart temperatures from which the location doesn't fit in the temperature chart
  loc_list = remove_locations(chart_total_landcover, loc_list, stations_validation_names, step_distance)
  # Calculate the numbers of the gridpoints where the weather stations used for validation of the chart temperatures are located
  vector_location_numbers = location_numbers_on_chart(chart_total_landcover, loc_list$locations, step_distance)
  # Calculate the indices of the gridpoints where the weather stations used for validation of the chart temperatures are located
  list_indices_loc = matrix_indices_locations(vector_location_numbers, num_horizontal_steps, num_vertical_steps)
  
  # Create a dataframe with the model variables for the gridpoints of the temperature chart
  # These model variables are used to predict the temperatures of the gridpoints of the chart
  chart_variables = chart_variables_selection(chart_total_landcover, model_variables)
  
  # Create a list containing the dataframe with the model variables for the gridpoints of the temperature chart
  # and the indices of the gridpoints where the weather stations used for validation of the chart temperatures are located
  chart_variables_loc_indices = list("chart_variables" = chart_variables, "indices_of_locations" = list_indices_loc, "chart_dataframe" = chart_dataframe, "validation_names" = loc_list$names)
  
  return(chart_variables_loc_indices)
}

# Function that creates a list of temperatures that will be used in the temperature charts 
chart_timeseries = function(landcover_dataframe, num_horizontal_steps, num_vertical_steps, model_variables, step_distance, corr_temp_timeseries, chart_variables){
  
  # Create a vector of the numbers of the reference datapoints that are used for different temperature decline corrections
  # The different temperature charts are made based on these different temperature decline corrected temperatures
  # The time interval between two consecutive reference datapoints is specified by the 'seconds_per_interval' variable
  vector_ref_numbers = seq(1, length(landcover_dataframe$fulldate), by = seconds_per_interval/10)
  
  # Initialize a list for the temperatures of the different temperature charts
  chart_list = vector(mode = "list", length = length(vector_ref_numbers))
  index_list = 1
  
  # Calculate the temperatures of the gridpoints of the temperature chart for each temperature chart
  # Calculate the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
  for (ref_number in vector_ref_numbers){
    
    # Calculate the temperatures of the gridpoints of the temperature chart and the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
    temperatures_positions = pred_chart_temp_and_selection(landcover_dataframe, chart_variables, model_variables, ref_number, corr_temp_timeseries, step_distance)
    
    # Add the temperatures of the gridpoints to the list for the temperatures of the different temperature charts
    chart_list[[index_list]] = temperatures_positions$chart_temperatures
    if (ref_number == 1){
      
      # Extract the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
      positions_no_temp = temperatures_positions$positions_no_temp
    }
    index_list = index_list + 1
  }
  
  # Create a list consisting of: a list of temperatures that will be used in the temperature charts 
  # and a vector with the positions of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements
  returnlist = list("chart_temperatures_list" = chart_list, "positions_no_temp" = positions_no_temp)
  return(returnlist)
}

# Function that creates the temperature charts of the environment of the route that is used for the mobile bicycle measurement campaign 
# The measurements of this campaign are used as input for the linear model with which the charts are made
# The temperature decline correction that is used for building the linear model is based on either the BBK map or ESM map depending on the 'land_cover_map' variable
# The charts are made at specific reference times. The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings of the main script
create_temp_chart = function(chart_list, num_horizontal_steps, num_vertical_steps, seconds_per_interval, landcover_dataframe, step_distance, number_of_boundaries_removed, list_indices_loc, stations_validation_names, correction_type, land_cover_map, rotate){
  
  # Initialize vectors for the maximum temperature and minimum temperature of all temperature charts
  minimumvector = c()
  maximumvector = c()
  
  # Initialize a list for the temperatures of the different temperature charts
  chart_list_temp = vector(mode = "list", length = length(chart_list$chart_temperatures_list))
  
  # Fill the list for the temperatures of the different temperature charts
  for (index in 1:length(chart_list$chart_temperatures_list)){
    # Extract the temperatures of the gridpoints of the temperature chart
    chart_temperatures = chart_list$chart_temperatures_list[[index]]
    # Set temperatures of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements to NA value
    chart_temperatures[chart_list$positions_no_temp] = NA
    
    # Remove the outermost boundary of the temperature chart a specified number of times
    if (number_of_boundaries_removed > 0){
      for (number in 1:number_of_boundaries_removed){
        
        chart_temperatures = remove_boundary(chart_temperatures, num_vertical_steps, num_horizontal_steps)
        
        # Change the values of the number of horizontal steps and number of vertical steps of the temperature chart
        num_horizontal_steps = num_horizontal_steps-2
        num_vertical_steps = num_vertical_steps-2
      }
      
      # Change the values of the number of horizontal steps and number of vertical steps of the temperature chart to the original values (without boundary cells removed)
      num_horizontal_steps = num_horizontal_steps+(2*number_of_boundaries_removed)
      num_vertical_steps = num_vertical_steps+(2*number_of_boundaries_removed)
    }
    # Add the maximum and minimum temperatures of the temperature chart to the vectors for the maximum and minimum temperature of all temperature charts
    minimumvector = c(minimumvector, min(chart_temperatures, na.rm = TRUE))
    maximumvector = c(maximumvector, max(chart_temperatures, na.rm = TRUE))
    
    # Add the temperatures of the temperature chart to the list for the temperatures of the different temperature charts
    chart_list_temp[[index]] = chart_temperatures
  }
  
  # Calculate the absolute maximum and minimum temperature based on all temperature charts (used in plot legend)
  minimum_temp = min(minimumvector)
  maximum_temp = max(maximumvector)
  
  # Create a list with the indices and names of the weather stations that will be used for validation of the temperatures of the temperature charts
  list_indices_loc = c(list_indices_loc, "names" = list(stations_validation_names))
  # Change the indices of the location of the weather stations on the temperature charts when the outermost boundary is removed a specified number of times
  if (number_of_boundaries_removed > 0){
    for (number in 1:number_of_boundaries_removed){
      # Change the indices of the location of the weather stations on the temperature charts (because the outermost boundary is removed)
      list_indices_loc = indices_convert(list_indices_loc, num_vertical_steps, num_horizontal_steps)
      
      # Change the values of the number of horizontal steps and number of vertical steps of the temperature charts
      num_horizontal_steps = num_horizontal_steps-2
      num_vertical_steps = num_vertical_steps-2
    }
  }
  
  # Plot each of the temperature charts
  for (i in 1:length(chart_list_temp)){
    # Extract the temperatures of the chart
    chart_temp = chart_list_temp[[i]]
    
    # Transform the vector with the chart temperatures to a matrix
    chart = making_chart(chart_temp, num_horizontal_steps, num_vertical_steps)
    
    # Calculate the reference number of the datapoint that is used as reference for the temperature decline correction and thus as reference for the temperature chart
    reference_num = 1 + (i-1)*(seconds_per_interval/10)
    
    # Plot the temperature chart
    plot_chart(chart, landcover_dataframe, num_horizontal_steps, num_vertical_steps, step_distance, reference_num, minimum_temp,maximum_temp, list_indices_loc, list_indices_loc$names, correction_type, land_cover_map, rotate) 
  }
  
  return(chart_list_temp)
}

# Function that creates temperature charts based on the ESM land cover map
# The temperature charts are either in Lambert-72 coordinates or LAEA Europe coordinates
create_temp_chart_ESM = function(chart_list_ESM, landcover_dataframe, num_horizontal_steps, num_vertical_steps, chart_dataframe_ESM, chart_dataframe_BBK, seconds_per_interval, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, step_distance, stations_validation_names_ESM, stations_validation_names_BBK, correction_type, rotate){
  
  # Create ESM temperature charts in Lambert-72 coordinates
  if (rotate){
    
    # Initialize vectors for the maximum temperature and minimum temperature of all temperature charts
    minimumvector = c()
    maximumvector = c()
    
    # Initialize a list for the temperatures of the different temperature charts
    chart_list = vector(mode = "list", length = length(chart_list_ESM$chart_temperatures_list))
    # Calculate the temperatures of the temperature charts when projecting the temperature charts from LAEA Europe to Lambert-72 coordinates by using bilinear interpollation
    for (index in 1:length(chart_list_ESM$chart_temperatures_list)){
      
      # Create a raster for the ESM temperature chart
      raster_ESM = raster(nrow = num_vertical_steps, ncol = num_horizontal_steps, ext=extent(min(chart_dataframe_ESM$X_laea), max(chart_dataframe_ESM$X_laea), min(chart_dataframe_ESM$Y_laea), max(chart_dataframe_ESM$Y_laea)), crs = "+init=epsg:3035")
      
      # Extract the temperatures of the ESM temperature chart
      temp_ESM = chart_list_ESM$chart_temperatures_list[[index]]
      # Set temperatures of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements to NA value
      temp_ESM[chart_list_ESM$positions_no_temp] = NA
      
      # Fill the raster for the ESM temperature chart with the temperatures of the ESM temperature chart
      raster_ESM = setValues(raster_ESM, temp_ESM)
      # Project the raster for the ESM temperature chart to Lambert-72 coordinates
      raster_ESM_proj = projectRaster(raster_ESM, crs="+init=epsg:31370")
      # Crop the projected raster for the ESM temperature chart to the same extent as the BBK temperature chart
      ext = extent(min(chart_dataframe_BBK$X_lambert), max(chart_dataframe_BBK$X_lambert), min(chart_dataframe_BBK$Y_lambert), max(chart_dataframe_BBK$Y_lambert))
      final_raster_ESM_proj = crop(raster_ESM_proj, ext)
      
      # Extract the temperatures of the projected raster
      temp_ESM = getValues(final_raster_ESM_proj)
      
      # Remove the outermost boundary of the projected temperature chart a specified number of times
      if (number_of_boundaries_removed > 0){
        for (number in 1:number_of_boundaries_removed){
          
          temp_ESM = remove_boundary(temp_ESM, num_vertical_steps, num_horizontal_steps)
          
          # Change the values of the number of horizontal steps and number of vertical steps of the temperature chart
          num_horizontal_steps = num_horizontal_steps-2
          num_vertical_steps = num_vertical_steps-2
        }
        
        # Change the values of the number of horizontal steps and number of vertical steps of the temperature chart to the original values (without boundary cells removed)
        num_horizontal_steps = num_horizontal_steps+(2*number_of_boundaries_removed)
        num_vertical_steps = num_vertical_steps+(2*number_of_boundaries_removed)
      }
      
      
      # Add the maximum and minimum temperatures of the projected temperature chart to the vectors for the maximum and minimum temperature of all temperature charts
      maximumvector = c(maximumvector, max(temp_ESM, na.rm = TRUE))
      minimumvector = c(minimumvector, min(temp_ESM, na.rm = TRUE))
      
      # Add the temperatures of the projected temperature chart to the list for the temperatures of the different temperature charts
      chart_list[[index]] = temp_ESM
    }
    
    # Calculate the absolute maximum and minimum temperature based on all temperature charts (used in plot legend)
    maximum_temp = max(maximumvector)
    minimum_temp = min(minimumvector)
    
    # Create a list with the indices and names of the weather stations that will be used for validation of the temperatures of the temperature charts
    indices_of_locations_BBK = c(indices_of_locations_BBK, "names" = list(stations_validation_names_BBK))
    # Change the indices of the location of the weather stations on the temperature charts when the outermost boundary is removed a specified number of times
    if (number_of_boundaries_removed > 0){
      for (number in 1:number_of_boundaries_removed){
        # Change the indices of the location of the weather stations on the temperature charts (because the outermost boundary is removed)
        indices_of_locations_BBK = indices_convert(indices_of_locations_BBK, num_vertical_steps, num_horizontal_steps)
        
        # Change the values of the number of horizontal steps and number of vertical steps of the temperature charts
        num_horizontal_steps = num_horizontal_steps-2
        num_vertical_steps = num_vertical_steps-2
      }
    }
    
    # Plot each of the temperature charts
    for (i in 1:length(chart_list)){
      # Extract the temperatures of the chart
      chart_temperatures = chart_list[[i]]
      
      # Transform the vector with the chart temperatures to a matrix
      chart = making_chart(chart_temperatures, num_horizontal_steps, num_vertical_steps)
      
      # Calculate the reference number of the datapoint that is used as reference for the temperature decline correction and thus as reference for the temperature chart
      reference_num = 1 + (i-1)*(seconds_per_interval/10)
      
      # Plot the temperature chart
      plot_chart(chart, landcover_dataframe, num_horizontal_steps, num_vertical_steps, step_distance, reference_num, minimum_temp, maximum_temp, indices_of_locations_BBK, indices_of_locations_BBK$names, correction_type, "ESM", rotate) 
    }
    
  }
  
  # Create ESM temperature charts in LAEA Europe coordinates
  else{
    
    # Create the temperature chart
    chart_list = create_temp_chart(chart_list_ESM, num_horizontal_steps, num_vertical_steps, seconds_per_interval, landcover_dataframe, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, stations_validation_names_ESM, correction_type, "ESM", rotate)
    
  }
  
  return(chart_list)
}

# Function that creates temperature charts with the temperature residuals (begin temperature - temperature) for the different temperature charts
create_chart_temp_res = function(chart_list, dataframe, num_horizontal_steps, num_vertical_steps, seconds_per_interval, step_distance, number_of_boundaries_removed, indices_of_locations_ESM, indices_of_locations_BBK, stations_validation_names_ESM, stations_validation_names_BBK, correction_type, land_cover_map, rotate){
  
  # Initialize vectors for the maximum temperature and minimum temperature of all temperature charts
  maximumvector = c()
  minimumvector = c()
  
  # Create a list with the indices and names of the weather stations that will be used for validation of the temperatures of the temperature charts
  indices_of_locations_BBK = c(indices_of_locations_BBK, "names" = list(stations_validation_names_BBK))
  indices_of_locations_ESM = c(indices_of_locations_ESM, "names" = list(stations_validation_names_ESM))
  # Change the indices of the location of the weather stations on the temperature charts when the outermost boundary is removed a specified number of times
  if (number_of_boundaries_removed > 0){
    for (number in 1:number_of_boundaries_removed){
      # Change the indices of the location of the weather stations on the temperature charts (because the outermost boundary is removed)
      indices_of_locations_BBK = indices_convert(indices_of_locations_BBK, num_vertical_steps, num_horizontal_steps)
      indices_of_locations_ESM = indices_convert(indices_of_locations_ESM, num_vertical_steps, num_horizontal_steps)
      
      # Change the values of the number of horizontal steps and number of vertical steps of the temperature charts
      num_horizontal_steps = num_horizontal_steps-2
      num_vertical_steps = num_vertical_steps-2
    }
  }
  
  # Initialize a list for the temperature residuals of the different temperature charts
  chart_temp_res_list = vector(mode = "list", length = (length(chart_list)-1))
  
  # Calculate the temperature residuals (begin temperature - temperature) for all temperature charts
  for (index in 2:length(chart_list)){
    # Calculate the temperature residuals for the partcular temperature chart
    temp_res = chart_list[[1]]-chart_list[[index]]
    
    # Add the maximum and minimum temperature residuals for the particular temperature chart to the vectors of the maximum and minimum temperature residuals 
    # of the different temperature charts
    maximumvector = c(maximumvector, max(temp_res, na.rm = TRUE))
    minimumvector = c(minimumvector, min(temp_res, na.rm = TRUE))
    
    # Add the temperature resiuals of the particular temperature chart to the list for the temperature residuals of the different temperature charts
    chart_temp_res_list[[index-1]] = temp_res
  }
  
  # Calculate the absolute maximum and minimum temperature residuals based on all temperature charts (used for plot legend)
  max_temp_res = max(maximumvector)
  min_temp_res = min(minimumvector)
  
  
  # Plot each of the temperature residual charts
  for (index_chart in 1:length(chart_temp_res_list)){
    # Extract the temperature residuals of the chart
    chart_temp_res = chart_temp_res_list[[index_chart]]
    
    # Transform the vector with the chart temperature residuals to a matrix
    chart_temp_res_matrix = making_chart(chart_temp_res, num_horizontal_steps, num_vertical_steps)
    
    # Calculate the reference number of the datapoint that is used as reference for the temperature decline correction and thus as reference for the temperature residual chart
    reference_num = 1 + (index_chart)*(seconds_per_interval/10)
    
    if (rotate == FALSE & land_cover_map == "ESM"){
      # Plot the temperature residual chart
      plot_chart_temp_res(chart_temp_res_matrix, dataframe, num_horizontal_steps, num_vertical_steps, step_distance, reference_num, min_temp_res, max_temp_res, indices_of_locations_ESM, indices_of_locations_ESM$names, correction_type, "ESM", rotate)
    }
    if (land_cover_map == "BBK"){
      # Plot the temperature residual chart
      plot_chart_temp_res(chart_temp_res_matrix, dataframe, num_horizontal_steps, num_vertical_steps, step_distance, reference_num, min_temp_res, max_temp_res, indices_of_locations_BBK, indices_of_locations_BBK$names, correction_type, "BBK", rotate)
    }
    if (rotate == TRUE & land_cover_map == "ESM"){
      # Plot the temperature residual chart
      plot_chart_temp_res(chart_temp_res_matrix, dataframe, num_horizontal_steps, num_vertical_steps, step_distance, reference_num, min_temp_res, max_temp_res, indices_of_locations_BBK, indices_of_locations_BBK$names, correction_type, "ESM", rotate)
    }
  }
}

# Function that removes the outermost edge of a raster
# The raster is in vector format with first element in upper left corner and last element in bottom right corner
remove_boundary = function(vector_raster, nrows, ncols){
  
  # Calculate the indices of all elements that should be removed
  upper_row = 1:ncols
  bottom_row = 1:ncols + (nrows-1)*ncols
  left_side = 1:(nrows-2)*ncols+1
  right_side = (1:(nrows-2) + 1)*ncols
  
  # Create a vector with the indices of all elements that should be removed
  remove_vector = c(upper_row, bottom_row, right_side, left_side)
  
  # Remove the elements
  new_vector_raster = vector_raster[-remove_vector]
  return(new_vector_raster)
}

# Function that changes the indices of locations of weather stations on the temperature charts when the outermost edge of the charts is removed
indices_convert = function(indices_list, nrows, ncols){
  
  # Extract the X-indices, Y-indices and names of the weather stations (without the outermost edge removed)
  X_indices = indices_list$X_indices
  Y_indices = indices_list$Y_indices
  names = indices_list$names
  
  # Convert the indices
  X_indices = X_indices-1
  Y_indices = Y_indices-1
  
  # Remove indices and names of weather stations that fall out of the domain of the temperature chart when removing the outermost edge of the chart
  if (0 %in% X_indices | (ncols-1) %in% X_indices){
    indices_to_remove = which(X_indices == 0 | X_indices == (ncols-1))
    X_indices = X_indices[-indices_to_remove]
    Y_indices = Y_indices[-indices_to_remove]
    names = names[-indices_to_remove]
  }
  if (0 %in% Y_indices | (nrows-1) %in% Y_indices){
    indices_to_remove = which(Y_indices == 0 | Y_indices == (nrows-1))
    X_indices = X_indices[-indices_to_remove]
    Y_indices = Y_indices[-indices_to_remove]
    names = names[-indices_to_remove]
  }
  
  ind_list = list("X_indices" = X_indices, "Y_indices" = Y_indices, "names" = names)
  return(ind_list)
}

create_diff_chart_mocca_vlinder = function(chart_temp_list_mocca, chart_temp_list_vlinder, dataframe, num_horizontal_steps, num_vertical_steps, number_of_boundaries_removed, step_distance, seconds_per_interval, indices_of_locations, stations_validation_names, land_cover_map){
  
  # Initialize vectors for the minimum and maximum temperature differences between the BBK and ESM temperature charts (at the particular times)
  maximumvector = c()
  minimumvector = c()
  
  # Initialize a list for all temperature differences between the BBK and ESM temperature charts (at the particular times)
  diff_temp_chart_list = vector(mode = "list", length = length(chart_temp_list_mocca$chart_temperatures_list))
  
  # Fill the list for the temperatures of the different temperature charts
  for (index in 1:length(chart_temp_list_mocca$chart_temperatures_list)){
    
    # Extract the temperatures of the gridpoints of the temperature chart
    chart_temperatures_mocca = chart_temp_list_mocca$chart_temperatures_list[[index]]
    # Set temperatures of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements to NA value
    chart_temperatures_mocca[chart_temp_list_mocca$positions_no_temp] = NA
    
    # Extract the temperatures of the gridpoints of the temperature chart
    chart_temperatures_vlinder = chart_temp_list_vlinder$chart_temperatures_list[[index]]
    # Set temperatures of the gridpoints with land cover fractions that are not likely to the land cover fractions that are encountered in the measurements to NA value
    chart_temperatures_vlinder[chart_temp_list_vlinder$positions_no_temp] = NA
    
    # Remove the outermost boundary of the temperature chart a specified number of times
    if (number_of_boundaries_removed > 0){
      for (number in 1:number_of_boundaries_removed){
        
        chart_temperatures_mocca = remove_boundary(chart_temperatures_mocca, num_vertical_steps, num_horizontal_steps)
        chart_temperatures_vlinder = remove_boundary(chart_temperatures_vlinder, num_vertical_steps, num_horizontal_steps)
        
        # Change the values of the number of horizontal steps and number of vertical steps of the temperature chart
        num_horizontal_steps = num_horizontal_steps-2
        num_vertical_steps = num_vertical_steps-2
      }
      
      # Change the values of the number of horizontal steps and number of vertical steps of the temperature chart to the original values (without boundary cells removed)
      num_horizontal_steps = num_horizontal_steps+(2*number_of_boundaries_removed)
      num_vertical_steps = num_vertical_steps+(2*number_of_boundaries_removed)
    }
    
    # Calculate the temperature differences between the BBK temperature chart and ESM temperature chart
    diff_temp = chart_temperatures_mocca - chart_temperatures_vlinder
    
    # Add the maximum and minimum temperatures of the temperature chart to the vectors for the maximum and minimum temperature of all temperature charts
    minimumvector = c(minimumvector, min(diff_temp, na.rm = TRUE))
    maximumvector = c(maximumvector, max(diff_temp, na.rm = TRUE))
    
    # Add the temperatures of the temperature chart to the list for the temperatures of the different temperature charts
    diff_temp_chart_list[[index]] = diff_temp
  }
  
  # Calculate the absolute minimum and maximum temperature differences between the BBK and ESM temperature charts (used for plot legend) 
  max_diff = max(maximumvector)
  min_diff = min(minimumvector)
  
  # Create a list with the indices and names of the weather stations that will be used for validation of the temperatures of the temperature charts
  indices_of_locations = c(indices_of_locations, "names" = list(stations_validation_names))
  
  # Change the indices of the location of the weather stations on the temperature charts when the outermost boundary is removed a specified number of times
  if (number_of_boundaries_removed > 0){
    for (number in 1:number_of_boundaries_removed){
      # Change the indices of the location of the weather stations on the temperature charts (because the outermost boundary is removed)
      indices_of_locations = indices_convert(indices_of_locations, num_vertical_steps, num_horizontal_steps)
      
      # Change the values of the number of horizontal steps and number of vertical steps of the temperature charts
      num_horizontal_steps = num_horizontal_steps-2
      num_vertical_steps = num_vertical_steps-2
    }
  }
  
  # Create the different temperature charts with the temperature differences between the BBK and ESM temperature charts and make a plot of them
  for (index_chart in 1:length(diff_temp_chart_list)){
    # Extract the temperature differences of the chart
    diff_temp_for_chart = diff_temp_chart_list[[index_chart]]
    
    # Transform the vector with the temperature differences between the BBK and ESM temperature charts to a matrix
    diff_temp_matrix = making_chart(diff_temp_for_chart, num_horizontal_steps, num_vertical_steps)
    
    # Calculate the reference number of the datapoint that is used as reference for the temperature decline correction and thus as reference for the temperature chart
    reference_num = 1 + (index_chart-1)*(seconds_per_interval/10)
    
    # Plot the temperature difference chart
    plot_chart_diff_mocca_vlinder(diff_temp_matrix, dataframe, num_horizontal_steps, num_vertical_steps, step_distance, min_diff, max_diff, reference_num, indices_of_locations, indices_of_locations$names, land_cover_map)
  }
  
}

# Function that creates temperature charts with the temperature differences between the BBK temperature chart and the ESM temperature chart (projected to the BBK temperature chart domain) at particular times
create_diff_chart_BBK_ESM = function(chart_temp_list_BBK, chart_temp_list_ESM, chart_dataframe_BBK, chart_dataframe_ESM, dataframe, num_horizontal_steps, num_vertical_steps, step_distance, seconds_per_interval, number_of_boundaries_removed, indices_of_locations, stations_validation_names, correction_type){
  
  # Initialize vectors for the minimum and maximum temperature differences between the BBK and ESM temperature charts (at the particular times)
  maximumvector = c()
  minimumvector = c()
  
  # Initialize a list for all temperature differences between the BBK and ESM temperature charts (at the particular times)
  diff_temp_chart_list = vector(mode = "list", length = length(chart_temp_list_BBK))
  
  # Calculate the temperature differences between the BBK and ESM temperature charts (at all particular times)
  for (index in 1:length(chart_temp_list_ESM)){
    
    # Extract the temperatures of the BBK and ESM temperature charts (at the particular time) 
    temp_BBK = chart_temp_list_BBK[[index]]
    temp_ESM = chart_temp_list_ESM[[index]]
    
    # Calculate the temperature differences between the BBK temperature chart and ESM temperature chart
    diff_temp = temp_BBK-temp_ESM
    
    # Add the temperature differences to the list for the temperature differences between the BBK and ESM temperature charts 
    diff_temp_chart_list[[index]] = diff_temp
    
    # Add the minimum and maximum temperature differences to the vectors for the minimum and maximum temperature differences between the BBK and ESM temperature charts
    maximumvector = c(maximumvector, max(diff_temp, na.rm = TRUE))
    minimumvector = c(minimumvector, min(diff_temp, na.rm = TRUE))
  }
  
  # Calculate the absolute minimum and maximum temperature differences between the BBK and ESM temperature charts (used for plot legend) 
  max_diff = max(maximumvector)
  min_diff = min(minimumvector)
  
  # Create a list with the indices and names of the weather stations that will be used for validation of the temperatures of the temperature charts
  indices_of_locations = c(indices_of_locations, "names" = list(stations_validation_names))
  # Change the indices of the location of the weather stations on the temperature charts when the outermost boundary is removed a specified number of times
  if (number_of_boundaries_removed > 0){
    for (number in 1:number_of_boundaries_removed){
      # Change the indices of the location of the weather stations on the temperature charts (because the outermost boundary is removed)
      indices_of_locations = indices_convert(indices_of_locations, num_vertical_steps, num_horizontal_steps)
      
      # Change the values of the number of horizontal steps and number of vertical steps of the temperature charts
      num_horizontal_steps = num_horizontal_steps-2
      num_vertical_steps = num_vertical_steps-2
    }
  }
  
  # Create the different temperature charts with the temperature differences between the BBK and ESM temperature charts and make a plot of them
  for (index_chart in 1:length(diff_temp_chart_list)){
    # Extract the temperature differences of the chart
    diff_temp_for_chart = diff_temp_chart_list[[index_chart]]
    
    # Transform the vector with the temperature differences between the BBK and ESM temperature charts to a matrix
    diff_temp_matrix = making_chart(diff_temp_for_chart, num_horizontal_steps, num_vertical_steps)
    
    # Calculate the reference number of the datapoint that is used as reference for the temperature decline correction and thus as reference for the temperature chart
    reference_num = 1 + (index_chart-1)*(seconds_per_interval/10)
    
    # Plot the temperature difference chart
    plot_chart_diff(diff_temp_matrix, dataframe, num_horizontal_steps, num_vertical_steps, step_distance, reference_num, min_diff, max_diff, indices_of_locations, indices_of_locations$names, correction_type, "BBK_ESM")
  }
}

#-----------------------------------------------------------------------------------------------------------#

#------------------------------------ROUTE VISUALISATION FUNCTIONS------------------------------------------#

# Function that manipulates the dataframe that is used for plotting of the ESM small land cover map 
dataframe_landcover_map_ESM_setup = function(dataframe_landcover_map_ESM){
  for (row in 1:length(dataframe_landcover_map_ESM$x)){
    number = dataframe_landcover_map_ESM$ESM2012_Flanders[row]
    if (number >= 1.5 & number < 6){
      class_number = 2
    }
    if (number < 1.5){
      class_number = round(number)
    }
    if (number >= 6 & number <= 12.5){
      class_number = 10
    }
    if (number > 12.5 & number <= 40.5){
      class_number = round(number/5)*5
    }
    if (number > 40.5 & number <= 43){
      class_number = 41
    }
    if (number > 43){
      class_number = round(number/5)*5
    }
    dataframe_landcover_map_ESM$ESM2012_Flanders[row] = class_number
  }
  
  return(dataframe_landcover_map_ESM)
}

# Function that visualises the route on the BBK land cover map of Flanders
visualisation_route_BBK = function(landcover_dataframe, data_landcover_map){
  
  # Visualise the route
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_route_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest impervious", "Rail road", "Water", "Rest non impervious", "Crop land", "Grass shrub", "Tree", "Grass shrub agriculture", "Grass shrub road", "Trees road", "Grass shrub water", "Trees water"), name = "Land cover type")                                                                                                    
  plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y), col = "black", shape = 19, size = 1) + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the route on the BBK land cover map of Flanders")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
  print(plot3)
  dev.off()
}

# Function that visualises the route on the ESM land cover map of Europe
visualisation_route_ESM = function(landcover_dataframe, data_landcover_map){
  
  # Visualise the route
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_route_ESM.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")                                                                                                    
  plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y), col = "black", shape = 19, size = 1) + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the route on the ESM land cover map of Europe")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
  print(plot3)
  dev.off()
}

# Function that visualises the measured uncorrected temperatures of the route 
# The route is plotted on the BBK land cover map of Flanders
visualisation_route_measured_temp_BBK = function(landcover_dataframe, data_landcover_map, inertia_bool, decline_bool, corr_temp_timeseries = 0){
  
  if (inertia_bool == FALSE & decline_bool == FALSE){
    # Calculate the minimum and maximum measured temperatures
    min_temp = min(landcover_dataframe$temperature)
    max_temp = max(landcover_dataframe$temperature)
    print(min_temp)
    print(max_temp)
    
    # Visualise the measured temperatures 
    pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_measured_temp_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
    plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest impervious", "Rail road", "Water", "Rest non impervious", "Crop land", "Grass shrub", "Tree", "Grass shrub agriculture", "Grass shrub road", "Trees road", "Grass shrub water", "Trees water"), name = "Land cover type")                                                                                                    
    plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = temperature), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the measured uncorrected temperatures of the route \n on the BBK land cover map of Flanders")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
    print(plot3)
    dev.off()
  }
  
  if (inertia_bool == TRUE & decline_bool == FALSE){
    # Calculate the minimum and maximum measured temperatures
    min_temp = min(landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    max_temp = max(landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    
    # Visualise the measured temperatures 
    pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_measured_temp_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
    plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest impervious", "Rail road", "Water", "Rest non impervious", "Crop land", "Grass shrub", "Tree", "Grass shrub agriculture", "Grass shrub road", "Trees road", "Grass shrub water", "Trees water"), name = "Land cover type")                                                                                                    
    plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = measured_temp), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the measured uncorrected temperatures of the route \n on the BBK land cover map of Flanders")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
    print(plot3)
    dev.off()
  }
  
  if (inertia_bool == TRUE & decline_bool == TRUE){
    # Calculate the minimum and maximum measured temperatures
    min_temp = min(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    max_temp = max(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    
    # Visualise the measured temperatures 
    pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_measured_temp_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
    plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest impervious", "Rail road", "Water", "Rest non impervious", "Crop land", "Grass shrub", "Tree", "Grass shrub agriculture", "Grass shrub road", "Trees road", "Grass shrub water", "Trees water"), name = "Land cover type")                                                                                                    
    plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = measured_temp), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the measured uncorrected temperatures of the route \n on the BBK land cover map of Flanders")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
    print(plot3)
    dev.off()
  }
}

# Function that visualises the measured uncorrected temperatures of the route 
# The route is plotted on the ESM land cover map of Europe
visualisation_route_measured_temp_ESM = function(landcover_dataframe, data_landcover_map, inertia_bool, decline_bool, corr_temp_timeseries = 0){
  
  if (inertia_bool == FALSE & decline_bool == FALSE){
    # Calculate the minimum and maximum measured temperatures
    min_temp = min(landcover_dataframe$temperature)
    max_temp = max(landcover_dataframe$temperature)
    
    # Visualise the measured temperatures 
    pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_measured_temp_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
    plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
    plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = temperature), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the measured uncorrected temperatures of the route \n on the ESM land cover map of Europe")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))                                                                                                 
    print(plot3)
    dev.off()
  }
  
  if (inertia_bool == TRUE & decline_bool == FALSE){
    # Calculate the minimum and maximum measured temperatures
    min_temp = min(landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    max_temp = max(landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    
    # Visualise the measured temperatures 
    pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_measured_temp_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
    plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
    plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = measured_temp), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the measured uncorrected temperatures of the route \n on the ESM land cover map of Europe")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))                                                                                                 
    print(plot3)
    dev.off()
  }
  
  if (inertia_bool == TRUE & decline_bool == TRUE){
    # Calculate the minimum and maximum measured temperatures
    min_temp = min(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    max_temp = max(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
    
    # Visualise the measured temperatures 
    pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_measured_temp_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
    plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
    plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = measured_temp), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the measured uncorrected temperatures of the route \n on the ESM land cover map of Europe")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))                                                                                                 
    print(plot3)
    dev.off()
  }
}

# Function that visualises the temperatures of the route that are corrected for thermal inertia
# The route is plotted on the BBK land cover map of Europe
visualisation_route_thermal_inertia_BBK = function(landcover_dataframe, data_landcover_map, decline_bool, corr_temp_timeseries = 0){
  
  if (decline_bool == FALSE){
    # Calculate the minimum and maximum thermal inertia corrected temperatures
    min_temp = min(landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
    max_temp = max(landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
  }
  
  if (decline_bool == TRUE){
    # Calculate the minimum and maximum thermal inertia corrected temperatures
    min_temp = min(corr_temp_timeseries, landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
    max_temp = max(corr_temp_timeseries, landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
  }
  
  # Visualise the temperatures corrected for thermal inertia
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_thermal_inertia_BBK.pdf", width = 9) # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest impervious", "Rail road", "Water", "Rest non impervious", "Crop land", "Grass shrub", "Tree", "Grass shrub agriculture", "Grass shrub road", "Trees road", "Grass shrub water", "Trees water"), name = "Land cover type")                                                                                                    
  plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = thermal_inertia), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the temperatures of the route \n corrected for thermal inertia and not corrected for temperature decline \n on the BBK land cover map of Flanders")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
  print(plot3)
  dev.off()
}

# Function that visualises the temperatures of the route that are corrected for thermal inertia
# The route is plotted on the ESM land cover map of Europe
visualisation_route_thermal_inertia_ESM = function(landcover_dataframe, data_landcover_map, decline_bool, corr_temp_timeseries = 0){
  
  if (decline_bool == FALSE){
    # Calculate the minimum and maximum thermal inertia corrected temperatures
    min_temp = min(landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
    max_temp = max(landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
  }
  
  if (decline_bool == TRUE){
    # Calculate the minimum and maximum thermal inertia corrected temperatures
    min_temp = min(corr_temp_timeseries, landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
    max_temp = max(corr_temp_timeseries, landcover_dataframe$thermal_inertia, landcover_dataframe$measured_temp)
  }
  
  # Visualise the temperatures corrected for thermal inertia
  pdf("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_thermal_inertia_ESM.pdf", width = 9) # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY CHANGE
  plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
  plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = thermal_inertia), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the temperatures of the route \n corrected for thermal inertia and not corrected for temperature decline \n on the ESM land cover map of Europe")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))                                                                                                  
  print(plot3)
  dev.off()
}

# Function that visualises the temperatures of the route for a specific reference time
# The temperatures are corrected for temperature decline (based on the BBK land cover map of Flanders) and the route is plotted on the BBK land cover map of Europe
visualisation_route_temp_decline_BBK = function(landcover_dataframe, data_landcover_map, ref_number, min_temp, max_temp, correction_type){
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_temp_decline_BBK_",correction_type, "_", ref_number, ".pdf"), width = 9)
  # THE NAME 'BBK_FLANDERS2015' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE BBK MAP AND MAY BE CHANGED
  plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(BBK_Flanders2015))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("red3", "grey20", "grey40", "gray60", "blue", "goldenrod4", "yellow", "lawngreen", "green3", "olivedrab1", "darkolivegreen4", "darkgreen", "turquoise1", "seagreen3"), labels = c("Building", "Road", "Rest impervious", "Rail road", "Water", "Rest non impervious", "Crop land", "Grass shrub", "Tree", "Grass shrub agriculture", "Grass shrub road", "Trees road", "Grass shrub water", "Trees water"), name = "Land cover type")                                                                                                    
  plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = temperature), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the temperatures of the route \n corrected for thermal inertia and temperature decline \n on the BBK land cover map of Flanders \n Temperatures at: ", landcover_dataframe$fulldate[ref_number], " (in UTC)")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
  print(plot3)
  dev.off()
}

# Function that visualises the temperatures of the route for a specific reference time
# The temperatures are corrected for temperature decline (based on the ESM land cover map of Europe) and the route is plotted on the ESM land cover map of Europe
visualisation_route_temp_decline_ESM = function(landcover_dataframe, data_landcover_map, ref_number, min_temp, max_temp, correction_type){
  pdf(paste0("/home/michiel/Documenten/VLINDER/Produced_data/visualisation_temp_decline_ESM_",correction_type, "_", ref_number, ".pdf"), width = 9)
  # THE NAME 'ESM2012_FLANDERS' IN THE 'FILL' ARGUMENT IS THE NAME OF THE TIF-FILE OF THE ESM MAP AND MAY BE CHANGED
  plot = ggplot() + geom_raster(data = data_landcover_map, aes(x = x, y = y, fill = as.factor(round(ESM2012_Flanders))), alpha = 0.65) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0), axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  plot2 = plot + scale_fill_manual(values = c("blue", "gray60", "wheat1", "mediumseagreen", "chartreuse", "green2", "yellow3", "gray25", "darkolivegreen3", "palegreen", "darkgreen", "red3"), labels = c("Water", "Railways", "NBU Area - open space", "NBU Area - streets", "NBU Area - green NDVI", "NBU Area - street green NDVI", "BU Area - open space", "BU Area - streets", "BU Area - green NDVI", "BU Area - green urban atlas", "BU Area - street green NDVI", "BU Area - buildings"), name = "Land cover type")
  plot3 = plot2 + geom_point(data = landcover_dataframe, aes(x = X, y = Y, col = temperature), shape = 19, size = 1) + scale_colour_gradientn(colours = c("black", "blue", "red", "yellow", "white"), guide = "colourbar", limits = c(min_temp, max_temp), breaks = c(min_temp, (max_temp-min_temp)/2 + min_temp, max_temp), labels = c(paste0(round(min_temp, digits = 2), "°C"), paste0(round((max_temp-min_temp)/2 + min_temp, digits = 2), "°C"), paste0(round(max_temp, digits = 2), "°C")), name = "Temperature") + guides(alpha = FALSE) + geom_point(data = landcover_dataframe, aes(x = X[1], y = Y[1]), col = "cyan", shape = 5, size = 3.5, stroke = 1.5) + ggtitle(paste0("Visualisation of the temperatures of the route \n corrected for thermal inertia and temperature decline \n on the ESM land cover map of Europe \n Temperatures at: ", landcover_dataframe$fulldate[ref_number], " (in UTC)")) + theme(plot.title = element_text(size = 13), legend.title=element_text(size=13), legend.text=element_text(size=13))
  print(plot3)
  dev.off()
}

# Function that visualises the temperatures of the route corrected for temperature decline (based on the BBK land cover map of Flanders)
# Different temperature decline corrections are used based on different reference datapoints
# The result is a series of plots of the temperatures of the route at different reference times and the route is plotted on the BBK land cover map of Flanders
# The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings of the main script
visualisation_route_timeseries_BBK = function(landcover_dataframe, dataframe_landcover_map, seconds_per_interval, corr_temp_timeseries, correction_type){
  
  # Create a vector of the numbers of the reference datapoints that are used for different temperature decline corrections
  # The time interval between two consecutive reference datapoints is specified by the 'seconds_per_interval' variable
  vector_ref_numbers = seq(1, length(landcover_dataframe$fulldate), by = seconds_per_interval/10)
  
  # Calculate the absolute minimum and maximum temperatures for all temperature decline corrected temperatures and the measured temperatures (used for temperature legend)
  minimum_temp = min(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
  maximum_temp = max(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
  
  # Plot the temperatures of the route for each temperature decline correction (reference time)
  for (reference_number in vector_ref_numbers){
    
    # Extract the temperature decline correction corresponding to the specified reference number
    corr_temperatures = corr_temp_timeseries[[paste0("corrected_", reference_number)]]
    landcover_dataframe$temperature = corr_temperatures
    
    # Visualise the temperatures of the route for the specific reference time
    visualisation_route_temp_decline_BBK(landcover_dataframe, dataframe_landcover_map, reference_number, minimum_temp, maximum_temp, correction_type)
  }
}

# Function that visualises the temperatures of the route corrected for temperature decline (based on the ESM land cover map of Europe)
# Different temperature decline corrections are used based on different reference datapoints
# The result is a series of plots of the temperatures of the route at different reference times and the route is plotted on the ESM land cover map of Europe
# The begin time of the measurements is always included and the time interval is specified by the 'seconds_per_interval' variable in the general settings of the main script
visualisation_route_timeseries_ESM = function(landcover_dataframe, dataframe_landcover_map, seconds_per_interval, corr_temp_timeseries, correction_type){
  
  # Create a vector of the numbers of the reference datapoints that are used for different temperature decline corrections
  # The time interval between two consecutive reference datapoints is specified by the 'seconds_per_interval' variable
  vector_ref_numbers = seq(1, length(landcover_dataframe$fulldate), by = seconds_per_interval/10)
  
  # Calculate the absolute minimum and maximum temperatures for all temperature decline corrected temperatures and the measured temperatures (used for temperature legend)
  minimum_temp = min(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
  maximum_temp = max(corr_temp_timeseries, landcover_dataframe$measured_temp, landcover_dataframe$thermal_inertia)
  
  # Plot the temperatures of the route for each temperature decline correction (reference time)
  for (reference_number in vector_ref_numbers){
    
    # Extract the temperature decline correction corresponding to the specified reference number
    corr_temperatures = corr_temp_timeseries[[paste0("corrected_", reference_number)]]
    landcover_dataframe$temperature = corr_temperatures
    
    # Visualise the temperatures of the route for the specific reference time
    visualisation_route_temp_decline_ESM(landcover_dataframe, dataframe_landcover_map, reference_number, minimum_temp, maximum_temp, correction_type)
  }
}

#-----------------------------------------------------------------------------------------------------------#
