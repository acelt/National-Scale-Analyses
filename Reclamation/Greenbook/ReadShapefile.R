ReadShapefile <- function(Shapefile_Name, Shapefile_Path, Attribute_Field, Attribute_Name){
  if(is.na(Shapefile_Name)){
    # When no Shapefile specified, Shapefile <- NA  
      Shapefile <- NA
      output <- Shapefile
    return(output)
  }
  
  if(is.na(Attribute_Field)){
    # When no Shapefile attributes are used, Shapefile <- NA
      Attribute_Name <- NA
      Shapefile <- NA
      output <- Shapefile
    return(output)
  }
 
  if(is.na(Attribute_Name)){
    # Set coordinate reference systems for intersection
      projection <- sf::st_crs("+proj=longlat +datum=NAD83")
    
    # Read in Shapefile and filter only by attribute Field
      Shapefile <- sf::st_read(dsn = Shapefile_Path,layer = Shapefile_Name)
      Shapefile <- sf::st_transform(Shapefile, crs = projection)
      Shapefile <- Shapefile %>% 
        dplyr::select(all_of(Attribute_Field))
      output <- Shapefile
    return(output)
  }
  
    else {
    # Set coordinate reference systems for intersection
      projection <- sf::st_crs("+proj=longlat +datum=NAD83")
    
    # Read in Shapefile and filter by Attribute Field with the specified Attribute_Name
      Shapefile <- sf::st_read(dsn = Shapefile_Path,layer = Shapefile_Name)
      Shapefile <- sf::st_transform(Shapefile, crs = projection)
      Shapefile <- dplyr::filter(Shapefile, get(Attribute_Field) == Attribute_Name)
      output <- Shapefile
    return(output)
  }
} 