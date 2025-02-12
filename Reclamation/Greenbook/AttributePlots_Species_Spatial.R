AttributePlots_Species_Spatial <- function(Species_Indicator, TDat_LMF, Shapefile){
  if(is.na(Shapefile)) {
    output <- Species_Indicator
  return(output)
    
  } 
  
  # Set coordinate reference systems for intersection
    projection <- sf::st_crs("+proj=longlat +datum=NAD83")
  
  # Pull coordinates from TDat_LMF to join to species indicators by PrimaryKey 
    coordinates <- TDat_LMF %>% dplyr::select(PrimaryKey, Latitude_NAD83, Longitude_NAD83)
  
  # Join species indicators by PrimaryKey to get spatial reference
    Species_Indicator_Attributed <- left_join(Species_Indicator, 
                                              coordinates, 
                                              by = "PrimaryKey")
  
  # Convert TDat_LMF data into "simple feature" object class. Not removing any NA or entries without coordinates.
    Species_Indicator_Attributed <- sf::st_as_sf(Species_Indicator_Attributed, 
                                              crs = projection,
                                              coords = c("Longitude_NAD83", "Latitude_NAD83"), 
                                              na.fail = FALSE, 
                                              remove = FALSE)
  
  #Intersect TDat_LMF with shapefile to bind attributes
    Species_Indicator_Attributed <- sf::st_intersection(Species_Indicator_Attributed, sf::st_make_valid(Shapefile))
    
    output <- Species_Indicator_Attributed
  return(output)
}
