AttributePlots_TDat_LMF_Spatial <- function(TDat_LMF, Shapefile) {
  if(is.na(Shapefile)) {
    output <- TDat_LMF
  return(output)
  
  }
  
  # Set coordinate reference systems for intersection
    projection <- sf::st_crs("+proj=longlat +datum=NAD83")
  
  # Convert TDat_LMF data into "simple feature" object class. Not removing any NA or entries without coordinates.
    TDat_LMF_Attributed <- sf::st_as_sf(TDat_LMF, 
                                        coords = c("Longitude_NAD83", "Latitude_NAD83"), 
                                        crs = projection,
                                        na.fail = FALSE, 
                                        remove = FALSE) 
  
  # Intersect TDat_LMF with shapefile to keep only plots with attributes.
    TDat_LMF_Attributed <- sf::st_intersection(TDat_LMF_Attributed, sf::st_make_valid(Shapefile))
    output <- TDat_LMF_Attributed
  return(output)
}