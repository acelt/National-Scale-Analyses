MakeMap_Site <- function(TDat_LMF_Attributed, Shapefile, Attribute_Name){
  
  # Filter Tdat plots with coordinates
  TDat_LMF_Attributed_Map <- TDat_LMF_Attributed %>%
    dplyr::filter(Latitude_NAD83 > 0)
    
  # Create links to photos in AIM Data Portal
    TDat_LMF_Attributed_Map <- TDat_LMF_Attributed_Map %>% 
      mutate(link = paste("https://gis.blm.doi.net/attachments/AIM/Terradat/", State, PrimaryKey, sep = "/"))
    TDat_LMF <- TDat_LMF %>% 
      mutate(link = paste("https://gis.blm.doi.net/attachments/AIM/Terradat/", State, PrimaryKey, sep = "/"))
    
  Map <- leaflet::leaflet(height = 650 , width = 650)
  
  Map <- leaflet::addTiles(Map) %>% 
    leaflet::addPolygons(data = Shapefile,
                         color = "blue",
                         fillOpacity = 0.1,
                         popup = paste(Attribute_Name)) %>%
    leaflet::addCircleMarkers(data = TDat_LMF,
                              lng = ~Longitude_NAD83 , 
                              lat = ~Latitude_NAD83 ,
                              radius = 3 ,
                              fillOpacity = 0.5 ,
                              color = "black" ,
                              popup = paste("<b>Plot ID:</b>" , TDat_LMF$PlotID ,
                                            "<b>Date:</b>", TDat_LMF$Year,
                                            "<b>Ecological Site Id:</b>" , TDat_LMF$EcologicalSiteId,
                                            "<b>Photos:</b>", paste0("<a href='",TDat_LMF_Attributed_Map$link,"'>", "Link", "</a>"),
                                            sep = "<br>")) %>%
    leaflet::addCircleMarkers(data = TDat_LMF_Attributed_Map,
                              lng = ~Longitude_NAD83 , 
                              lat = ~Latitude_NAD83 ,
                              radius = 3 ,
                              fillOpacity = 1.0 ,
                              color = "red" ,
                              popup = paste("<b>Plot ID:</b>" , TDat_LMF_Attributed_Map$PlotID ,
                                            "<b>Date:</b>", TDat_LMF_Attributed_Map$Year,
                                            "<b>Ecological Site Id:</b>" , TDat_LMF_Attributed_Map$EcologicalSiteId,
                                            "<b>Photos:</b>", paste0("<a href='",TDat_LMF_Attributed_Map$link,"'>", "Link", "</a>"),
                                            sep = "<br>")) %>%
    
  return(Map)
}