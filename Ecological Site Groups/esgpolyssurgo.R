# lets wrap this into a function and try some different areas
make_esg <- function(aoi,
                     resolution = 800,
                     merge_by = "component",
                     database = "gssurgo"){
  #libraries
  require(sf)
  require(soilDB)
  require(terra)
  require(tidyverse)
  require(arcgisbinding)
  arc.check_product()
  
  ## QC
  # aoi should be an sf st_bbox object
  if(any(class(aoi)) %in% c("sf","bbox")){ # deal with the case where theres more than one class 
    stop("AOI is not an sf bounding box")
  }
  
  # check for size limits based on aoi and resolution
  
  
  if(!database %in% c("gssurgo","gnatsgo", "RSS")){
    stop("Database needs to be one of the following: 'gssurgo','gnatsgo', 'RSS'")
  }
  
  # The standard spatial reference for the grids is Albers Equal Area Conic (NAD83) coordinate reference system ("EPSG:5070")
  # double check projections here
  if(!st_crs(aoi)$input %in% c("NAD83 / Conus Albers", "EPSG:5070")){
    stop("AOI needs to be in NAD83")
  }
  
  # get our soils raster 
  mu <- mukey.wcs(aoi = aoi, db = database, res = resolution)
  
  # well want to join associated map unit component/ESD to this raster based on map unit keys
  # extract mukeys for thematic mapping
  rat <- cats(mu)[[1]]$mukey
  
  # this is throwing a error for some reason..
  # lets try to run it one by one
  comp <- lapply(X = rat, FUN = function(X){
    soilDB::get_SDA_coecoclass(X, method = "Dominant Component")
  }) 
  
  # Now pull down ESG data from EDIT and match to ESD raster
  # this is all of them
  classlist <- jsonlite::fromJSON("https://edit.jornada.nmsu.edu/services/downloads/esg/class-list.json")
  
  ecoclasses <- as.data.frame(classlist$ecoclasses)
  
  base_url <- "https://edit.jornada.nmsu.edu/services/descriptions/esg/"
  
  # Make a list of all the data frames for the ecoclasses IDs
  doc.url.list <- lapply(X = ecoclasses$id, FUN = function(X) {
    mlra <- ecoclasses$geoUnit[ecoclasses$id == X]
    jsonlite::fromJSON(paste0(base_url, mlra, "/", X, "/overview.json"))
  })
  
  # convert to data frame to join to raster
  new_list <- lapply(X = doc.url.list, FUN = function(X){
    y <- as.data.frame(unlist(X, recursive = TRUE))
    y <- t(y)
    row.names(y) <- NULL
    y <- as.data.frame(y)
    return(y)})
  
  esg_df <- bind_rows(new_list)
  if(nrow(esg_df)==0){
    next("There are no ESGs in this AOI")
  }
  
  # simplify this to have just list of esds
  # so basically just an easy lut
  if(merge_by == "ESD"){
    esg_esd <- esg_df %>% 
      select(c("generalInformation.ecoclassId",starts_with("generalInformation.subclasses.U.S. ecological sites.classes.symbol"))) %>% 
      pivot_longer(cols = starts_with("generalInformation.subclasses.U.S. ecological sites.classes.symbol")) %>% 
      select(-name) %>% 
      filter(!is.na(value)) %>% 
      rename("EcositeGroup" = "generalInformation.ecoclassId")
    
    # remove duplicates
    esg_esd_nodups <- esg_esd[!duplicated(esg_esd$value),]
    
    #join this to other soils info
    lut <- merge(x = comp,
                 y = esg_esd,
                 by.x = "ecoclassid",
                 by.y ="value",
                 all.x = TRUE,
                 all.y = FALSE)
  }
  
  if(merge_by == "component"){
    
    esg_comp <- esg_df %>% 
      select(c("generalInformation.ecoclassId",starts_with("generalInformation.components"),"generalInformation.ecoclassName")) %>% 
      pivot_longer(cols = starts_with("generalInformation.components")) %>% 
      select(-name) %>% 
      filter(!is.na(value)) %>% 
      rename("EcositeGroup" = "generalInformation.ecoclassId") %>% 
      unique()
    
    # theres still duplicates here ...i.e. map unit components which match to multiple ESGs (probably complexes etc.,)
    dups <- esg_comp[duplicated(esg_comp$value),] # 1,181 component dups, maybe just remove these for now?
    
    esg_comp_nodups <- esg_comp[!duplicated(esg_comp$value),]
    
    # merge components to other soils data
    lut <- merge(x = comp,
                 y = esg_comp_nodups,
                 by.x = "cokey",
                 by.y = "value",
                 all.x = FALSE,
                 all.y = FALSE)
  }
  
  # recode raster ad plot
  # set raster categories
  levels(mu) <- lut[, c('mukey', 'EcositeGroup')]
  
  return(list(mu,lut))
}