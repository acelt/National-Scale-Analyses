library(sf)
library(soilDB)
library(terra)
library(tidyverse)
library(arcgisbinding)
arc.check_product()

############################################# TESTING ############################################
# 1. Get SSURGO data
# make a bounding box for each state
state <- "MT"
blm_basemap_url <- "https://gis.blm.gov/arcgis/rest/services/lands/BLM_Natl_SMA_Cached_BLM_Only/MapServer/2"
blm_basemap <- arc.open(blm_basemap_url)

# filter to NM BLM
clause <-  paste0("ADMIN_ST = '", state, "'")

blm_sma <- arc.select(blm_basemap,
                      where_clause = clause)

blm_sma_sf <- arc.data2sf(blm_sma)

blm_sma_sf_aea <- st_transform(blm_sma_sf,crs = st_crs(5070))

# make a bounding box for the state
bb <- st_bbox(blm_sma_sf_aea,
              crs = st_crs(5070))

# fetch gSSURGO map unit keys at 800m resolution 
# may want to subset this by MLRA in the future to get finer resolution
mu <- mukey.wcs(aoi = bb, db = 'gssurgo', res = 800)
mu2 <- mukey.wcs(aoi = bb, db = 'STATSGO', res = 800)
mu3 <- mukey.wcs(aoi = bb, db = 'gNATSGO', res = 800)

# plotting just map units
# plot(mu,
#      main = 'gSSURGO map units',
#      sub = 'AEA Projection',
#      axes = FALSE, 
#      legend = FALSE)

# well want to join associated map unit component/ESD to this raster based on map unit keys
# extract mukeys for thematic mapping
rat <- cats(mu)[[1]]
rat2 <- cats(mu2)[[1]]
rat3 <- cats(mu3)[[1]]

# theres a bug in this function - need to edit function 
comp <- get_SDA_coecoclass(mukeys = rat[["mukey"]],
                         method = "Dominant Component",
                         include_minors = FALSE)
comp_statsgo <- get_SDA_coecoclass(mukeys = rat2[["mukey"]],
                           method = "Dominant Component",
                           include_minors = FALSE)
comp_gnatsgo <- get_SDA_coecoclass(mukeys = rat3[["mukey"]],
                           method = "Dominant Component",
                           include_minors = FALSE)

# Now pull down ESG data from EDIT and match to ESD raster
#classlist <- jsonlite::fromJSON(paste0("https://edit.jornada.nmsu.edu/services/downloads/esg/",mlra, "/class-list.json"))
classlist <- jsonlite::fromJSON("https://edit.jornada.nmsu.edu/services/downloads/esg/class-list.json")
esd_classlist <- jsonlite::fromJSON("https://edit.jornada.nmsu.edu/services/downloads/esd/class-list.json")

#ecoclasses <- as.data.frame(classlist$ecoclasses)

ecoclasses <- as.data.frame(esd_classlist$ecoclasses)
  
base_url <- "https://edit.jornada.nmsu.edu/services/descriptions/esd/"

# Make a list of all the data frames for the ecoclasses IDs
doc.url.list <- lapply(X = ecoclasses$id, FUN = function(X) {
  mlra <- ecoclasses$geoUnit[ecoclasses$id == X]
  jsonlite::fromJSON(paste0(base_url, mlra, "/", X, "/overview.json"))
})

#ESDs are found here: doc.url.list[[n]][["generalInformation"]][["subclasses"]][["U.S. ecological sites"]][["classes"]]

# convert to data frame to join to raster
new_list <- lapply(X = doc.url.list, FUN = function(X){
  y <- as.data.frame(unlist(X, recursive = TRUE))
  y <- t(y)
  row.names(y) <- NULL
  y <- as.data.frame(y)
  return(y)})

esg_df <- bind_rows(new_list)

# Look at how complete each of these are
# count of ESDs with veg attributes

# count fo ESDs with complete STMs

# simplify this to have just list of esds
# so basically just an easy lut
# esg_esd <- esg_df %>%   
#   select(c("generalInformation.ecoclassId",starts_with("generalInformation.subclasses.U.S. ecological sites.classes.symbol"), generalInformation.geoUnitSymbol)) %>% 
#   pivot_longer(cols = starts_with("generalInformation.subclasses.U.S. ecological sites.classes.symbol")) %>% 
#   select(-name) %>% 
#   filter(!is.na(value)) %>% 
#   rename("EcositeGroup" = "generalInformation.ecoclassId")

esd_df <- esg_df %>%
  select(c("generalInformation.ecoclassId",starts_with("generalInformation.subclasses.U.S. ecological sites.classes.symbol"), generalInformation.geoUnitSymbol)) %>%
  pivot_longer(cols = starts_with("generalInformation.subclasses.U.S. ecological sites.classes.symbol")) %>%
  select(-name) %>%
  filter(!is.na(value)) %>%
  rename("EcositeGroup" = "generalInformation.ecoclassId")
# I think I should do the same but with the correlated map unit components

#join this to other soils info
comp2 <- merge(x = comp,
               y = esg_esd,
               by.x = "ecoclassid",
               by.y ="value",
               all.x = FALSE,
               all.y = FALSE)

install.packages("remotes")
remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

sankey_data <-  comp2[,c("ecoclassid", "EcositeGroup", "generalInformation.geoUnitSymbol")] %>% make_long(c("ecoclassid", "EcositeGroup", "generalInformation.geoUnitSymbol"))

ggplot(sankey_data, aes(x = x,
                  next_x = next_x,
                  node= node,
                  next_node = next_node,
                  fill = as.factor(generalInformation.geoUnitSymbol)))+
  geom_sankey()

esg_esd_nodups <- comp2[!duplicated(comp2),]#NONE

# looking at map unit components
esg_comp <- esg_df %>% 
  select(c("generalInformation.ecoclassId",starts_with("generalInformation.components"),"generalInformation.ecoclassName")) %>% 
  pivot_longer(cols = starts_with("generalInformation.components")) %>% 
  select(-name) %>% 
  filter(!is.na(value)) %>% 
  rename("EcositeGroup" = "generalInformation.ecoclassId") %>% 
  unique()

# in case of dups
esg_comp_nodups <- esg_comp[!duplicated(esg_comp),]# none right now

esg_comp_nodups$cokey <- as.numeric(esg_comp_nodups$value)

# merge components to other soils data
comp$cokey_char <- as.character(comp$cokey)

# check join fields
comp[comp$cokey_char %in% esg_comp_nodups$value,]
comp[comp$cokey %in% esg_comp_nodups$cokey,]

esg_comp_nodups[esg_comp_nodups$value %in% comp$cokey_char,]
esg_comp_nodups[esg_comp_nodups$cokey %in% comp$cokey,]

comp3 <- merge(x = comp,
               y = esg_comp_nodups,
               by.x = "cokey_char",
               by.y = "value",
               all.x = FALSE,
               all.y = TRUE) %>% 
  filter(!is.na(mukey),
         !is.na(EcositeGroup))

# recode raster ad plot
# set raster categories
comp2_lut <- unique(comp2[,c('mukey', 'EcositeGroup')])

levels(mu) <- comp2_lut[, c('mukey', 'EcositeGroup')]

#ESD_mapped <- catalyze(mu)
plot(mu,
     main = 'Ecological Site Groups',
     sub = 'WGS 84 Projection',
     axes = FALSE, 
     legend = FALSE)

# lets export this and check it out in ArcPro
terra::writeRaster(mu, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\NM3_esgs.tif", overwrite=TRUE)

#########################################################################################################################
##################################### FUNCTIONS ########################################################

# lets wrap this into a function and try some different areas
make_esg <- function(aoi,
                     resolution = 800,
                     merge_by = "component",
                     database = "gssurgo"){
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

############################################################################################################################################################
# nm
blm_basemap_url <- "https://gis.blm.gov/arcgis/rest/services/lands/BLM_Natl_SMA_Cached_BLM_Only/MapServer/2"
blm_basemap <- arc.open(blm_basemap_url)

# this is for Arizona
blm_basemap_url <- "https://gis.blm.gov/arcgis/rest/services/lands/BLM_Natl_SMA_Cached_BLM_Only/MapServer/2"
blm_basemap <- arc.open(blm_basemap_url)

# filter to NM BLM
blm_sma <- arc.select(blm_basemap,
                      where_clause = "ADMIN_ST = 'NM'")

blm_sma_sf <- arc.data2sf(blm_sma)

blm_sma_sf_aea <- st_transform(blm_sma_sf,crs = st_crs(5070))

# make a bounding box for the state
bb <- st_bbox(blm_sma_sf_aea,
              crs = st_crs(5070))

az <- make_esg(aoi = bb)
                  
plot(az)

# lets export this and check it out in ArcPro
#terra::writeRaster(test1, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\az_esgs.tif", overwrite=TRUE)

# UT
# this is for Arizona
blm_basemap_url <- "https://gis.blm.gov/arcgis/rest/services/lands/BLM_Natl_SMA_Cached_BLM_Only/MapServer/2"
blm_basemap <- arc.open(blm_basemap_url)

# filter to NM BLM
blm_sma <- arc.select(blm_basemap,
                      where_clause = "ADMIN_ST = 'UT'")

blm_sma_sf <- arc.data2sf(blm_sma)

blm_sma_sf_aea <- st_transform(blm_sma_sf,crs = st_crs(5070))

# make a bounding box for the state
bb <- st_bbox(blm_sma_sf_aea,
              crs = st_crs(5070))

utah <- make_esg(aoi = bb)

plot(utah)
#terra::writeRaster(utah, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\ut_esgs.tif", overwrite=TRUE)

# Nevada
bb <- st_bbox(c(xmax = -120.005746,
                ymin = 35.001857,
                xmin = -114.039648,
                ymax =  42.002207),
              crs = st_crs(4326))
nv <- make_esg(aoi = bb)
plot(nv)
#terra::writeRaster(nv, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\nv_esgs.tif", overwrite=TRUE)

# # California # this is giving errors for now
bb <- st_bbox(c(xmax = -124.409591,
                ymin = 32.534156,
                xmin = -114.131211,
                ymax =  42.009518),
              crs = st_crs(4326))

bb <- bb %>% 
  st_as_sfc %>% 
  st_transform(crs = st_crs(mu))

ca <- make_esg(aoi = bb)
plot(ca)
#terra::writeRaster(ca, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\ca_esgs.tif", overwrite=TRUE)

# colorado
bb <- st_bbox(c(xmax = -109.060253,
                ymin = 36.992426,
                xmin = -102.041524,
                ymax =  41.003444),
              crs = st_crs(4326))
co <- make_esg(aoi = bb)
#plot(co)
#terra::writeRaster(co, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\co_esgs.tif", overwrite=TRUE)

# oregon
bb <- st_bbox(c(xmax = -124.566244,
                ymin = 41.991794,
                xmin = -116.463504,
                ymax =  46.292035),
              crs = st_crs(4326))
or <- make_esg(aoi = bb)
#plot(or)
#terra::writeRaster(or, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\or_esgs.tif", overwrite=TRUE)

# WY
bb <- st_bbox(c(xmax = -111.056888,
                ymin = 40.994746,
                xmin = -104.05216,
                ymax =  45.005904),
              crs = st_crs(4326))
wy <- make_esg(aoi = bb)
#plot(wy)
#terra::writeRaster(wy, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\wy_esgs.tif", overwrite=TRUE)

  
# mt
bb <- st_bbox(c(xmax = -116.050003,
                ymin = 44.358221,
                xmin = -104.039138,
                ymax =  49.00139),
              crs = st_crs(4326))
mt <- make_esg(aoi = bb)
#plot(mt)
#terra::writeRaster(mt, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\mt_esgs.tif", overwrite=TRUE)

# idaho
bb <- st_bbox(c(xmax = -117.243027,
                ymin = 41.988057,
                xmin = -111.043564,
                ymax =  49.001146),
              crs = st_crs(4326))
id <- make_esg(aoi = bb)
#plot(id)
#terra::writeRaster(id, "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\id_esgs.tif", overwrite=TRUE)

# merging raster into one mega raster
az_nm <- terra::merge(mu, test1)
az_nm_co <- terra::merge(az_nm, co)
southwest <- terra::merge(az_nm_co, utah)
southwest_nv <- terra::merge(southwest, nv)
all <- terra::merge(southwest_nv, id)
all <- terra::merge(all, mt)
all <- terra::merge(all, wy)
all <- terra::merge(all, or)
plot(all,
     legend = FALSE)

writeRaster(all,"C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\all_esgs.tif", overwrite=TRUE )

# Merging all the look up tables and writing to csv
az_nm <- rbind(nm[[2]], az[[2]])
az_nm_co <- rbind(az_nm, co[[2]])
southwest <- rbind(az_nm_co, utah[[2]])
southwest_nv <- rbind(southwest, nv[[2]])
all <- rbind(southwest_nv, id[[2]])
all <- rbind(all, mt[[2]])
all <- rbind(all, wy[[2]])
all <- rbind(all, or[[2]])

all_nodups <- all[!duplicated(all),]
# converting eco group to integer for reclass
comp_reclass <- all_nodups %>%
  mutate(new_value = as.integer(factor(EcositeGroup)))

write.csv(comp_reclass, file = "C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\all_lut2.csv")

# grabbing several MLRA polys from AGOL
library(arcgis)

# authenticate with ArcPro
token <- auth_binding()
set_arc_token(token)

mlra_url <-  "https://services.arcgis.com/SXbDpmb7xQkk44JV/arcgis/rest/services/MLRA_52_FINAL_REVIEW/FeatureServer/0"

# make sure its in the same projection as the soil rasters
mlra_fc <- arc_select(arc_open(mlra_url))
mlra_fc_nad83 <- st_transform(mlra_fc, crs = st_crs(mu))

# first subset MLRAs to those on BLM
blm_mlras <- read.csv("C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\MLRA_52_BLM.csv")
mlra_fc_nad83 <- mlra_fc_nad83[mlra_fc_nad83$MLRARSYM %in% blm_mlras$MLRARSYM,]

# test this out
test1 <- mlra_fc_nad83[mlra_fc_nad83$MLRARSYM == "34A",]

mlra1_esg <- make_esg(test1)

mu <- mukey.wcs(aoi = test1, db = 'gssurgo', res = 1600)

# loop through each mlra to grab soil and ESG data
big_list_esgs <- lapply(X = split(mlra_fc_nad83, mlra_fc_nad83$geometry), FUN = make_esg)

## looking a summary of ESGs by MLRA
esg_mlra_summary <- esg_esd %>% 
  group_by(generalInformation.geoUnitSymbol) %>% 
  summarise(n_distinct(EcositeGroup)) %>% 
  mutate(MLRA = gsub("X","",generalInformation.geoUnitSymbol))%>% # i think we need to remove the X suffix
  mutate(MLRA = stringr::str_remove(MLRA, "^0+"))# also need to remove preceeding 0s

write.csv(esg_mlra_summary,"C:\\Users\\alaurencetraynor\\Documents\\Ecological Site Groups\\MLRA_ESG_Summary.csv")
