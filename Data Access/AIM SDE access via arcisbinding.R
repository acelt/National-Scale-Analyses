#### AIM data access using arcgisbinding package example script ####

### FIRST FOLLOW THE STEPS OUTLINE HERE: https://github.com/R-ArcGIS/r-bridge-install 
# TO INITIATE THE R-ARGIS BRIDGE ####

### For this script to run you will need:
# 1) To be connected to the BLM internal VPN to access AIM data
# 2) To be running a 64 bit version of R (the script was tested using [64 bit] R 4.0.3)

### More information on the arcgisbinding package can be found here: https://r.esri.com/assets/arcgisbinding-vignette.html 
### As well as by executing the typing: ??arcgisbinding into your R console

### Install arcgisbinding in ArcPro: Go to Project -> Options -> Geoprocessing -> R-ArcGIS Support
library(arcgisbinding)
library(sf)
library(terra)

# Run this to check your Arc license
arcgisbinding::arc.check_product()

terra_sde_path <- "\\\\blm\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\AIMDataTools\\SDE/AIMTerrestrialPub.sde"
lotic_sde_path <- "\\\\blm\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\AIMDataTools\\SDE\\AIMLoticPub.sde"

terradat_path <- paste0(terra_sde_path, "\\ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrADat")
lotic_path <- paste0(lotic_sde_path, "\\ilmocAIMLoticPub.ILMOCAIMPUBDBO.I_Indicators")

# Import terrestrial data
terradat <- arcgisbinding::arc.open(path = terradat_path)

# Import lotic data
aquadat <- arcgisbinding::arc.open(path = lotic_path)

### Can also import the data from the ArcGIS rest service URL
terra_url <- "https://gis.dev.blm.doi.net/arcgis/rest/services/vegetation/AIM_Terrestrial_View/MapServer/0"

# Import terrestrial data
terradat <- arcgisbinding::arc.open(path = terra_url)

# Same for lotic
lotic_url <- "https://gis.blm.doi.net/arcgis/rest/services/hydrography/BLM_Natl_AIM_Lotic/MapServer/0"

aquadat <- arcgisbinding::arc.open(path = lotic_url)

# Once opened, you can use arc.select to load that dataset into a data frame 
terradat_df <- arcgisbinding::arc.select(object = terradat,
                                         fields = c("PrimaryKey", "BareSoilCover", "DateVisited"), # you can use the fields parameter to select only certain fields or leave blank for them all
                                         where_clause = "State = 'NM'") # can also use a SQL query here to subset the data

#Or for Lotic (selecting all fields)
aquadat_df<-arcgisbinding::arc.select(object=aquadat)

# You can convert the data frame to a sf object to manipulate spatially
terradat_spdf <- arcgisbinding::arc.data2sf(terradat_df)

# View points in plots window
plot(terradat_spdf)

# Can also use leaflet to make interactive maps
library(leaflet)

leaflet::leaflet(data = terradat_spdf) %>%
  addTiles()%>%
  addCircleMarkers()

# Can write final products with arc.write. This can write to shapefiles and geodatabases
arcgisbinding::arc.write(path = paste0(getwd(),"\\test"),
                         data = terradat_spdf)

