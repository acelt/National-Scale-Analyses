### Setup
# documentation here: https://r.esri.com/r-bridge-site/
library(sf)
library(arcgis)
library(arcgisbinding) # R was crashing for me when trying to use both of these packages together, use with caution...
# arc.check_product() # don't need this with arcgis

# can use this to check portal URL if needed
# arc.check_portal

# You'll first need to follow instructions here: https://r.esri.com/r-bridge-site/location-services/connecting-to-a-portal.html 
# to authorize the Arc portal (requires logging into AGOL on your browser)
token <- auth_code()
set_arc_token(token) # this token expires after a certain amount of time, 30 mins or so?

# can also authorise this way if youre signed into Pro and have loaded arcgisbinding:
# this has the advantage of not needing to click through steps on your web browser
token <- auth_binding()
set_arc_token(token)

#######################################################################################################################################
### Here's an example of publishing a new feature service ###

# Heres a feature service I created and published a while back to test
# Import - this is a point layer containing AIM and LMF points 
HUC_stuff <- arc_open("https://services1.arcgis.com/KbxwQRRfWyEYLgp4/arcgis/rest/services/AIM_Point_Density_in_BLM_Grazing_Allotments_WFL1/FeatureServer/0")

# Convert to data frame so I can mess around with it
HUC_stuff_df <- arc_select(HUC_stuff)

# Change it in some way
HUC_stuff_df$DateEdited <- Sys.Date()

# Convert back to spatial in case I want to mess with it in arc or export to GDB
# We'll convert to sf object using sf
HUC_stuff_sf <- st_as_sf(HUC_stuff_df)

# can try publishing a smaller feature service for now for efficiency
# Subsetting to OR
# need to be signed in to AGOL for this to work otherwise may get error
HUC_stuff_OR <- HUC_stuff_df[HUC_stuff_df$State == "OR",]
publish <- publish_layer(HUC_stuff_OR, "OREGON_AIM_Point_Density_in_BLM_Grazing_Allotments_2024")

# Or if we want to publish the spatial verison
HUC_stuff_OR_sf <- HUC_stuff_sf[HUC_stuff_sf$State == "OR",]
publish2 <- publish_layer(HUC_stuff_OR_sf, "OREGON_AIM_Point_Density_in_BLM_Grazing_Allotments_2024_SPATIAL")

publish2

# THIS DOES SEEM TO BE WORKING! HOORAH!

# Lets try to append something to that feature service
HUC_stuff_WY <- HUC_stuff_sf[HUC_stuff_sf$State == "WY",]

# Lets pull down that new service
OR_service <- arc_open(url = paste0(publish2$services$serviceurl,"/0"), # this url needs the /0 to point to the layer rather than the service
                       token = token)

# we can check field names first if we like
tibble::as_tibble(list_fields(OR_service))

add_res <- add_features(x = OR_service, 
                        .data = HUC_stuff_WY, # these should have the same fields so no need to specify matches
                        chunk_size = 100) # Chunk is set to 2000 by default, if the addition is really big may want to reduce this

add_res # im getting a field type error which is weird since both datasets come from the same feature layer...perhaps field type were changed during publishing?

