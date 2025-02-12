### Setup
# documentation here: https://r.esri.com/r-bridge-site/
library(sf)
library(arcgis) # R was crashing for me when trying to use both of these packages together, use with caution...
library(tidyverse)
# can also authorise this way if youre signed into Pro and have loaded arcgisbinding:
# this has the advantage of not needing to click through steps on your web browser
token <- auth_binding()
set_arc_token(token)

path <- "\\\\blm.doi.net\\dfs\\ak\\gis\\GIS_Projects\\Alaska_SO\\BIL_IRA_Aquatics\\NomeCrMonitoring\\NomeCrMonitoring.gdb"
layer <-  "NomeCreekReaches"

# Import to sf object
NomeCreekReaches <-  st_read(dsn = path,
                             layer = layer)
# Take a peak
plot(NomeCreekReaches$Shape)

# Subset Reaches
# I couldnt see the list of reaches anywhere so making this up...
my_fav_reaches <- sample(1:length(NomeCreekReaches$reaches), 15, replace = FALSE)

NomeCreekReaches_subset <- NomeCreekReaches %>% 
  filter(reaches %in% my_fav_reaches)

# Take a peak
plot(NomeCreekReaches_subset$Shape)

# Publish
publish_reaches <- publish_layer(NomeCreekReaches_subset, "NomeCreekReaches_sample")

# Check it out
publish_reaches
