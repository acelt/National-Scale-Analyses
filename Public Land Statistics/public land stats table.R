library(tidyverse)

### Organising LMF estimates for PLS table
pls <- read.csv("C:\\Users\\alaurencetraynor\\Documents\\National Report\\2021_publiclandstats.csv")

pls <- pls %>% 
  mutate(ci = ifelse(estimate == 0, "", round(estimate-lowerci, 1)),
         tidy_estimate = paste0(round(estimate,1), " +/-",ci)) %>% 
  select(-c(estimate, lowerci,upperci,ci)) %>% 
  pivot_wider(names_from = indicator, values_from = tidy_estimate) %>% 
  filter(!state %in% c("WA", "SD"))

write.csv(pls,"C:\\Users\\alaurencetraynor\\Documents\\National Report\\2021_publiclandstats_for_table2.csv" )

####################################################################################

### Caluclating acres of BLM per state
library(arcgisbinding)
library(sf)
arc.check_product()

## grab SMA
sma_path <- "\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\AIMDataTools\\SDE\\BLMPub.sde\\ilmocpub.ILMOCDBO.SMA\\ilmocpub.ILMOCDBO.SurfaceManagementAgency"
sma <- arc.open(sma_path)
sma_df <- arc.select(sma,where_clause = "ADMIN_AGENCY_CODE = 'BLM'")
sma_spdf <- arc.data2sf(sma_df)

## project to AEA
projection <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
sma_spdf <- st_transform(sma_spdf, crs = projection)

# pull in states
state_path <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_States_Generalized/FeatureServer/0"
state <- arc.open(state_path)
state_df <- arc.select(state)
state_spdf <- arc.data2sf(state_df)

state_spdf <- st_transform(state_spdf, crs = projection)

# Intersect
pi <- st_intersection(sma_spdf, state_spdf)
