# setup
library(arcgis)
library(arcgisbinding)
library(tidyverse)
library(sf)

arc.check_product()
token <- auth_binding()
set_arc_token(token)

# set parameters
# need to update this to the url for the feature service 
sde <- "T:\\ProjectsNational\\AIM\\AIMDataTools\\SDE\\AIMTerrestrialPub.sde"
output_path <- "\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\C drive back up\\2023\\National Report\\Restoration priorities HQ\\"
indicator_url <- "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/0"
# Projection to use. Alber's Equal Area is useful for calculating areas
projection <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# subset to date range
start_date <- as.Date("2016-01-01")
end_date <- Sys.Date() # Today
# exisiting WCCT data
benchmarked_points_url <- "https://services1.arcgis.com/KbxwQRRfWyEYLgp4/arcgis/rest/services/TerrestrialAIM_HQ_Restoration/FeatureServer/27"
# species_indicators
species_indicator_url <- "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/1"
# are there additional calcs to add to terradat?
additional_calcs <- TRUE # this is true since cecelia has updated native/invasive calcs separate from terradat for now
# if there are additional calcs where can I find them?
calcs_path <- c("C:/Users/alaurencetraynor/Downloads/terrestrialAttributed_CEVA7Aug24.rds",
                "C:/Users/alaurencetraynor/Downloads/terrestrialAttributed_CEVA23Aug24.rds") # these should at least have primarykeys to link them
calcs_format <- "RDS"
additional_indicators <- c("nativeFoliarAH", "exoticFoliarAH", "invasiveFoliarAH", "noxiousFoliarAH","invasiveAnnualGrassFoliarAH")
huc8_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/Watershed_Boundary_Dataset_HUC_8s/FeatureServer/0"
focal_area_url <-  "https://services1.arcgis.com/KbxwQRRfWyEYLgp4/arcgis/rest/services/BLM_RestorationLandscapes_051923/FeatureServer/0"
# Turn off spherical geometry (this will prevent errors in spatial joins)
sf_use_s2(FALSE)
##################################################################################################################################
# pull in all terrestrial data
indicator_data <- arc_select(arc_open(url = indicator_url))

# import existing benchmarked points
benchmarked_points <- arc_select(arc_open(url = benchmarked_points_url))

# we really just need to match these column names
colnames(benchmarked_points)

# how up to date is this?
# most recent
most_recent <- max(benchmarked_points$DateLoadedInDb)

# filter indicators to everything after most recent and within date range
indicator_data_recent <- indicator_data %>% 
  filter(DateLoadedInDb > most_recent,
         DateVisited > start_date,
         DateVisited < end_date)

# keeping only most recent visit if revisits
most_recent_pks <- indicator_data_recent %>% 
  group_by(PlotKey) %>% 
  slice(which.max(as.POSIXct(DateVisited, "%Y-%m-%d %H:%M:%OS"))) %>% 
  select(PrimaryKey) %>% 
  ungroup()

most_recent <- indicator_data_recent[indicator_data_recent$PrimaryKey %in% most_recent_pks$PrimaryKey,] 

# merge the two
combined_points <- bind_rows(benchmarked_points,
                             most_recent)
data <- list()
# Add additional calcs if needed
if(additional_calcs){
  if(calcs_format == 'RDS'){
    for(i in calcs_path){
      data[[i]] <- readRDS(i)
      # just keep the indicators were interested in
      data[[i]] <- select(data[[i]],
                        c(PrimaryKey, any_of(additional_indicators)))
    }
  }
  additional_data <-  bind_cols(data, .name_repair = "minimal")
  additional_data <- additional_data[,!duplicated(colnames(additional_data))]
  # merge
  combined_points <- merge(x = combined_points,
                           y = additional_data,
                           by = "PrimaryKey",
                           all.x = TRUE,
                           all.y = FALSE)
}

# remove the ones labelled "delete"
combined_points <- combined_points[combined_points$PrimaryKey != "Delete",]

# what about rows which have no data?
combined_points <- combined_points %>%
  filter(!is.na(nativeFoliarAH),
         !is.na(exoticFoliarAH),
         !is.na(invasiveFoliarAH),
         !is.na(noxiousFoliarAH),
         !is.na(invasiveAnnualGrassFoliarAH))

odd_pks <- combined_points[!combined_points$PrimaryKey %in% additional_data$PrimaryKey,]
# these are all LMF, best to just remove the for now
combined_points <- combined_points[!combined_points$PrimaryKey %in% odd_pks$PrimaryKey,]

# theres a number of duplicate pks here I need to deal with first
# in the og data
nrow(combined_points)==length(unique(combined_points$PrimaryKey))

# remove the ones labelled "delete"
combined_points <- combined_points[combined_points$PrimaryKey != "Delete",]
# lets look at other dups
combined_points <- combined_points[!duplicated(combined_points),]

# well need to pull in species in data to join BRTE cover
species_indicators <- arc_select(arc_open(url = species_indicator_url))
species_indicators_new <- species_indicators[species_indicators$PrimaryKey %in% combined_points$PrimaryKey & species_indicators$Species == "BRTE",] %>% 
  as.data.frame()

# Join this to BM points
benchmarked_points_brte <- sp::merge(x = combined_points,
                                     y = species_indicators_new[,c("PrimaryKey", "AH_SpeciesCover")],
                                     by = "PrimaryKey",
                                     all.x = TRUE,
                                     all.y = FALSE,
                                     duplicateGeoms = TRUE) 

benchmarked_points_brte$AH_SpeciesCover[is.na(benchmarked_points_brte$AH_SpeciesCover)] <- 0
benchmarked_points_brte$BRTEfoliar[is.na(benchmarked_points_brte$BRTEfoliar)] <- 0

# define benchmarks
# apply benchmarks
# join
benchmarked_points_brte <- benchmarked_points_brte %>% 
  mutate(NativeProportion = (nativeFoliarAH/TotalFoliarCover)*100)

benchmarked_points_brte <-  benchmarked_points_brte %>% 
  mutate(BRTEfoliar = AH_SpeciesCover,
       invasiveAGfoliar = invasiveAnnualGrassFoliarAH,
       nativeProportion_foliarCover = NativeProportion,
       nativeFoliar = nativeFoliarAH)

# Ill need to add native plant benchmarks once Patrick has calced these
benchmarked_points_new <- benchmarked_points_brte %>% 
  mutate(Canopy_Gap = ifelse(GapCover_101_200 < 20 & GapCover_200_plus < 20, "Gaps > 1m < 20% AND Gaps > 2m < 20%",
                             ifelse(GapCover_101_200 >= 20 & GapCover_200_plus >= 20, "Gaps > 1m > 20% AND Gaps > 2m > 20%",
                                    ifelse(GapCover_101_200 >= 20 | GapCover_200_plus >= 20, "Gaps > 1m > 20% OR Gaps > 2m > 20%", NA)))) %>% 
  mutate(Bare_Soil = ifelse(BareSoilCover < 15, "<15% Bare Soil",
                            ifelse(BareSoilCover >= 15 & BareSoilCover < 25, "15-25% Bare Soil",
                                   ifelse(BareSoilCover >= 25, ">25% Bare Soil", NA)))) %>% 
  mutate(Soil_Stability = ifelse(SoilStability_All >= 4, ">=4 Soil Aggregate Stability", 
                                 ifelse(SoilStability_All >= 3 & SoilStability_All < 4, "3-4 Soil Aggregate Stability",
                                        ifelse(SoilStability_All < 3, "Soil Aggregate Stability <3",NA)))) %>% 
  mutate(Tree_Cover = ifelse(AH_NonNoxTreeCover >= 0 & AH_NonNoxTreeCover < 5, "0-5%",
                             ifelse(AH_NonNoxTreeCover >= 5 & AH_NonNoxTreeCover < 15,"5-15%",
                                    ifelse(AH_NonNoxTreeCover >= 15 & AH_NonNoxTreeCover < 25,"15-25",
                                           ifelse(AH_NonNoxTreeCover >= 25,">=25%",NA))))) %>% 
  mutate(Shrub_Cover = ifelse(AH_ShrubCover >= 0 & AH_ShrubCover < 5, "0-5%",
                             ifelse(AH_ShrubCover >= 5 & AH_ShrubCover < 15,"5-15%",
                                    ifelse(AH_ShrubCover >= 15 & AH_ShrubCover < 25,"15-25",
                                           ifelse(AH_ShrubCover >= 25,">=25%",NA))))) %>%
  mutate(PerenGrassForb_Cover = ifelse(AH_PerenForbGrassCover >= 0 & AH_PerenForbGrassCover < 5, "0-5%",
                             ifelse(AH_PerenForbGrassCover >= 5 & AH_PerenForbGrassCover < 15,"5-15%",
                                    ifelse(AH_PerenForbGrassCover >= 15 & AH_PerenForbGrassCover < 25,"15-25",
                                           ifelse(AH_PerenForbGrassCover >= 25,">=25%",NA)))))

# Add native and invasive scores
benchmarked_points_native <- benchmarked_points_new %>% 
  mutate(Native_score = ifelse(NativeProportion < 75 | TotalFoliarCover == 0, 0,
                               ifelse(NativeProportion >= 75 & NativeProportion < 95, 0.5,
                                      ifelse(NativeProportion >= 95, 1,
                                             ifelse(TotalFoliarCover == 0, 0, NA))))) %>% 
   mutate(InvasiveAG_score = ifelse(invasiveAnnualGrassFoliarAH < 1, 1,
                                    ifelse(invasiveAnnualGrassFoliarAH>=1 & invasiveAnnualGrassFoliarAH <5, 0.5,
                                           ifelse(invasiveAnnualGrassFoliarAH >=5, 0, NA)))) %>% # removing this for now since we dont have invasive AG calcs
  mutate(Invasive_score = ifelse(invasiveFoliarAH < 1, 1,
                                   ifelse(invasiveFoliarAH>=1 & invasiveFoliarAH <5, 0.5,
                                          ifelse(invasiveFoliarAH>=5, 0, NA)))) %>% 
  rowwise %>% mutate(Mean_Restoration_Score = mean(c(InvasiveAG_score, Native_score), na.rm =TRUE))

# change the names/update with new calcs to match existing feature class
benchmarked_points_newnames <- benchmarked_points_native %>% 
  mutate(CanopyGapScore = ifelse(GapCover_101_200 < 20 & GapCover_200_plus < 20, 1,
                             ifelse(GapCover_101_200 >= 20 & GapCover_200_plus >= 20, 0,
                                    ifelse(GapCover_101_200 >= 20 | GapCover_200_plus >= 20, 0.5, NA)))) %>% 
  mutate(BareSoilScore = ifelse(BareSoilCover < 15, 1,
                            ifelse(BareSoilCover >= 15 & BareSoilCover < 25, 0.5,
                                   ifelse(BareSoilCover >= 25, 0, NA)))) %>% 
  mutate(SoilStabilityScore = ifelse(SoilStability_All >= 4, 1, 
                                 ifelse(SoilStability_All >= 3 & SoilStability_All < 4, 0.5,
                                        ifelse(SoilStability_All < 3, 0,NA)))) %>% 
  mutate(invasiveAGproportion_foliarCove = invasiveAGfoliar/TotalFoliarCover) %>% 
  mutate(SpeciesRichness = NumSpp_NoxPlant + NumSpp_NonNoxPlant,
         BioticIntegrityScore = mean(Native_score + InvasiveAG_score + Invasive_score, na.rm = TRUE),
         SoilSiteStabilityScore = mean(CanopyGapScore + BareSoilScore + SoilStabilityScore, na.rm = TRUE) ) %>% 
  mutate(IndexDenominator = "1 + BareSoilCover + invasiveFoliarAH + GapCover_200_plus") %>%  # aka stuff that is bad when it increase
  mutate(IndexNumerator =  "SoilStability_All + nativeProportion_foliarCover + AH_NoxPerenForbGrassCover") %>%  # aka stuff that is good when it increases
  mutate(Multiplier = "SpeciesRichness") %>% # also recalc resilience
  mutate(ResilienceIndex = (sum(SoilStability_All + nativeProportion_foliarCover + AH_NoxPerenForbGrassCover, na.rm = TRUE)/(1 + sum(BareSoilCover + invasiveFoliarAH + GapCover_200_plus, na.rm = TRUE)))*SpeciesRichness,
         BRTE_PG_ratio = BRTEfoliar/AH_PerenGrassCover) %>% 
  mutate(BRTE_Restoration = ifelse(BRTE_PG_ratio >=4 & BRTE_PG_ratio <= 10, "TRUE", "FALSE"),
         BRTE_Restoration = ifelse(is.na(BRTE_Restoration),"FALSE",BRTE_Restoration)) %>% # Also calc BRTE/PG ratio for everything
  select(PrimaryKey,
         PlotKey,
         PlotID,
         State,
         HUC8,
         NAME,
         FocalArea,
         PriorityRank,
         SpeciesState,
         ProjectName,
         PhotoLink,
         EcologicalSiteId,
         Latitude_NAD83,
         Longitude_NAD83,
         DateEstablished,
         DateVisited,
         BareSoilCover,
         Bare_Soil,
         BareSoilScore,
         TotalFoliarCover,
         GapCover_101_200,
         GapCover_200_plus,
         Canopy_Gap,
         CanopyGapScore,
         SoilStability_All,
         Soil_Stability,
         SoilStabilityScore,
         AH_PerenGrassCover,
         NumSpp_NoxPlant,
         NumSpp_NonNoxPlant,
         SpeciesRichness,
         Spp_Nox,
         AH_NonNoxTreeCover,
         Tree_Cover,
         AH_ShrubCover,
         Shrub_Cover,
         AH_NonNoxPerenForbGrassCover,
         PerenGrassForb_Cover,
         BRTEfoliar,
         BRTEproportion_foliarCover,
         BRTE_PG_ratio,
         BRTE_Restoration,
         invasiveAGfoliar,
         invasiveAGproportion_foliarCove,
         InvasiveAG_score,
         nativeFoliar,
         nativeProportion_foliarCover,
         Native_score,
         exoticFoliarAH,
         invasiveFoliarAH,
         Invasive_score,
         noxiousFoliarAH,
         Mean_Restoration_Score,
         SoilSiteStabilityScore,
         BioticIntegrityScore,
         IndexNumerator,
         IndexDenominator,
         Multiplier,
         ResilienceIndex)# removing unnecessary fields

# Adding departure from benchmarks
# instead of a categorical benchmark we can make these continuous based on a linear relationship from the lower limit to the benchmark or upper limit
# lower lower for 'positive' indicators (i.e. ones which increase alongside ecological function) should be 0 and departure score will increase until the upper limit (the benchmark)
# for 'negative' indicators (increases are associated with a decrease in ecological function) and so departure score should decrease from 0 to the benchmark

# theres some issues with gap - convert to NAs for now
benchmarked_points_newnames$GapCover_200_plus[benchmarked_points_newnames$GapCover_200_plus>100] <- NA

benchmarked_points_departure <- benchmarked_points_newnames %>% 
  mutate(Native_score_dep = ifelse(nativeProportion_foliarCover == 0 | TotalFoliarCover == 0,0,
                                   ifelse(nativeProportion_foliarCover >= 95, 1, nativeProportion_foliarCover/95))) %>% 
  mutate(InvasiveAG_score_dep = ifelse(invasiveAGfoliar < 1, 1,
                                          ifelse(invasiveAGfoliar >=5, 0,
                                                 invasiveAGfoliar/5))) %>% # removing this for now since we dont have invasive AG calcs
  mutate(Invasive_score_dep = ifelse(invasiveFoliarAH < 1, 1,
                                        ifelse(invasiveFoliarAH>=5, 0,
                                               invasiveFoliarAH/5))) %>% 
  mutate(CanopyGapScore101_200_dep = ifelse(GapCover_101_200 < 20, 1,
                                 1-(GapCover_101_200/100))) %>%
  mutate(CanopyGapScore200plus_dep = ifelse(GapCover_200_plus < 20, 1,
                                                            1-(GapCover_200_plus/100))) %>%         
  mutate(BareSoilScore_dep = ifelse(BareSoilCover < 15, 1,
                                       ifelse(BareSoilCover >= 25, 0,
                                              1-((BareSoilCover-15)/25))))  %>% 
  mutate(SoilStabilityScore_dep = ifelse(SoilStability_All >= 4, 1,
                                            ifelse(SoilStability_All < 3, 0,
                                                   SoilStability_All/4)))
## WAIT! WHY DO WE HAVE GAPS >100%????

# some tidying up
# replace NAs
# Replace NAs with 0s
benchmarked_points_departure$BRTEfoliar[is.na(benchmarked_points_newnames$BRTEfoliar)] <- 0

# ill need to join to HUCs
# build SQL query string to filter arc select - its timin out otherwise
hucs <- unique(benchmarked_points_departure$HUC8)
query <- paste0(hucs, collapse = "', '")
query <- paste0("HUC8 IN ('", query, "')")

# import huc FC
huc8 <- arc_open(url = huc8_url)
huc8 <- arc_select(huc8, where = query)

# Harmoise projection
huc8_trans <- st_transform(huc8, crs = st_crs(benchmarked_points_departure))

# join to points 
benchmarked_points_huc8 <- st_join(x = benchmarked_points_departure,
                                   y = huc8_trans)
benchmarked_points_huc8 <- benchmarked_points_huc8 %>% 
  mutate(NAME = NAME.y,
         HUC8 = HUC8.y) %>% 
  select(-c(NAME.x, NAME.y, HUC8.x, HUC8.y,OBJECTID, Shape__Area, Shape__Length))

# export to gdb
arc.write(path = paste0(output_path, "/","Restoration priorities HQ.gdb/benchmarked_points_9272024"),
          data = benchmarked_points_huc8,
          overwrite = TRUE)

write.csv(st_drop_geometry(benchmarked_points_huc8),paste0("C:\\Users\\alaurencetraynor\\Documents\\National Report\\Restoration priorities HQ", "/","benchmarked_points_9272024.csv") )

# Summarise points to HUC8 and join back to HUC8 polys
huc_summary <- benchmarked_points_huc8 %>% 
  pivot_longer(cols = c(BareSoilCover, BareSoilScore:GapCover_200_plus,CanopyGapScore, SoilStability_All,
                        SoilStabilityScore:SpeciesRichness,AH_NonNoxTreeCover, AH_ShrubCover,
                        AH_NonNoxPerenForbGrassCover, BRTEfoliar:BRTE_PG_ratio, invasiveAGfoliar:BioticIntegrityScore, ResilienceIndex),
               names_to = "Indicator",
               values_to = "value") %>% 
  group_by(HUC8, Indicator) %>% 
  summarise(mean_value = mean(value, na.rm =TRUE)) %>% 
  pivot_wider(names_from = "Indicator",
              values_from = "mean_value") %>% 
  st_drop_geometry()

# Join
huc_summary_poly <- sp::merge(x = huc8_trans,
                              y = huc_summary,
                              by = "HUC8")

# we also need to join the restoration landscapes
# should just be able to do this with a spatial join to HUCs
rest_landscapes <-  arc_select(arc_open(focal_area_url))
# remove uneccessary fields
rest_landscapes <-  st_transform(rest_landscapes[,"FocalName"],crs = st_crs(huc_summary_poly) )

huc_summary_poly <- st_join(huc_summary_poly,
                            rest_landscapes) %>% 
  select(-OBJECTID)# Can remove this since it will be added in Arc

# write this polygon to gdb
arc.write(path = paste0(output_path, "/","Restoration priorities HQ.gdb/huc_summary_poly_9272024"),
          data = huc_summary_poly,
          overwrite = TRUE)

# lets also publish these to AGOL
layer1 <- publish_layer(benchmarked_points_huc8, title = "Terrestrial AIM Watershed Condition Points")

layer2 <- publish_layer(huc_summary_poly,title = "Terrestrial AIM HUC 8 Summary Polygons")




