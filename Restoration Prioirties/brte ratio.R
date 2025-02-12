# setup
library(arcgisbinding)
library(tidyverse)
library(spdplyr)

arc.check_product()

# lets grab the brte cover and calc the brte:pg ratio
gdb <- '\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\National\\HQ Restoration\\Restoration priorities HQ.gdb'
brte_fc <- paste0(gdb, "/","BRTE_cover")
terraAIM_fc <- paste0(gdb,"/", "restorationHQ_terraAIM")
              
brte <- arc.select(arc.open(brte_fc))
terraAIM <- arc.select(arc.open(terraAIM_fc))

length(unique(brte$PrimaryKey)) - nrow(brte)

# there are 2 dups - need to remove
dups <- brte[duplicated(brte$PrimaryKey),]

brte <- brte[!brte$PrimaryKey %in% dups$PrimaryKey,]

# merge back with other benchmarks
most_recent_brte <- sp::merge(x = benchmarked_points,
                              y = brte[c("PrimaryKey","AH_SpeciesCover")],
                              by = "PrimaryKey",
                              all.x = TRUE,
                              all.y = FALSE)
# calc ratio
# convert NAs to 0
most_recent_brte <- most_recent_brte %>% 
  mutate(AH_SpeciesCover = ifelse(is.na(AH_SpeciesCover),0, AH_SpeciesCover))

most_recent_brte$BRTE_PG_ratio <- most_recent_brte$AH_SpeciesCover/most_recent_brte$AH_PerenGrassCover

most_recent_brte <- most_recent_brte %>% 
  mutate(BRTE_Restoration = ifelse(BRTE_PG_ratio >=4 & BRTE_PG_ratio <= 10, "TRUE", "FALSE"),
         BRTE_Restoration = ifelse(is.na(BRTE_Restoration),"FALSE",BRTE_Restoration))

# pull in custom calcs from patrick
patrick_data <-  read.csv("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\National\\HQ Restoration\\TerrADat_attributed_PJA5Dec22\\TerrADat_attributed_PJA5Dec22.csv")

# reduce to only needed fields
patrick_data <- patrick_data[,c("PrimaryKey","nativeFoliarAH","exoticFoliarAH","invasiveFoliarAH","noxiousFoliarAH")]

# there are 2 dups - need to remove
dups <- patrick_data[duplicated(patrick_data$PrimaryKey),]

patrick_data <- patrick_data[!patrick_data$PrimaryKey %in% dups$PrimaryKey,]

final_points <- sp::merge(x = most_recent_brte,
                          y = patrick_data,
                          by = "PrimaryKey",
                          all.x = TRUE,
                          all.y = FALSE)

# Benchmark new indicators
final_points_benchmarked <- final_points %>% 
  mutate(nativeFoliar_RelativeCover = nativeFoliarAH/TotalFoliarCover*100,
         Native_Plants = ifelse(nativeFoliar_RelativeCover>=95,">=95% Native",
                                ifelse(nativeFoliar_RelativeCover >=75 & nativeFoliar_RelativeCover <95,"75-95% Native",
                                       ifelse(nativeFoliar_RelativeCover <75, "<75 Native",NA)))) %>% 
  mutate(Invasive_Cover = ifelse(invasiveFoliarAH>=5,">=5% Invasive",
                                 ifelse(invasiveFoliarAH >=1 & invasiveFoliarAH <5,"1-5% Invasive",
                                        ifelse(invasiveFoliarAH <1, "<1% Invasive",NA)))) %>% 
  mutate(Exotic_Cover = ifelse(exoticFoliarAH>=5,">=5% Exotic",
                                  ifelse(exoticFoliarAH >=1 & exoticFoliarAH <5,"1-5% Exotic",
                                         ifelse(exoticFoliarAH <1, "<1% Exotic",NA)))) %>% 
  mutate(Noxious_Cover = ifelse(noxiousFoliarAH>=5,">=5% Noxious",
                               ifelse(noxiousFoliarAH >=1 & noxiousFoliarAH <5,"1-5% Noxious",
                                      ifelse(noxiousFoliarAH <1, "<1% Noxious",NA))))

final_points_benchmarked <- final_points_benchmarked %>% 
  mutate(Native_Score = ifelse(nativeFoliar_RelativeCover>=95,1,
                                ifelse(nativeFoliar_RelativeCover >=75 & nativeFoliar_RelativeCover <95,0.5,
                                       ifelse(nativeFoliar_RelativeCover <75, 0 ,NA)))) %>% 
  mutate(Invasive_Score = ifelse(invasiveFoliarAH>=5,0,
                                 ifelse(invasiveFoliarAH >=1 & invasiveFoliarAH <5,0.5,
                                        ifelse(invasiveFoliarAH <1, 1,NA)))) %>% 
  mutate(Exotic_Score = ifelse(exoticFoliarAH>=5,0,
                               ifelse(exoticFoliarAH >=1 & exoticFoliarAH <5,0.5,
                                      ifelse(exoticFoliarAH <1, 1,NA)))) %>% 
  mutate(Noxious_Score = ifelse(noxiousFoliarAH>=5,0,
                                ifelse(noxiousFoliarAH >=1 & noxiousFoliarAH <5,0.5,
                                       ifelse(noxiousFoliarAH <1, 1,NA)))) %>% 
  mutate(Canopy_Gap_Score = ifelse(GapCover_101_200 < 20 & GapCover_200_plus < 20, 1,
                             ifelse(GapCover_101_200 >= 20 & GapCover_200_plus >= 20, 0,
                                    ifelse(GapCover_101_200 >= 20 | GapCover_200_plus >= 20, 0.5, NA)))) %>% 
  mutate(Bare_Soil_Score = ifelse(BareSoilCover < 15, 1,
                            ifelse(BareSoilCover >= 15 & BareSoilCover < 25, 0.5,
                                   ifelse(BareSoilCover >= 25, 0, NA)))) %>% 
  mutate(Soil_Stability_Score = ifelse(SoilStability_All >= 4, 1, 
                                 ifelse(SoilStability_All >= 3 & SoilStability_All < 4, 0.5,
                                        ifelse(SoilStability_All < 3, 0, NA)))) %>% 
  mutate(Total_Restoration_Score = Native_Score+Invasive_Score+Exotic_Score+Noxious_Score+Canopy_Gap_Score+Bare_Soil_Score+Soil_Stability_Score)

final_points_benchmarked@data <- final_points_benchmarked@data %>% 
  rowwise() %>% 
  mutate(ERCEPT = mean(c(Native_Score,Invasive_Score,Exotic_Score,Noxious_Score,Canopy_Gap_Score,Bare_Soil_Score,Soil_Stability_Score),na.rm = TRUE))

pointtoremove <- arc.select(arc.open(path = "C:\\Users\\alaurencetraynor\\Documents\\National Report\\Restoration priorities HQ\\Restoration priorities HQ.gdb\\V1_POINTS"))

final_points_benchmarked <- final_points_benchmarked[!final_points_benchmarked$PrimaryKey %in% pointtoremove$PrimaryKey,]

arc.write(path = "\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\National\\HQ Restoration\\Restoration priorities HQ.gdb\\final_points_benchmarked_score_12822_norevisits2",
          data = final_points_benchmarked)

## working on a summary table by HUC

