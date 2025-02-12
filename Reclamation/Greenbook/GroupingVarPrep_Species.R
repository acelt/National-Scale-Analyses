GroupingVarPrep_Species <- function(TimePeriodGroup, 
                                    TDat_All,
                                    TDat_LMF,
                                    TDat_LMF_Attributed,
                                    Species_Indicator,
                                    Species_Indicator_Attributed) {
  if(TimePeriodGroup) {
    if (any(class(Species_Indicator_Attributed) == "sf")) {
      Species_Indicator_Attributed <- st_drop_geometry(Species_Indicator_Attributed)
    }
    
    # Filter species indicator by terradat that is already attributed with time period grouping variable 
    # Select PrimaryKeys and EcologicalSiteIds from Terradat to merge by Primarykey to species indicator
    Ecosite_PrimaryKeys_Attributed <- TDat_All %>%
      dplyr::select(PrimaryKey, EcologicalSiteId, es_name, PlotGrouping)
    
    # Join species indicator by primarykey for ecologicalsiteID and time period group on data frame
    Species_Indicator_All <- left_join(Species_Indicator_Attributed, Ecosite_PrimaryKeys_Attributed, by = "PrimaryKey")
    
    # Select relevant data from SpeciesList and create link for USDA plants
    SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                                 Family, SpeciesState,
                                                 SynonymOf, UpdatedSpeciesCode) %>% 
      dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))
    
    # Added in attribute title here instead of allotment name
    Species_Indicator_All <- left_join(Species_Indicator_All , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
      dplyr::select(Species, ScientificName, Family, SpeciesState,
                    SynonymOf, UpdatedSpeciesCode, CommonName, PrimaryKey, 
                    PlotID,  AH_SpeciesCover, 
                    AH_SpeciesCover_n, Hgt_Species_Avg, 
                    Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                    Noxious, SG_Group, link, PlotGrouping, EcologicalSiteId, es_name) %>%
      dplyr::mutate_if(is.numeric, round , digits = 2) 
    
    output <- Species_Indicator_All
    output <- output[!is.na(output$PlotGrouping),]
    
      return(output)
    }

  if(!TimePeriodGroup) {
    if (any(class(Species_Indicator_Attributed) == "sf")) {
      Species_Indicator_Attributed <- st_drop_geometry(Species_Indicator_Attributed)
    } 
    
    # Select PrimaryKeys and EcologicalSiteIds from Terradat to merge by Primarykey to species indicator
    Ecosite_PrimaryKeys <- TDat_LMF %>%
      dplyr::select(PrimaryKey, EcologicalSiteId, es_name)
    Ecosite_PrimaryKeys_Attributed <- TDat_LMF_Attributed %>%
      dplyr::select(PrimaryKey, EcologicalSiteId, es_name)
    
    # Join species indicator by primarykey for ecologicalsiteID on data frame
    # Add Group variable for all plots with this ecological site
    Species_Indicator <- left_join(Species_Indicator, Ecosite_PrimaryKeys, by = "PrimaryKey") %>%
      mutate(PlotGrouping = "Ecological Site")
    # Join species indicator by primarykey for ecologicalsiteID on data frame 
    # Add Group variable for all plots in the area of interest with this ecological site 
    Species_Indicator_Attributed <- left_join(Species_Indicator_Attributed, Ecosite_PrimaryKeys_Attributed, by = "PrimaryKey") %>%
      mutate(PlotGrouping = "AOI")
    
    # Merge to apply Group variable to species indicator and consolidate all data into single data frame
    Species_Indicator_All <- bind_rows(Species_Indicator, Species_Indicator_Attributed)
    
    # Select relevant data from SpeciesList and create link for USDA plants
    SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                                 Family, SpeciesState,
                                                 SynonymOf, UpdatedSpeciesCode) %>% 
      dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))
    
    # Added in attribute title here instead of allotment name
    Species_Indicator_All <- left_join(Species_Indicator_All , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
      dplyr::select(Species, ScientificName, Family, SpeciesState,
                    SynonymOf, UpdatedSpeciesCode, CommonName, PrimaryKey, 
                    PlotID,  AH_SpeciesCover, 
                    AH_SpeciesCover_n, Hgt_Species_Avg, 
                    Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                    Noxious, SG_Group, link, PlotGrouping, EcologicalSiteId, es_name) %>%
      dplyr::mutate_if(is.numeric, round , digits = 2) 
    
    output <- Species_Indicator_All
    
    return(output)
  }
}