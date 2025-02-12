SummaryTables_WithAttributes <- function(TDat_All, 
                                         Species_Indicator_All, 
                                         SummaryVar, 
                                         SummarizeBy) {
 
  # Filter data frames by list of ecological sites from attributed terradat
  # Without this, R can't handle the size of the "by plot" data tables when exceeding ~100 plots
  ecosites <- unique(TDat_LMF_Attributed$EcologicalSiteId) %>% na.omit()
  ecosites <- EcologicalSiteIds[!EcologicalSiteIds %in% " UNKNOWN" & !EcologicalSiteIds %in% ""]
  
  TDat_All <- TDat_All[TDat_All$EcologicalSiteId %in% ecosites,]
  Species_Indicator_All <- Species_Indicator_All[Species_Indicator_All$EcologicalSiteId %in% ecosites,]
  
  
  Species_Indicator_All <- Species_Indicator_All %>% filter(!is.na(Species),!is.na(GrowthHabit))
  #Get Noxious versus Non in Standard Format
  
  Species_Indicator_All$Noxious <- gsub("YES" , "Yes", Species_Indicator_All$Noxious)
  Species_Indicator_All$Noxious <- gsub("NO", "No", Species_Indicator_All$Noxious)
  
  #Prep
  
  #Detected in richness only on plot
  RichnessPresent <- Species_Indicator_All %>% filter(is.na(AH_SpeciesCover))
  #Detected in LPI on plot
  LPI_Present <- Species_Indicator_All %>% filter(AH_SpeciesCover > 0.000000)
  #Removes duplicates
  LPI_Present_String <- unique(LPI_Present$Species)
  #Removes values from richness that also occurred in LPI
  RichnessSpecies_Only <- RichnessPresent[!(RichnessPresent[["Species"]] %in% LPI_Present_String),]
  #Removes duplicates
  TraceCover_List <- unique(RichnessSpecies_Only$Species)
  #Get into dataframe (just select state species that were trace)
  TraceSpeciesCover <- Species_Indicator_All[Species_Indicator_All[["Species"]] %in% TraceCover_List,]
  
  TraceCover_Table_SpList <- TraceSpeciesCover %>%
    dplyr::select(Species, ScientificName , Family , GrowthHabit ,
                  GrowthHabitSub , Duration, Noxious , SG_Group ,
                  SynonymOf , CommonName ,
                  UpdatedSpeciesCode, link, EcologicalSiteId, PlotGrouping) %>% unique() %>% filter(!is.na(Species))
  
  if(SummaryVar == "Species" & SummarizeBy == "Plot"){
    #hyperlink species
    Species_Indicator_All$Species <- paste0("<a href='",Species_Indicator_All$link,"'>",Species_Indicator_All$Species,"</a>")
    
    table <- Species_Indicator_All %>% select(-link) %>% filter(!is.na(AH_SpeciesCover)) %>% 
      DT::datatable(escape = FALSE, extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(extend = 'collection', buttons = c('csv', 'excel'),
                                               text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Species by Plot within " , Shapefile_Name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "Species" & SummarizeBy == "EcologicalSite"){
    #For summarizing across all plots
    # well add in time periodd here too
    Species_cover_summary <- Species_Indicator_All %>% filter(!is.na(AH_SpeciesCover)) %>% 
      mutate(Tally = 1) %>%
      group_by(Species , GrowthHabit , GrowthHabitSub , 
               Duration , Noxious , ScientificName , 
               CommonName , SG_Group, EcologicalSiteId, PlotGrouping) %>% 
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      dplyr::select(Species, ScientificName, 
                    AveragePercentCover, StandardDeviation,
                    MinCover, MaxCover, n, GrowthHabit, 
                    GrowthHabitSub, Duration, 
                    Noxious, CommonName, SG_Group, EcologicalSiteId, PlotGrouping)
    
    #hyperlink species
    Species_cover_summary$Species <- paste0("<a href='",Species_cover_summary$link,"'>", Species_cover_summary$Species,"</a>")
    
    table <- Species_cover_summary %>% DT::datatable(escape = FALSE, 
                                                     extensions = 'Buttons', 
                                                     filter = "top" , 
                                                     options = list(scrollX = TRUE ,
                                                                    dom = 'Bfrtip',
                                                                    buttons =
                                                                      list(list(
                                                                        extend = 'collection',
                                                                        buttons = c('csv', 'excel'),
                                                                        text = 'Download Table'))) , 
                                                     caption = (paste("Average Percent Cover Values Across" , Shapefile_Name)) , 
                                                     rownames = FALSE)
  }
  
  
  if(SummaryVar== "GrowthHabitSub" & SummarizeBy == "Plot"){
    
    table <-  Species_Indicator_All %>% 
      group_by(PrimaryKey , PlotID , GrowthHabitSub , Duration) %>% 
      filter(!is.na(AH_SpeciesCover)) %>% 
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      filter(!is.na(GrowthHabitSub)) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) ,
                    caption = (paste("Percent Cover by Structure and Functional Group by Plot  within " , 
                                     Shapefile_Name)), 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "GrowthHabitSub" & SummarizeBy == "EcologicalSite"){
    #removes trace species
    table <- Species_Indicator_All %>% filter(!is.na(AH_SpeciesCover)) %>% 
      mutate(Tally = 1) %>%
      group_by(GrowthHabitSub, Duration, Tally, EcologicalSiteId, PlotGrouping) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      select(-Tally) %>%
      filter(!is.na(GrowthHabitSub)) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) ,
                    caption = (paste("Percent Cover by Structure and Functional Group in " , 
                                     Shapefile_Name)), 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar== "Noxious" & SummarizeBy == "Plot"){
    
    table <- TDat_All %>% dplyr::select(PlotID, PrimaryKey, AH_NoxCover, AH_NonNoxCover, EcologicalSiteId, PlotGrouping) %>%
      filter(!is.na(AH_NonNoxCover)) %>%
      rename(NonNoxious = AH_NonNoxCover, Noxious = AH_NoxCover) %>%
      dplyr::mutate_if(is.numeric, round , digits = 2) %>% 
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Noxious Versus Non by Plot within " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar== "Noxious" & SummarizeBy == "EcologicalSite"){
    
    prep <-  TDat_All %>% dplyr::select(PlotID, PrimaryKey, AH_NoxCover, AH_NonNoxCover, EcologicalSiteId, PlotGrouping) %>% 
      filter(!is.na(AH_NonNoxCover)) %>%
      dplyr::rename(NonNoxious = AH_NonNoxCover, Noxious = AH_NoxCover) %>%
      gather(key = "Noxious", value = Percent,
             NonNoxious:Noxious) %>%
      dplyr::mutate(Tally = 1) 
    
    prep$Noxious <- gsub("NonNoxious" , "No", prep$Noxious)
    prep$Noxious <- gsub("Noxious", "Yes", prep$Noxious)
    
    table <-   prep %>% group_by(Noxious, EcologicalSiteId, PlotGrouping) %>% 
      summarize(AveragePercentCover = mean(Percent) ,
                StandardDeviation = sd(Percent),
                MinCover = min(Percent) ,
                MaxCover = max(Percent) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Noxious Versus Non in " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "Woody" & SummarizeBy == "Plot"){
    
    table <-  Species_Indicator_All %>% filter(!is.na(AH_SpeciesCover)) %>%
      group_by(GrowthHabit , PrimaryKey , PlotID) %>%
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Woody vs. Non by Plot within: " , Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "Woody" & SummarizeBy == "EcologicalSite"){
    
    table <- Species_Indicator_All %>% filter(!is.na(AH_SpeciesCover)) %>%
      mutate(Tally = 1) %>%
      group_by(GrowthHabit , Tally, EcologicalSiteId, PlotGrouping) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      subset(AveragePercentCover > 0.0000) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      dplyr::select(-Tally) %>%
      filter(!is.na(GrowthHabit)) %>% 
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" , options = list(scrollX = TRUE ,
                                                    dom = 'Bfrtip',
                                                    buttons =
                                                      list(list(
                                                        extend = 'collection',
                                                        buttons = c('csv', 'excel'),
                                                        text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Woody vs. Non in: " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  
  if(SummaryVar == "SageGrouseGroup" & SummarizeBy == "Plot"){
    table <-     Species_Indicator_All %>% filter(!is.na(SG_Group)) %>% 
      filter(!is.na(AH_SpeciesCover)) %>%
      group_by(SG_Group, PrimaryKey , PlotID) %>%
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" ,  options = list(scrollX = TRUE ,
                                                     dom = 'Bfrtip',
                                                     buttons =
                                                       list(list(
                                                         extend = 'collection',
                                                         buttons = c('csv', 'excel'),
                                                         text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Sage-Grouse Group by Plot within: " , 
                                     Shapefile_Name)), 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "SageGrouseGroup" & SummarizeBy == "EcologicalSite"){
    
    table <-  Species_Indicator_All %>% filter(!is.na(SG_Group)) %>% 
      filter(!is.na(AH_SpeciesCover)) %>% mutate(Tally = 1) %>%
      group_by(SG_Group, Tally, EcologicalSiteId, PlotGrouping) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      dplyr::select(-Tally) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) ,
                    caption = (paste("Percent Cover by Sage-Grouse Group in " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "PreferredForb" & SummarizeBy == "Plot"){
    
    table <- Species_Indicator_All %>% 
      mutate(PreferredForb = (SG_Group == "PreferredForb")) %>% 
      subset(PreferredForb == TRUE) %>% 
      subset(AH_SpeciesCover > 0.0000) %>%
      filter(!is.na(AH_SpeciesCover)) %>%
      group_by(Species, PrimaryKey , PlotID) %>%
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Preferred Forb By Plot within " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
    
  }
  
  if(SummaryVar == "PreferredForb" & SummarizeBy == "EcologicalSite"){
    
    table <- Species_Indicator_All %>% 
      mutate(PreferredForb = (SG_Group == "PreferredForb")) %>% 
      subset(PreferredForb == TRUE) %>% 
      subset(AH_SpeciesCover > 0.0000) %>%
      filter(!is.na(AH_SpeciesCover)) %>%
      filter(!is.na(AH_SpeciesCover)) %>%
      mutate(Tally = 1) %>%
      group_by(Species, Tally, EcologicalSiteId, PlotGrouping) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% dplyr::select(-Tally) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Preferred Forb in " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "TraceSpecies" & SummarizeBy == "Plot"){
    
    RichnessPresent <-  RichnessPresent %>% 
      dplyr::select(Species, ScientificName , GrowthHabit ,
                    GrowthHabitSub , Duration, Noxious , SG_Group, 
                    PrimaryKey, PlotID, link, EcologicalSiteId, PlotGrouping) %>% filter(!is.na(Species))
    
    RichnessPresent$Species <- paste0("<a href='",RichnessPresent$link,"'>", RichnessPresent$Species,"</a>")
    
    table <- RichnessPresent %>% select(-link) %>%  
      DT::datatable(escape = FALSE, extensions = 'Buttons', 
                    filter = "top" , options = list(scrollX = TRUE ,
                                                    dom = 'Bfrtip',
                                                    buttons =
                                                      list(list(
                                                        extend = 'collection',
                                                        buttons = c('csv', 'excel'),
                                                        text = 'Download Table'))) , 
                    caption = (paste("Trace Species by Plot within " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "TraceSpecies" & SummarizeBy == "EcologicalSite"){
    
    
    TraceCover_Table_SpList$Species <- paste0("<a href='",TraceCover_Table_SpList$link,"'>", TraceCover_Table_SpList$Species,"</a>")
    
    table <- TraceCover_Table_SpList %>% arrange(Species) %>% 
      select(-link) %>% 
      DT::datatable(escape = FALSE, 
                    extensions = 'Buttons', 
                    filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Trace species in " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
    
  }
  
  if(SummaryVar == "GroundCover" & SummarizeBy == "Plot"){
    
    table <- TDat_All %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                       TotalFoliarCover , FH_TotalLitterCover , 
                                                       FH_RockCover, EcologicalSiteId, PlotGrouping) %>% 
      gather(key = Indicator , value = Percent, 
             BareSoilCover:FH_RockCover) %>%
      filter(!is.na(Percent)) %>% mutate(Tally = 1) %>% 
      group_by(PlotID, PrimaryKey, Indicator) %>% 
      mutate_if(is.numeric, round , digits = 2) %>% select(-Tally) %>% 
      rename(PercentCover = Percent) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent cover by plot within " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "GroundCover" & SummarizeBy == "EcologicalSite"){
    
    table <- TDat_All %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                       TotalFoliarCover , FH_TotalLitterCover , 
                                                       FH_RockCover, EcologicalSiteId, PlotGrouping) %>%
      gather(key = Indicator , value = Percent, 
             BareSoilCover:FH_RockCover) %>% 
      filter(!is.na(Percent)) %>% mutate(Tally = 1) %>%
      group_by(Indicator, EcologicalSiteId, PlotGrouping) %>%
      summarize(AveragePercentCover = mean(Percent) ,
                Standard_Deviation = sd(Percent) ,
                Low = min(Percent) ,
                High = max(Percent), n = sum(Tally)) %>% 
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Average percent cover in " , 
                                     Shapefile_Name)), 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "Gap" & SummarizeBy == "Plot"){
    
    table  <- TDat_All %>% dplyr::select(PlotID , PrimaryKey , 
                                                        GapCover_25_50 , GapCover_51_100 , 
                                                        GapCover_101_200 , GapCover_200_plus , 
                                                        GapCover_25_plus, EcologicalSiteId, PlotGrouping) %>% 
      gather(key = Gap_Class_cm , 
             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
      filter(!is.na(Percent)) %>% 
      mutate_if(is.numeric , round, digits = 2) %>% 
      group_by(PlotID , PrimaryKey) %>%  
      mutate_if(is.numeric, round , digits = 2) %>% 
      rename(Percent_Cover = Percent) %>% 
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent cover by canopy gap class by plot within " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "Gap" & SummarizeBy == "EcologicalSite"){
    
    table <- TDat_All %>% dplyr::select(PlotID , PrimaryKey , 
                                                       GapCover_25_50 , GapCover_51_100 , 
                                                       GapCover_101_200 , GapCover_200_plus , 
                                                       GapCover_25_plus, EcologicalSiteId, PlotGrouping) %>% 
      gather(key = Gap_Class_cm , 
             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
      filter(!is.na(Percent)) %>%
      mutate_if(is.numeric , round, digits = 2) %>%
      group_by(Gap_Class_cm, EcologicalSiteId, PlotGrouping) %>%
      summarize(AveragePercentCover = mean(Percent) ,
                StandardDeviation = sd(Percent),
                MinPercentCover = min(Percent) ,
                MaxPercentCover = max(Percent)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" , options = list(scrollX = TRUE ,
                                                    dom = 'Bfrtip',
                                                    buttons =
                                                      list(list(
                                                        extend = 'collection',
                                                        buttons = c('csv', 'excel'),
                                                        text = 'Download Table'))) , 
                    caption = (paste("Percent cover by canopy gap class in: " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "SoilStability" & SummarizeBy == "Plot"){
    
    table <-  TDat_All %>% dplyr::select(PlotID , PrimaryKey , 
                                                        SoilStability_All , 
                                                        SoilStability_Protected , 
                                                        SoilStability_Unprotected, EcologicalSiteId, PlotGrouping) %>%
      gather(key = Veg , value = Rating , 
             SoilStability_All:SoilStability_Unprotected) %>%
      filter(!is.na(Rating)) %>% 
      mutate_if(is.numeric, round, digits = 2)  %>% 
      group_by(PrimaryKey , PlotID) %>% 
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Soil stability ratings by plot in: " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "SoilStability" & SummarizeBy == "EcologicalSite"){
    
    table <-  TDat_All %>% dplyr::select(PlotID , PrimaryKey , 
                                                        SoilStability_All , 
                                                        SoilStability_Protected , 
                                                        SoilStability_Unprotected, EcologicalSiteId, PlotGrouping) %>%
      gather(key = Veg , value = Rating , 
             SoilStability_All:SoilStability_Unprotected) %>%
      filter(!is.na(Rating)) %>% 
      mutate_if(is.numeric, round, digits = 2)  %>% 
      group_by(Veg, EcologicalSiteId, PlotGrouping) %>% 
      summarize(AverageSoilStability = mean(Rating , na.rm = TRUE) ,
                StandardDeviation = sd(Rating , na.rm = TRUE) ,
                MinSoilStability = min(Rating , na.rm = TRUE) ,
                MaxSoilStability = max(Rating, na.rm = TRUE)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Average soil stability ratings in: " , 
                                     Shapefile_Name)) , 
                    rownames = FALSE)
    
  }
  
  return(table)
  
}