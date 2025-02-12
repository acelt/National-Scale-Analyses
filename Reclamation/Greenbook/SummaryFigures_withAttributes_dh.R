SummaryFigures_WithAttributes <- function(SpeciesList, 
                                          TDat_All,
                                          Species_Indicator_All,
                                              EcologicalSite, 
                                              SummaryVar, 
                                              Interactive, 
                                              alpha = 0.2){
  
  group_palette <- c("#E69F00", "#56B4E9")
  
  # Filter input dataset to include only relevant plots within the individual ecological site 
  TDat_All <- TDat_All[TDat_All$EcologicalSiteId == EcologicalSite,]
  
  # Filter input dataset to include only relevant plots within the individual ecological site 
  Species_Indicator_All <- Species_Indicator_All[Species_Indicator_All$EcologicalSiteId == EcologicalSite,]
  
  NoxNonPal_Fill <- c("grey75"  , "#D55E00")
  NoxNonPal_Dot <- c("grey33" , "#993300")
  
  dodge1 <- position_dodge(width = 0.9)
  dodge2 <- position_dodge(width = 0.4)
  
  if(SummaryVar == "GrowthHabitSub"){
    if(Interactive){
      
      
      Plots <-  lapply(X = split(Species_Indicator_All, Species_Indicator_All[["GrowthHabitSub"]] , 
                                 drop = TRUE),
                       
                       FUN = function(Species_Indicator_All){
                         
                         current_plot <- ggplot2::ggplot(Species_Indicator_All[!is.na(Species_Indicator_All$GrowthHabitSub) & Species_Indicator_All$EcologicalSiteId == EcologicalSite,], 
                                                         aes(x = GrowthHabitSub, 
                                                             y = AH_SpeciesCover, 
                                                             fill = Group,
                                                             text = paste("Primary Key: " , PrimaryKey , 
                                                                          "Plot ID: " , PlotID , 
                                                                          "Species: " , ScientificName , 
                                                                          "Code: " , Species , 
                                                                          "Percent Cover: " , AH_SpeciesCover , 
                                                                          "Noxious: " , Noxious ,
                                                                          "Group: ", Group,
                                                                          sep = "<br>"
                                                                          ))) +
                           geom_boxplot(width = 1, outlier.shape = NA) +
                           theme_light() + # remove ylims here
                           theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                 axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                 axis.line.y = element_blank()) + 
                           theme(panel.grid.major.y = element_blank() , axis.title.y = element_blank()) +
                           ggtitle(paste0("Percent Cover by Functional Group: " , 
                                          Species_Indicator_All$GrowthHabitSub, ", Ecological Site: ", EcologicalSite)) +
                           coord_flip() + 
                           scale_fill_manual(values = group_palette, drop = FALSE) +
                           facet_grid(cols = vars(GrowthHabitSub) ,
                                      rows = vars(Duration) ,
                                      switch = "y" ,
                                      scales = "free" , drop = TRUE)
                         
                         return(current_plot)
                       }
      )
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(Species_Indicator_All, Species_Indicator_All[["GrowthHabitSub"]] , drop = TRUE), 
                      FUN = function(Species_Indicator_All){
                        
                        current_plot <- ggplot2::ggplot(Species_Indicator_All[Species_Indicator_All$EcologicalSiteId == EcologicalSite,], aes(x = GrowthHabitSub , 
                                                                                                                                              y = AH_SpeciesCover, 
                                                                                                                                              fill = PlotGrouping)) +
                          geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
                          geom_point(position = dodge1, aes(shape = Noxious)) +
                          labs(y = "Percent Cover") + # remove ylims
                          theme_light() + 
                          scale_fill_manual(values = group_palette, drop = FALSE) +
                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                       axis.title.y = element_blank()) +
                          ggtitle(paste("Percent Cover by Functional Group:", Species_Indicator_All$GrowthHabitSub, ", Ecological Site:", EcologicalSite, sep = " ")) +
                          coord_flip() + facet_grid(cols = vars(GrowthHabitSub) ,
                                                    rows = vars(Duration) , 
                                                    switch = "y" ,
                                                    scales = "free" , 
                                                    drop = TRUE)
                        
                        
                        return(current_plot)
                      })
    }
  }
  
  if(SummaryVar == "Noxious"){
    if(Interactive){
      Plots <- Species_Indicator_All %>% 
        group_by(Noxious, PlotGrouping) %>% 
        filter(!is.na(Noxious)) %>% 
        filter(!is.na(AH_SpeciesCover)) %>%
        ggplot2::ggplot((aes(x = Noxious,
                             y = AH_SpeciesCover,
                             fill = PlotGrouping,
                             text = paste("Primary Key : " , PrimaryKey, 
                                          "Plot ID: " , PlotID,
                                          "Species: " ,  ScientificName, 
                                          "Code: " , Species, 
                                          "Percent Cover: " , AH_SpeciesCover, 
                                          "Noxious: " , Noxious, 
                                          "Group: ", PlotGrouping, 
                                          sep = "<br>")))) +
        geom_boxplot(width = .6 , outlier.shape = NA) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() ,
              axis.ticks.y = element_blank() , axis.line.y = element_blank() , 
              axis.title.x = element_blank()) +
        theme(panel.grid.major.y = element_blank() , legend.position = "none") +
        coord_flip() + 
        facet_grid(rows = vars(Noxious) , switch = "y" , scales = "free" , 
                   drop = TRUE) 
      return(Plots)
    }
    
    if(!Interactive){
      Plots <- Species_Indicator_All %>% 
        group_by(Noxious, PlotGrouping) %>% 
        filter(!is.na(Noxious)) %>% 
        filter(!is.na(AH_SpeciesCover)) %>%
        ggplot2::ggplot((aes(x = Noxious , 
                             y = AH_SpeciesCover, 
                             fill = PlotGrouping))) +
        geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
        geom_point(position = dodge1, aes(shape = Noxious), size = 2) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() , 
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        coord_flip() + 
        facet_grid(rows = vars(Noxious) ,
                   switch = "y" , scales = "free" , drop = TRUE)
      
    }}
  
  if(SummaryVar == "Species"){
    PercentCover <- Species_Indicator_All %>% subset(AH_SpeciesCover > 0.000000)
    
    if(Interactive){
      Plots <-lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , PercentCover$Duration) , drop = TRUE),
                     FUN = function(PercentCover){
                       current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , 
                                                                          y = AH_SpeciesCover,
                                                                          fill = PlotGrouping,
                                                                          text = paste("PrimaryKey: ", PrimaryKey , 
                                                                                       "Plot ID: " , PlotID , 
                                                                                       "Species: " , ScientificName , 
                                                                                       "Code: " , Species , 
                                                                                       "Percent Cover: " , AH_SpeciesCover , 
                                                                                       "Noxious: " , Noxious , 
                                                                                       "Group: ", PlotGrouping, 
                                                                                       sep = "<br>"))) +
                         geom_boxplot(outlier.shape = NA) +
                         theme_light() +
                         scale_fill_manual(values = group_palette, drop = FALSE) +
                         labs(y = "Percent Cover") + 
                         ggtitle(paste("Percent Cover by Species, " , 
                                       PercentCover$GrowthHabitSub, 
                                       PercentCover$Duration ,
                                       EcologicalSite)) + # removed tostring function
                         theme(axis.title.y = element_blank() ,
                               axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(), 
                               axis.title.x = element_blank())+   
                         theme(panel.grid.major.y = element_blank() ,
                               axis.title.y = element_blank()) +
                         coord_flip() +  
                         facet_grid(rows = vars(Species), 
                                    scales = "free" , 
                                    switch = "y",  drop = TRUE) 
                       return(current_plot)
                     })
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , 
                                                   PercentCover$Duration) , 
                                drop = TRUE),
                      FUN = function(PercentCover){
                        current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , 
                                                                           y = AH_SpeciesCover, 
                                                                           fill = PlotGrouping)) +
                          geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
                          geom_point(size = 1 , aes(shape = Noxious), position = dodge1) +
                          theme_light() +
                          scale_fill_manual(values = group_palette, drop = FALSE) +
                          labs(y = "Percent Cover") + 
                          ggtitle(paste("Percent Cover by Species, " , 
                                        PercentCover$GrowthHabitSub , 
                                        PercentCover$Duration, 
                                        EcologicalSite, sep = ",")) + 
                          theme(axis.title.y = element_blank()) +
                          coord_flip() +  facet_grid(cols = vars(GrowthHabitSub) , rows = vars(Duration) ,
                                                     switch = "y" , scales = "free" , drop = TRUE) 
                        return(current_plot)
                      })
      
    }
  }
  
  if(SummaryVar == "GroundCover"){
    
    #Prep
    #BareSoilCover
    #TotalFoliarCover
    #FH_TotalLitterCover
    #FH_RockCover
    
    Ground_Cover_Tall <- TDat_All %>% 
      dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                    TotalFoliarCover , FH_TotalLitterCover , 
                    FH_RockCover, PlotGrouping) %>%
      gather(key = Indicator , value = Percent, 
             BareSoilCover:FH_RockCover) %>% mutate(Tally = 1) 
    if(Interactive){
      
      
      Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
        ggplot2::ggplot((aes(x = Indicator , 
                             y = Percent , 
                             fill = PlotGrouping, 
                             text = paste("PlotID: " , PlotID, 
                                          "PrimaryKey: " , PrimaryKey , 
                                          "Indicator: " , Indicator ,
                                          "Percent Cover: " , Percent , 
                                          "Group: ", PlotGrouping, 
                                          sep = "<br>" )))) +
        geom_boxplot(width = .6 , outlier.shape = NA) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() ,  
              axis.title.x = element_blank() , 
              axis.title.y = element_blank()) +
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
        ggplot2::ggplot((aes(x = Indicator , y = Percent, fill = PlotGrouping))) +
        geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
        geom_point(position = dodge1) +
        theme_light(base_size = 16) +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank()) + 
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
      
    }
    
    
  }
  
  if(SummaryVar == "Gap"){
    
    Gap <- TDat_All %>% dplyr::select(PlotID, PrimaryKey , 
                                                     GapCover_25_50 , GapCover_51_100 , 
                                                     GapCover_101_200 , GapCover_200_plus , 
                                                     GapCover_25_plus, PlotGrouping) %>% 
      gather(key = Gap_Class_cm , 
             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
      mutate_if(is.numeric , round, digits = 2)
    
    # reorder factors by size
    Gap$Gap_Class_cm <- as.factor(Gap$Gap_Class_cm)
    Gap$Gap_Class_cm <-  factor(Gap$Gap_Class_cm, levels = c("GapCover_200_plus","GapCover_101_200" , "GapCover_51_100", "GapCover_25_50", 'GapCover_25_plus'))
    Gap <- Gap[order(Gap$Gap_Class_cm),]
    
    #Plot prep
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , 
                                                y = Percent , 
                                                fill = PlotGrouping,
                                                text = paste("PlotID: " , PlotID , 
                                                             "PrimaryKey: ", PrimaryKey , 
                                                             "Gap Class (cm): " , Gap_Class_cm, 
                                                             "Percent Cover: " , Percent , 
                                                             "Group: ", PlotGrouping, 
                                                             sep = "<br>"))) +
        geom_boxplot() + coord_flip() + 
        theme_light() + 
        theme(axis.title.x = element_blank() ,
              axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() , 
              axis.title.y = element_blank() , 
              axis.line.y = element_blank(), 
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , 
                                                y = Percent, 
                                                fill = PlotGrouping)) +
        labs(y = "Percent Cover" , x = "Gap Size Class (cm)", 
             caption = paste("Percent cover of canopy gap in: ", 
                             EcologicalSite, sep = " ")) +
        geom_boxplot(position = dodge1) + 
        coord_flip() + 
        geom_point(position = dodge1) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme_light(base_size = 16) + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
    }
    
  }
  
  if(SummaryVar == "SoilStability"){
    
    soil_labels <- c("SoilStability_All" = "All" , 
                     "SoilStability_Protected" = "Protected" , 
                     "SoilStability_Unprotected" = "Unprotected")
    
    SoilStability <- TDat_All %>% dplyr::select(PlotID , PrimaryKey , 
                                                               SoilStability_All , 
                                                               SoilStability_Protected , 
                                                               SoilStability_Unprotected,
                                                               PlotGrouping) %>%
      gather(key = Veg , value = Rating , 
             SoilStability_All:SoilStability_Unprotected) %>%
      mutate_if(is.numeric, round, digits = 2) %>% dplyr::filter(!is.na(Rating))
    
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = SoilStability , 
                               aes(x = Veg , 
                                   y = Rating , 
                                   fill = PlotGrouping, 
                                   text = paste("Primary Key: " , PrimaryKey,
                                                "Plot ID: " , PlotID , 
                                                "Rating: " , Rating , 
                                                "Group: ", PlotGrouping, 
                                                sep = "<br>"))) +
        geom_boxplot(position = dodge1) + 
        scale_fill_manual(values = group_palette, drop = FALSE) +
        coord_flip() +
        theme_light() + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.title = element_blank()) +
        facet_grid(rows = vars(Veg) , switch = "y" ,
                   scales = "free_y" , drop = TRUE , 
                   labeller = as_labeller(soil_labels))
    }
    
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = SoilStability, aes(x = Veg , 
                                                         y = Rating, 
                                                         fill = PlotGrouping)) +
        labs(x = "Vegetation cover class", 
             y = "Soil Stability Rating",
             caption = paste("Soil stability ratings in: ", 
                             EcologicalSite)) +
        geom_boxplot(position = dodge1) +
        coord_flip() + 
        geom_point(position = dodge1) +
        theme_light(base_size = 16) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Veg),
                   switch = "y",
                   scales = "free_y", 
                   drop = TRUE,
                   labeller = as_labeller(soil_labels))
      
    }
    
  }
  
  return(Plots)
  
}
