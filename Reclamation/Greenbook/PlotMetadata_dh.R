PlotMetadata_EcoSite <- function(TDat_LMF, TDat_LMF_Attributed, EcologicalSite){
  
  Plots <- c("Plots")
  
   # Filter input dataset to include only relevant plots within the individual ecological site 
  TDat_LMF <- TDat_LMF[TDat_LMF$EcologicalSiteId == EcologicalSite,] # dataset for ground cover and growth habit
  TDat_LMF_Attributed <- TDat_LMF_Attributed[TDat_LMF_Attributed$EcologicalSiteId == EcologicalSite,]
  
  # Add grouping variable for all plots with this ecological site
  TDat_LMF <- TDat_LMF %>%
    mutate(Group = "Ecological Site")
  
  # Add grouping variable for all plots in the area of interest with this ecological site
  TDat_LMF_Attributed <- TDat_LMF_Attributed %>%
    mutate(Group = "Ecological Site in AOI")
  
  
  # Remove geometry from terradat if it exists to reduce file size of data frame
  if (any(class(TDat_LMF_Attributed) == "sf")){
    TDat_LMF_Attributed <- st_drop_geometry(TDat_LMF_Attributed)
  }  
  
  # Merge to apply grouping variable to Terradat and terradat in AOI and consolidate all data into single data frame
  TDat_All <- merge(TDat_LMF_Attributed, TDat_LMF, all = TRUE)
  
  #Create color palettes 
  Plots_Simple <- TDat_All %>% 
    filter(EcologicalSiteId == EcologicalSite)%>%
    dplyr::select(PrimaryKey, Group) %>%   
    dplyr::mutate(PlotsPerYear = Plots) %>%
    dplyr::arrange(Group)
  
  PlotsPerYear <- ggplot2::ggplot(Plots_Simple , aes(x= Plots, 
                                                     text = stat(count))) +
    geom_bar(position = "dodge",
             aes(fill = Group) , width = .2) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
    ggtitle(paste("Plots Per Year in", EcologicalSite)) +
    theme_minimal() +
    coord_flip() + 
    theme(axis.text.y = element_blank())
  
  
  return(PlotsPerYear)

}