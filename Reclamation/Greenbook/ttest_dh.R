### T test function
# need to use tdat long and we'll leave the species summary for now since it has overlapping observations
t.tests <-  function(TDat_All, EcologicalSite, alpha){
  
  TDat_Cover_Indicators <-  c("AH_NoxAnnForbCover","AH_NoxAnnGrassCover","AH_NoxCover","AH_NonNoxAnnForbCover", "AH_PerenGrassCover","AH_PerenForbCover")
  #TDat_Cover_Indicators <-  enquo(TDat_Cover_Indicators)
  
  #TDat_FG_Indicators <-  c("AH_NoxAnnForbCover",
  #                         "AH_NonNoxPerenGrassCover",
  #                         "AH_NonNoxPerenForbCover",
  #                         "AH_NonNoxShrubCover",
  #                         "AH_NonNoxSubShrubCover",
  #                         "AH_NonNoxTreeCover",
  #                         "AH_SagebrushCover")
  
  # Filter input dataset to include only relevant plots within the individual ecological site 
  TDat_All <- TDat_All[TDat_All$EcologicalSiteId == EcologicalSite,]
  
  # Remove any NA from PrimaryKey
  TDat_All <- TDat_All[!is.na(TDat_All$PrimaryKey),]
  
  # Summarizing Tdat to get sample stats across ecological site
  # Make long for individual rows for each indicator rather than columns
  TDat_long <- TDat_All %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                          TotalFoliarCover , FH_TotalLitterCover , 
                                          FH_RockCover, !!TDat_Cover_Indicators, SoilStability_All , 
                                          SoilStability_Protected , 
                                          SoilStability_Unprotected, GapCover_25_50 , GapCover_51_100 , 
                                          GapCover_101_200 , GapCover_200_plus, 
                                          GapCover_25_plus, EcologicalSiteId, es_name, PlotGrouping) %>% # removing un-needed cols
    gather(key = Indicator , value = Percent, BareSoilCover:GapCover_25_plus) %>%
    filter(!is.na(Percent))
  
  tdat_groups <- unique(TDat_long$PlotGrouping)
  
  test <- lapply(X = split(TDat_long, TDat_long[["Indicator"]]),
                 FUN = function(TDat_long){
                   tdat_groups <- unique(TDat_long$PlotGrouping)
                   
                   # SPILT INTO GROUPS
                   t1 <- TDat_long[TDat_long[["PlotGrouping"]] == tdat_groups[1],]
                   t2 <- TDat_long[TDat_long[["PlotGrouping"]] == tdat_groups[2],]
  
                   # logistic transformation
                   #t1$Percent <- qlogis(t1$Percent/100)
                   # t2$Percent <- qlogis(t2$Percent/100)
                   
                   # CHECK SAMPLE SIZES
                   if(nrow(t1)>3 & nrow(t2)>3){
                     
                     # TEST NORMALITY
                     if(length(unique(t1$Percent)) > 1){
                       t1p <- shapiro.test(t1$Percent)$p.value
                     } else {t1p <- 0}
                     
                     if(length(unique(t2$Percent)) > 1){
                       t2p <- shapiro.test(t2$Percent)$p.value
                     } else {t2p <- 0}
                     
                     if(is.na(t1p)|is.na(t2p)) {
                       test <- list(p.value = NA, estimate = NA)
                     } else if(t1p > 0.05 & t2p > 0.05) {
                       test<- t.test(x = t1$Percent,
                                     y = t2$Percent,
                                     conf.level = 1-alpha)
                       
                       p.value <- round(test[["p.value"]],2)
                       mean.t1 <- round(mean(t1$Percent),2)
                       mean.t2 <- round(mean(t2$Percent),2)
                       
                       test_df <- c(t1_mean_name = mean.t1, 
                                    t2_mean_name = mean.t2,
                                    "p.value" = p.value)
                       
                       return(test_df)
                     } else {
                       test <- wilcox.test(x = t1$Percent,
                                           y = t2$Percent,
                                           conf.level = 1-alpha,
                                           exact = FALSE)
                       
                       p.value <- round(test[["p.value"]],2)
                       mean.t1 <- round(mean(t1$Percent),2)
                       mean.t2 <- round(mean(t2$Percent),2)
                       
                       test_df <- c(t1_mean_name = mean.t1, 
                                     t2_mean_name = mean.t2,
                                     "p.value" = p.value)
                       
                       return(test_df)
                     }
                   } else {test_df <-  c(t1_mean_name = mean(t1$Percent), 
                                         t2_mean_name = mean(t2$Percent),
                                         "p.value" = NA)}
                   return(test_df)
                 })
 t1_name <- tdat_groups[1]
 t2_name <- tdat_groups[2]
  
  test <- do.call(rbind,test) %>%
    DT::datatable(escape = FALSE,
                  extensions = "Buttons",
                  filter = "top" ,
                  options = list(scrollX = TRUE,
                                 dom = "Bfrtip",
                                 buttons =
                                   list(list(
                                     extend = "collection",
                                     buttons = c("csv", "excel"),
                                     text = "Download Table"))) ,
                  caption = (paste("Mean Indicator Values in:" , EcologicalSite)), 
                  rownames = TRUE,
                  colnames = c(paste("Mean indicator value in", t1_name), 
                               paste("Mean indicator value in", t2_name), 
                               "p.value")) %>%
    formatStyle(columns = "p.value", backgroundColor = styleInterval(c(0, alpha), c("lightgreen", "lightgreen", "white"))) 
  # Highlight significant p.value based on alpha level
  
  return(test)
}
