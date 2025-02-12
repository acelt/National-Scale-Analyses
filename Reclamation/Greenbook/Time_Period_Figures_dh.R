Time_Period_Figures <- function(EcologicalSite, 
                                SummaryVar, 
                                Interactive, 
                                TDat_All,
                                Species_Indicator_All, 
                                SpeciesList, 
                                alpha = 0.2) 
{
  # Gather indicators that are relevant to making all figures based on  Terradat
  TDat_Cover_Indicators <-  c("AH_NoxAnnForbCover","AH_NoxAnnGrassCover","AH_NoxCover","AH_NonNoxAnnForbCover", "AH_PerenGrassCover","AH_PerenForbCover","AH_TallPerenGrassCover","AH_ShortPerenGrassCover")
  #TDat_Cover_Indicators <-  enquo(TDat_Cover_Indicators)
  
  #=======================================================================
  # Confidence interval model functions
  #=======================================================================
  ci_model <-  function(data, y_var, x_var){
    
    if(any(grepl("SoilStability", data$Indicator))){ # using any() here since we split() the data by indicator below for modeling
      
      # Converting soil stability data to proportion for glm() call
      ss_fun <- function(x){
        y <- (x-1)/5
        return(y)
      }
      
      data[[y_var]] <- ss_fun(data[[y_var]])
      
      if(length(unique(data[[x_var]]))< 2) {
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]] ~ 1)
        object$xlevels <- unique(data[[x_var]])
        return(object)
      }else{
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]] ~ data[[x_var]])
        object$xlevels <- unique(data[[x_var]])
        return(object)
      }
      
    } else {
      if(length(unique(data[[x_var]]))< 2){
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]]/100 ~ 1)
        object$xlevels <- unique(data[[x_var]])# this prevent errors when there aren't plots in both x var groups
        return(object)
      }else{
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]]/100 ~ data[[x_var]])
        object$xlevels <-unique(data[[x_var]])
        return(object)
      }
    }
  }
  
  # logit transform for proportions
  logit_to_real_cis <- function(object,level=0.8) {
    
    sum.obj <- summary(object)
    z.score <- c(qnorm((1-level)/2),qnorm(1-(1-level)/2))
    
    
    logit.means <- c(sum.obj$coefficients[1,1],sum.obj$coefficients[1,1]+sum.obj$coefficients[-1,1])
    real.means <- plogis(logit.means)
    
    n.groups <- dim(sum.obj$coefficients)[1]
    
    out.df <- data.frame(Group=object$xlevels, Mean=real.means, LCI=NA, UCI=NA)  # changed from coefficients to xlevels to get real x names instead of "intercept"
    
    logit.var <- numeric(n.groups)
    for(i in 1:n.groups) {
      if(i==1) {logit.var[i] <- vcov(object)[i,i]}
      if(i>1) {
        vc <- vcov(object)[c(1,i),c(1,i)]
        logit.var[i] <- matrix(c(1,1),nrow=1,ncol=2) %*% vc %*% matrix(c(1,1),nrow=2,ncol=1)
      }
      out.df[i,c('LCI','UCI')] <- plogis(logit.means[i]+z.score*sqrt(logit.var[i]))
    }
    
    return(out.df)
  }
  
  # log transforming is for heights
  log_to_real_cis <- function(object,level=0.95) {
    
    sum.obj <- summary(object)
    z.score <- c(qnorm((1-level)/2),qnorm(1-(1-level)/2))
    
    log.means <- c(sum.obj$coefficients[1,1],sum.obj$coefficients[1,1]+sum.obj$coefficients[-1,1])
    real.means <- exp(log.means)
    
    n.groups <- dim(sum.obj$coefficients)[1]
    
    out.df <- data.frame(Group=rownames(object$xlevels),Mean=real.means,LCI=NA,UCI=NA) # changed from coeficients to xlevels to get real x names instead of "intercept"
    
    log.var <- numeric(n.groups)
    for(i in 1:n.groups) {
      if(i==1) {log.var[i] <- vcov(object)[i,i]}
      if(i>1) {
        vc <- vcov(object)[c(1,i),c(1,i)]
        logit.var[i] <- matrix(c(1,1),nrow=1,ncol=2) %*% vc %*% matrix(c(1,1),nrow=2,ncol=1)
      }
      out.df[i,c('LCI','UCI')] <- exp(log.means[i]+z.score*sqrt(log.var[i]))
    }
    
    return(out.df)
  }
  
  # Filter input dataset to include only relevant plots within the individual ecological site 
  TDat_All <- TDat_All[TDat_All$EcologicalSiteId == EcologicalSite,]
  
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
  
  # Order by grouping variable to ensure that xvar/group doesn't mismatch during model
  TDat_long <- TDat_long[order(TDat_long$PlotGrouping),]
  
  # Split Terradat df into list of lists (of each indicator) and apply model for confidence intervals to each list
  TDat_cis_list <- lapply(X = split(TDat_long, TDat_long[["Indicator"]]), FUN = function(TDat_long){
    mod <- ci_model(data = TDat_long, y_var = "Percent", x_var = "PlotGrouping")
    cis_df <- logit_to_real_cis(mod)
    return(cis_df)
  })
  
 # Function to organize dataframe of CIs into dataframe and clean up indicator names
  plot_org <- function(ci_list){
    TDat_cis_df <- do.call("rbind",(ci_list))
    TDat_cis_df$Indicator <- row.names(TDat_cis_df)
    TDat_cis_df$Indicator <-  gsub("\\.[0-9]","",TDat_cis_df$Indicator)
    TDat_cis_df$Group <- gsub("data\\[\\[x_var\\]\\]", "",TDat_cis_df$Group)
    
    # this replaces intercept with the appropriate group name
   # if(any(unique(TDat_cis_df$Group)== "2016-2020")){
   #   TDat_cis_df$Group <- gsub("\\(Intercept\\)", "2011-2015",TDat_cis_df$Group)
   # } else if(EcologicalSite == "Clayey Foothills"){
   #   TDat_cis_df$Group <- gsub("\\(Intercept\\)", "2011-2015",TDat_cis_df$Group)
   # } else {
   #   TDat_cis_df$Group <- gsub("\\(Intercept\\)", "2016-2020",TDat_cis_df$Group)
   # }
    ## THIS IS STILL GIVING ERRORS FOR BRUSHY LOAM
    
    # Convert proportion back to percent cover
    times100 <- function(x){x * 100}
    # Convert soil stability ratings back to 1-6 scale
    nuf_ss <- function(x){(x*5)+1}
    
    TDat_cis_df[!grepl("SoilStability", TDat_cis_df$Indicator),] <- TDat_cis_df[!grepl("SoilStability", TDat_cis_df$Indicator),] %>% mutate_if(is.numeric, times100)
    TDat_cis_df[grepl("SoilStability", TDat_cis_df$Indicator),] <-  TDat_cis_df[grepl("SoilStability", TDat_cis_df$Indicator),] %>% mutate_if(is.numeric, nuf_ss)
    TDat_summary <- TDat_cis_df %>% mutate_if(is.numeric, round , digits = 2)
    TDat_summary$EcologicalSite <- EcologicalSite
    return(TDat_summary)
  }
  
  TDat_summary <- plot_org(TDat_cis_list)
  
  # Changing upper confidence limit to 0 if it equals 100. This happens with mean of 0.
  TDat_summary$UCI[TDat_summary$UCI >= 100] <- 0
  
  #=======================================================================
  # Species indicator data prep
  #=======================================================================
  # Filter input dataset to include only relevant plots within the individual ecological site 
  Species_Indicator_All <- Species_Indicator_All[Species_Indicator_All$EcologicalSiteId == EcologicalSite,]
  
  # Summarize species level data (at ecosite level only)
  Species_summary <- Species_Indicator_All %>% 
    filter(!is.na(AH_SpeciesCover))
  
  # Split species indicator df into list of lists (of each species) and apply model for confidence intervals to each list
  species_cis_list <- lapply(X = split(Species_summary, Species_summary[["Species"]]), FUN = function(Species_summary) {
    mod <- ci_model(data = Species_summary, y_var = "AH_SpeciesCover", x_var = "PlotGrouping")
    cis_df <- logit_to_real_cis(mod)
    return(cis_df)
  })
  
  species_cis_list <- plot_org(species_cis_list)
  
  # Merge df of CIs with species list information
  Species_summary <- merge(x = species_cis_list,
                           y = Species_summary[,c("GrowthHabitSub", "Duration", "Species","CommonName","ScientificName", "Noxious")],
                           by.x = "Indicator",
                           by.y = "Species",
                           all.y = FALSE)
  
  
  #=======================================================================
  # Random formatting for figures
  #=======================================================================
  NoxNonPal_Fill <- c("grey75"  , "#D55E00")
  NoxNonPal_Dot <- c("grey33" , "#993300")
  
  dodge1 <- position_dodge(width = 0.9)
  dodge2 <- position_dodge(width = 0.4)
  
  group_palette <- c("#E69F00", "#56B4E9") 
  ## Setting color for attribute title
  #Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed[[attribute_title]]))))
  
  #=======================================================================
  # Figures
  #=======================================================================
  if(SummaryVar == "GrowthHabitSub"){
    
    GH_Species_summary <- TDat_summary %>%
      filter(Indicator == "AH_NonNoxAnnForbCover" |
               Indicator == "AH_PerenForbCover"| 
               Indicator =="AH_PerenGrassCover"| 
               Indicator == "AH_ShortPerenGrassCover"|
               Indicator =="AH_TallPerenGrassCover") %>%
      filter(EcologicalSite == EcologicalSite)
    
    if(Interactive){
      
      Plots <-  lapply(X = split(GH_Species_summary, GH_Species_summary[["Indicator"]] , 
                                 drop = TRUE),
                       
                       FUN = function(GH_Species_summary){
                         
                         current_plot <- ggplot2::ggplot(GH_Species_summary[!is.na(GH_Species_summary$Indicator),], 
                                                         aes(x = Indicator, 
                                                             y = Mean, 
                                                             text = paste("Percent Cover: " , Mean,
                                                                          "Ecological Site: ", EcologicalSite,
                                                                          "Group: ", Group,
                                                                          "Upper Condfidence Interval: ", UCI,
                                                                          "Lower Condfidence Interval: ", LCI,
                                                                          sep = "<br>"),
                                                             fill = Group)) +
                           
                           geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                           geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                           theme_light() + 
                           theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                 axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                 axis.line.y = element_blank()) +
                           scale_fill_manual(values = group_palette, drop = FALSE) +
                           theme(panel.grid.major.y = element_blank(), axis.title.y = element_blank()) +
                           ggtitle(paste0("Percent Cover by Functional Group: " , 
                                          GH_Species_summary$Indicator, ", Ecological Site: ", EcologicalSite)) + # this is throwing errors 
                           coord_flip()
                         
                         return(current_plot)
                       }
      )
    }
    
    if(!Interactive){
      
      Plots <- lapply(X = split(GH_Species_summary, GH_Species_summary[["Indicator"]] , drop = TRUE), 
                      FUN = function(GH_Species_summary){
                        
                        current_plot <- ggplot2::ggplot(GH_Species_summary[!is.na(GH_Species_summary$Indicator),],
                                                        aes(x = Indicator ,
                                                            y = Mean,
                                                            fill = Group)) +
                          geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                          geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                          labs(y = "Percent Cover") +
                          theme_light() + 
                          scale_fill_manual(values = group_palette, drop = FALSE) +
                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                axis.line.y = element_blank()) +
                          theme(panel.grid.major.y = element_blank(),axis.title.y = element_blank()) +
                          ggtitle(paste("Percent Cover by Functional Group:", 
                                        GH_Species_summary$Indicator, ", Ecological Site:", EcologicalSite)) +
                          coord_flip()
                        
                        
                        return(current_plot)
                      })
    }
  }
  
  if(SummaryVar == "Noxious"){
    
    Noxious_summary <-  TDat_summary %>% 
      filter(Indicator == "AH_NoxAnnForbCover" | 
               Indicator == "AH_NoxAnnGrassCover" | 
               Indicator == "AH_NoxCover") %>%
      filter(EcologicalSite == EcologicalSite)
    
    if(Interactive){
      Plots <- ggplot2::ggplot(Noxious_summary,(aes(x = Indicator,
                                                    y = Mean,
                                                    fill = Group,
                                                    text = paste("Percent Cover: " , Mean, 
                                                                 "Noxious Indicator: " , Indicator, 
                                                                 "Group: ", Group, 
                                                                 sep = "<br>")))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() ,
              axis.ticks.y = element_blank() , axis.line.y = element_blank() , 
              axis.title.x = element_blank()) +
        theme(panel.grid.major.y = element_blank() , legend.position = "none") +
        
        scale_fill_manual(values = group_palette, drop = FALSE) +
        coord_flip() + 
        facet_grid(rows = vars(Indicator) , switch = "y" , scales = "free" , 
                   drop = TRUE) 
      return(Plots)
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(Noxious_summary,(aes(x = Indicator , 
                                                    y = Mean, 
                                                    fill = Group))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() , 
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        coord_flip() + 
        facet_grid(rows = vars(Indicator) ,
                   switch = "y" , scales = "free" , drop = TRUE)
      
    }}
  
  if(SummaryVar == "Species"){
    
    if(Interactive){
      Plots <-lapply(X = split(Species_summary, list(Species_summary$GrowthHabitSub , Species_summary$Duration) , drop = TRUE),
                     FUN = function(Species_summary){
                       Species_summary <- Species_summary %>% 
                         filter(EcologicalSite == EcologicalSite)
                       current_plot <- ggplot2::ggplot(Species_summary , aes(x = Indicator , 
                                                                             y = Mean,
                                                                             fill = Group,
                                                                             text = paste("Species: " , ScientificName , 
                                                                                          "Code: " , Indicator , 
                                                                                          "Common Name: ", CommonName,
                                                                                          "Percent Cover: " , Mean, 
                                                                                          "Noxious: " , Noxious , 
                                                                                          "Group: ", Group,
                                                                                          "Ecological Site: ", EcologicalSite,
                                                                                          sep = "<br>"))) +
                         geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                         geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                         theme_light() +
                         labs(y = "Percent Cover") + 
                         ggtitle(paste("Percent Cover by Species, " , 
                                       Species_summary$GrowthHabitSub, 
                                       Species_summary$Duration ,
                                       EcologicalSite)) + # removed tostring function
                         theme(axis.title.y = element_blank() ,
                               axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(), 
                               axis.title.x = element_blank())+   
                         theme(panel.grid.major.y = element_blank() ,
                               axis.title.y = element_blank()) +
                         scale_fill_manual(values = group_palette, drop = FALSE) +
                         coord_flip() +  facet_grid(rows = vars(Indicator),
                                                    scales = "free" , switch = "y",  drop = TRUE) 
                       return(current_plot)
                     })
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(Species_summary, list(Species_summary$GrowthHabitSub , 
                                                      Species_summary$Duration) , 
                                drop = TRUE),
                      FUN = function(Species_summary){
                        SSpecies_summary <- Species_summary %>% 
                          filter(EcologicalSite == EcologicalSite)
                        current_plot <- ggplot2::ggplot(Species_summary , aes(x = Indicator , 
                                                                              y = Mean, 
                                                                              fill = Group)) +
                          geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                          geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                          theme_light() +
                          labs(y = "Percent Cover") + 
                          scale_fill_manual(values = group_palette, drop = FALSE) +
                          ggtitle(paste("Percent Cover by Species, " , 
                                        Species_summary$Duration, Species_summary$GrowthHabitSub , 
                                        EcologicalSite)) + 
                          theme(axis.title.y = element_blank()) +
                          coord_flip() +  facet_grid(rows = vars(Indicator) ,
                                                     switch = "y" , scales = "free" , drop = TRUE) 
                        return(current_plot)
                      })
      
    }
  }
  
  if(SummaryVar == "GroundCover"){
    
    GC_summary <- TDat_summary %>% 
      filter(Indicator == "BareSoilCover" | 
               Indicator == "FH_RockCover" | 
               Indicator == "FH_TotalLitterCover" | 
               Indicator == "TotalFoliarCover") %>%
      filter(EcologicalSite == EcologicalSite)
    
    if(Interactive){
      
      
      Plots <-  ggplot2::ggplot(GC_summary,(aes(x = Indicator , 
                                                y = Mean , 
                                                fill = Group,
                                                text = paste("Indicator: " , Indicator ,
                                                             "Percent Cover: " , Mean , 
                                                             "Group: ", Group,
                                                             "Ecological Site: ", EcologicalSite,
                                                             sep = "<br>" )))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        scale_fill_manual(values = group_palette, drop = FALSE) +
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
      Plots <- ggplot2::ggplot(GC_summary,(aes(x = Indicator , 
                                               y = Mean,
                                               fill = Group))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Ground Cover (%)" , x = "Indicator",
             caption = paste("Ground Cover indicators in: ", 
                             EcologicalSite, sep = " ")) +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() ,  
              axis.title.x = element_blank() , 
              axis.title.y = element_blank()) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
      
    }
    
    
  }
  
  if(SummaryVar == "Gap"){
    
    Gap_summary <-  TDat_summary %>% 
      filter(Indicator == 'GapCover_25_plus'|
               Indicator =="GapCover_25_50"|
               Indicator =="GapCover_101_200"|
               Indicator =="GapCover_200_plus"|
               Indicator =="GapCover_51_100")  %>%
      filter(EcologicalSite == EcologicalSite)
    
    # reorder factors by size
    Gap_summary$Indicator <- as.factor(Gap_summary$Indicator)
    Gap_summary$Indicator <-  factor(Gap_summary$Indicator, levels = c("GapCover_200_plus","GapCover_101_200" , "GapCover_51_100", "GapCover_25_50", 'GapCover_25_plus'))
    Gap_summary <- Gap_summary[order(Gap_summary$Indicator),]
    
    #Plot prep
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Gap_summary , aes(x = Indicator , 
                                                        y = Mean , 
                                                        fill = Group,
                                                        text = paste("Gap Class (cm): " , Indicator , 
                                                                     "Percent Cover: " , Mean , 
                                                                     "Group: ", Group,
                                                                     "Ecological Site: ", EcologicalSite,
                                                                     sep = "<br>"))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) + coord_flip() + 
        theme_light() + 
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme(axis.title.x = element_blank() ,
              axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() , 
              axis.title.y = element_blank() , 
              axis.line.y = element_blank(), 
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Indicator) , switch = "y" ,
                   scales = "free" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Gap_summary , aes(x = Indicator , 
                                                        y = Mean, 
                                                        fill = Group)) +
        labs(y = "Percent Cover" , x = "Gap Size Class (cm)", 
             caption = paste("Percent cover of canopy gap in: ", 
                             EcologicalSite, sep = " ")) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip() + 
        theme_light(base_size = 16) + 
        scale_fill_manual(values = group_palette, drop = FALSE) + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Indicator) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
    }
    
  }
  
  if(SummaryVar == "SoilStability"){
    
    soil_labels <- c("SoilStability_All" = "All" , "SoilStability_Protected" = "Protected" , 
                     "SoilStability_Unprotected" = "Unprotected")
    
    Soil_summary <- TDat_summary %>% 
      filter(Indicator == "SoilStability_All"|
               Indicator =="SoilStability_Protected"| 
               Indicator  == "SoilStability_Unprotected")  %>%
      filter(EcologicalSite == EcologicalSite)
    
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Soil_summary , 
                               aes(x = Indicator , 
                                   y = Mean , 
                                   fill = Group, 
                                   text = paste("Ecological Site: ", EcologicalSite,
                                                "Group: ", Group, 
                                                "Rating: " , Mean ,
                                                sep = "<br>"))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip(ylim = c(0,6)) +
        theme_light() + 
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.title = element_blank()) +
        facet_grid(rows = vars(Indicator) , switch = "y" ,
                   scales = "free_y" , drop = TRUE , 
                   labeller = as_labeller(soil_labels))
    }
    
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Soil_summary, aes(x = Indicator , 
                                                        y = Mean, 
                                                        fill = Group)) +
        labs(x = "Vegetation cover class", 
             y = "Soil Stability Rating",
             caption = paste("Soil stability ratings in: ", 
                             EcologicalSite)) +
        geom_errorbar(aes(ymin =LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip(ylim = c(0,6)) +
        theme_light(base_size = 16) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Indicator),
                   switch = "y",
                   scales = "free_y", 
                   drop = TRUE,
                   labeller = as_labeller(soil_labels))
      
    }
    
  }
  
  return(Plots)
  
}
