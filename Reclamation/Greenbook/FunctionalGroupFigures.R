FunctionalGroup_Figures <- function(EcologicalSite, 
                                    SummaryVar, 
                                    Interactive, 
                                    TDat_All,
                                    Species_Indicator_All,
                                    SpeciesList, 
                                    alpha = 0.2) {
  
  # Create list of functional group indicators for figure
  TDat_FG_Indicators <-  c("AH_NoxAnnForbCover",
                            "AH_NonNoxPerenGrassCover",
                            "AH_NonNoxPerenForbCover",
                            "AH_NonNoxShrubCover",
                            "AH_NonNoxSubShrubCover",
                            "AH_NonNoxTreeCover",
                            "AH_SagebrushCover",
                           "AH_SagebrushCover_Live",
                           "AH_SagebrushCover_Dead")
# Confidence interval model
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

TDat_All$AH_SagebrushCover_Dead <- TDat_All$AH_SagebrushCover - TDat_All$AH_SagebrushCover_Live

# Convert Terradat into long format for applying CIs
TDat_long <- TDat_All %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                        TotalFoliarCover , FH_TotalLitterCover , 
                                        FH_RockCover, !!TDat_FG_Indicators, SoilStability_All , 
                                        SoilStability_Protected , 
                                        SoilStability_Unprotected, GapCover_25_50 , GapCover_51_100 , 
                                        GapCover_101_200 , GapCover_200_plus, 
                                        GapCover_25_plus, EcologicalSiteId, PlotGrouping) %>% # removing un-needed cols
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

# Select es names and linked ecologicalsiteids for merging
esnames <- TDat_All %>%
  dplyr::select(EcologicalSiteId, es_name) %>%
  rename("EcologicalSite" = EcologicalSiteId) %>%
  distinct()

# Join data summary with es names for both number and word format for ecological site names
TDat_summary <- inner_join(TDat_summary, esnames, by = "EcologicalSite")

# Set upper confidence interval to 0 if calculated as 100. This happens when the mean is 0.
TDat_summary$UCI[TDat_summary$UCI >= 100] <- 0

# Filter out relevant functional group indicators for figure
FunctionalGroupSum <- TDat_summary %>%
  filter(Indicator %in% TDat_FG_Indicators)
FunctionalGroupSum <- FunctionalGroupSum %>%
  filter(!(FunctionalGroupSum$Indicator == "AH_SagebrushCover"))
FunctionalGroupSum <- FunctionalGroupSum %>%
  filter(!(FunctionalGroupSum$Indicator == "AH_SagebrushCover_Live"))

# Rename indicators for better formatting and easier to understand names
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_NoxAnnForbCover"] <- "Noxious Annual Forb"
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_NonNoxPerenGrassCover"] <- "Perennial Grass"
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_NonNoxPerenForbCover"] <- "Perennial Forb"
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_NonNoxTreeCover"] <- "Tree"
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_NonNoxShrubCover"] <- "Shrub"
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_NonNoxSubShrubCover"] <- "Subshrub"
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_SagebrushCover"] <- "Sagebrush"
FunctionalGroupSum$Indicator[FunctionalGroupSum$Indicator == "AH_SagebrushCover_Dead"] <- "Dead Sagebrush"

# Palette of two colors for comparing Groups
group_palette <- c("#E69F00", "#56B4E9") 

if(SummaryVar == "FunctionalGroup") {

  if(!Interactive) {
Plot <- ggplot2::ggplot(FunctionalGroupSum,(aes(x = Indicator , 
                                            y = Mean,
                                            fill = Group))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.1, position = position_dodge(0.4))+
  theme_light() +
  labs(y = "Percent Cover" , 
       x = "Plant Functional Group",
       title = paste("Functional Group in", FunctionalGroupSum$es_name, FunctionalGroupSum$EcologicalSite, sep = " ")) +
  scale_fill_manual(values = group_palette, drop = FALSE)
                                  }
                  }
    return(Plot)
}