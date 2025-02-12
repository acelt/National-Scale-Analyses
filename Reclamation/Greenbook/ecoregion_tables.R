# Script to pull out simple percentiles (unweighted) of AIM and LMF data for indicators relevant to greenbook for Level 3 ecoregional benchmarks
require(tidyverse)

tdat_lmf = TDat_LMF

grouping_var = group_name
  
indicators = c("GapCover_101_200",
               "GapCover_200_plus",
               "AH_NoxPerenForbCover",
               "AH_NoxAnnForbCover",
               "AH_NoxPerenGrassCover",
               "AH_NoxAnnGrassCover",
               "AH_NoxAnnForbGrassCover",
               "AH_NoxPerenForbGrassCover",
               "AH_NoxSucculentCover",
               "AH_NoxShrubCover",
               "AH_NoxSubShrubCover",
               "AH_NoxTreeCover",
               "NumSpp_NoxPlant",
               "AH_NonNoxPerenForbCover",
               "AH_NonNoxAnnForbCover",
               "AH_NonNoxPerenGrassCover",
               "AH_NonNoxAnnGrassCover",
               "AH_NonNoxSucculentCover",
               "AH_NonNoxShrubCover",
               "AH_NonNoxSubShrubCover",
               "AH_NonNoxTreeCover",
               "NumSpp_NonNoxPlant",
               "Hgt_Woody_Avg",
               "Hgt_Herbaceous_Avg",
               "AH_TotalLitterCover",
               "BareSoilCover",
               "TotalFoliarCover",
               "AH_NoxCover")

percentiles = c(0.05,0.25,0.50,0.75,0.95)

output_path = "C:\\Users\\alaurencetraynor\\Documents\\2021\\Analysis\\USGS\\Greenbook\\"

percentile_tables <- function(tdat_lmf, grouping_var, indicators, percentiles = c(10,25,50,75,90), output_path){
  
  # Filter data set based on group and indicator
  needed_vars <- c(indicators,grouping_var)
  tdat_lmf_needed <- tdat_lmf[,colnames(tdat_lmf) %in% needed_vars]
  
  # Make long
  tdat_lmf_needed_long <- pivot_longer(data = tdat_lmf_needed, cols = all_of(indicators), names_to = "Indicator")
  
  # Split by grouping_var and generate quantiles for each indicator
  tdat_lmf_quantiles <- list()
  
  for(i in unique(tdat_lmf_needed_long[[grouping_var]])){
    for(j in unique(tdat_lmf_needed_long[["Indicator"]])){
      # subset data
      data <- tdat_lmf_needed_long[tdat_lmf_needed_long[[grouping_var]] == i & tdat_lmf_needed_long[["Indicator"]] == j, ]
      # Generate quanitles
      tdat_lmf_quantiles[[i]][[j]] <- quantile(x = data["value"] ,
                                               probs = percentiles,
                                               na.rm = TRUE)
    }
  }
  
  # export csv
  export <- list()
  for(i in unique(tdat_lmf_needed_long[[grouping_var]])){
    export[[i]] <- do.call(rbind,tdat_lmf_quantiles[[i]])
    #make sure no wierd characters in i
    x = 1
    group_names <- gsub("[[:punct:]]", "", i)
    write.csv(export[[i]], paste0(output_path,"percentile_table_",group_names[x],".csv"))
    x <- x+1
  }
  
}

# Checking out sample size
for(i in unique(TDat_LMF$US_L3NAME)){
  print(i)
  print(nrow(tdat_lmf[tdat_lmf$US_L3NAME == i,]))
  }
# Running the script
percentile_tables(tdat_lmf = TDat_LMF,
                  grouping_var = group_name,
                  indicators = indicators,
                  percentiles = percentiles,
                  output_path = output_path)

# how do these percentile changes when tdat/lmf are screened for disturbance?
                  
