GroupingVarPrep_TDat <- function(TimePeriodGroup, 
                             TDat_LMF, 
                             TDat_LMF_Attributed,
                             time_period1,
                             time_period2) {
  if(TimePeriodGroup){
    if (any(class(TDat_LMF_Attributed) == "sf")) {
      TDat_LMF_Attributed <- st_drop_geometry(TDat_LMF_Attributed)
    }
    
    # Attribute Terradat in area of interest  with time periods for grouping
    attribute_time <- function(TDat_LMF_Attributed, time_period1, time_period2) {
      
      # Use min and max of time periods to name
      t1_name <- paste0(min(time_period1), "-", max(time_period1))
      t2_name <- paste0(min(time_period2), "-", max(time_period2))
      
      t1 <- cbind(Year = time_period1, PlotGrouping = rep(t1_name, times = length(time_period1)))
      t2 <- cbind(Year = time_period2,  PlotGrouping = rep(t2_name, times = length(time_period2)))
      time_period_table <- rbind(t1,t2)
      
      output <-  merge(x = TDat_LMF_Attributed,
                       y = time_period_table,
                       by = "Year",
                       all.x = TRUE)
    }
    output <- attribute_time(TDat_LMF_Attributed, time_period1, time_period2)
    
    # Remove NAs - plots that are outside of the time period ranges
    output <- output[!is.na(output$PlotGrouping),]
    
    
      return(output)
  }
  
  if(!TimePeriodGroup) {
    # Add Group variable for all plots with this ecological site
    TDat_LMF <- TDat_LMF %>%
      mutate(PlotGrouping = "Ecological Site")
    
    TDat_LMF <- TDat_LMF %>%
      filter(!PrimaryKey %in% TDat_LMF_Attributed$PrimaryKey)
    
    # Add Group variable for all plots in the area of interest with this ecological site
    TDat_LMF_Attributed <- TDat_LMF_Attributed %>%
      mutate(PlotGrouping = "AOI")
    
    # Remove geometry from terradat if it exists to reduce file size of data frame
    if (any(class(TDat_LMF_Attributed) == "sf")){
      TDat_LMF_Attributed <- st_drop_geometry(TDat_LMF_Attributed)
    }  
    
    # Merge to apply Group variable to Terradat and terradat in AOI and consolidate all data into single data frame
    TDat_All <- merge(TDat_LMF_Attributed, TDat_LMF, all = TRUE)
    output <- TDat_All
    
    return(output)
  }
}