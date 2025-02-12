Ecoregion <- read.csv(file = "C:\\Users\\alaurencetraynor\\Documents\\LMF\\RPA\\2022 National Report\\EstimatesByEcoregion_2012_2021_withlabels_complete_stauffer_20220915.csv", stringsAsFactors = FALSE)

SampleSize <- read.csv(file = "C:\\Users\\alaurencetraynor\\Documents\\LMF\\RPA\\2022 National Report\\New Indicators\\New Indicators\\Sample Sizes/NumberResponses_Ecoregions_CatVariables_all_years.csv", stringsAsFactors = FALSE)

#R is not reading numbers as numeric...fixing
#Make sure Ecoregion name formatting is the same
#I fixed this in excel 
unique(SampleSize$Ecoregion) == unique(Ecoregion$Ecoregion)

Ecoregion <- Ecoregion %>% 
  filter(Ecoregion != "X 1")

# make sure they have the same levels of year
SampleSize <- SampleSize[SampleSize$Year %in% unique(Ecoregion$Year),]

# Replace 2012 with 2013 because the sampling was actually in 2013
Ecoregion$Year <- as.character(Ecoregion$Year)
Ecoregion$Year <- gsub("2012", "2013", Ecoregion$Year)
Ecoregion$Year <- as.factor(Ecoregion$Year)

# Join with sample size - sample size already has 2012 convert to 2013 
Ecoregion <- merge(x = Ecoregion,
                   y = SampleSize,
                   by = c("Ecoregion", "Year"),
                   all.x = TRUE)

Ecoregion$Year <- format(as.Date(Ecoregion$Year, "%Y"),"%Y")

# want to make sure all levels exist in sample sizes i.e. NA and 0 are there when they should be for all combinations of year and ecoregion
#Ecoregion <- tidyr::complete(Ecoregion, Year, Ecoregion, Index, fill = list(BLM.Sample = 0, BLM.Acres = 0, Value = 0))

# # Remove Marine west coast forest- there is no data
#WCF <- Ecoregion %>% filter(Ecoregion == "Marine West Coast Forest") #No data

Ecoregion <- Ecoregion %>% filter(!Ecoregion == "Marine West Coast Forest")

#Pull the max value of acres for plot scale
#Put acres in millions

Ecoregion <- mutate(Ecoregion, Value2 = ifelse(var_type == "acres", Value/1000000, Value))

Ecoregion <- mutate(Ecoregion, CI_Lower2 = ifelse(var_type == "acres", CI_Lower/1000000, CI_Lower)) %>% mutate(Ecoregion, CI_Upper2 = ifelse(var_type == "acres", CI_Upper/1000000, CI_Upper))

#Replace small sample sizes with NA

Ecoregion <- mutate(Ecoregion, Flag = ifelse(Value != 0|is.na(Value), NA, 0))
Ecoregion$Flag <- as.integer(Ecoregion$Flag)

Ecoregion <- mutate(Ecoregion, Value2 = ifelse(Value != 0, Value2, NA))
Ecoregion <- mutate(Ecoregion, CI_Upper2 = ifelse(Value != 0, CI_Upper2, NA))
Ecoregion <- mutate(Ecoregion, CI_Lower2 = ifelse(Value != 0, CI_Lower2, NA))

Ecoregion <- mutate(Ecoregion, Value2 = ifelse(Number.of.BLM.range.observed.points >= 10, Value2, NA))
Ecoregion <- mutate(Ecoregion, CI_Upper2 = ifelse(Number.of.BLM.range.observed.points >= 10, CI_Upper2, NA))
Ecoregion <- mutate(Ecoregion, CI_Lower2 = ifelse(Number.of.BLM.range.observed.points >= 10, CI_Lower2, NA))

Ecoregion <- mutate(Ecoregion, Flag = ifelse(Number.of.BLM.range.observed.points >= 10, NA, 0))

# Set color scale
#For coloring by ecoregion
#cc <- scales::seq_gradient_pal("grey", "dodgerblue2", "Lab")(seq(0,1,length.out = (length(unique(Ecoregion$Ecoregion_name))))) 

#For coloring by year (this is what we wound up doing)                                                                 
#cc2 <- scales::seq_gradient_pal("grey", "dodgerblue2", "Lab")(seq(0,1,length.out = (length(unique(Ecoregion$Year))))) 

Ecoregion$Ecoregion <- as.factor(as.character(Ecoregion$Ecoregion))

# also remove number at start nof inidcator name
Ecoregion$Indicator_Name <- gsub("^\\d{1}\\. ","", Ecoregion$Indicator_Name)

# add Index field for ploting
Ecoregion <- Ecoregion %>%
  group_by(Indicator, Indicator_Name) %>%
  mutate(Index = cur_group_id()) %>%
  ungroup() %>%
  arrange(Index)

write.csv(Ecoregion, "C:\\Users\\alaurencetraynor\\Documents\\LMF\\RPA\\2022 National Report\\Ecoregion_plotdata_2011_2021.csv")