library(tidyverse)
#devtools::install_github("nacnudus/unpivotr")
library(unpivotr)
library(tools)

#Organize the data
path <-  "C:\\Users\\alaurencetraynor\\Documents\\PLS\\2022 Estimates\\"

estimates <- paste0(path, "\\2022 Estimates by State.csv")

est <- read.csv(file = estimates , skip = 58, header = FALSE, stringsAsFactors = FALSE)

Year <- "2022"

# Remove blank lines
est$Indicator <- cumsum(!nzchar(est$V1))
est <- est[nzchar(est$V1), ] 

# Get names of columns to vector to replace 
# Skip the header lines up until state names
head <- read.csv(file = estimates, skip = 6, header = TRUE, stringsAsFactors = FALSE)
head_string <- colnames(head[2:ncol(head)])

#Replace column names
names(est) <- c("Vars", head_string, "Indicator")
str(est)

#Change anything reading as int to chr
est$SD <- as.character(est$SD)

#Now spread the data table
est_tall <- est %>% pivot_longer((All.BLM.Range:WY), names_to = "State", values_to = "Value")
est_tall <- est_tall %>% mutate_all(na_if, "")

# Create dataframe of indicator names which do not have NAs
IndicatorName <- est_tall %>% filter(is.na(Value)) %>% rename(Indicator_Number = Indicator, Indicator = Vars) %>% select (-State, - Value)
Names <- IndicatorName$Indicator

# remove indicators that have NA values
est_tall_data <- est_tall %>% filter(!Vars %in% Names) 

#Give row number for sorting
# Remove the sample size parameters because they have no SE, Cv and will mess up ordering

est_tall_filtered <- est_tall_data %>% filter(!grepl("Number of BLM", Vars))

est_tall_filtered$id <- 1:nrow(est_tall_filtered)

# Filter out sage grouse estimates and do those separate because they are in a different format
# Sage grouse estimates start with (66. Percent Foliar Cover of Sagebrush on BLM range)
# This gets weird because there is a space every 5 lines with grouse estimates and 12 for the rest
# The first indicator number for a sage grouse indicator is 27

est_tall_filtered <- est_tall_filtered %>% filter(Indicator < 27)

# Now give row id for common state/indicator combos
# Make sure the id + # is the number or rows to get to next variable (SE, CV, CV lower, CV upper)
Indicator_SE_Merge <- est_tall_filtered %>% mutate(id.se = id + 13 , id.cv = id + 26 , id.lower = id + 39 , id.upper = id + 52)

#Now subset each feature and then merge it based on the values made above
SE <- est_tall_filtered %>% filter(Vars == "SE") %>% rename(SE = Value)
CV <- est_tall_filtered %>% filter(Vars == "CV") %>% rename(CV = Value)
Lower <- est_tall_filtered %>% filter(Vars == "80% confidence interval lower bound") %>% rename(CI_Lower = Value)
Upper <- est_tall_filtered %>% filter(Vars == "80% confidence interval upper bound") %>% rename(CI_Upper = Value)
NonIndicator <- c("SE", "CV" , "80% confidence interval lower bound" , "80% confidence interval upper bound")
Indicators <- Indicator_SE_Merge %>% filter(!Vars %in% NonIndicator) 

# Merge indicators back together
Merge1 <- merge(Indicators, SE, by.x = "id.se" , by.y = "id")
Merge2 <- merge(Merge1, CV, by.x = "id.cv" , by.y = "id")
Merge3 <- merge(Merge2, Lower, by.x = "id.lower" , by.y = "id")
Merge4 <- merge(Merge3, Upper, by.x = "id.upper" , by.y = "id") %>% subset(select=which(!duplicated(names(.))))

#Now clean it up and add the indicator names

#Note that "Indicator Number" is the indicator header number... not the sub-indicator 
Estimates <- Merge4 %>% select("Vars.x" , "Indicator.x", "State.x", "Value", "SE", "CV", "CI_Lower", "CI_Upper", "Indicator.x") %>%
  rename(Indicator_Name = Vars.x, Indicator_Number = Indicator.x, State = State.x)

Estimates <- left_join(Estimates, IndicatorName, by = "Indicator_Number") %>% unique()

# Some indicators don't have a header indicator name. Replace with indicator_name
Fillers <- Estimates %>% filter(is.na(Indicator)) 
Fillers$Indicator_Name <- gsub("[0-9.]+", "", Fillers$Indicator_Name) %>% toupper()

Estimates[is.na(Estimates$Indicator), ]$Indicator <- Fillers$Indicator_Name

#Get indicators in all caps (or all lower)
#Then you can change this in Excel (toProper)
Estimates$Indicator <- gsub("", "", Estimates$Indicator) %>% tolower()
Estimates$Indicator <- toTitleCase(Estimates$Indicator)
  
Estimates["Year"] <- Year

# Write to a csv and then will bind all the years after

#write.csv(Estimates, file = paste0(path,"State_", Year, ".csv") , row.names = FALSE)

##
# Clean Sage-grouse indicators

# Reading in raw data and starting over here

#Skip all the header info until you get to the blank line above the fist SAGE_GROUSE indicator estimate
# updated this to 390 for 2020 estimates
grouse <- read.csv(file = estimates , skip = 392, header = FALSE, stringsAsFactors = FALSE)

# Remove blank lines
grouse$Indicator <- cumsum(!nzchar(grouse$V1))
grouse <- grouse[nzchar(grouse$V1), ] 

# NAme the columns
names(grouse) <- c("Vars", head_string, "Indicator")

#Change anything reading as int to chr
grouse$SD <- as.character(grouse$SD)

#Now spread the data table
grouse_tall <- grouse %>% pivot_longer((All.BLM.Range:WY), names_to = "State", values_to = "Value")
grouse_tall <- grouse_tall %>% mutate_all(na_if, "") # i dont think this is doing anything..

# Remove indicator names
IndicatorName <- grouse_tall %>% filter(is.na(Value)) %>% rename(Indicator_Number = Indicator, Indicator = Vars) %>% select (-State, - Value)
Names <- IndicatorName$Indicator
# IndicatorName is empty here???

grouse_tall_data <- grouse_tall %>% filter(!Vars %in% Names) 

# Give row number for sorting
# Remove the sample size parameters because they have no SE, Cv and will mess up ordering

grouse_tall_filtered <- grouse_tall_data %>% filter(!grepl("Number of BLM", Vars))

grouse_tall_filtered$id <- 1:nrow(grouse_tall_filtered)

#Add 27 to id to get indicator # correct
grouse_tall_filtered <- grouse_tall_filtered %>% mutate(Indicator = Indicator + 27)

# Now give row id for common state/indicator combos
# Make sure the id + # is the number or rows to get to next variable (SE, CV, CV lower, CV upper)
Indicator_SE_Merge <- grouse_tall_filtered %>% mutate(id.se = id + 13 , id.cv = id + 26 , id.lower = id + 39 , id.upper = id + 52)

#Now subset each feature and then merge it based on the values made above
SE <- grouse_tall_filtered %>% filter(Vars == "SE") %>% rename(SE = Value)
CV <- grouse_tall_filtered %>% filter(Vars == "CV") %>% rename(CV = Value)
Lower <- grouse_tall_filtered %>% filter(Vars == "80% confidence interval lower bound") %>% rename(CI_Lower = Value)
Upper <- grouse_tall_filtered %>% filter(Vars == "80% confidence interval upper bound") %>% rename(CI_Upper = Value)
NonIndicator <- c("SE", "CV" , "80% confidence interval lower bound" , "80% confidence interval upper bound")
Indicators <- Indicator_SE_Merge %>% filter(!Vars %in% NonIndicator) 

# Merge indicators back together
Merge1 <- merge(Indicators, SE, by.x = "id.se" , by.y = "id")
Merge2 <- merge(Merge1, CV, by.x = "id.cv" , by.y = "id")
Merge3 <- merge(Merge2, Lower, by.x = "id.lower" , by.y = "id")
Merge4 <- merge(Merge3, Upper, by.x = "id.upper" , by.y = "id") %>% subset(select=which(!duplicated(names(.))))

#Now clean it up and add the indicator names

#Note that "Indicator Number" is the indicator header number... not the sub-indicator 

Estimates_Grouse <- Merge4 %>% select("Vars.x" , "Indicator.x", "State.x", "Value", "SE", "CV", "CI_Lower", "CI_Upper", "Indicator.y") %>%
  rename(Indicator_Name = Vars.x, Indicator_Number = Indicator.x, State = State.x)

Estimates_Grouse <- left_join(Estimates_Grouse, IndicatorName, by = "Indicator_Number") %>% unique()
Estimates_Grouse <- Estimates_Grouse %>% select(-Indicator.y)

# Some indicators don't have a header indicator name. Replace with indicator_name
Fillers <- Estimates_Grouse %>% filter(is.na(Indicator)) 
Fillers$Indicator_Name <- gsub("[0-9.]+", "", Fillers$Indicator_Name) %>% tolower()

Estimates_Grouse[is.na(Estimates_Grouse$Indicator), ]$Indicator <- Fillers$Indicator_Name

#Get indicators in all caps (or all lower)
#The you can change this in Excel (toProper)
Estimates_Grouse$Indicator <- gsub("", "", Estimates_Grouse$Indicator) %>% tolower()

Estimates_Grouse$Indicator <- toTitleCase(Estimates_Grouse$Indicator)

Estimates_Grouse["Year"] <- Year

# Write to a csv and then will bind all the years after
fulldataset <- rbind(Estimates, Estimates_Grouse)
#Rbind with the other indicators then write to csv

write.csv(fulldataset, file = paste0(path, "State_Tidy_",Year,".csv") , row.names = FALSE)

## Make separate table for figure labels
## in Excel, create new column, then =proper() on Indicator column, name Indicator_proper
fulldataset <- read.csv(file = paste0(path, "State_Tidy_",Year,".csv"))

### Get sample size info into the table
samplesize <- head %>% filter(grepl("Number of BLM range observed points", State))
samplesize["Year"] <- Year

#We probably dont need this but just in case
write.csv(samplesize, file = "NumberResponses_States_CatVariables.csv")

# Read in the other data set, merge with  data
FullYears <- read.csv(file = paste0(path, "LMF_estimates_2011_2021_byState_for_figures_101222.csv"))

# Read in the figure labels and merge
figurelabels <- read.csv(file = paste0(path,"FigureLabels_updated.csv"))
figurelabels <- figurelabels %>% select(-Indicator_Number)

figurelabels$Indicator <- tolower(figurelabels$Indicator)
figurelabels$Indicator <- toTitleCase(figurelabels$Indicator)

fulldata_withlabels <- merge(x= fulldataset, y= figurelabels, by = c("Indicator_Name", "Indicator"), all.x =TRUE)

FullYears <- FullYears %>% 
  rename(Value = Estimate,
         Index = index) %>% 
  select(all_of(colnames(fulldata_withlabels)))

fulldata_withlabels <- rbind(fulldata_withlabels, FullYears)

write.csv(fulldata_withlabels, paste0(path, "EstimatesByStateByType_2012_", Year, "_withlabels.csv", row.names = FALSE))

# You will need to make sure all the value cells are reading as numeric

# formatting for PLS 2022
# select indicators
PLS_2022 <- fulldata_withlabels %>% 
  filter(Year == "2022",
         Indicator_Name %in% all_of(c("1% or Greater Relative Cover Composed of Non-Native Invasive Plants",
                                 "Percent of lands where native plants have greater than or equal to 95% relative cover",
                                 "Estimated % of BLM range acres with all 3 rangeland health attributes <= 2"))) %>% 
  select(Indicator_Name,
         State, Value, CI_Lower, CI_Upper)

# grab acres from heading
acres_inventoried <- head %>% 
  filter(State == "Estimated BLM range acres") %>% 
  rename(Indicator_Name = State)

acres_inventoried$SD <- as.character(acres_inventoried$SD)

acres_inventoried <- acres_inventoried %>% 
  pivot_longer((All.BLM.Range:WY), names_to = "State", values_to = "Estimate")

PLS_2022 <- PLS_2022 %>% 
  mutate(interval = as.numeric(Value) - as.numeric(CI_Lower)) %>%
  mutate(interval = round(interval, 1)) %>% 
  mutate(Estimate = paste0(Value, " ± ", interval))%>% 
  select(Indicator_Name, State, Estimate)

PLS_2022 <- rbind(PLS_2022, acres_inventoried)

PLS_2022$State <- gsub("All.BLM.Range", "All BLM Rangeland",PLS_2022$State) 

PLS_2022 <- PLS_2022 %>% 
  pivot_wider(names_from = Indicator_Name, values_from = Estimate)

write.csv(PLS_2022, paste0(path,"PLS_2022_v2.csv"))
