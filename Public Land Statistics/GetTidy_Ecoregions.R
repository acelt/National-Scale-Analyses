
library(tidyverse)
devtools::install_github("nacnudus/unpivotr")
library(unpivotr)

#Organize the data
path <-  "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\LMF\\RPA\\"

estimates <- paste0(path, "2020 Estimates\\2020 Estimates by Ecoregion.csv")
  
est <- read.csv(file = estimates , skip = 56, header = FALSE, stringsAsFactors = FALSE)

Year <- "2020"

# Remove blank lines
est$Indicator <- cumsum(!nzchar(est$V1))
est <- est[nzchar(est$V1), ] 

# Get names of columns to vector to replace 
# Skip the header lines up until Ecoregion names
head <- read.csv(file = estimates, skip = 6, header = TRUE, stringsAsFactors = FALSE)
head_string <- colnames(head[2:ncol(head)])

#Replace column names
names(est) <- c("Vars", head_string, "Indicator")

#Change anything reading as int to chr
#est$MARINE.WEST.COAST.FOREST <- as.character(est$MARINE.WEST.COAST.FOREST)

#Now spread the data table
est_tall <- est %>% pivot_longer((All.BLM.Range:WESTERN.CORDILLERA), names_to = "Ecoregion", values_to = "Value")
est_tall <- est_tall %>% mutate_all(na_if, "")

# Remove indicator names
IndicatorName <- est_tall %>% filter(is.na(Value)) %>% rename(Indicator_Number = Indicator, Indicator = Vars) %>% select (-Ecoregion, - Value)
Names <- IndicatorName$Indicator
est_tall_data <- est_tall %>% filter(!Vars %in% Names) 

#Give row number for sorting
# Remove the sample size parameters because they have no SE, Cv and will mess up ordering

est_tall_filtered <- est_tall_data %>% filter(!grepl("Number of BLM", Vars))

est_tall_filtered$id <- 1:nrow(est_tall_filtered)

# Filter out sage grouse estimates and do those separate because they are in a different format
# Sage grouse estimates start with (66. Percent Foliar Cover of Sagebrush on BLM range)
# This gets weird because there is a space every 5 lines with grouse estimates and 12 for the rest
# The first indicator number for a sage grouse indicator is 27

est_tall_filtered <- est_tall_filtered %>% filter(Indicator < 26)

# Now give row id for common Ecoregion/indicator combos
# Make sure the id + # is the number or rows to get to next variable (SE, CV, CV lower, CV upper)
Indicator_SE_Merge <- est_tall_filtered %>% mutate(id.se = id + 12 , id.cv = id + 24 , id.lower = id + 36 , id.upper = id + 48)

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

Estimates <- Merge4 %>% select("Vars.x" , "Indicator.x", "Ecoregion.x", "Value", "SE", "CV", "CI_Lower", "CI_Upper", "Indicator.x") %>%
  rename(Indicator_Name = Vars.x, Indicator_Number = Indicator.x, Ecoregion = Ecoregion.x)

Estimates <- left_join(Estimates, IndicatorName, by = "Indicator_Number") %>% unique()

# Some indicators don't have a header indicator name. Replace with indicator_name
Fillers <- Estimates %>% filter(is.na(Indicator)) 
Fillers$Indicator_Name <- gsub("[0-9.]+", "", Fillers$Indicator_Name) %>% toupper()

Estimates[is.na(Estimates$Indicator), ]$Indicator <- Fillers$Indicator_Name

#Get indicators in all caps (or all lower)
#The you can change this in Excel (toProper)
Estimates$Indicator <- gsub("", "", Estimates$Indicator) %>% toupper()
Estimates["Year"] <- Year

# Clean Sage-grouse indicators

# Reading in raw data and starting over here

#Skip all the header info until you get to the blank line above the fist SAGE_GROUSE indicator estimate
grouse <- read.csv(file = estimates , skip = 378, header = FALSE, stringsAsFactors = FALSE)

# Remove blank lines
grouse$Indicator <- cumsum(!nzchar(grouse$V1))
grouse <- grouse[nzchar(grouse$V1), ] 

# NAme the columns
names(grouse) <- c("Vars", head_string, "Indicator")

#Now spread the data table
grouse_tall <- grouse %>% pivot_longer((All.BLM.Range:WESTERN.CORDILLERA), names_to = "Ecoregion", values_to = "Value")
grouse_tall <- grouse_tall %>% mutate_all(na_if, "")

# Remove indicator names
IndicatorName <- grouse_tall %>% filter(is.na(Value)) %>% rename(Indicator_Number = Indicator, Indicator = Vars) %>% select (-Ecoregion, - Value)
Names <- IndicatorName$Indicator
grouse_tall_data <- grouse_tall %>% filter(!Vars %in% Names) 

#Give row number for sorting
# Remove the sample size parameters because they have no SE, Cv and will mess up ordering

grouse_tall_filtered <- grouse_tall_data %>% filter(!grepl("Number of BLM", Vars))

grouse_tall_filtered$id <- 1:nrow(grouse_tall_filtered)
#Add 26 to id to get indicator # correct
grouse_tall_filtered <- grouse_tall_filtered %>% mutate(Indicator = Indicator + 24)

# Now give row id for common Ecoregion/indicator combos
# Make sure the id + # is the number or rows to get to next variable (SE, CV, CV lower, CV upper)
Indicator_SE_Merge <- grouse_tall_filtered %>% mutate(id.se = id + 12 , id.cv = id + 24 , id.lower = id + 36 , id.upper = id + 48)

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

Estimates_Grouse <- Merge4 %>% select("Vars.x" , "Indicator.x", "Ecoregion.x", "Value", "SE", "CV", "CI_Lower", "CI_Upper", "Indicator.x") %>%
  rename(Indicator_Name = Vars.x, Indicator_Number = Indicator.x, Ecoregion = Ecoregion.x)

Estimates_Grouse <- left_join(Estimates_Grouse, IndicatorName, by = "Indicator_Number") %>% unique()
#Estimates_Grouse <- Estimates_Grouse %>% select(-Indicator.y) %>% rename(Indicator = Indicator.x)
# Some indicators don't have a header indicator name. Replace with indicator_name
Fillers <- Estimates_Grouse %>% filter(is.na(Indicator)) 
Fillers$Indicator_Name <- gsub("[0-9.]+", "", Fillers$Indicator_Name) %>% toupper()

Estimates_Grouse[is.na(Estimates_Grouse$Indicator), ]$Indicator <- Fillers$Indicator_Name

 
#Get indicators in all caps (or all lower)
#The you can change this in Excel (toProper)
Estimates_Grouse$Indicator <- gsub("", "", Estimates_Grouse$Indicator) %>% toupper()
Estimates_Grouse["Year"] <- Year

# Write to a csv and then will bind all the years after
str(Estimates_Type)
str(Estimates_Grouse)

fulldataset <- rbind(Estimates, Estimates_Grouse)
#Rbind with the other indicators then write to csv

write.csv(fulldataset, file = paste0(path,"Ecoregion_",Year, ".csv") , row.names = FALSE)


### Get sample size info into the table
est_tall_samples <- est_tall_data %>% filter(grepl("Number of BLM", Vars))
est_tall_37 <- est_tall_samples %>% filter(grepl("38.", Vars)) %>% rename(NResp = Value)
est_tall_40 <- est_tall_samples %>% filter(grepl("41.", Vars)) %>% rename(NResp = Value)
est_tall_nresp <- rbind(est_tall_37, est_tall_40)

NResp["Year"] <- Year

#We probably dont need this but just in case
NResp_ <- NResp
write.csv(NResp, file = paste0(path, "NumberResponses_Ecoregions_CatVariables.csv"))

# Read in the other data set, merge with  data

FullYears <- read.csv(file = paste0(path,"FullTidyData_2012_2019/EstimatesByEcoregion_2012_2019_withlabels.csv"))

#There's an extra column in here, getting rid of
figurelabels <- read.csv(file = paste0(path,"Misc/FigureLabels_updated.csv"))
figurelabels <- figurelabels %>% select(-Indicator_Number, -Indicator)

# Read in the figure labels and merge
fulldata_withlabels <- merge(fulldataset, figurelabels, by = "Indicator_Name")

#bind them together
# ecoregion_name is missing from 2020 data
# make a lut real quick and merge
lut <- unique(FullYears[,3:4])

data_final <- merge(fulldata_withlabels,
                    lut,
                    by.x = "Ecoregion",
                    by.y = "Ecoregion_name",
                    all.x = TRUE)

# rename col names to match 2012-2019 data
data_final <- rename(data_final, Ecoregion_name = Ecoregion, Ecoregion = Ecoregion.y)

fulldataset_allyears <- rbind(FullYears, data_final)

write.csv(fulldataset_allyears, paste0(path, "EstimatesByEcoregion_2012_",Year, "_withlabels.csv"), row.names = FALSE)
# You will need to make sure all the value cells are reading as numeric