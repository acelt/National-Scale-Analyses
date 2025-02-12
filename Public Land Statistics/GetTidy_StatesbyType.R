library(tidyverse)
#devtools::install_github("nacnudus/unpivotr")
library(unpivotr)

#Organize the data
path <-  "C:\\Users\\alaurencetraynor\\Documents\\PLS\\2022 Estimates\\"

estimates <- paste0(path, "\\2022 Estimates by State by Type.csv")

est <- read.csv(file = estimates , skip = 56, header = FALSE, stringsAsFactors = FALSE)

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
#est2018$ND.Type.II <- as.character(est2018$ND.Type.II)

#Now spread the data table
est_tall <- est %>% pivot_longer((All.BLM.Range:WY.Type.II), names_to = "State", values_to = "Value")
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

est_tall_filtered <- est_tall_filtered %>% filter(Indicator < 26)

# Now give row id for common state/indicator combos
# Make sure the id + # is the number or rows to get to next variable (SE, CV, CV lower, CV upper)
Indicator_SE_Merge <- est_tall_filtered %>% mutate(id.se = id + 21 , id.cv = id + 42 , id.lower = id + 63 , id.upper = id + 84)

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

# Now separate State into State Type

Estimates_Type <- Estimates %>% separate(State, into = c("State", "TypeName", "TypeNum") , "\\.")
Estimates_Type$State <- gsub("All" , "BLM", Estimates_Type$State) 
Estimates_Type$TypeNum <- gsub("Range" , "NA", Estimates_Type$TypeNum) 
Estimates_Type$TypeNum <- gsub("II" , "2", Estimates_Type$TypeNum) 
Estimates_Type$TypeNum <- gsub("I" , "1", Estimates_Type$TypeNum) 
Estimates_Type <- Estimates_Type %>% select(-TypeName) %>% rename(Type = TypeNum) 

#Get indicators in all caps (or all lower)
#The you can change this in Excel (toProper)
Estimates_Type$Indicator <- gsub("", "", Estimates_Type$Indicator) %>% toupper()
Estimates_Type["Year"] <- Year

# Write to a csv and then will bind all the years after

write.csv(Estimates_Type, file = paste0(path,"StateType_", Year, ".csv") , row.names = FALSE)

##
# Clean Sage-grouse indicators

# Reading in raw data and starting over here

#Skip all the header info until you get to the blank line above the fist SAGE_GROUSE indicator estimate
# updated this to 390 for 2020 estimates
grouse <- read.csv(file = estimates , skip = 390, header = FALSE, stringsAsFactors = FALSE)

# Remove blank lines
grouse$Indicator <- cumsum(!nzchar(grouse$V1))
grouse <- grouse[nzchar(grouse$V1), ] 

# NAme the columns
names(grouse) <- c("Vars", head_string, "Indicator")

#Now spread the data table
grouse_tall <- grouse %>% pivot_longer((All.BLM.Range:WY.Type.II), names_to = "State", values_to = "Value")
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
#Add 26 to id to get indicator # correct
grouse_tall_filtered <- grouse_tall_filtered %>% mutate(Indicator = Indicator + 24)

# Now give row id for common state/indicator combos
# Make sure the id + # is the number or rows to get to next variable (SE, CV, CV lower, CV upper)
Indicator_SE_Merge <- grouse_tall_filtered %>% mutate(id.se = id + 21 , id.cv = id + 42 , id.lower = id + 63 , id.upper = id + 84)

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
Fillers$Indicator_Name <- gsub("[0-9.]+", "", Fillers$Indicator_Name) %>% toupper()

Estimates_Grouse[is.na(Estimates_Grouse$Indicator), ]$Indicator <- Fillers$Indicator_Name

# Now separate State into State Type

Estimates_Grouse_Type <- Estimates_Grouse %>% separate(State, into = c("State", "TypeName", "TypeNum") , "\\.")
Estimates_Grouse_Type$State <- gsub("All" , "BLM", Estimates_Grouse_Type$State) 
Estimates_Grouse_Type$TypeNum <- gsub("Range" , "NA", Estimates_Grouse_Type$TypeNum) 
Estimates_Grouse_Type$TypeNum <- gsub("II" , "2", Estimates_Grouse_Type$TypeNum) 
Estimates_Grouse_Type$TypeNum <- gsub("I" , "1", Estimates_Grouse_Type$TypeNum) 
Estimates_Grouse_Type <- Estimates_Grouse_Type %>% select(-TypeName) %>% rename(Type = TypeNum) 
#Get indicators in all caps (or all lower)
#The you can change this in Excel (toProper)
Estimates_Grouse_Type$Indicator <- gsub("", "", Estimates_Grouse_Type$Indicator) %>% toupper()
Estimates_Grouse_Type["Year"] <- Year

# Write to a csv and then will bind all the years after
str(Estimates_Type)
str(Estimates_Grouse_Type)
fulldataset <- rbind(Estimates_Type, Estimates_Grouse_Type)
#Rbind with the other indicators then write to csv

write.csv(fulldataset, file = paste0(path, "StateType_",Year,".csv") , row.names = FALSE)

## Make separate table for figure labels
## in Excel, create new column, then =proper() on Indicator column, name Indicator_proper
fulldataset <- read.csv(file = paste0(path, "StateType_",Year,".csv"))

Labels <- fulldataset %>% select(Indicator_Name, Indicator_Number, Indicator) %>% unique()

# commenting this out as theres no field called Indicator_proper
# Labels <- Labels %>% mutate(xlab = "Year" , ylab = Indicator_Name, title = Indicator_proper)
# Labels$ylab <- gsub("^[0-9. ]+", "", Labels$ylab)
# write.csv(Labels, file = "FigureLabels.csv", row.names = FALSE)


### Get sample size info into the table
est_tall_samples <- est_tall_data %>% filter(grepl("Number of BLM", Vars))
est_tall_37 <- est_tall_samples %>% filter(grepl("38.", Vars)) %>% rename(NResp = Value)
est_tall_40 <- est_tall_samples %>% filter(grepl("41.", Vars)) %>% rename(NResp = Value)
est_tall_nresp <- rbind(est_tall_37, est_tall_40)

NResp <- est_tall_nresp %>% separate(State, into = c("State", "TypeName", "TypeNum") , "\\.")
NResp$State <- gsub("All" , "BLM", NResp$State) 
NResp$TypeNum <- gsub("Range" , "NA", NResp$TypeNum) 
NResp$TypeNum <- gsub("II" , "2", NResp$TypeNum) 
NResp$TypeNum <- gsub("I" , "1", NResp$TypeNum) 
NResp <- NResp %>% select(-TypeName) %>% rename(Type = TypeNum) 

NResp["Year"] <- Year

#We probably dont need this but just in case
NResp_ <- NResp
write.csv(NResp, file = "NumberResponses_States_CatVariables.csv")

# Read in the other data set, merge with  data

FullYears <- read.csv(file = paste0(path, "FullTidyData_2012_2019/EstimatesByStateByType_2012_2019_withlabels.csv"))

# labels <- names(FullYears)
# fulldataset_ <- fulldataset %>% select(all_of(labels))
# #bind them together
# fulldataset_allyears <- rbind(FullYears, fulldataset_)

# Read in the figure labels and merge

figurelabels <- read.csv(file = paste0(path,"Misc/FigureLabels_updated.csv"))
figurelabels <- figurelabels %>% select(-Indicator_Number, -Indicator)

fulldata_withlabels <- merge(fulldataset_allyears, figurelabels, by = "Indicator_Name")

write.csv(fulldata_withlabels, paste0(path, "EstimatesByStateByType_2012_", Year, "_withlabels.csv", row.names = FALSE))
# You will need to make sure all the value cells are reading as numeric