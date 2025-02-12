library(tidyverse)
library(lme4)
library(arcgisbinding)
arc.check_product()
library(sf)

ecoregion <- read.csv('\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\National\\National Report Poster GIS\\EstimatesByEcoregion_2012_2021_withlabels.csv')

# lets reduce the number of indicators 
# focus on acres
# invasives, gap and bare soil, soil stability,
indices <-  c(26,32,34,38,39,49,50,55)
ecoregion <- ecoregion[ecoregion$Index %in% indices,]

# make wide so we can join it to the feature class
ecoregion <- ecoregion %>% 
  select(Ecoregion,
         Value,
         CI_Lower,
         CI_Upper,
         Year,
         figure_title) 

# makes as factors, this may make a difference in the models
ecoregion$Ecoregion <-  as.factor(ecoregion$Ecoregion)
ecoregion$figure_title <- as.factor(ecoregion$figure_title)
ecoregion$Year <- as.factor(ecoregion$Year)
figure_titles <- unique(ecoregion$figure_title)
  
ecoregion_extrawide <- ecoregion %>% 
  filter(Ecoregion != "All BLM Range") %>% 
  mutate(figure_title_edit = ifelse(figure_title == figure_titles[1], "IIRH_NS_SM",
                               ifelse(figure_title == figure_titles[2],"Non_native_invasive_present",
                                      ifelse(figure_title == figure_titles[3],"Non_native_invasive_abundant",
                                             ifelse(figure_title == figure_titles[4],"Large_gaps_20_baresoil_50",
                                                    ifelse(figure_title == figure_titles[5],"Mean_bare_soil",
                                                           ifelse(figure_title == figure_titles[6],"SoilStability4",
                                                                  ifelse(figure_title == figure_titles[7],"Mean_sagebrush",
                                                                         ifelse(figure_title == figure_titles[8],"Mean_PG_PF",NA))))))))) %>% 
  select(-c(figure_title)) %>% 
  pivot_wider(names_from = c(figure_title_edit,Year), values_from = c(Value, CI_Lower, CI_Upper))

# # add sample sizes to this?
# sample_size_ecoregion <- read.csv("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\C drive back up\\2022\\Analysis\\LMF\\RPA\\SampleSizeTables\\SampleSizeByEcoregion_Year.csv")
# 
# sample_size_ecoregion <- sample_size_ecoregion %>% 
#   select(Ecoregion, Year, BLM.Sample)
# 
# ecoregion_merge <- merge(x = ecoregion_wide,
#                         y = sample_size_ecoregion,
#                         by = c("Ecoregion", "Year"))
# 
# write.csv(x = ecoregion_merge,
#           file = "C:\\Users\\alaurencetraynor\\Documents\\National Report\\ecoregion_wide.csv")

# read in FC and merge
# sf seems to be able toi deal with wgs84 better than AEA
ecoregion_fc <- arc.select(arc.open("C:\\Users\\alaurencetraynor\\Documents\\National Report\\National Report Poster GIS.gdb\\ecoregions_wgs84"))

ecoregion_fc_sf <- arc.data2sf(ecoregion_fc)

ecoregion_fc_merge <- sp::merge(x = ecoregion_fc_sf,
                            y = ecoregion_extrawide,
                            by.x = "NA_L2NAME",
                            by.y = "Ecoregion")
# these fields are chars for some reason
for(i in 3:230){
  ecoregion_fc_merge[[i]] <- as.numeric( ecoregion_fc_merge[[i]])
}

arc.write("C:\\Users\\alaurencetraynor\\Documents\\National Report\\National Report Poster GIS.gdb\\ecoregions_join_final2",
          ecoregion_fc_merge, overwrite = TRUE)

write.csv(x = ecoregion_extrawide,
          file = "C:\\Users\\alaurencetraynor\\Documents\\National Report\\ecoregion_extrawide.csv",
          row.names = FALSE)

# do the same for states
states <- read.csv('C:\\Users\\alaurencetraynor\\Documents\\National Report\\LMF_estimates_2011_2021_byState_for_figures_101222.csv')

# lets reduce the number of indicators 
# focus on acres
# invasives, gap and bare soil, soil stability,
indices <-  c(74,70,56,60,122,73,121,119)

states <- states[states$index %in% indices,]

# make wide so we can join it to the feature class
states <- states %>% 
  select(State,
         Estimate,
         CI_Lower,
         CI_Upper,
         Year,
         figure_title) 

# makes as factors, this may make a difference in the models
states$State <-  as.factor(states$State)
states$figure_title <- as.factor(states$figure_title)
states$Year <- as.factor(states$Year)
figure_titles <- unique(states$figure_title)

# states_wide <- states %>% 
#   filter(State != "All BLM Range") %>% 
#   rename(Value = Value2,
#          CI_Lower = CI_Lower2,
#          CI_Upper = CI_Upper2) %>% 
#   unique() %>% 
#   pivot_wider(names_from = figure_title, values_from = c(Value, CI_Lower, CI_Upper))

states_extrawide <- states %>% 
  filter(State != "All BLM Range") %>% 
  rename(Value = Estimate) %>% 
  unique() %>% 
  mutate(figure_title_edit = ifelse(figure_title == figure_titles[1], "Large_gaps_20_baresoil_50",
                                    ifelse(figure_title == figure_titles[2],"IIRH_NS_SM",
                                           ifelse(figure_title == figure_titles[3],"Non_native_invasive_abundant",
                                                  ifelse(figure_title == figure_titles[4],"Non_native_invasive_present",
                                                         ifelse(figure_title == figure_titles[5],"SoilStability4",
                                                                ifelse(figure_title == figure_titles[6],"Mean_bare_soil",
                                                                       ifelse(figure_title == figure_titles[7],"Mean_PG_PF",
                                                                              ifelse(figure_title == figure_titles[8],"Mean_sagebrush",NA))))))))) %>%
  select(-c(figure_title)) %>% 
  pivot_wider(names_from = c(figure_title_edit,Year), values_from = c(Value, CI_Lower, CI_Upper))


# sample_sizes_state <-  read.csv("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\C drive back up\\2022\\Analysis\\LMF\\RPA\\SampleSizeTables\\SampleSizeByStateType_Year.csv")
# 
# sample_sizes_state <- sample_sizes_state %>% 
#   unique() %>% 
#   group_by(State, Year) %>% 
#   summarise(Plots = sum(as.numeric(BLM.Sample)))
# 
# states_merge <- merge(x = states_wide,
#                          y = sample_sizes_state,
#                          by = c("State", "Year"))

write.csv(x = states_extrawide,
          file = "C:\\Users\\alaurencetraynor\\Documents\\National Report\\states_extrawide.csv",
          row.names = FALSE)

# read in FC and merge
states_fc <- arc.select(arc.open("C:\\Users\\alaurencetraynor\\Documents\\National Report\\National Report Poster GIS.gdb\\blm_states"))

states_fc_sf <- arc.data2sf(states_fc)

states_fc_merge <- sp::merge(x = states_fc_sf,
                                y = states_extrawide,
                                by.x = "STATE_ABBR",
                                by.y = "State")

# these fields are chars for some reason

# for(i in 8:241){
#   states_fc_merge[[i]] <- as.numeric( states_fc_merge[[i]])
# }

arc.write("C:\\Users\\alaurencetraynor\\Documents\\National Report\\National Report Poster GIS.gdb\\states_join_final",
          states_fc_merge)
