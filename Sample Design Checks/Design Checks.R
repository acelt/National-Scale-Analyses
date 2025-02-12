### Setup
# documentation here: https://r.esri.com/r-bridge-site/
library(sf)
library(tidyverse)
library(spsurvey)
library(arcgis)
library(arcgisbinding) # R was crashing trying to use both of these packages
arc.check_product() # dont need this with arcgis
# can use this to check portal URL if needed
# arc.check_portal

# You'll first need to follow instructions here: https://r.esri.com/r-bridge-site/location-services/connecting-to-a-portal.html 
# to authorize the Arc portal (requires logging into AGOL on your browser)
token <- auth_code()
set_arc_token(token) # this token expires after a certain amount of time, 30 mins or so?

# can also authorise this way if youre signed into Pro and have loaded arcgisbinding:
# this has the advantage of not needing to click through steps on your web browser
token <- auth_binding()
set_arc_token(token)

########################################################################################################################################################################
### Another example
# Lets look at design implementation in the NV plot service
NV_plotservice_url <- "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_NV_Terrestrial_AIM_2024_Plot_Service/FeatureServer/0"

sdd_path <- "\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\AIMDataTools\\SDE\\AIMDev.sde\\ilmocAIMdev.ILMNATAIMDEV.DesignPoints"

# Import
NV_plotservice <- arc_open(NV_plotservice_url)

# Convert to data.frame for easy manipulation
NV_plotservice_df <- arc_select(NV_plotservice) %>% 
  st_drop_geometry()

# Pull from the SDD here to get proportions
# Might need a crosswalk for design names - Kaitlin has an initial crosswalk
sdd <- arc.open(sdd_path)
NV_sdd <- arc.select()

# Check for holes within each strata and proportionality 
# for the proportions lets look at the whole design and compare to the sampled points, per stratum
NV_plotservice_summary <- NV_plotservice_df %>% 
  group_by(DesignName, Stratum) %>% 
  count(EvalStatus) %>% 
  pivot_wider(names_from = EvalStatus, values_from = n) %>% 
  mutate(TotalPointsStratum = rowSums(across(where(is.numeric)), na.rm =TRUE),
         SampledProportion = Sampled/TotalPointsStratum) # may want to deal with NAs better here...

# theres probs a better way to do this but hey...
NV_plotservice_summary2 <- NV_plotservice_summary %>% 
  group_by(DesignName) %>% 
  summarise(TotalPointsDesign = sum(TotalPointsStratum)) %>% 
  inner_join(NV_plotservice_summary, by = "DesignName") %>% 
  mutate(DesignProportion = TotalPointsStratum/TotalPointsDesign) %>% 
  mutate(LookingGood = if_else(abs(DesignProportion-SampledProportion)>0.1,"No", "Yes")) # not really sure what a good threshold is here but 10% seems nice

# figure out what 30% of each stratum looks like
threshold <- NV_plotservice_summary2[,c("TotalPointsStratum","DesignName", "Stratum")]
threshold$ThirtyPercent <- threshold$TotalPointsStratum*0.3

# for assessing holes we need to look at each individual stratum, and look for consecutive un-evaluated points (these are labeled "base") and flag those holes are => 30% of that stratum's total points
# group by stratum, check for holes, inner join back to original df?
NV_plotservice_summary3 <- NV_plotservice_df %>% 
  group_by(DesignName, Stratum) %>% 
  filter(EvalStatus %in% c("Base","Reattempt needed")) %>% # filter to unevaluated
  mutate(outP = c(0, abs(diff(PlotOrder)) == 1)) %>%  # look for consecutive numbers in plot order
  inner_join(threshold, by = c("DesignName", "Stratum"))

# cumulative total that resets when 0 is encountered
NV_plotservice_summary3 <- transform(NV_plotservice_summary3, count = outP * ave(outP, c(0L, cumsum(diff(outP) != 0)), 
                                                                                 FUN = seq_along))

NV_plotservice_summary_holes <- mutate(NV_plotservice_summary3, HoleExists = if_else(count >= ThirtyPercent, "Yes", "No")) %>% 
  select(DesignName,
         PlotID,
         HoleExists) %>% 
  full_join(x = NV_plotservice_df[,c("DesignName","Stratum", "PlotID","PlotOrder","EvalStatus")],
            by = c("DesignName", "PlotID")) # add back all the plots

# fill in NAs
NV_plotservice_summary_holes$HoleExists[is.na(NV_plotservice_summary_holes$HoleExists)] <- "No"

# Publish both summary tables
publish_design_report <- publish_layer(NV_plotservice_summary2, "NV_plotservice_strata_proportion_report")

publish_design_report2 <- publish_layer(NV_plotservice_summary_holes,"NV_plotservice_strata_holes_report")

#######################################################################################################################################
### JANETS SCRIPT for appending to an existing FS ###

# https://r.esri.com/r-bridge-site/location-services/connecting-to-a-portal.html
# Authorize to portal
# Method for ArcGIS Pro users with arcgisbinding installed
token <- auth_binding()
set_arc_token(token)

####  Publish Layer #####
# Read in dataset with sf (skip if your script just created this)
dat1<- sf::st_read(dsn ="//blm/dfs/loc/EGIS/ProjectsNational/AIM/Lotic/Projects/DesignsWorking/2024/Designs2024.gdb", layer="OR_KlamathFallsFO_Revisit2024_forOfficeEvalMap")
# publish layer
res <- publish_layer(dat1, "JMTestOfficeEval")
res

#### Add feature ######
#https://r.esri.com/r-bridge-site/location-services/workflows/add-delete-update.html
dat2<- sf::st_read(dsn ="//blm/dfs/loc/EGIS/ProjectsNational/AIM/Lotic/Projects/DesignsWorking/2024/Designs2024.gdb", layer="OR_LakeviewFO_Revisit2024_forOfficeEvalMap")

# reference the layer to which we are adding a feature
# Note need to specify layer at end of URL ie.  "/0"
om_url<- "https://services1.arcgis.com/KbxwQRRfWyEYLgp4/arcgis/rest/services/JMTestOfficeEval/FeatureServer/0"
om <- arc_open(om_url)

add_res <- add_features(om, dat2)