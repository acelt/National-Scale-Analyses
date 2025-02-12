library(tidyverse)
library(arcgisbinding)

arc.check_product()

# Pull in Terradat and LMF
sde_dir <- "\\blm\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\AIMDataTools\\SDE\\AIMPub.sde"
terradat_fc <- "ilmocAIMPub.ILMOCAIMPUBDBO.TerrADat"
terra_sde <- paste0(sde_dir, terradat_fc, sep = "/")
terradat <- arc.open(terra_sde)

                     