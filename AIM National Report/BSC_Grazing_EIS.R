library(RODBC)
library(tidyverse)
#devtools::install_github('Landscape-Data-Commons/terradactyl')
library(terradactyl)
library(sf)
library(arcgisbinding)

arcgisbinding::arc.check_product()

con1 <-  odbcConnect("AIMPub", rows_at_time = 1)

## Look at AIM SDE structure
odbcGetInfo(con1)
tables <- sqlTables(con1)

# Load all AIM/LMF indicators in case we need other indicators and metadata
#terradat <- sqlQuery(con1, "SELECT * FROM ilmocAIMTerrestrialPub.ilmocAIMPubDBO.TerrestrialIndicators;")

# Grabbing spatial data instead 
terra_sde_path <- "\\\\blm\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\AIMDataTools\\SDE\\AIMTerrestrialPub.sde"

terradat_path <- paste0(terra_sde_path, "\\ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrADat")
lmf_path <- paste0(terra_sde_path, "\\ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.LMF")

# Import terrestrial data
terradat <- arcgisbinding::arc.open(path = terradat_path)
lmf <- arcgisbinding::arc.open(path = lmf_path)

terradat_df <- arcgisbinding::arc.select(object = terradat)
lmf_df <- arcgisbinding::arc.select(object = lmf)

# grab LPI detail from terradat
terradat_lpi <- RODBC::sqlQuery(con1, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIDETAIL;')

# grab lpi header info - terradactyl function wants this
tbllpiheader <- RODBC::sqlQuery(con1, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIHEADER;')

tbllpiheader <- tbllpiheader[,c("LineKey", "RecKey")]

# Make tall
tdat_tall <- tidyr::pivot_longer(data = terradat_lpi,
                                 cols = c(TopCanopy:SoilSurface),
                                 names_to = "layer",
                                 values_to = "code")

tdat_tall <- merge(x = tdat_tall,
                   y = tbllpiheader,
                   by = "RecKey",
                   all.x = T,
                   all.y = F)

tdat_tall <- tdat_tall[,c("PrimaryKey", "layer", "code", "PointNbr", "LineKey")]

tdat_tall$bsc <- "NotBSC"
tdat_tall[tdat_tall$code %in% c("M", "LC","CY","VL"), "bsc"] <- "BSC" # do these make sense? including VL for now

aim_cover_bsc <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        bsc)

# grab LPI detail from lmf
lmf_lpi <- RODBC::sqlQuery(con1, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.PINTERCEPT;')

lmf_tall <- terradactyl::gather_lpi_lmf(PINTERCEPT = lmf_lpi)

lmf_tall$bsc <- "NotBSC"
lmf_tall[lmf_tall$code %in% c("M", "LC","CY", "VL"), "bsc"] <- "BSC" # do these make sense? including VL for now

lmf_cover_bsc <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        bsc)

aim_lmf_bsc <- rbind(lmf_cover_bsc, aim_cover_bsc)

# Adding indicator for just presence of BSC
aim_lmf_bsc$BSC_present <- "FALSE"
aim_lmf_bsc[aim_lmf_bsc$BSC>0,"BSC_present"] <- "TRUE"

# merge with other indicators
# merge is loosing spatial data
# need to convert to sp object first

terradat_spdf <- arc.data2sp(terradat_df)

terradat_bsc <- sp::merge(x = terradat_spdf,
                      y = aim_lmf_bsc[,c("PrimaryKey", "BSC","BSC_present")],
                      all.x = FALSE,
                      all.y = TRUE)

shape_info <- list(type="Point", WKT=arc.shapeinfo(terradat)$WKT)
# this is still giving faulty geometry...
arc.write(path = 'C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\Grazing EIS\\GrazingEIS.gdb\\terradat_bsc',
          data = terradat_bsc,
          shape_info=shape_info,
          overwrite = TRUE)

write.csv(terradat_bsc, "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\Grazing EIS\\bsc_cover.csv")

##############################################################################################################
# Plot




###################### CHECK LPI ##########################
# Plots present in lpi calc but not terrestrial indicators
missing_pk <- terradat_lpi[terradat_lpi$PrimaryKey %notin% terradat$PrimaryKey,]

length(unique(missing_pk$PrimaryKey)) # 136

# Plots present in terrestrial indicators but missing from lpi calc
more_missing_pk <- terradat[terradat$PrimaryKey %notin% terradat_lpi$PrimaryKey,]

length(unique(more_missing_pk$PrimaryKey)) #3,784

write.csv(pks, paste0(out_dir, "\\missing_pks_LPI.csv"))

# Unique plots in lmf lpi raw data
length(unique(lmf_lpi$PrimaryKey)) #13,917

# Unique plots in aim lpi raw data
length(unique(terradat_lpi$PrimaryKey)) # 23,553

23553+13917 # 37,470

# Maybe repeats in terradat?
length(unique(terradat$PrimaryKey)) #41,117

odbcCloseAll()
