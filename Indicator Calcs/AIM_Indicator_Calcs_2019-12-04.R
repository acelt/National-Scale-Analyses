#  Calculating terrestrial indicators with terradactyl
#  Adapt all file paths to your relevant folder structure
# Because it can take days (or weeks) to work all the way through this script, 
# I usually save a copy of this script specific to each state or set of calcs

#########

library(terradactyl)
library(arcgisbinding) # if not installed, download from github 

# Step 1: Identify file paths, inputs, outputs
# Set path to file geodatabase with data
dsn <- "~/AIM/Data/TerradatCalcs/NV/NV-TDandLMF.gdb11-25-19/NV-TDandLMF.gdb"

# Output geodatabase, can be built on the fly
dsn_out <- "~AIM/Data/NV_calcs_2019-11-25.gdb"

# Run Species Checks First #
# Folder to store tall data files, must be built first
dsn_tall <- "~/AIM/Data/TerradatCalcs/NV/"

# Set the state of interest
state <- "NV"

# Step Two: Gather data into tall formats 
# This may take a while ####
### Only run this if new data have also been added to aim.gdb ###
terradactyl::gather_all(dsn = dsn,
                        folder = dsn_tall)


# Step Three: Species checks ####
# the outputs are stored in the same folder as your species list
terradactyl::species_list_check(dsn_tall = dsn_tall,
                                species_list_file = dsn, # can sub dsn for file path to species list CSV
                                SpeciesState == state 
)

# Step Four: If Species Checks pass, calculate indicators ####
# You'll need to provide the path to each tall Rdata file, which is created in step 1
#Calculate LMF indicators:

lmf <- terradactyl::build_indicators(dsn = dsn,
                                     source = "LMF",
                                     lpi_tall = "~/AIM/Data/TerradatCalcs/NV/lpi_tall.Rdata",
                                     spp_inventory_tall = "~/AIM/Data/TerradatCalcs/NV/spp_inventory_tall.Rdata",
                                     gap_tall = "~/AIM/Data/TerradatCalcs/NV/gap_tall.Rdata",
                                     soil_stability_tall = "~/AIM/Data/TerradatCalcs/NV/soil_stability_tall.Rdata",
                                     height_tall = "~/AIM/Data/TerradatCalcs/NV/height_tall.Rdata",
                                     species_file = dsn,
                                     SpeciesState %in% state)

# write out, no need to update file paths
arcgisbinding::arc.check_product()
arcgisbinding::arc.write(data = lmf,
                         path = paste(dsn_out, "/LMF_", Sys.Date(), sep = "") %>%
                           gsub(pattern = "-", replacement = "_"),
                         coords = c( "Longitude_NAD83","Latitude_NAD83"),
                         shape_info=list(type='Point',
                                         hasZ=FALSE,
                                         WKID=4269 # code for NAD83
                         ),
                         overwrite = TRUE)


terradat <- terradactyl::build_indicators(dsn = dsn,
                                          source = "TerrADat",
                                          lpi_tall = "~/AIM/Data/TerradatCalcs/NV/lpi_tall.Rdata",
                                          spp_inventory_tall = "~/AIM/Data/TerradatCalcs/NV/spp_inventory_tall.Rdata",
                                          gap_tall = "~/AIM/Data/TerradatCalcs/NV/gap_tall.Rdata",
                                          soil_stability_tall = "~/AIM/Data/TerradatCalcs/NV/soil_stability_tall.Rdata",
                                          height_tall = "~/AIM/Data/TerradatCalcs/NV/height_tall.Rdata",
                                          species_file = dsn,
                                          SpeciesState %in% state)

# write out, no need to update file paths
arcgisbinding::arc.check_product()
arcgisbinding::arc.write(data = terradat,
                         path = paste(dsn_out, "/TerrADat_", Sys.Date(), sep = "") %>%
                           gsub(pattern = "-", replacement = "_"),
                         coords = c( "Longitude_NAD83","Latitude_NAD83"),
                         shape_info=list(type='Point',
                                         hasZ=FALSE,
                                         WKID=4269 # code for NAD83
                         ),
                         overwrite = TRUE)

# Calculate accumulated species
# Update the file paths to the relevant tall files

species <- accumulated_species( lpi_tall = "~/AIM/Data/lpi_tall.Rdata",
                                spp_inventory_tall = "~/AIM/Data/spp_inventory_tall.Rdata",
                                height_tall = "~/AIM/Data/height_tall.Rdata",
                                species_file = dsn,
                                header = "~/AIM/Data/header.Rdata",
                                SpeciesState %in% state)

# make spatial
terradat_species <- sf::st_read(dsn = dsn, layer = "tblPlots") %>%
  dplyr::left_join(species %>% subset(source == "AIM"), .) %>%
  dplyr::select(names(species), Longitude_NAD83 = Longitude, 
                Latitude_NAD83 = Latitude)

# write out, no need to update file paths
arcgisbinding::arc.check_product()
arcgisbinding::arc.write(data = terradat_species,
                         path = paste(dsn_out, "/TerrADat_SpeciesIndicators", Sys.Date(), sep = "") %>%
                           gsub(pattern = "-", replacement = "_"),
                         # coords = c( "Longitude_NAD83","Latitude_NAD83"),
                         shape_info=list(type='Point',
                                         hasZ=FALSE,
                                         WKID=4269 # code for NAD83
                         ),
                         overwrite = TRUE)
# make LMF spatial
lmf_species <- sf::st_read(dsn = dsn, layer = "POINTCOORDINATES") %>%
  dplyr::left_join(species %>% subset(source == "LMF"), .) %>%
  dplyr::select(names(species),
                Longitude_NAD83=FIELD_LONGITUDE,
                Latitude_NAD83=FIELD_LATITUDE)

# write out LMF species, no need to update file paths
arcgisbinding::arc.check_product()
arcgisbinding::arc.write(data = lmf_species,
                         path = paste(dsn_out, "/LMF_SpeciesIndicators", Sys.Date(), sep = "") %>%
                           gsub(pattern = "-", replacement = "_"),
                         coords = c( "Longitude_NAD83","Latitude_NAD83"),
                         shape_info=list(type='Point',
                                         hasZ=FALSE,
                                         WKID=4269 # code for NAD83
                         ),
                         overwrite = TRUE)

