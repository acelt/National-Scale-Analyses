#### AIM data access using arcgisbinding package example script ####

### For this script to run you will need:
# 1) To be connected to the BLM internal VPN to access AIM data
# 2) To be running a 64 bit version of R (the script was tested using [64 bit] R 4.0.3)

#### More information on the RODBC package here: https://cran.r-project.org/web/packages/RODBC/RODBC.pdf

### BEFORE RUNNING THIS SCIRPT YOU NEED TO HAVE SET UP AN ODBC CONNECTION TO THE AIM SDE

# First go into your microsoft search bar and search for ODBC Data Source Adminstrator 32 bit
  # Add a User DSN by clicking add
  # select SQL Server
  # Name is "AIMPub" (this must match the dsn in the script below)
  # Description is "ilmocAIMLoticPub" for lotic and "ilmocAIMTerrestrialPub" for terrestrial
  # Server is egdbAIMPub.blm.doi.net\SdeSqlAim
  # Then click OK to test your connection

### For more details and screenshots see this word document: "How to Connect to SQL in R directly.doc"

# For more infomation on how to build SQL queries: 

install.packages("RODBC")
library(RODBC)

#Create connection
conn <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)

# View the available tables
tables <- RODBC::sqlTables(conn)
View(tables)

#Add combined terrestrial (terradat+lmf) layer
terrestrial <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrestrialIndicators;') # The '*' here means retrieve everything from this table

# Add all of Terradat
terradat <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrADat_evw;') # the _evw suffix is the versioned view of the table and will contain the most current version
# Add LMF
lmf <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.LMF_evw;')
# Add Terradat species indicators
terradat_species <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrADatSpeciesIndicators_evw;')
# Add LMF species indicators
lmf_species <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.LMFSpeciesIndicators_evw;')
# Can also add all related tables e.g., the state species lists
state_species_list <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.tblStateSpecies_evw;')
# Add Lotic indicators
lotic <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMLoticPub.ILMOCAIMPUBDBO.I_Indicators_evw;')

# Close the connection
RODBC::odbcCloseAll()
