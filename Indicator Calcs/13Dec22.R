library(tidyverse)
library(readxl)

# Read nominaTaxa. Yes, I know there isn't a 31st of November. This file is at polyploid.net/nominaTaxa

nominaTaxa <- readxl::read_xlsx("nominaTaxa_31Nov22.xlsx",sheet="nominaTaxa",col_types="text")

# Convert all the blanks in "exotic" to "NATIVE". And "TRUE" as a column name does funny things later, so let's change 
# that, too...

nominaTaxa <- nominaTaxa %>%
  mutate(exotic = ifelse(is.na(exotic),"NATIVE",exotic)) %>%
  mutate(exotic = ifelse(exotic=="TRUE","EXOTIC",exotic),
         invasive = ifelse(invasive == "TRUE","INVASIVE",invasive))

# Starting with a bare bones tall LPI file generated from RODBC as follows... (done on a separate computer in my case, hence
# writing the file then reading it in...)
# 
# tblLPI <- left_join(tblLPIheader,tblLPIdetail,by="RecKey")
# 
# tblLPI <- tblLPI %>% select(PrimaryKey.x,LineKey,PointNbr,TopCanopy,Lower1,Lower2,Lower3,Lower4,SoilSurface) %>%
#   rename(PrimaryKey = PrimaryKey.x)
# 
# tblLPI <- tblLPI %>% pivot_longer(c("TopCanopy","Lower1","Lower2","Lower3","Lower4","SoilSurface"),names_to="layer",values_to="code") %>%
#   filter(!(code==""))
# 
# write.csv(tblLPI,"tblLPI.csv")

# Read in terradat & tall LPI file.

tblLPI <- read.csv("~/Desktop/tblLPI.csv")
terradat <- read.csv("~/Desktop/terradat.csv")

# Add SpeciesState from terradat, for state-specific noxious lists.

tblLPI <- tblLPI %>%
  mutate(speciesState = terradat$SpeciesState[match(PrimaryKey,terradat$PrimaryKey)])

tblLPI_att <- tblLPI %>%
  mutate(invasive = nominaTaxa$invasive[match(code,nominaTaxa$namCode)],
         exotic = nominaTaxa$exotic[match(code,nominaTaxa$namCode)],
         noxious_list = nominaTaxa$noxious[match(code,nominaTaxa$namCode)]) %>%
  mutate(noxious = ifelse(str_detect(noxious_list,speciesState),"NOXIOUS",NA))

# Using pct_cover() from terradactyl... we'll do the separate indicators one at a time. If it matters, my copy of the terradactyl
# scripts is from June 2020. I suppose I should download a newer copy, but if it changed I might have to, like, learn something.

# In this context, by the way, I am making no attempt whatsoever to correct or otherwise modify the codes. Since only the 
# native / invasive / noxious attributes are of interest at the moment, worrying about the codes beyond their utility in 
# assigning those attributes seems superfluous.

invasive <- pct_cover(lpi_tall = tblLPI_att,
                  tall = FALSE,
                  hit = "any",
                  by_line = FALSE,
                  invasive)

native <- pct_cover(lpi_tall = tblLPI_att,
                    tall = FALSE,
                    hit = "any",
                    by_line = FALSE,
                    exotic) %>%
  select(-ABSENT) # I suppose you could check if the codes for plants not known to occur in the US are showing up in the data,
                  # but the odds seem to be low--and if these codes did appear, would we trust them to mean something?

noxious <- pct_cover(lpi_tall = tblLPI_att,
                      tall = FALSE,
                      hit = "any",
                      by_line = FALSE,
                      noxious)

# Add those to terradat. I notice, by the way, that terradat has 30,295 rows, the outputs from pct_cover() have 29,305 rows.
# Are there 1000-ish plots that have shown up in terradat but not in tblLPIdetail / tblLPIheader? Alas, at present I do not
# have time to investigate.

terradat <- terradat %>%
  mutate(nativeFoliarAH = native$NATIVE[match(PrimaryKey,native$PrimaryKey)],
         exoticFoliarAH = native$EXOTIC[match(PrimaryKey,native$PrimaryKey)],
         invasiveFoliarAH = invasive$INVASIVE[match(PrimaryKey,native$PrimaryKey)],
         noxiousFoliarAH = noxious$NOXIOUS[match(PrimaryKey,native$PrimaryKey)])

write.csv(terradat,"TerrADat_attributed_PJA9Dec22.csv")

# And the same for LMF...
# 
# lmfLPI <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.PINTERCEPT;')
# 
# lmfLPI <- lmfLPI %>% select(PrimaryKey,TRANSECT,MARK,HIT1,HIT2,HIT3,HIT4,HIT5,HIT6,BASAL,NONSOIL) %>%
#   rename(LineKey = TRANSECT,
#          PointNbr = MARK,
#          TopCanopy = HIT1,
#          Lower1 = HIT2,
#          Lower2 = HIT3,
#          Lower3 = HIT4,
#          Lower4 = HIT5,
#          Lower5 = HIT6,
#          Lower6 = NONSOIL,
#          SoilSurface = BASAL)
# 
# lmfLPI <- lmfLPI %>% pivot_longer(c("TopCanopy","Lower1","Lower2","Lower3","Lower4","Lower5","Lower6","SoilSurface"),names_to="layer",values_to="code") %>%
#   filter(!(code==""))
# 
# write.csv(lmfLPI,"lmfLPI.csv")

# Read in LMF & tall LPI file.

lmfLPI <- read.csv("~/Desktop/lmfLPI.csv")
lmf <- read.csv("~/Desktop/lmf.csv")

# Add SpeciesState from LMF, for state-specific noxious lists.

lmfLPI <- lmfLPI %>%
  mutate(speciesState = as.character(lmf$SpeciesState[match(PrimaryKey,lmf$PrimaryKey)]),
         code = ifelse(layer=="SoilSurface",ifelse(code=="None","S",code),code)) # Many entries in 
              # LMF's PINTERCEPT have "None" as a soil surface ("BASAL") code. We need to convert
              # these to a valid code, or else pct_cover() will drop them before calculating the
              # number of LPI pts. Converting all to "S" is a very bad idea in general. In this
              # context, we only want the foliar cover values, so it won't break anything.

lmfLPI_att <- lmfLPI %>%
  mutate(invasive = nominaTaxa$invasive[match(code,nominaTaxa$namCode)],
         exotic = nominaTaxa$exotic[match(code,nominaTaxa$namCode)],
         noxious_list = as.character(nominaTaxa$noxious[match(code,nominaTaxa$namCode)])) %>%
  mutate(noxious = ifelse(str_detect(noxious_list,speciesState),"NOXIOUS",NA))

# Using pct_cover() from terradactyl... we'll do the separate indicators one at a time. If it matters, my copy of the terradactyl
# scripts is from June 2020. I suppose I should download a newer copy, but if it changed I might have to, like, learn something.

# In this context, by the way, I am making no attempt whatsoever to correct or otherwise modify the codes. Since only the 
# native / invasive / noxious attributes are of interest at the moment, worrying about the codes beyond their utility in 
# assigning those attributes seems superfluous.

invasive <- pct_cover(lpi_tall = lmfLPI_att,
                      tall = FALSE,
                      hit = "any",
                      by_line = FALSE,
                      invasive)

native <- pct_cover(lpi_tall = lmfLPI_att,
                    tall = FALSE,
                    hit = "any",
                    by_line = FALSE,
                    exotic) %>%
  select(-ABSENT) # I suppose you could check if the codes for plants not known to occur in the US are showing up in the data,
# but the odds seem to be low--and if these codes did appear, would we trust them to mean something?

noxious <- pct_cover(lpi_tall = lmfLPI_att,
                     tall = FALSE,
                     hit = "any",
                     by_line = FALSE,
                     noxious)

# Add those to the LMF table.

lmf <- lmf %>%
  mutate(nativeFoliarAH = native$NATIVE[match(PrimaryKey,native$PrimaryKey)],
         exoticFoliarAH = native$EXOTIC[match(PrimaryKey,native$PrimaryKey)],
         invasiveFoliarAH = invasive$INVASIVE[match(PrimaryKey,native$PrimaryKey)],
         noxiousFoliarAH = noxious$NOXIOUS[match(PrimaryKey,native$PrimaryKey)])

write.csv(lmf,"LMF_attributed_PJA9Dec22.csv")

# Joining the two...

lmf <- lmf %>% mutate(
  DateLoadedInDb = as.character(DateLoadedInDb),
  DBKey = as.character(DBKey))

terr <- bind_rows(terradat,lmf)

write.csv(terr,"terrestrial_attributed_PJA9Dec22.csv")

