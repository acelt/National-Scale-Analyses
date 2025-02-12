# Patrick Alexander, 9 Feb 2022
# This script uses my list of all introduced species in the western US and the 'SpeciesIndicators' table to calculate
# the proportion of the plants listed for each plot that are introduced, and the proportion of any hit foliar cover
# from introduced species. It then adds these values to the 'TerrADat' table as two new columns, 
# exoticProportion_speciesRichness and exoticProportion_foliarCover. Last, it creates a table summarizing the number
# of occurrences and foliar cover for each introduced species, using the taxa given in the introduced species table.

library(tidyverse)

# In my case, working from local csv copies... replace with your TerrADat / SpeciesIndicators import method of choice.

TerrADat <- read.csv("TerrADat_29Nov21/TerrADat.csv")

SpeciesIndicators <- read.csv("TerrADat_29Nov21/SpeciesIndicators.csv")

# "NA"s can cause trouble later... and for species found in species richness but not LPI, we know that they do have
# a cover value that is not 0. We don't know what that value is, but it is probably quite small, so I'm using 0.1.

SpeciesIndicators$AH_SpeciesCover[is.na(SpeciesIndicators$AH_SpeciesCover)] <- 0.1

exotics <- read.csv("westUS_exotics_PJA9Feb22.csv")

# Create an "introduced" field in SpeciesIndicators. I'm using the "namecode" field in 'exotics' so that it should
# match any name in SpeciesIndicators whether the accepted name in 'exotics' or not. There are a bunch of names in
# 'exotics' without codes, but these are mostly rare synonyms--they shouldn't be in SpeciesIndicators.

exoticCodes <- exotics %>% select(namecode) %>%
  filter(!is.na(namecode)) %>%
  mutate(Introduced = "TRUE")

SpeciesIndicators <- left_join(SpeciesIndicators,exoticCodes,by=c("Species" = "namecode"))

# Create tables with species counts for all plants, and for only introduced plants.

SIcount_all <- SpeciesIndicators %>%
  count(PrimaryKey)

SpeciesIndicators_exotic <- SpeciesIndicators %>%
  filter(Introduced == "TRUE")

SIcount_exotic <- SpeciesIndicators_exotic %>%
  count(PrimaryKey)

# Join them, calculate the proportion of species that are introduced, drop the count columns

SIcount <- left_join(SIcount_all,SIcount_exotic,by="PrimaryKey") %>%
  mutate(exoticProportion_speciesRichness = n.y / n.x) %>%
  select(-n.y, -n.x)

# Calculate sum of any hit foliar cover per plot, and sum of any hit foliar cover for only introduced species.

SItotalFoliar <- SpeciesIndicators %>%
  # filter(!is.na(AH_SpeciesCover)) %>%
  group_by(PrimaryKey) %>% 
  summarise(totalFoliar = sum(AH_SpeciesCover))

SIexoticFoliar <- SpeciesIndicators %>%
  filter(Introduced == "TRUE") %>%
  group_by(PrimaryKey) %>% 
  summarise(exoticFoliar = sum(AH_SpeciesCover))

# Join, calculate proportion of foliar cover from introduced species, and drop the foliar cover sum columns.

SIfoliar <- left_join(SIexoticFoliar,SItotalFoliar,by="PrimaryKey") %>%
  mutate(exoticProportion_foliarCover = exoticFoliar / totalFoliar) %>%
  select(-totalFoliar, -exoticFoliar)

# Join the species richness values to TerrADat. Replace NAs with 0s.

TerrADat <- left_join(TerrADat,SIcount,by="PrimaryKey")

TerrADat$exoticProportion_speciesRichness[is.na(TerrADat$exoticProportion_speciesRichness)] <- 0

# Join the foliar cover values to TerrADat. Replace NAs with 0s.

TerrADat <- left_join(TerrADat,SIfoliar,by="PrimaryKey")

TerrADat$exoticProportion_foliarCover[is.na(TerrADat$exoticProportion_foliarCover)] <- 0

# And I guess we'll write that to csv.

write.csv(TerrADat,"output/TerrADat_wIntroduced.csv")

# Now create a summary by introduced plant. First, convert the names in 'SpeciesIndicators' to the taxa in 
# the introduced species table. Remember not all taxa have codes! And, yes, that is something I'll want to fix.

exotics <- read.csv("westUS_exotics_PJA9Feb22.csv") %>%
  select(namecode,taxcode,taxon)

introducedSpeciesIndicators <- left_join(SpeciesIndicators,exotics,by=c("Species" = "namecode")) %>%
  filter(Introduced == "TRUE") %>%
  select(-Species)

# Now create sums of occurrences, averages of any hit foliar cover, and proportions of total foliar cover for
# introduced species. Re-use SItotalFoliar from above in the proportional calculations.

introducedSpeciesIndicators$AH_SpeciesCover[is.na(introducedSpeciesIndicators$AH_SpeciesCover)] <- 0.1

introducedCount <- introducedSpeciesIndicators %>% 
  count(taxon) %>%
  rename(nOccurrences = n)

introducedAverageCover <- introducedSpeciesIndicators %>%
  group_by(taxon) %>% 
  summarise(averageAHcover = mean(AH_SpeciesCover))

introducedProportionCover <- left_join(introducedSpeciesIndicators,SItotalFoliar,by="PrimaryKey") %>%
  mutate(prop_AHspeciesCover = AH_SpeciesCover / totalFoliar) %>%
  group_by(taxon) %>% 
  summarise(proportionAHcover = mean(prop_AHspeciesCover))

# Join those. Add taxon codes after filtering the 'exotics' table to unique taxon values.

introducedSummary <- left_join(introducedCount,introducedAverageCover,by="taxon")

introducedSummary <- left_join(introducedSummary,introducedProportionCover,by="taxon")

exotics <- exotics %>%
  distinct(taxon,.keep_all="TRUE")

introducedSummary <- left_join(introducedSummary,exotics,by="taxon") %>%
  select(taxon,taxcode,nOccurrences,averageAHcover,proportionAHcover)

# And let's write that table...

write.csv(introducedSummary,"output/introducedSummary.csv")