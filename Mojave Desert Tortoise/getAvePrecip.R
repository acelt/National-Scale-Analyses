library(tidyverse)
library(lubridate)
library(sf)
library(raster)

# Modify the date range (lines 33-36) and output names (end) as needed. As currently written (in presumably the
# clunkiest way possible), this only works with ±90 day intervals. And there may be cases in which it just doesn't
# work properly. Caveat emptor!

# Works with the terrestrial table pulled from the corporate server, at least as of Feb 2023, and the monthly 30-year
# normals rasters from PRISM. Using gridMET for actual precip & PRISM for average precip is not ideal, but for whatever
# reason I find that querying PRISM for the actuals takes about an order of magnitude longer to run... and gridMET
# doesn't have a raster with calculated normals. I'm also getting the average precip for a time interval by multiplying
# the normal total for a month by the proportion of that month that falls within the time interval. This implicitly
# assumes that per-day average precip is constant within a month, which is not the case... but hopefully close enough
# to the case as not to cause anything too odd. In the absence of daily normals, this seems to be the best solution.

CONUS <- st_read("~/Desktop","CONUS_NAD83", crs = '+proj=longlat +datum=NAD83 +no_defs')

terrestrial <- read.csv("~/Desktop/terrestrial.csv")

terrestrialS.sf <- st_as_sf(terrestrial, coords = c("Longitude_NAD83", "Latitude_NAD83"), 
                            crs = '+proj=longlat +datum=NAD83 +no_defs', remove="FALSE")

terrestrialS.sf <- terrestrialS.sf[CONUS,]

terrestrialS <- as_tibble(terrestrialS.sf) %>%
  mutate(latitude = terrestrial$Latitude_NAD83[match(PrimaryKey,terrestrial$PrimaryKey)],
         longitude = terrestrial$Longitude_NAD83[match(PrimaryKey,terrestrial$PrimaryKey)])

terrestrialS <- terrestrialS %>%
  filter(!is.na(DateVisited)) %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude)) %>%
  mutate(endDate = as_date(DateVisited)-15,
         midDate1 = as_date(DateVisited)-45,
         midDate2 = as_date(DateVisited)-75,
         startDate = as_date(DateVisited)-105) %>%
  mutate(endMonth = month(endDate),
         midMonth1 = month(midDate1),
         midMonth2 = month(midDate2),
         startMonth = month(startDate)) %>%
  mutate(midMonth1 = ifelse(endMonth==midMonth1,NA,midMonth1),
         midMonth2 = ifelse(startMonth==midMonth2,NA,midMonth2))

month1 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_01_bil.bil")
month2 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_02_bil.bil")
month3 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_03_bil.bil")
month4 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_04_bil.bil")
month5 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_05_bil.bil")
month6 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_06_bil.bil")
month7 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_07_bil.bil")
month8 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_08_bil.bil")
month9 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_09_bil.bil")
month10 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_10_bil.bil")
month11 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_11_bil.bil")
month12 <- raster("~/Desktop/PRISM_ppt_30yr_normal_4kmM4_all_bil/PRISM_ppt_30yr_normal_4kmM4_12_bil.bil")

stk <- stack(month1,month2,month3,month4,month5,month6,month7,month8,month9,month10,month11,month12)

terrestrialS.sf <- st_as_sf(terrestrialS, coords = c("longitude", "latitude"), 
                            crs = '+proj=longlat +datum=NAD83 +no_defs', remove="FALSE")

terrestrialS.sf$month1 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_01_bil,terrestrialS.sf)
terrestrialS.sf$month2 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_02_bil,terrestrialS.sf)
terrestrialS.sf$month3 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_03_bil,terrestrialS.sf)
terrestrialS.sf$month4 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_04_bil,terrestrialS.sf)
terrestrialS.sf$month5 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_05_bil,terrestrialS.sf)
terrestrialS.sf$month6 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_06_bil,terrestrialS.sf)
terrestrialS.sf$month7 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_07_bil,terrestrialS.sf)
terrestrialS.sf$month8 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_08_bil,terrestrialS.sf)
terrestrialS.sf$month9 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_09_bil,terrestrialS.sf)
terrestrialS.sf$month10 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_10_bil,terrestrialS.sf)
terrestrialS.sf$month11 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_11_bil,terrestrialS.sf)
terrestrialS.sf$month12 <- extract(stk$PRISM_ppt_30yr_normal_4kmM4_12_bil,terrestrialS.sf)

terrestrialS <- as_tibble(terrestrialS.sf) %>%
  mutate(latitude = terrestrial$Latitude_NAD83[match(PrimaryKey,terrestrial$PrimaryKey)],
         longitude = terrestrial$Longitude_NAD83[match(PrimaryKey,terrestrial$PrimaryKey)]) %>%
  dplyr::select(-geometry) # Not sure why, but having this column breaks things...

terrestrialS2 <- terrestrialS %>%
  mutate(month0 = "0",
         startM = paste("month",as.character(startMonth),sep=""),
         endM = paste("month",as.character(endMonth),sep=""),
         midM1 = ifelse(is.na(midMonth1),"month0",paste("month",as.character(midMonth1),sep="")),
         midM2 = ifelse(is.na(midMonth2),"month0",paste("month",as.character(midMonth2),sep="")))

terrestrialS2$startMp <- apply(terrestrialS2, 1, function(x) { x[names(x)==x[names(x)=="startM"]] })
terrestrialS2$midMp1 <- apply(terrestrialS2, 1, function(x) { x[names(x)==x[names(x)=="midM1"]] })
terrestrialS2$midMp2 <- apply(terrestrialS2, 1, function(x) { x[names(x)==x[names(x)=="midM2"]] })
terrestrialS2$endMp <- apply(terrestrialS2, 1, function(x) { x[names(x)==x[names(x)=="endM"]] })

terrestrialS2 <- terrestrialS2 %>%
  mutate(startMp = as.numeric(startMp),
         midMp1 = as.numeric(midMp1),
         midMp2 = as.numeric(midMp2),
         endMp = as.numeric(endMp)) %>%
  mutate(startMpercent = as.numeric(day(startDate) /days_in_month(startDate)),
         endMpercent = as.numeric(day(endDate) /days_in_month(endDate))) %>%
  mutate(startMp = startMp * startMpercent,
         endMp = endMp * endMpercent) %>%
  mutate(avePrecip90 = startMp + midMp1 + midMp2 + endMp)

avePrecip <- terrestrialS2 %>% dplyr::select(PrimaryKey,avePrecip90)
write.csv(avePrecip,"~/Desktop/terrestrialAvePrecip105.csv")

