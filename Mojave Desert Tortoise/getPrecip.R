
CONUS <- st_read("~/Desktop","CONUS_NAD83", crs = '+proj=longlat +datum=NAD83 +no_defs')

terrestrial <- read.csv("~/Desktop/terrestrial.csv")

terrestrialS.sf <- st_as_sf(terrestrial, coords = c("Longitude_NAD83", "Latitude_NAD83"), 
                            crs = '+proj=longlat +datum=NAD83 +no_defs', remove="FALSE")

terrestrialS.sf <- terrestrial.sf[CONUS,]

terrestrialS <- as_tibble(terrestrialS.sf) %>%
  mutate(latitude = terrestrial$Latitude_NAD83[match(PrimaryKey,terrestrial$PrimaryKey)],
         longitude = terrestrial$Longitude_NAD83[match(PrimaryKey,terrestrial$PrimaryKey)])

terrestrialS <- terrestrialS[45001:50000,] %>%
  filter(!is.na(DateVisited)) %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude)) %>%
  mutate(endDate = as_date(DateVisited)-15,
         startDate = as_date(DateVisited)-105)

################################

  getPrecip <- function(points) {
    
    point <- tibble(PrimaryKey = points$PrimaryKey,
                     longitude = points$longitude,
                     latitude = points$latitude)

    start.date <- points$startDate
    start.date <- as_date(start.date)
    end.date <- points$endDate
    end.date <- as_date(end.date)


    point.sf <- st_as_sf(point, coords = c("longitude", "latitude"),
                         crs = '+proj=longlat +datum=NAD83 +no_defs')

    pointPrecip <- getGridMET(point.sf, param = "prcp",
                                      startDate = start.date,
                                      endDate = end.date)

    return(sum(pointPrecip$prcp))
  }
  
#######################################

prcp <-  apply(terrestrialS,1,getPrecip)
  
terrestrial50000b <- cbind(terrestrialS,precip15to105day = prcp)
  
write.csv(terrestrial50000b,"~/Desktop/terrestrial50000b.csv")  
  
  ############################
  
    spp <- read.csv("~/Desktop/gsenm_species.csv")
    
    spp[is.na(spp)]=0
    
    
    points <- spp %>%
      group_by(PrimaryKey) %>%
      summarise(points = sum(AH_SpeciesCover_n))
    
    cover <- spp %>%
      group_by(PrimaryKey) %>%
      summarise(cover = sum(AH_SpeciesCover) / 100)
    
    nPts <- left_join(points,cover,by="PrimaryKey") %>%
      mutate(nPts = points / cover) %>%
      select(-cover, -points)
    