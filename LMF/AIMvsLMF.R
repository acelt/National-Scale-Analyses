# Setup
library(tidyverse)
library(betareg)
# import data from Mike
# Heres some backgroun
# "Just as a top-level summary: intersected AIM and LMF plots with ESG raster. 
# Filtered out plots that were burned or had veg treatments. 
# Binned plots into final ESGs based on the field-determined ESD, cross-referenced to an ESD/ESG crosswalk table produced by Travis Nauman. 
# Unfortunately not all of our Colorado Plateau ESDs are covered in the table, 
# so this takes the sample from ~ 7000 AIM and LMF plots down to ~ 3400"
#"ESG" field is the raster intersected ESG. "ESG_ID" is from the field-determined ESD/ESG crosswalk. "Dataset" is AIM or LMF
base_path <-  "C:\\Users\\alaurencetraynor\\Documents\\National\\"
csv <- paste0(base_path,"ESG_testplots 1.csv")
data <- read.csv(csv)

# check for normality and transform if needed
set.seed(0)
norm_data <- data.frame(x = rnorm(n=2000, mean = mean(data$BareSoilCover), sd = sd(data$BareSoilCover)))

ggplot(data = data, aes(x=BareSoilCover, fill = Dataset))+
  geom_density(alpha = 0.6)+
  geom_density(data = norm_data, aes(x = x), fill = "lightgray", alpha = 0.4)+
  theme_bw()# compare this to normal distribution

lm <- lm(data$BareSoilCover ~ data$Dataset)
summary(lm)
#plot(lm) # qq plot is surprizingly normal except for being capped by 0

# shapiro-wilk
# split by dataset
x <- data$BareSoilCover[data$Dataset == "AIM"]

y <- data$BareSoilCover[data$Dataset == "LMF"]
shapiro.test(x) # The p-value is less than .05, which indicates that the data is NOT normally distributed
shapiro.test(y) # not particularly normal
# and for context
shapiro.test(norm_data$x)

# so we need a transformation here to make a linear model
# classic transformation for proportional data is arcsine
data$BareSoilCover_trans <- asin(sqrt(data$BareSoilCover/100))
x_t <- data$BareSoilCover_trans[data$Dataset == "AIM"]
y_t <- data$BareSoilCover_trans[data$Dataset == "LMF"]
shapiro.test(x_t) # The p-value is less than .05, which indicates that the data is NOT normally distributed
shapiro.test(y_t) # this is better

# K-S test to compare AIM and LMF

ggplot(data = data, aes(x=BareSoilCover_trans, fill = Dataset))+
  geom_density(alpha = 0.6)+
  geom_density(data = asin(sqrt(norm_data/100)), aes(x = x), fill = "lightgray", alpha = 0.4)+
  theme_bw()+
  coord_cartesian(xlim = c(0,2.5))

# AIM data is still skewed
# will be best to use a glm or beta regression here to account for this
glm <- glm(data$BareSoilCover_trans ~ data$Dataset + data$ESG_ID)
summary(glm)
#plot(glm) 

# LETS PLOT THIS
ggplot(data, aes(y = BareSoilCover, x = Dataset, fill = Dataset))+
  geom_boxplot()+
  facet_wrap(.~ESG_ID)+
  theme_bw()

anova(glm)

# going the beta regression route
data$BareSoilCover_01 <- data$BareSoilCover/100
b_reg <- betareg::betareg(formula = BareSoilCover_01 ~ Dataset + ESG_ID,
                          data = data,
                          link = "logit")

summary(b_reg)

# may want to do some exploratory analyses/multivariate variance decomposition to explore reasons for this difference
# Potential explanatory variables = tree cover, elevation, slope, foliar cover, precip, time of year, time since last precip
# much of these are already in the dataset except for precip and maybe elevation 

# first lets grab the spatial data from arc
library(arcgis)
library(arcgisbinding)
arc.check_product()
library(elevatr)
token <- auth_binding()
set_arc_token(token)

# Import terrestrial spatial data
terradat <- arc_open(url = "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/FeatureServer/0",
                     token = token)

unique(data$State)
# NEED TO RUN THIS MULTIPLE TIMES SINCE ITS MAXING OUT
terradat_df <- arc_select(x = terradat, where = "State IN ('CO', 'AZ')")
terradat_df_PART2 <- arc_select(x = terradat, where = "State IN ('NM')")
terradat_df_PART3 <- arc_select(x = terradat, where = "State IN ('UT')")
terradat_df_PART4 <- arc_select(x = terradat, where = "State IN ('WY')")

terradat_df <- rbind(terradat_df, terradat_df_PART2, terradat_df_PART3, terradat_df_PART4)
terradat_df_filtered <- terradat_df[terradat_df$PrimaryKey %in% data$PrimaryKey,]

elevation_points <-  get_elev_point(terradat_df_filtered)

# in case R crashes again..
write.csv(elevation_points, "C:\\Users\\alaurencetraynor\\Documents\\National\\Elevation_points.csv")
# also get daymnet data and extract to points
library(daymetr)
library(sf)

xy <- st_coordinates(elevation_points)
elevation_points <- cbind(elevation_points, xy)

max_year <- max(format(elevation_points$DateVisited, "%Y"))
min_year <- min(format(elevation_points$DateVisited, "%Y"))

# Loop to grab data for all AIM plots
get_daymet <- function(i){
  
  temp_lat <- elevation_points[i, ] %>% pull(Y)
  temp_lon <- elevation_points[i, ] %>% pull(X)
  temp_site <- elevation_points[i, ] %>% pull(PrimaryKey)
  min_year <-  elevation_points[i,"DateVisited"]
  min_year <- format(min_year$DateVisited, "%Y")
  max_year <- min_year
  
  temp_daymet <- download_daymet(
    lat = temp_lat,
    lon = temp_lon,
    start = min_year,
    end = max_year
  ) %>% 
    #--- just get the data part ---#
    .$data %>% 
    #--- convert to tibble (not strictly necessary) ---#
    as_tibble() %>% 
    #--- assign site_id so you know which record is for which site_id ---#
    mutate(PrimaryKey = temp_site) %>% 
    #--- get date from day of the year ---#
    mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
  
  return(temp_daymet)
}  

daymet_all_points <- lapply(1:nrow(elevation_points), get_daymet) %>% 
    #--- need to combine the list of data.frames into a single data.frame ---#
    bind_rows()

# save this too
write.csv(daymet_all_points, paste0("C:\\Users\\alaurencetraynor\\Documents\\National\\daymet_points_", Sys.Date(), ".csv"))

# this is huge -  add date/year visted to subset to data from teh uyear of sampling
daymet_all_points <- daymet_all_points %>% 
  left_join(y = elevation_points[,c("PrimaryKey", "DateVisited")], by =PrimaryKey) %>% 
  mutate(YearVisited = format(DateVisited, "%Y")) %>% 
  filter(YearVisited == year)

# calc time since peak precip based on date visited and daymet maxmimum

# get temp data too?


