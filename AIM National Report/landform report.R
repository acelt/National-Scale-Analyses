### Look at which Landform types have been commonly used in the past
#### Pull in terradat and tblPlots from SDE

odbc_connection <- "AIMPub"
sde <- RODBC::odbcConnect(odbc_connection)

# Extract terradat layer for metadata like date visited etc
terradat <- RODBC::sqlQuery(sde, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.TerrADat;')

# Extract tblPlots for landform
tbl_plots <- RODBC::sqlQuery(sde, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.tblPlots;')

head(tbl_plots)

# Mash together
terradat_tblplot <-  merge(terradat[,c("PlotKey", "DateVisited")],
                           tbl_plots,
                           by = "PlotKey")
# create year field 
terradat_tblplot$Year <- format(as.POSIXct(terradat_tblplot$DateVisited, format = "%Y-%m-%d %H:%M:%OS"),"%Y")

# Summarise
library(tidyverse)

landform_summary <- terradat_tblplot %>% 
  filter(!is.na(Year),
         !is.na(LandscapeType)) %>%
  group_by(Year, LandscapeType) %>%
  summarise(n = n()) %>%
  mutate(Proportion = n/sum(n))

landformsec_summary <- terradat_tblplot %>% 
  filter(!is.na(Year),
         !is.na(LandscapeTypeSecondary)) %>%
  group_by(Year, LandscapeTypeSecondary) %>%
  summarise(n = n()) %>%
  mutate(Proportion = n/sum(n))

landformsec_summary2 <- terradat_tblplot %>% 
  filter(!is.na(Year),
         !is.na(LandscapeTypeSecondary)) %>%
  group_by(Year, LandscapeType, LandscapeTypeSecondary) %>%
  summarise(n = n()) %>%
  ungroup()%>%
  mutate(Proportion = n/sum(n))

landformESD_summary <- terradat_tblplot %>% 
  filter(!is.na(Year),
         !is.na(ESD_MajorLandform)) %>%
  group_by(Year, ESD_MajorLandform) %>%
  summarise(n = n()) %>%
  mutate(Proportion = n/sum(n))

# Plot figures
# set palette
library("RColorBrewer")
nlt <- length(unique(landform_summary$LandscapeType))
nlts <- length(unique(landformsec_summary$LandscapeTypeSecondary))
nesd <- length(unique(landformESD_summary$ESD_MajorLandform))
  
my_pal_lt <- colorRampPalette(RColorBrewer::brewer.pal(n = 8,
                                   name = "Set2"))(nlt)
my_pal_lts <- colorRampPalette(RColorBrewer::brewer.pal(n = 8,
                                                       name = "Set3"))(nlts)
my_pal_esd <- colorRampPalette(RColorBrewer::brewer.pal(n = 8,
                                                       name = "Set1"))(nesd)

my_position <- position_dodge(width = 1)
# Landform type by year

ggplot(data = landform_summary, aes(x = Year , y = Proportion, fill = LandscapeType)) +
  geom_col(position = my_position) +
  facet_wrap(.~Year, scales = "free") +
  scale_fill_manual(values = my_pal_lt)  +
  #geom_text(label = landform_summary$n,
          #  position = my_position)+
  theme_minimal(base_size = 16)+
  theme(legend.position = "bottom")

# Landform Type Secondary by year
ggplot(data = landformsec_summary, aes(x = Year , y = Proportion, fill = LandscapeTypeSecondary)) +
  geom_col(position = my_position) +
  facet_wrap(.~Year, scales = "free") +
  scale_fill_manual(values = my_pal_lts) +
  #geom_text(label = landformsec_summary$n,
  #          position = my_position)+
  theme_minimal(base_size = 16)+
  theme(legend.position = "bottom")

# messing with ggnimate
install.packages("gganimate")
install.packages("gifski")
library("gifski")
library("gganimate")

# ESD Major landform by year
ggplot(data = landformESD_summary, aes(x = Year , y = Proportion, fill = ESD_MajorLandform)) +
  geom_col(position = my_position) +
  facet_wrap(.~Year, scales = "free")+
  scale_fill_manual(values = my_pal_esd) +
  theme_minimal(base_size = 16)+
  theme(legend.position = "bottom") +
  geom_text(label = landformESD_summary$n,
            position = my_position)

### Looking at soil horizon modifiers used in the past
soil_horizons <- RODBC::sqlQuery(sde, 'SELECT * FROM ilmocAIMPub.ILMOCAIMPUBDBO.tblSoilPitHorizons;')

length(unique(soil_horizons$ESD_HorizonModifier))
summary(as.factor(soil_horizons$ESD_HorizonModifier))
soil_table <- table(soil_horizons$ESD_HorizonModifier)

soil_summary <-  soil_horizons %>%
  filter(!is.na(ESD_HorizonModifier)) %>%
  group_by(ESD_HorizonModifier)%>%
  summarise(n = n())

# most common 
ggplot(soil_summary[soil_summary$n > 50,], aes(x = ESD_HorizonModifier, y = n))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
barplot(soil_table)

ggplot(soil_horizons, aes(x = ESD_HorizonModifier))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
