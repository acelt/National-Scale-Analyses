library(arcgis)

token <- auth_binding()
set_arc_token(token)

tdat <- arc_open(url = "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/2",
                 token =token)
tdat_df <- arc_select(tdat, where = "PrimaryKey IN ('CA_RDFO_Rancho_2023_FallowField-04_V12023-09-01','CA_RDFO_Rancho_2023_WalnutOrchard-02_V12023-09-01')")

soil_pit <- arc_open("https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/44",
                     token = token)
soil_pit_df <- arc_select(soil_pit, 
                          where = "PrimaryKey IN ('CA_RDFO_Rancho_2023_FallowField-04_V12023-09-01','CA_RDFO_Rancho_2023_WalnutOrchard-02_V12023-09-01')")

soil_pit_horizon <- arc_open("https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/43",
                             token = token)

soil_pit_horizon_df <- arc_select(soil_pit_horizon,
                                  where = "PrimaryKey IN ('CA_RDFO_Rancho_2023_FallowField-04_V12023-09-01','CA_RDFO_Rancho_2023_WalnutOrchard-02_V12023-09-01')")

soil_all <- merge(x = soil_pit_df,
                  y = soil_pit_horizon_df,
                  by = "SoilKey")
write.csv(soil_all, file = "C:\\tmp\\soil_MY.csv")
