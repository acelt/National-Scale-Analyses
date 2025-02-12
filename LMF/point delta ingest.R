point_delta_ingest <- function(points, gdb, txt_file){
  
  require(arcgisbinding)
  arc.check_product()
  require(tidyverse)
  require(sf)
  
  points_sf <- st_read(dsn = gdb,
                       points)
  
  txt_file_df <- read.delim(txt_file, sep = "|",header = FALSE)
  
  # add column names
  headers <- c("SURVEY","STATE","COUNTY", "PSU", "POINT", "DELTA", "POINT_ID")
  
  colnames(txt_file_df) <- headers
  
  # define delta values
  # 1. 1;jk = 1, point is on BLM-managed rangelands and observed (eligible and observed for
  # 2020 BLM Rangeland Survey); SAMPLED
  # 2. 2;jk = 1, point is on BLM-managed rangelands but not observed; IA
  #3. 3;jk = 1, point is on BLM-managed land but is not on rangelands; NT
  # 4.4;jk = 1, point is not on BLM-managed land; and NT
  # 5.5;jk = 1, point has no eligibility identication. UNKNOWN
  
  txt_file_df <- txt_file_df %>% 
    mutate(FinalDesignation = ifelse(DELTA == 1,
                                     "Target Sampled",
                                     ifelse(DELTA == 2,
                                            "Inaccessible",
                                            ifelse(DELTA == 3| DELTA == 4,
                                                   "Non Target",
                                                   "Unknown"))),
           POINT_ID = paste0(str_pad(STATE,2,pad ="0"), str_pad(COUNTY,3, pad = "0"), "_",PSU, POINT),
           DateEvaluated = as.POSIXct(paste0(SURVEY, "-01-01")),
           PlotKey = paste0(SURVEY,str_pad(STATE,2,pad ="0"),str_pad(COUNTY,3, pad = "0"),PSU, POINT))
    
  # NEED PRECEEDING 00
  # build point id to join to points
  # also need point evaluated date
  # i think we only know year
  
  # append to points
  points_txt_merge <- merge(points_sf,
                            txt_file_df[,c("DELTA","FinalDesignation","PlotKey","DateEvaluated", "POINT_ID", "SURVEY")],
                            by = "POINT_ID",
                            all.x = TRUE,
                            all.y = FALSE)
  
  # lets match these fields with the SDE SDD
  lmf_nonsampled <- points_txt_merge %>% 
    mutate(PlotID = PlotKey,
           Comments = "Random",
           VisitNumber = "1",
           SampledStrata = "NA",
           DesignPolygonID = "LMF",
           NotSampledReason = "NA",
           NotSampledNotes = "NA",
           DesignPointKey = paste0("LMF_",PlotKey),
           ADMIN_ST = "NA") 
  
  lmf_samplepoints <- lmf_nonsampled %>% 
    mutate(DesignName = "LMF",
           PointDraw = "BASE",
           Panel = SURVEY,
           InitialPointWeight = NA,
           SampleDesignAlgorithm = "Two-stage Random",
           Comment = "NA",
           WithinStratumOrder = NA) %>% 
    select(PlotID,DesignPolygonID,ADMIN_ST,DesignName,PointDraw,Panel,InitialPointWeight, SampleDesignAlgorithm, Comments, WithinStratumOrder, DesignPointKey)
  
  # export for feature
  lmf_nonsampled <- lmf_nonsampled[lmf_nonsampled$FinalDesignation != "Target Sampled",] %>%
    select(PlotID,PlotKey,FinalDesignation,DateEvaluated,Comments, VisitNumber, SampledStrata,DesignPolygonID, NotSampledReason, NotSampledNotes,
           DesignPointKey, ADMIN_ST)
  
    # write
  arc.write(path = paste0(gdb,"/", "NotSampled_LMF") ,
            data = lmf_nonsampled,
            overwrite = TRUE)
  
  # write
  arc.write(path = paste0(gdb,"/", "SamplePoints_LMF") ,
            data = lmf_samplepoints,
            overwrite = TRUE)
}

gdb <- "\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Projects\\National\\Landscape_Monitoring_Framework\\Sample_Design\\Sample Design Database\\LMF Office Screening.gdb"
points = "Screening_20142022"
txt_file <-"\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Projects\\National\\Landscape_Monitoring_Framework\\Sample_Design\\pointdelta2011_2022.txt"

# run
point_delta_ingest(points, gdb, txt_file)
