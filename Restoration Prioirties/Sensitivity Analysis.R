# Test out different variables and variable weights for use in the WCCT
# we could use a set of refernce and non-reference sites or sites previously listed as meeting land health standards 
# as a train and test dataset

# Can then potentially use random forest, mlr, or, glmm to select variables

# Setup
library(corrplot)
library(tidyverse)
library(GGally)

# set paths
output_path <- "C:\\Users\\alaurencetraynor\\Documents\\National Report\\Restoration priorities HQ"

# Import data
data <- read.csv(paste0(output_path, "/","benchmarked_points_9272024.csv"))

# test correlation of variables
# make correlation matrix
# pare data down
# may want to do this with both raw indicators values and benchmarked values

# start with benchmarked/departure scores
cor_data_bm <- data[,c("Native_score_dep",
                       "InvasiveAG_score_dep",
                       "Invasive_score_dep",
                       "SoilStabilityScore_dep",
                       "CanopyGapScore101_200_dep",
                       "CanopyGapScore200plus_dep",
                       "BareSoilScore_dep",
                       "HUC8")]
# lets first look at how these are distributed
ggplot(data = cor_data_bm[,1:7] %>% pivot_longer(cols = everything(),names_to = 'indicator',values_to = "value"),
       aes(x = value))+
  geom_histogram()+
  facet_wrap(.~indicator, scales = "free")+
  theme_bw()

# look at variance and covariance
var <- var(cor_data_bm[,1:7], na.rm = TRUE)

# default is Pearson's correlation coefficient
cov <- cov(cor_data_bm[,1:7], use = "complete.obs")
cor <- cor(cor_data_bm[,1:7], use = "complete.obs")

# using spearmans rank - this is probably most appropriate given the zero inflation
cov_sr <- cov(cor_data_bm[,1:7], use = "complete.obs", method = "spearman")
cor_sr <- cor(cor_data_bm[,1:7], use = "complete.obs", method = "spearman")

# lets plot these
corrplot(cor_sr, method = "ellipse", tl.pos = "l")
corrplot(cor_sr, method = "number")
corrplot(cor, method = "ellipse")# ellipse gives thin lines for high correlation and circles for smaller correlations
# perhaps unsurprisingly the invasive/native scores are highly correlated
# should probably only keep one of them, everything else looks generally OK
# invasive annual grass makes the most sense to me since those benchmarks are the most legit
# more exploratory plotting
ggplot(cor_data_bm, aes(x = Native_score_dep,
                        y = InvasiveAG_score_dep,
                        col = as.factor(HUC8)))+
  geom_point()+
  theme_bw()+
  theme(legend.position = "")# these look really odd, 'cos math.

ggplot(cor_data_bm, aes(x = Native_score_dep,
                        y = InvasiveAG_score_dep,
                        col = as.factor(HUC8)))+
  geom_point()+
  theme_bw()+
  theme(legend.position = "")

# what about indicator values
ggplot(data, aes(x = nativeFoliar,
                        y = invasiveFoliarAH,
                        col = as.factor(HUC8)))+
  geom_point()+
  theme_bw()+
  theme(legend.position = "")

# sticking to 3 at a time here for simplicity,
ggcor <- ggpairs(cor_data_bm[,1:3])+
  theme_minimal()

ggcor

ggcor2 <- ggpairs(cor_data_bm[,4:7])+
  theme_minimal()

ggcor2

# PCA
pca1 <-  prcomp(cor_data_bm[,1:7], center = TRUE, scale. = TRUE)
summary(pca1)
pca1$rotation

ggbiplot::ggbiplot(pca1, groups = pca_data$GrazTyp, ellipse = TRUE)
ggbiplot::ggbiplot(pca1, groups = pca_data$Obsr_LA, ellipse = TRUE)
ggbiplot::ggbiplot(pca1, groups = pca_data$Obsr_HW, ellipse = TRUE)
ggbiplot::ggbiplot(pca1, groups = pca_data$Obs_LPI, ellipse = TRUE)
ggbiplot::ggbiplot(pca1, groups = pca_data$Block, ellipse = TRUE)
ggbiplot::ggbiplot(pca1, groups = pca_data$Clb_typ, ellipse = TRUE)
ggbiplot::ggbiplot(pca1,choices = 3:4 )
ggbiplot::ggbiplot(pca1,choices = 5:6 )

# Looking at variable selection for models
# checking variance inflation factors
car::vif(lm())
# Load in primarykey for reference plots


# we can use those we determined reference for Malheur FO 

# any others?
