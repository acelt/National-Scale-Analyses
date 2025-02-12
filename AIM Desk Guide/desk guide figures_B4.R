#### Setup
library(tidyr)
library(ggplot2)
library(gridExtra)
library(viridis)
library(sf)
library(spatstat)
library(spsurvey)
library(ggrepel)

#### Figure B-4
### This is a figure showing the 5 steps for setting benchmarks from AIM data. Minimum font size should be 12 and we should avoid using red, green, or yellow colors

## Step 1: Identify available data – in this case we selected all AIM points within out analysis area and the within the broad geographic region​
# Create point and polygon data
set.seed(1)
polygon =
  # The syntax for creating a polygon with sf is a little strange.
  # It has to be a list of matrices and the first point has to be 
  # repeated as the last point (2, 1).
  list(
    matrix(
      c(10,10,10,0,0,0,0,10,10,10),
      ncol=2, byrow=T
    )
  ) 

# Create an sf polygon
polygon = st_sfc(st_polygon(polygon))

# Sample 10 random points within the polygon
points = st_as_sf(st_sample(st_buffer(polygon, -1), 15))
# add id numbers
points$id <- 1:15
# add type for color
points$type <- c(rep("Type 1", 9), rep("Type 2", 6))

points$remove <- c(rep("TRUE", 5), rep("FALSE", 10))

# Create second poly, one inside the other
aoi =
  # The syntax for creating a polygon with sf is a little strange.
  # It has to be a list of matrices and the first point has to be 
  # repeated as the last point (2, 1).
  list(
    matrix(
      c(8,7,8,5,6,5,6,7,8,7),
      ncol=2, byrow=T
    )
  ) 
aoi = st_sfc(st_polygon(aoi))

# Plot
base <- ggplot() + 
  geom_sf(aes(), data=polygon, fill = "white", colour = "black", linewidth =2) + 
  geom_sf(aes(), data = aoi, color = "purple",  linewidth =2)+
  geom_text(aes(x = 7, y = 7.5), label = "Analysis Area", color = "purple", size =6)+
  theme_void(base_size = 20)+
  labs(subtitle = "Ecoregion, watershed, MLRA etc.,")+
  theme(plot.subtitle = element_text(size = 16, vjust = -2.5, hjust = 0.5))

step1 <- base +
  geom_sf(aes(), data=points, size = 15, shape = 20)+
  geom_sf_text(data = points, aes(label = id), size = 6, color = "white")+
  ggtitle(label = "Step 1")

## Step 2: Screen monitoring data to identify plots that represent reference conditions – in this step we remove several plots because they are close to roads, close to intensive grazing or have had some recent disturbance such as veg treatment or wildfire​
step2 <- step1 +
  geom_sf(data = points[points$remove == "TRUE",], shape = 4, size = 20, col = "black", stroke = 1.5)+
  ggtitle(label = "Step 2")

step2

## Step 3: Group monitoring plots by geographic areas having similar climatic, topographic, geologic, vegetation, and soil conditions – for example this could use the ecological site concept, vegetation or soil types or other areas that are likely to respond similarly to management. These are our benchmark groups​
step3 <- base +
  geom_sf(aes(col = type), data=points[points$remove == "FALSE",], size = 15, shape = 20)+
  geom_sf_text(data = points[points$remove == "FALSE",], aes(label = id), size = 6, color = "white")+
  scale_color_viridis_d()+
  guides(color=guide_legend(title = "Benchmark Group"))+
  ggtitle(label = "Step 3")+
  theme(legend.position = "bottom")

step3

## Step 4: Visualize indicator values – for each of our benchmark groups we visualize the benchmark using boxplots of histograms​
# create some data
Type1 <- rnorm(n = 100, mean = 35, sd = 10)
Type2 <- rnorm(n = 100, mean = 15, sd = 5)
data <- data.frame(Type1, Type2)
data_long <-  pivot_longer(cols = everything(), data, names_to = "Type")

data_long$Type <- gsub("e1","e 1",data_long$Type)
data_long$Type <- gsub("e2","e 2",data_long$Type)

segment <- data.frame(a = c(0.5,1.5), b = c(42.3,18), Xend = c(1.5,2.5))
label <- data.frame(value=c(42.3,18), Type=c("Type 1","Type 2"), name= "75th percentile")  


step45 <- ggplot(data = data_long, aes(x = Type, y = value, fill = Type))+
  geom_boxplot(alpha = 0.5, outliers = FALSE)+
  theme_bw(base_size = 20)+
  guides(fill ="none")+
  scale_fill_viridis_d()+
  labs(y = "Indicator Value (%)", x = "")+
  geom_segment(data = segment, aes(x = a, y = b, xend = Xend), inherit.aes = FALSE, linetype = 2, linewidth= 1)+
  geom_label_repel(data = label, aes(label = name), color = "black", segment.color = "black", alpha = 0.5, nudge_y = 5, nudge_x = 0.5, size =6)+
  ggtitle(label = "Step 4 & 5")

step45
## Step 5: Select percentiles of the indicator value distribution. Lastly, based on that data we select a benchmark – a simple method for doing this is selecting a percentile of the data above or below which represents our desired conditions for example this may be the 75th percentile for the bare soil indicator – this would give us a benchmark of 50% for the blue sites and around 30% for the yellow sites.
final <- grid.arrange(step1, step2, step3, step45, ncol = 2, nrow = 2)
## Display plots together in 2x2 grid

ggsave(filename = "Figure B-4.jpg", final, path = "C:\\Users\\alaurencetraynor\\Documents", device = "jpeg", dpi = 600, width = 12, height = 10, units ="in")
