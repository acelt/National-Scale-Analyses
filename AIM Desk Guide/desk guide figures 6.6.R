#### Setup
library(ggplot2)
library(viridis)

#### Figure 6-6 
### This is an example of a boxplot + jittered points on top, color coded by quartile (from Nelson's app)

# Create some fake data
data <- data.frame(value = rbeta(n = 100, shape1 = 1, shape2 = 2))
data$percent = data$value*100

# add label for quartiles
quantiles <- quantile(data$percent)

# Labeling data with respective quantile (im stealing code from Nelson here :)
for (current_quantile in names(quantiles)[length(quantiles):1]) {
  data[["Quantile"]][data[["percent"]] <= quantiles[current_quantile]] <- current_quantile
}

# convert to factor and order
data[["Quantile"]] <- factor(data[["Quantile"]],
                             levels = c("0%","25%", "50%","75%","100%"))
# Now plot
boxplot <-  ggplot(data = data, aes(y = percent, x = ""))+
  geom_boxplot(width =0.5)+
  theme_bw(base_size = 20)+
  geom_jitter(aes(y = percent, col = Quantile, x=""), width =0.2)+
  scale_color_viridis_d()+
  coord_flip()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  labs(x ="", y = "Indicator Value (%)")
  
boxplot  

ggsave("Figure 6-6.jpg",
       plot = boxplot,
       path = 'C:\\Users\\alaurencetraynor\\Documents',
       device = "jpeg",
       dpi = 600,
       width = 10,
       height = 6)
