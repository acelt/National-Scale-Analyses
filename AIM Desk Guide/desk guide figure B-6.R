#### Setup
library(ggplot2)

#### Figure B-6
### This is a series of histograms and colored ribbons showing percentiles

# create some data
set.seed(2)
data <-  data.frame(Clayey = rbeta(100, shape1 = 1,shape2 = 2),
                    Gravelly = rbeta(100, shape1 = 2,shape2 = 6),
                    Mountains = rbeta(100, shape1 = 4,shape2 = 18),
                    Loamy = rbeta(100, shape1 = 1.5,shape2 = 1.35),
                    Sandy = rbeta(100, shape1 = 1,shape2 = 1))
data <- pivot_longer(data, cols = everything(), names_to = "Ecosite")

quantiles <- c(0.1,0.25,0.5,0.75,0.9)
quantile(x = data$value[data$Ecosite=="Clayey"],
         probs = quantiles)

lines <- data.frame(Clayey = quantile(x = data$value[data$Ecosite=="Clayey"],
                                      probs = quantiles),
                    Gravelly = quantile(x = data$value[data$Ecosite=="Gravelly"],
                                        probs = quantiles),
                    Mountains = quantile(x = data$value[data$Ecosite=="Mountains"],
                                        probs = quantiles),
                    Loamy = quantile(x = data$value[data$Ecosite=="Loamy"],
                                        probs = quantiles),
                    Sandy = quantile(x = data$value[data$Ecosite=="Sandy"],
                                        probs = quantiles))
lines$q <- row.names(lines)
lines <- pivot_longer(lines, cols = Clayey:Sandy, names_to = "Ecosite")
  
hist <- ggplot(data, aes(x = value*100))+
  geom_histogram(bins =15, fill = "grey", col = "black")+
  theme_bw(base_size = 20)+
  facet_wrap(.~Ecosite)+
  labs(y = "Number of Plots",
       x = "Bare Ground Amount (%)",
       color = "Percentile")+
  geom_vline(data =lines, aes(xintercept = value*100, col =q),
             key_glyph = "path")+
  scale_color_viridis_d()+
  theme(legend.position = c(0.85,0.22))

hist

ggsave("Figure B-6.jpg",
       hist,
       "C:\\Users\\alaurencetraynor\\Documents",
       dpi =600,
       width = 12,
       height = 6,
       device = "jpeg")


