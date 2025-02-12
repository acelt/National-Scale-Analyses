# Need forage and invasive cover by year as well for Laura 12/23/2024
library(tidyverse)

# read in csv
data <-  read.csv("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\Mojave Desert Tortoise\\AIM_LMF_MDTCalcs.csv")

# group by year and calc means
data_year <- data |> 
  group_by(Year,Group) |> 
  summarise(forage_mean = mean(PREFERRED, na.rm = TRUE),
            forage_std = sd(PREFERRED, na.rm = TRUE),
            invasive_mean = mean(MDTINVASIVE, na.rm = TRUE),
            invasive_std = sd(MDTINVASIVE, na.rm = TRUE),
            plot_count = n()) |> 
  mutate(forage_se = forage_std/sqrt(plot_count),
         forage_lower_ci = forage_mean - qt(1 - (0.2 / 2), plot_count - 1) * forage_se,
         forage_upper_ci = forage_mean + qt(1 - (0.2 / 2), plot_count - 1) * forage_se) |> 
  mutate(invasive_se = invasive_std/sqrt(plot_count),
         invasive_lower_ci = invasive_mean - qt(1 - (0.2 / 2), plot_count - 1) * invasive_se,
         invasive_upper_ci = invasive_mean + qt(1 - (0.2 / 2), plot_count - 1) * invasive_se)

  
mutate(se.mpg = sd.mpg / sqrt(n.mpg),
       lower.ci.mpg = mean.mpg - qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg,
       upper.ci.mpg = mean.mpg + qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg)