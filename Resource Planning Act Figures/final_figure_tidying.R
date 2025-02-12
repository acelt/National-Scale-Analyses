library(tidyverse)
library(viridis)

#this file has 2012 values replaced with year 2013 since the surveys actually took place in 2013
states <- read.csv(file = "C:\\Users\\alaurencetraynor\\Documents\\LMF\\RPA\\2022 National Report\\LMF_estimates_2011_2021_byState.csv", stringsAsFactors = FALSE)

SampleSize <- read.csv(file = "C:\\Users\\alaurencetraynor\\Documents\\LMF\\RPA\\2022 National Report\\New Indicators\\New Indicators\\Sample Sizes/AllYears_SampleSize_byState.csv", stringsAsFactors = FALSE)

# Join with sample size - sample size already has 2012 convert to 2013 
states <- merge(x = states,
                y = SampleSize,
                by = c("State", "Year"),
                all.x = TRUE)

# Replace 2012 with 2013 because the sampling was actually in 2013
states$Year <- as.character(states$Year)
states$Year <- gsub("2012" , "2013", states$Year)

states$Year <- format(as.Date(states$Year, "%Y"),"%Y")

# need to redo the index since it got messed up when combining multiple csvs
states <- states %>%
  group_by(SubIndicator, Indicator_Name) %>%
  mutate(index = cur_group_id()) %>%
  ungroup() %>%
  arrange(index)

# want to make sure all levels exist in sample sizes i.e. NA and 0 are there when they should be for all combinations of year and state
states_complete <- states %>% 
  tidyr::complete(State, Year, index)

#Put acres in millions
states_complete <- states_complete %>% 
  mutate(Value2 = ifelse(var_type == "acres", Estimate/1000000, Estimate),
         CI_Lower2 = ifelse(var_type == "acres", X80..confidence.interval.lower.bound/1000000, X80..confidence.interval.lower.bound),
         CI_Upper2 = ifelse(var_type == "acres", X80..confidence.interval.upper.bound/1000000, X80..confidence.interval.upper.bound))

# Add a flag for small sample sizes
states_complete <- states_complete %>% 
  mutate(Flag = ifelse(Number.of.BLM.range.observed.points >= 10 , NA, 0),
         Flag = ifelse(is.na(Number.of.BLM.range.observed.points)| Number.of.BLM.range.observed.points <10, 0, NA))

# make index lut
figure_labels <- states_complete %>% 
  group_by(index) %>% 
  filter(!is.na(xlab)) %>% 
  select(index, 13:18) %>% 
  unique()

# need to join back in plotting info
states_complete <- states_complete %>% 
  select(-c(13:18)) %>% 
  left_join(y = figure_labels,
            by = "index")

write.csv(states_complete, "C:\\Users\\alaurencetraynor\\Documents\\LMF\\RPA\\2022 National Report\\LMF_estimates_2011_2021_byState_for_figures.csv")