# install.packages("tidyverse")
# install.packages("emmeans")

library(tidyverse)
library(emmeans)

# Use mtcars data set as an example
mtcars

# Create balanced categorical variable with 4 levels
data.anova <- mtcars %>%
  mutate(categorical = rep(paste0("level", 1:4), each = 8))

# Calculate group means and standard deviations for mpg variable
data.anova %>%
  group_by(categorical) %>%
  summarise(mpg_mean = mean(mpg),
            mpg_sd = sd(mpg))

# Boxplots of mpg means by categorical groups
ggplot(data.anova, aes(x = categorical, y = mpg, fill = categorical)) +
  geom_boxplot()

# Fit ANOVA-type linear model
mod1 <- lm(mpg ~ categorical, data = data.anova)

summary(mod1)

# It stands to reason that the term "intercept" refers to the overall mean.
    # But this is incorrect!!

# In these ANOVA-type models the default parameterization in lm() sets the 
    # intercept equal to the first categorical level. 

# So in the model (m1), the intercept is the mean for "categorical" level 1. 

# The other 3 model coefficients are not means but rather the difference between  
  #  categorical level 1 and categorical levels 2, 3, and 4 respectively. 

# You can change the model's paramterization if you want to set the intercept to the overall mean.
   # One way is to specify sum-to-zero contrasts, or "sum coding".
# See ?contrasts and ?contr.sum for more information.

# These only work with factors 

# Make the categorical a factor 
data.anova <- data.anova %>%
  mutate(categorical = factor(categorical))

mod1.sumCoding <- lm(mpg ~ categorical, data = data.anova,
                     contrasts = list(categorical = contr.sum))
summary(mod1.sumCoding)
# Now the intercept is the overall mean
# Note: to estimate the mean for categorical3 use these model coefficents:
    # intercept - (categorical1 + categorical2 + categorical3)
20.0906 - (0.1219 - 3.9031 + 2.0219)

# The Least Squares Means/estimated marginal means are the same for each model
emmeans(mod1, ~categorical)
emmeans(mod1.sumCoding, ~categorical)

# Setting the intercept to zero with sum-to-zero parameterization will produce the "means model"
# This is the model parameterization that many users might expect to be the default
mod1.means <- lm(mpg ~ categorical - 1, data = data.anova,
                     contrasts = list(categorical = contr.sum))
summary(mod1.means)

# Check current contrast specifications:
options("contrasts")
# Note: you can set contrasts to contr.sum this way:
# options("contr.sum", "contr.poly")

# If you write your own custom contrasts then you must pay very close attention to the model's
  # parameterization to ensure you specify the contrasts correctly

# Using the emmeans package for group estimates will handle different model parameterizations automatically.

