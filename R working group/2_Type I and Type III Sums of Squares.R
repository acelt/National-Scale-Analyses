
# Sums of Squares

# These are relevant when testing for the overall significance of a fixed effect in a model.
# In general: don't examine/interpret individual levels of a fixed effect when the fixed effect is nonsignificant overall.

# Type I: Sequential Sums of Squares
  # Default in R  
  # Depend on the order of terms in your model
      # This is rarely what we want in practice
  # Only appropriate when data are balanced
  # Same as Type III SS when data are balanced

# Type II: Disregards interaction term(s)
      # This is rarely what we want in practice
  # Default in Python statsmodel library

# Type III: Partial Sums of Squares
  # Consider all effects in the model at the same time.  Order is not important.
      # This is usually what we want in practice
  # Appropriate for imbalanced designs.
  # In SAS, Type III Sums of Squares are recommended as general purpose.
  # In R your must change the model's default parameterization to get correct Type III SS.


library(tidyverse)
library(car)
library(emmeans)

# Generate random data with 2 fixed effects (varA and varB) in a balanced design
set.seed(2964)
df <- data.frame(response = rnorm(n = 32, mean = seq(10, 25, 5)), 
                 varA = factor(rep(paste0("A", 1:4), times = 4)),
                 varB = factor(rep(paste0("B", 1:2), each = 8)))

# Contingency table: verify data are balanced 
with(df, table(varA, varB))

# Boxplots of varA, summing over varB
ggplot(df, aes(x = varA, y = response, fill = varA)) +
  geom_boxplot()

# Boxplots of varB, summing over varA
ggplot(df, aes(x = varB, y = response, fill = varB)) +
  geom_boxplot()

# Boxplots of varA*varB combinations
ggplot(df, aes(x = varA, y = response, fill = varB)) +
  geom_boxplot()

# Create interaction plot with interaction.plot()
  # If the two lines on the interaction plot are parallel then likely there is no interaction effect. 
  # If the lines intersect then there is likely an interaction effect.
interaction.plot(x.factor = df$varA, #x-axis variable
                 trace.factor = df$varB, #variable for lines
                 response = df$response)

# Run linear model with main effects and interaction
m1 <- lm(response ~ varA*varB, data = df)

# Type I Sums of Squares with anova()
anova(m1)

# "Default" Type III Sums of Squares with car::Anova
# These are incorrect for the main effects
Anova(m1, type = "III", test.statistic = "F")
# Notice the significance for varB.  This is bogus.

# "Correct" Type III Sums of Squares produced under sum coding
# These correctly match the Type I Sums of Squares
m1.sumCoding <- lm(response ~ varA*varB, data = df, 
                   contrasts = list(varA = contr.sum, varB = contr.sum))
Anova(m1.sumCoding, 
           type = "III", 
           test.statistic = "F")

# Both of these models (m1 and m1.sumCoding) produce the same means and standard errors for each group.

# Check Least Squares Means/Estimated Marginal Means with emmeans::emmeans()
# VarA main effect
emmeans(m1, ~varA)
emmeans(m1.sumCoding, ~varA)

# VarB main effect
emmeans(m1, ~varB)
emmeans(m1.sumCoding, ~varB)

# VarA*varB Interaction
emmeans(m1, ~varA*varB)
emmeans(m1.sumCoding, ~varA*varB)

# Darren's recommendations:

# Use default Type I SS with stats::anova() if data are balanced.  They will be the same as Type III SS.
# If data are imbalanced, use sum coding (contr.sum) and car::Anova to get correct Type III SS.
# Always interpret interactions before main effects
# Always compare exploratory plots (boxplots, etc.) to model results

# Links for more information
# First link works in Chrome but not Firefox 
# http://myowelt.blogspot.com/2008/05/obtaining-same-anova-results-in-r-as-in.html 
# https://stats.stackexchange.com/questions/4544/how-does-one-do-a-type-iii-ss-anova-in-r-with-contrast-codes
                 
