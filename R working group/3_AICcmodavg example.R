
# install.packages("AICcmodavg")

library(tidyverse)
library(AICcmodavg)

# Generate random data with 2 fixed effects (varA and varB) in a balanced design
set.seed(2964)
df <- data.frame(response = rnorm(n = 32, mean = seq(10, 25, 5)), 
                 varA = factor(rep(paste0("A", 1:4), times = 4)),
                 varB = factor(rep(paste0("B", 1:2), each = 8)))

# Analysis question: which is more important: varA, varB, or the varA*varB interaction?

# Compare 4 candidate models with AICc 
mod.null <- lm(response ~ 1, data = df)
mod.varA <- lm(response ~ varA, data = df)
mod.varB <- lm(response ~ varB, data = df)
mod.AB.interaction <- lm(response ~ varA:varB, data = df)

# Put models in a named list and pass to aic_tab()
cand.list <- list(
  "null" = mod.null,
  "varA" = mod.varA,
  "varB" = mod.varB,
  "varA*varB" = mod.AB.interaction
) 

# General rule of thumb: models within 2 AIC units of each other explain about the same amount of variation.

# Create model selection table
aictab(cand.list)


# Answer to Analysis question: varA is most important (84% of AICc weight)
# There is some support for varA*varB interaction (16% of AICc weight)
# The varB main effect performs worse than the null model

# Review the 10 basic guidelines in the AICcmodavg Introduction:

# https://cran.r-project.org/web/packages/AICcmodavg/vignettes/AICcmodavg.pdf
