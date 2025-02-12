
# install.packages("AICcmodavg")

library(tidyverse)
library(AICcmodavg)

# Use mtcars as example data set
# Create random response variable
data.aic <- mtcars %>%
  mutate(`random response` = rnorm(32))

# Model mpg: Compare 4 candidate models with AICc 
m.null <- lm(mpg ~ 1, data = data.aic)
m.carb <- lm(mpg ~ factor(carb), data = data.aic)
m.gear <- lm(mpg ~ factor(gear), data = data.aic)
m.carb.gear <- lm(mpg ~ factor(carb)*factor(gear), data = data.aic)

# Put models in a named list and pass to aic_tab()
list(
  "null" = m.null,
  "carb" = m.carb,
  "gear" = m.gear,
  "carb*gear" = m.carb.gear
) %>% 
  aictab()

# What if you have many response variables and various candidate model sets?
# One strategy: create chanracter vectors of responses and candidate model sets
  # Use paste() to formulate the model, then pass them to parse() and eval() 

response <- "mpg"
model <- "factor(gear)"

paste("formula = ", response, "~", model)

lm(eval(parse(text = (paste("formula = ", response, "~", model)))), data = data.aic)


# Create a general function to model other responses with these same candidate models 
# Many thanks to Nelson Stauffer for dramatically improving this function!!

#' @description A general function to model responses with candidate models 
#' @param data Data frame. The data to use. Must contain all variables specified in \code{response_var} and \code{formulas}.
#' @param response_var Character string. The name of the response variable in \code{data}.
#' @param formulas Named vector of character strings. The formula or formulas to apply to the response variable, e.g., \code{"~ factor(gear)"}. All specified variables must appear in \code{data}. The names of this vector will be used as row names in the function output.
model.aicc.compare <- function(data,
                               response_var,
                               formulas) {
  
  # Build the formula strings that'll be passed to lm() through eval(parse())
  formula_strings <- paste("formula =",
                           response_var,
                           formulas)
  
  # Move the names over for use in the next step
  names(formula_strings) <- names(formulas)
  
  # Run the linear models, returning the results as a list
  # The list should inherit the names from formula_strings
  lm_list <- lapply(X = formula_strings,
                    data = data,
                    FUN = function(X, data){
                      lm(eval(parse(text = X)),
                         data = data)
                    })
  
  # Feed that into aictab
  aicc.table <- AICcmodavg::aictab(lm_list)
  
  # Return the results!
  return(aicc.table)
  
}


## Here it is in action!
# We'll use data.aic from above
# But define the formulas here
formulas <- c("null" = "~ 1",
              "gear" = "~ factor(gear)",
              "carb" = "~ factor(carb)",
              "gear*carb" = "~ factor(gear)*factor(carb)")

# Run it!
model.aicc.compare(data = data.aic,
                   response_var = "disp",
                   formulas = formulas)

model.aicc.compare(data = data.aic,
                   response_var = "`random response`",
                   formulas = formulas)
