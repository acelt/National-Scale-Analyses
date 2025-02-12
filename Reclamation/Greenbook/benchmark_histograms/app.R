#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(anytime)
# library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Plotting Indicator Values and Benchmarks"),
  
  # Sidebar with a slider input for number of bins 
  # Could add dropdowns to subsets of reference data for specific regions/climates
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "raw_data",
                label = "Exported TerrADat CSV",
                multiple = FALSE,
                accept = "CSV"),
      helpText("OR"),
      textInput(inputId = "ecosite_id",
                label = "Ecological Site ID",
                value = "",
                placeholder = "R042XB012NM"),
      textOutput(outputId = "ecosite_error"),
      actionButton(inputId = "fetch_data",
                   label = "Fetch data from the Landscape Data Commons"),
      hr(),
      selectInput(inputId = "id_variables", 
                  label = "Variable(s) with identifying, non-indicator information",
                  multiple = TRUE,
                  choices = c("")),
      selectInput(inputId = "variable",
                  label = "Indicator to plot",
                  choices = c("")),
      textInput(inputId = "variable_name",
                label = "X-axis label",
                value = ""),
      textInput(inputId = "quantiles",
                label = "Quantile break percentages, separated by commas",
                value = "25,50,75"),
      hr(),
      
      # If you want to compare all data to a single plot that is within the csv/fetch command
      conditionalPanel(condition = "input.comparison_checkbox ==''",
                       checkboxInput(inputId = "singleplot_checkbox",
                                     label = "Compare Plot(s)?",
                                     value = FALSE)),
      
      # Only show plot id dropdown  if single plot checkbox is clicked
      conditionalPanel(condition = "input.singleplot_checkbox != ''",
                       selectInput(inputId = "plot_id_var",
                                   label = "Plot ID Variable",
                                   choices = c(""))),
      
      # if the single plot checkbox is selected, add a field to specify which plot is needed
      conditionalPanel(condition = "input.singleplot_checkbox != ''",
                       selectInput(inputId = "study_plot",
                                   label = "Comparison Plot(s)",
                                   multiple = TRUE,
                                   choices = c(""))),
      
      # If you want to compare to a single value based on a slider
      conditionalPanel(condition = "input.singleplot_checkbox == ''",
                       checkboxInput(inputId = "comparison_checkbox",
                                     label = "Compare to Single Value?",
                                     value = FALSE)),
      
      # Only show comparison box if comparison checkbox is selected
      conditionalPanel(condition = "input.comparison_checkbox != ''",
                       numericInput(inputId = "comparison_value",
                                   label = "Comparison Value",
                                   min = 0,
                                   max = 100,
                                   value = 15)),
      # Add time series checkbox
      checkboxInput(inputId = "time_checkbox",
                    label = "Include Time Series?",
                    value = FALSE),
      
      # Add conditional panel for year/date field (from identifying fields)
      conditionalPanel(condition = "input.time_checkbox != ''",
                       selectInput(inputId = "date_field",
                                   label = "Date Field",
                                   choices = c(""))),
      
      # Only show the plot button if data have been uploaded/downloaded
      conditionalPanel(condition = "input.variable != ''",
                       actionButton(inputId = "plot_button",
                                    label = "Plot indicator distribution!")),
      # Only show if there are plot images to download
      conditionalPanel(condition = "input.plot_button >= 1",
                       downloadButton(outputId = 'downloadData',
                                      label = 'Download results')),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "maintabs",
                  
                  tabPanel(title = "Instructions",
                           includeHTML("instructions.html")),
                  
                  tabPanel(title = "Benchmark ranges",
                           selectInput(inputId = "range_count",
                                       label = "Number of benchmark ranges",
                                       choices = 2:6,
                                       selected = 2),
                           # Labels
                           fluidRow(column(width = 2,
                                           helpText("Lower limit")),
                                    column(width = 1,
                                           helpText("Lower limit relation")),
                                    column(width = 1,
                                           helpText("Indicator value")),
                                    column(width = 1,
                                           helpText("Upper limit relation")),
                                    column(width = 2,
                                           helpText("Upper limit")),
                                    
                                    column(width = 3,
                                           helpText("Benchmark category")),
                                    hr()
                           ),
                           # Row 1
                           fluidRow(column(width = 2,
                                           numericInput(inputId = "benchmark_range_limit_lower_1",
                                                        label = "",
                                                        value = 0,
                                                        min = -Inf,
                                                        max = Inf)),
                                    column(width = 1,
                                           selectInput(inputId = "benchmark_relationship_lower_1",
                                                       label = "",
                                                       choices = c("<", "<="),
                                                       selected = "<=")),
                                    column(width = 1,
                                           helpText("x")),
                                    column(width = 1,
                                           selectInput(inputId = "benchmark_relationship_upper_1",
                                                       label = "",
                                                       choices = c("<", "<="),
                                                       selected = "<=")),
                                    column(width = 2,
                                           numericInput(inputId = "benchmark_range_limit_upper_1",
                                                        label = "",
                                                        value = 50,
                                                        min = -Inf,
                                                        max = Inf)),
                                    
                                    column(width = 3,
                                           textInput(inputId = "benchmark_category_1",
                                                     label = "",
                                                     placeholder = "Not Meeting",
                                                     value = "Not Meeting")),
                                    hr()
                           ),
                           # Row 2
                           fluidRow(column(width = 2,
                                           numericInput(inputId = "benchmark_range_limit_lower_2",
                                                        label = "",
                                                        value = 50,
                                                        min = -Inf,
                                                        max = Inf)),
                                    column(width = 1,
                                           selectInput(inputId = "benchmark_relationship_lower_2",
                                                       label = "",
                                                       choices = c("<", "<="),
                                                       selected = "<")),
                                    column(width = 1,
                                           helpText("x")),
                                    column(width = 1,
                                           selectInput(inputId = "benchmark_relationship_upper_2",
                                                       label = "",
                                                       choices = c("<", "<="),
                                                       selected = "<=")),
                                    column(width = 2,
                                           numericInput(inputId = "benchmark_range_limit_upper_2",
                                                        label = "",
                                                        value = 100,
                                                        min = -Inf,
                                                        max = Inf)),
                                    
                                    column(width = 3,
                                           textInput(inputId = "benchmark_category_2",
                                                     label = "",
                                                     placeholder = "Not Meeting",
                                                     value = "Meeting")),
                                    hr()
                           ),
                           # Row 3
                           conditionalPanel(condition = "input.range_count > 2",
                                            fluidRow(column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_lower_3",
                                                                         label = "",
                                                                         value = 0,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_lower_3",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<")),
                                                     column(width = 1,
                                                            helpText("x")),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_upper_3",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<=")),
                                                     column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_upper_3",
                                                                         label = "",
                                                                         value = 50,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     
                                                     column(width = 3,
                                                            textInput(inputId = "benchmark_category_3",
                                                                      label = "",
                                                                      placeholder = "Not Meeting",
                                                                      value = "")),
                                                     hr()
                                            )),
                           # Row 4
                           conditionalPanel(condition = "input.range_count > 3",
                                            fluidRow(column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_lower_4",
                                                                         label = "",
                                                                         value = 0,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_lower_4",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<")),
                                                     column(width = 1,
                                                            helpText("x")),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_upper_4",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<=")),
                                                     column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_upper_4",
                                                                         label = "",
                                                                         value = 50,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     
                                                     column(width = 3,
                                                            textInput(inputId = "benchmark_category_4",
                                                                      label = "",
                                                                      placeholder = "Not Meeting",
                                                                      value = "")),
                                                     hr()
                                            )),
                           # Row 5
                           conditionalPanel(condition = "input.range_count > 4",
                                            fluidRow(column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_lower_5",
                                                                         label = "",
                                                                         value = 0,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_lower_5",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<")),
                                                     column(width = 1,
                                                            helpText("x")),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_upper_5",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<=")),
                                                     column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_upper_5",
                                                                         label = "",
                                                                         value = 50,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     
                                                     column(width = 3,
                                                            textInput(inputId = "benchmark_category_5",
                                                                      label = "",
                                                                      placeholder = "Not Meeting",
                                                                      value = "")),
                                                     hr()
                                            )),
                           # Row 6
                           conditionalPanel(condition = "input.range_count > 5",
                                            fluidRow(column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_lower_6",
                                                                         label = "",
                                                                         value = 0,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_lower_6",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<")),
                                                     column(width = 1,
                                                            helpText("x")),
                                                     column(width = 1,
                                                            selectInput(inputId = "benchmark_relationship_upper_6",
                                                                        label = "",
                                                                        choices = c("<", "<="),
                                                                        selected = "<=")),
                                                     column(width = 2,
                                                            numericInput(inputId = "benchmark_range_limit_upper_6",
                                                                         label = "",
                                                                         value = 50,
                                                                         min = -Inf,
                                                                         max = Inf)),
                                                     
                                                     column(width = 3,
                                                            textInput(inputId = "benchmark_category_6",
                                                                      label = "",
                                                                      placeholder = "Not Meeting",
                                                                      value = "")),
                                                     hr()
                                            ))
                  ),
                  
                  tabPanel(title = "Results",
                           plotOutput("quantiles_plot"),
                           # plotlyOutput("quantiles_plot"),
                           textOutput("quantile_breaks"),
                           plotOutput("benchmark_plot"),
                           # plotlyOutput("benchmark_plot"),
                           textOutput("benchmark_summary"),
                           
                           # Add conditional plot for time series
                           conditionalPanel(condition = "input.time_checkbox != ''",
                                            plotOutput("timeseries_plot"),
                                            textOutput("timeseries_caption"))),
                                           
                  
                  tabPanel(title = "Data",
                           tableOutput("data_table")))
      
    )
  )
) 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Create a workspace list to store objects and values in
  workspace <- reactiveValues(placeholder = TRUE,
                              temp_directory = tempdir(),
                              original_directory = getwd(),
                              quantiles = c(0.25, 0.5, 0.75),
                              # The color palette for the figures
                              palette = c("#f5bb57ff",
                                          "#f8674cff",
                                          "#4a8b9fff",
                                          "#685b7fff",
                                          "#c95294ff",
                                          "#f5a9c6ff",
                                          "#75c6c5ff",
                                          "#fd6794ff"))
  
  # Nor are there plots
  output$plot_files <- renderText("FALSE")
  
  #### When a CSV is uploaded, do this ####
  observeEvent(eventExpr = input$raw_data,
               handlerExpr = {
                 workspace[["raw_data"]] <- read.csv(input$raw_data$datapath,
                                                     stringsAsFactors = FALSE)
               })
  
  #### When someone fetches data from the Landscape Data Commons, do this ####
  observeEvent(eventExpr = input$fetch_data,
               handlerExpr = {
                 output$ecosite_error <- renderText("")
                 # Only do anything if there's an ecosite ID
                 if (input$ecosite_id != "") {
                   # Make sure it's uppercase
                   ecosite_id <- toupper(input$ecosite_id)
                   
                   # Build the query
                   query <- paste0("http://api.landscapedatacommons.org/api/",
                                   "geoindicators?",
                                   "EcologicalSiteId=",
                                   ecosite_id)
                   
                   # Getting the data via curl
                   # connection <- curl::curl(query)
                   # results_raw <- readLines(connection)
                   # results <- jsonlite::fromJSON(results_raw)
                   print("Attempting to query EDIT")
                   # Full query results for geoindicators based on ecosite
                   full_results <- httr::GET(query,
                                             config = httr::timeout(30))
                   # Grab only the data portion
                   results_raw <- full_results[["content"]]
                   # Convert from raw to character
                   results_character <- rawToChar(results_raw)
                   # Convert from character to data frame
                   results <- jsonlite::fromJSON(results_character)
                   
                   # Only keep going if there are results!!!!
                   if (length(results) > 0) {
                     # Convert from character to numeric variables where possible
                     data_corrected <- lapply(X = names(results),
                                              data = results,
                                              FUN = function(X, data){
                                                # Get the current variable values as a vector
                                                vector <- data[[X]]
                                                # Try to coerce into numeric
                                                numeric_vector <- as.numeric(vector)
                                                # If that works without introducing NAs, return the numeric vector
                                                # Otherwise, return the original character vector
                                                if (all(!is.na(numeric_vector))) {
                                                  return(numeric_vector)
                                                } else {
                                                  return(vector)
                                                }
                                              })
                     
                     # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
                     # so I'm resorting to using dplyr
                     data <- dplyr::bind_cols(data_corrected)
                     # Correct the names of the variables
                     names(data) <- names(results)
                     
                     # Put it in the workspace list
                     workspace$raw_data <- data
                   } else {
                     output$ecosite_error <- renderText(paste(ecosite_id, "is not a valid ecological site ID recognized by EDIT"))
                   }
                 }
               })
  
  #### When a CSV is read in, do this ####
  observeEvent(eventExpr = workspace$raw_data,
               handlerExpr = {
                 # Get the variable names in the CSV
                 variable_names <- names(workspace$raw_data)
                 
                 # Look at each column and determine if it can be coerced into numeric
                 viable_variables <- apply(X = workspace$raw_data,
                                           MARGIN = 2,
                                           FUN = function(X){
                                             vector <- X
                                             numeric_vector <- as.numeric(vector)
                                             any(!is.na(numeric_vector))
                                           })
                 
                 # Update the dropdown options to include those variables
                 updateSelectInput(inputId = "id_variables",
                                   choices = c("", variable_names))
                 updateSelectInput(inputId = "variable",
                                   choices = c("", variable_names[viable_variables]))
                 
                 output$data_table <- renderTable(workspace$raw_data)
                 
                 updateTabsetPanel(session,
                                   inputId = "maintabs",
                                   selected = "Data")
               })
  
  #### When the indicator is updated, do this ####
  observeEvent(eventExpr = input$variable,
               handlerExpr = {
                 updateTextInput(session,
                                 inputId = "variable_name",
                                 value = input$variable)
               })
  
  #### When the identifying variables are updated, do this ####
  observeEvent(eventExpr = input$id_variables,
               handlerExpr = {
                 # Get the variable names in the CSV
                 variable_names <- names(workspace$raw_data)
                 
                 # Look at each column and determine if it can be coerced into numeric
                 viable_variables <- apply(X = workspace$raw_data,
                                           MARGIN = 2,
                                           FUN = function(X){
                                             vector <- X
                                             numeric_vector <- as.numeric(vector)
                                             any(!is.na(numeric_vector))
                                           })
                 
                 not_id_variable <- !(variable_names %in% input$id_variables)
                 
                 # Update!
                 updateSelectInput(inputId = "variable",
                                   choices = c("", variable_names[viable_variables & not_id_variable]))
                 
                 # List of identifying variables from the raw data
                 updateSelectInput(inputId = "plot_id_var",
                                   choices = c("",input$id_variables))
                 
                 # update drop-down list for date field based on id_variables
                 if (input$time_checkbox != "") {
                   updateSelectInput(inputId = "date_field",
                                     choices = c("",input$id_variables))
                 }
               })
  
  observeEvent(eventExpr = input$plot_id_var,
               handlerExpr = {
                 
                 # Updating possible comparison plots based on plot_id_var
                 if (input$plot_id_var != "" & input$singleplot_checkbox != "") {
                   
                   plot_ids <- workspace$raw_data[[input$plot_id_var]]
                   
                   updateSelectInput(inputId = "study_plot",
                                     choices = c("", plot_ids))
                   
                 }
               })
  
  ### When the quantiles are updated, do this ####
  observeEvent(eventExpr = input$quantiles,
               handlerExpr = {
                 # Split the string along the commas
                 quantiles_vector <- unlist(strsplit(input$quantiles,
                                                     split = ","))
                 # Remove any spaces
                 quantiles_vector <- trimws(quantiles_vector)
                 
                 # Convert to numeric
                 quantiles_vector <- as.numeric(quantiles_vector)
                 
                 # IF ANY AREN'T NUMERIC, WE HAVE A PROBLEM
                 if (any(is.na(quantiles_vector))) {
                   # INSERT ERROR MESSAGE HERE
                   # Default to just 50%
                   workspace[["quantiles"]] <- c(0.5)
                 } else {
                   # Convert to proportion for quantile()
                   quantiles_vector <- quantiles_vector / 100
                   
                   # If any of the percentages were < 0 or > 100, there's a problem
                   if (all(quantiles_vector >= 0) & all(quantiles_vector <= 1)) {
                     workspace[["quantiles"]] <- quantiles_vector
                   } else {
                     # INSERT ERROR MESSAGE HERE
                     # Default to just 50%
                     workspace[["quantiles"]] <- c(0.5)
                   }
                 }
                 
               })
  
  #### When the plot button is hit, do this ####
  observeEvent(eventExpr = input$plot_button,
               handlerExpr = {
                 # Get a copy of the data to manipulate for plotting
                 plotting_data <- workspace$raw_data
                 
                 message("plotting_data row one is:")
                 message(plotting_data[1,])
                 
                 # Only plot if there's data!!!!
                 if (!is.null(plotting_data)){
                   # Grab the variable name to work with
                   variable_name <- input$variable
                   
                   #! What are the quantile proportions?
                   quantile_proportions <- workspace$quantiles
                   # Make sure they're in order from smallest to largest
                   quantile_proportions <- quantile_proportions[order(quantile_proportions)]
                   
                   # Write that current variable to a new one so we can plot easily
                   # And make sure it's numeric, not integer or something
                   plotting_data[["current_variable"]] <- as.numeric(plotting_data[[variable_name]])
                   
                   # Get those values as a vector
                   current_data_vector <- plotting_data[[variable_name]]
                   
                   # Find the quantile breaks
                   quantiles <- quantile(current_data_vector,
                                         probs = quantile_proportions,
                                         na.rm = TRUE)
                   # Drop 0% and 100% if they're in there
                   quantile_indices <- !(names(quantiles) %in% c("0%", "100%"))
                   quantiles <- quantiles[quantile_indices]
                   
                   # Write in the quantile that the values belong in, starting with all of them in 100%
                   plotting_data[["Quantile"]] <- "100%"
                   # Then in order from largest to smallest, assign the rest of the quantiles
                   for (current_quantile in names(quantiles)[length(quantiles):1]) {
                     plotting_data[["Quantile"]][plotting_data[["current_variable"]] <= quantiles[current_quantile]] <- current_quantile
                   }
                   
                   # Correct the order of the legend items
                   quantile_names <- unique(c(names(quantiles), "100%"))
                   plotting_data[["Quantile"]] <- factor(plotting_data[["Quantile"]],
                                                         levels = quantile_names)
                   
                   
                   # Make the dang plot happen!
                   if (nrow(plotting_data) > 0) {
                     workspace$quantile_plot <- ggplot(data = plotting_data) +
                       geom_histogram(aes(y = current_variable,
                                          fill = Quantile),
                                      binwidth = 1) +
                       scale_fill_manual(values = workspace$palette) +
                       geom_hline(yintercept = quantiles,
                                  size = 1.5,
                                  color = "gray50") +
                       labs(x = "Count of data points",
                            y = input$variable_name) +
                       theme(panel.grid = element_blank(),
                             panel.background = element_rect(fill = "gray95")) +
                       coord_flip()
                     
                     
                     # add hline for single plot value
                     
                     if (input$study_plot != "" & input$plot_id_var != "") {
                       
                       # Subset input data to only include the single plot specified in the inputs
                       comp_plotting_data <- plotting_data[plotting_data[[input$plot_id_var]] == input$study_plot,]
                       
                       # Redrawing workspace plot without the comparison plot data
                       workspace$quantile_plot <- ggplot(data = plotting_data[plotting_data[[input$plot_id_var]] != input$study_plot,]) +
                         geom_histogram(aes(y = current_variable,
                                            fill = Quantile),
                                        binwidth = 1) +
                         scale_fill_manual(values = workspace$palette) +
                         geom_hline(yintercept = quantiles,
                                    size = 1.5,
                                    color = "gray50") +
                         labs(x = "Count of data points",
                              y = input$variable_name) +
                         theme(panel.grid = element_blank(),
                               panel.background = element_rect(fill = "gray95")) +
                         coord_flip() + 
                         geom_hline(data = comp_plotting_data,
                                    aes(yintercept = current_variable),
                                    col = "violet",
                                    size = 1.5,
                                    linetype = "dashed")
                     } else if (input$comparison_value != "" & input$comparison_checkbox != "FALSE") {  # add hline for comparison value
                       # Subset input data to only include the single plot specified in the inputs
                       workspace$quantile_plot <- workspace$quantile_plot + 
                         geom_hline(aes(yintercept = input$comparison_value),
                                    col = "violet",
                                    size = 1.5,
                                    linetype = "dashed")
                     }
                     
                     
                     
                     output$quantiles_plot <- renderPlot(workspace$quantile_plot)
                     # output$quantiles_plot <- renderPlotly(ggplotly(workspace$quantile_plot))
                     
                     ggsave(filename = paste0(workspace$temp_directory, "/fig1_quantile_plot.png"),
                            plot = workspace$quantile_plot,
                            device = "png",
                            width = 9,
                            height = 4,
                            units = "in")
                   }
                   
                   ##### Do the benchmarking ####
                   print("Benchmark time!")
                   # Now time to deal with benchmarks
                   # Make a data frame with all the benchmark ranges in it
                   benchmark_table <- data.frame(range_limit_lower = c(input$benchmark_range_limit_lower_1,
                                                                       input$benchmark_range_limit_lower_2,
                                                                       input$benchmark_range_limit_lower_3,
                                                                       input$benchmark_range_limit_lower_4,
                                                                       input$benchmark_range_limit_lower_5,
                                                                       input$benchmark_range_limit_lower_6),
                                                 range_relation_lower = c(input$benchmark_relationship_lower_1,
                                                                          input$benchmark_relationship_lower_2,
                                                                          input$benchmark_relationship_lower_3,
                                                                          input$benchmark_relationship_lower_4,
                                                                          input$benchmark_relationship_lower_5,
                                                                          input$benchmark_relationship_lower_6),
                                                 range_relation_upper = c(input$benchmark_relationship_upper_1,
                                                                          input$benchmark_relationship_upper_2,
                                                                          input$benchmark_relationship_upper_3,
                                                                          input$benchmark_relationship_upper_4,
                                                                          input$benchmark_relationship_upper_5,
                                                                          input$benchmark_relationship_upper_6),
                                                 range_limit_upper = c(input$benchmark_range_limit_upper_1,
                                                                       input$benchmark_range_limit_upper_2,
                                                                       input$benchmark_range_limit_upper_3,
                                                                       input$benchmark_range_limit_upper_4,
                                                                       input$benchmark_range_limit_upper_5,
                                                                       input$benchmark_range_limit_upper_6),
                                                 benchmark_category = c(input$benchmark_category_1,
                                                                        input$benchmark_category_2,
                                                                        input$benchmark_category_3,
                                                                        input$benchmark_category_4,
                                                                        input$benchmark_category_5,
                                                                        input$benchmark_category_6))
                   
                   # Limit that to just the first however many ranges the user has chosen to define for now
                   # This means that even if they populated more ranges then reduced the count they'll still
                   # only get the ones they can see in the app
                   benchmark_table <- benchmark_table[1:input$range_count, ]
                   # Also strip out undefined ranges
                   invalid_benchmark_indices <- benchmark_table$benchmark_category == ""
                   benchmark_table <- benchmark_table[!invalid_benchmark_indices, ]
                   
                   print(benchmark_table)
                   # First create the string vector we'll write into for each category
                   benchmark_results <- rep("Undefined",
                                            times = nrow(plotting_data))
                   # Then step through the ranges in a loop
                   # We loop because the user might've accidentally defined it so that ranges overlap
                   for (benchmark in 1:nrow(benchmark_table)) {
                     current_range_string_lower <- paste(benchmark_table$range_limit_lower[benchmark],
                                                         benchmark_table$range_relation_lower[benchmark])
                     current_range_string_upper <- paste(benchmark_table$range_relation_upper[benchmark],
                                                         benchmark_table$range_limit_upper[benchmark])
                     current_benchmark_category <- benchmark_table$benchmark_category[benchmark]
                     
                     # Convert the range inequalities into evaluatable statements by addint in the values
                     evaluation_strings_lower <- sapply(X = plotting_data$current_variable,
                                                        range_string = current_range_string_lower,
                                                        FUN = function(X, range_string){
                                                          paste(range_string, X)
                                                        })
                     evaluation_strings_upper <- sapply(X = plotting_data$current_variable,
                                                        range_string = current_range_string_upper,
                                                        FUN = function(X, range_string){
                                                          paste(X, range_string)
                                                        })
                     # Then parse and evaluate those strings
                     # This is the easiest way I've been able to come up with for evaluating benchmarks
                     # that are entered piecemeal and with a dropdown
                     lower_strings_evaluated <- sapply(X = evaluation_strings_lower,
                                                       FUN = function(X){
                                                         eval(parse(text = X))
                                                       })
                     upper_strings_evaluated <- sapply(X = evaluation_strings_upper,
                                                       FUN = function(X){
                                                         eval(parse(text = X))
                                                       })
                     
                     # Write in the benchmark category into our benchmark_results at the appropriate indices
                     correct_indices <- lower_strings_evaluated & upper_strings_evaluated
                     benchmark_results[correct_indices] <- current_benchmark_category
                   }
                   
                   
                   # Put the results into the data
                   plotting_data[["benchmark_results"]] <- benchmark_results
                   
                   # Calculate the percent of points meeting and not meeting
                   benchmark_results_summary <- table(benchmark_results)
                   missing_categories <- benchmark_table$benchmark_category[!(benchmark_table$benchmark_category %in% names(benchmark_results_summary))]
                   if (length(missing_categories) > 0) {
                     for (missing_category in missing_categories) {
                       benchmark_results_summary[[missing_category]] <- 0
                     }
                   }
                   
                   percent_by_category <- round(100 * benchmark_results_summary / sum(benchmark_results_summary),
                                                digits = 1)
                   
                   # Plot the histogram with benchmark info!
                   workspace$benchmark_plot <- ggplot(data = plotting_data) +
                     geom_histogram(aes(y = current_variable,
                                        fill = benchmark_results),
                                    binwidth = 1) +
                     scale_fill_manual(values = workspace$palette) +
                     labs(x = "Count of data points",
                          y = input$variable_name,
                          fill = "Benchmark status") +
                     theme(panel.grid = element_blank(),
                           panel.background = element_rect(fill = "gray95")) +
                     coord_flip()
                   
                   
                   # add hline for single plot value
                   if (input$study_plot != "" & input$plot_id_var != "") {
                     
                     # Subset input data to only include the single plot specified in the inputs
                     comp_plotting_data <- plotting_data[plotting_data[[input$plot_id_var]] == input$study_plot,]
                     
                     workspace$benchmark_plot <- workspace$benchmark_plot + 
                       geom_hline(data = comp_plotting_data,
                                  aes(yintercept = current_variable),
                                  col = "violet",
                                  size = 1.5,
                                  linetype = "dashed")
                     # add hline for comparison value
                   } else if (input$comparison_value != "" & input$comparison_checkbox != "FALSE") {
                     # Subset input data to only include the single plot specified in the inputs
                     workspace$benchmark_plot <- workspace$benchmark_plot + 
                       geom_hline(aes(yintercept = input$comparison_value),
                                  col = "violet",
                                  size = 1.5,
                                  linetype = "dashed")
                   }
                   
                   
                   output$benchmark_plot <- renderPlot(workspace$benchmark_plot)
                   # output$benchmark_plot <- renderPlotly(plotly::ggplotly(workspace$benchmark_plot))
                   
                   ggsave(filename = paste0(workspace$temp_directory, "/fig2_benchmark_plot.png"),
                          plot = workspace$benchmark_plot,
                          device = "png",
                          width = 9,
                          height = 4,
                          units = "in")
                   
                   if (input$time_checkbox != "" & input$date_field != "") {
                     
                     # Format date variable
                     plotting_data[["date"]] <- anytime::anydate(plotting_data[[input$date_field]])
                     
                     # Pull out year from date
                     plotting_data[["year"]] <- format(plotting_data[["date"]], format = "%Y")
                     
                     # Add boxplots for each year 
                     workspace$timeseries_plot <- ggplot2::ggplot(data = plotting_data) +
                       geom_boxplot(aes(x = year,
                                        y = current_variable),
                                    outlier.shape = NA) +
                       labs(x = "Year",
                            y = input$variable_name) +
                       theme(panel.grid = element_blank(),
                             panel.background = element_rect(fill = "gray95"))+
                       coord_cartesian(ylim = quantile(plotting_data[["current_variable"]], c(0.1,0.95)))
                     
                     # add hline for single plot value
                     if (input$study_plot != "" & input$plot_id_var != "") {
                       
                       # Subset input data to only include the single plot specified in the inputs
                       comp_plotting_data <- plotting_data[plotting_data[[input$plot_id_var]] == input$study_plot,]
                       
                       workspace$timeseries_plot <- workspace$timeseries_plot + 
                         geom_hline(data = comp_plotting_data,
                                    aes(yintercept = current_variable),
                                    col = "violet",
                                    size = 1.5,
                                    linetype = "dashed")
                       # add hline for comparison value
                     } else if (input$comparison_value != "" & input$comparison_checkbox != "FALSE") {
                       # Subset input data to only include the single plot specified in the inputs
                       workspace$timeseries_plot <- workspace$timeseries_plot + 
                         geom_hline(aes(yintercept = input$comparison_value),
                                    col = "violet",
                                    size = 1.5,
                                    linetype = "dashed")
                     }
                     
                     # Render Figure
                     output$timeseries_plot <- renderPlot(workspace$timeseries_plot)
                     
                     # Build caption
                     # Create the caption for the time series plot
                     timeseries_plot_caption <- paste0("Figure 3: The distribution of values for the indicator across time")
                     
                     output$timeseries_caption <- renderText(timeseries_plot_caption)
                     
                     # Save figure
                     ggsave(filename = paste0(workspace$temp_directory, "/fig3_timeseries_plot.png"),
                            plot = workspace$timeseries_plot,
                            device = "png",
                            width = 9,
                            height = 4,
                            units = "in")
                     
                   }
                   
                   updateTabsetPanel(session,
                                     inputId = "maintabs",
                                     selected = "Results") 
                   
                   # Create the caption for the quantile plot
                   if (input$study_plot != "" & input$plot_id_var != "") {
                     quantile_plot_caption <- paste0("Figure 1: The distribution of values for the indicator across ", sum(benchmark_results_summary), " data points, broken into ", length(quantiles) + 1, " quantiles. ",
                                                     paste0(paste0(paste0(names(quantiles), " of data points have a value <= "),
                                                                   round(quantiles, digits = 1),
                                                                   collapse = ", "),
                                                            ", and 100% of data points have a value <= ", round(max(current_data_vector, na.rm = TRUE), digits = 1)), ". The ", paste0(input$study_plot), " plot is shown as a pink dashed line and has a value of ", paste0(comp_plotting_data[[input$variable]]))
                   } else if (input$comparison_value != "" & input$comparison_checkbox != "FALSE") {
                     quantile_plot_caption <- paste0("Figure 1: The distribution of values for the indicator across ", sum(benchmark_results_summary), " data points, broken into ", length(quantiles) + 1, " quantiles. ",
                                                     paste0(paste0(paste0(names(quantiles), " of data points have a value <= "),
                                                                   round(quantiles, digits = 1),
                                                                   collapse = ", "),
                                                            ", and 100% of data points have a value <= ", round(max(current_data_vector, na.rm = TRUE), digits = 1)), ". The ", paste0(input$study_plot), "comparison value is shown as a pink dashed line and has a value of ", paste0(input$comparison_value))
                   } else {
                     quantile_plot_caption <- paste0("Figure 1: The distribution of values for the indicator across ", sum(benchmark_results_summary), " data points, broken into ", length(quantiles) + 1, " quantiles. ",
                                                     paste0(paste0(paste0(names(quantiles), " of data points have a value <= "),
                                                                   round(quantiles, digits = 1),
                                                                   collapse = ", "),
                                                            ", and 100% of data points have a value <= ", round(max(current_data_vector, na.rm = TRUE), digits = 1)))
                   }
                   
                   
                   output$quantile_breaks <- renderText(quantile_plot_caption)
                   
                   # Make the statements about each condition category for the caption
                   category_statements <- paste0(benchmark_results_summary, " data points (", percent_by_category, "%) ",
                                                 "fall in the benchmark category ", names(benchmark_results_summary),
                                                 collapse = ", ")
                   
                   # Create the caption for the benchmark plot
                   benchmark_plot_caption <- paste0("Figure 2: The distribution of values for the indicator across ", sum(benchmark_results_summary),
                                                    " data points classified into benchmark categories. ",
                                                    "Of the ", sum(benchmark_results_summary), " data points, ",
                                                    category_statements, ".")
                   
                   output$benchmark_summary <- renderText(benchmark_plot_caption)
                   
                   output_data <- plotting_data[, c(input$id_variables,
                                                    input$variable,
                                                    "benchmark_results")]
                   
                   # Create a .zip fle in case user wants the plots, which depends on a system call
                   # I'm not sure why we switch working directories for this, but I'm afraid to change it
                   setwd(workspace$temp_directory)
                   
                   # Write out the benchmarked data for download
                   write.csv(output_data,
                             file = "benchmarked_data.csv",
                             row.names = FALSE)
                   
                   # Write out the captions as a text file for downloading
                   writeLines(text = paste(quantile_plot_caption,
                                           benchmark_plot_caption,
                                           sep = "\n\n"),
                              con = "captions.txt")
                   
                   files_to_zip <- list.files(pattern = "\\.(png|txt|csv)$",
                                              ignore.case = TRUE)
                   
                   switch(Sys.info()[["sysname"]],
                          Windows = {
                            system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe a -tzip plots.zip ",
                                          paste(files_to_zip,
                                                collapse = " ")))
                          },
                          Linux = {
                            system(paste("zip plots %s",
                                         paste(files_to_zip,
                                               collapse = " ")))
                          })
                   if (!any(grepl(x = list.files(workspace$temp_directory), pattern = "^plots\\.(zip)|(ZIP)"))) {
                     stop("No valid .zip file called 'results' exists in the directory.")
                   }
                   setwd(workspace$original_directory)
                 }
               })
  
  ##### Download handler for the .zip file created with plots ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("plots_",
             paste0(format(Sys.Date(), "%Y-%m-%d"), "_",
                    format(Sys.time(), "%H%M", tz = "GMT")),
             ".zip")
    },
    content = function(file) {
      file.copy(paste0(workspace$temp_directory, "/plots.zip"), file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
