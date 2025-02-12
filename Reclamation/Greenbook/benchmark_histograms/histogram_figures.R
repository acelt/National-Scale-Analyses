library(ggplot2)

# Read in data
data_path <- "C:/Users/Nelson/Documents/Projects/benchmark_histograms/terradat_query_result_20210513.csv"

data <- read.csv(data_path,
                 stringsAsFactors = FALSE)

# Look at each column and determine if it can be coerced into numeric
viable_variables <- apply(X = data,
                          MARGIN = 2,
                          FUN = function(X){
                            vector <- X
                            numeric_vector <- as.numeric(vector)
                            any(!is.na(numeric_vector))
                          })
names(data)[viable_variables]

#! What's the variable to plot?
variable_name <- "Bare.Soil.Pct"
variable_friendly_name <- "Bare soil cover (%)"

# Make the palette!
current_palette <- c("#f5bb57ff", # Mustard
                     "#f8674cff", # Orange
                     "#4a8b9fff", # Blue
                     "#685b7fff", # Purple
                     "#c95294ff", # Fuschia
                     "#f5a9c6ff", # Pink
                     "#75c6c5ff", # Teal
                     "#fd6794ff") # Hot Pink

#! What are the quantile proportions?
quantile_proportions <- c(0.25, 0.5, 0.75)
# Make sure they're in order from smallest to largest
quantile_proportions <- quantile_proportions[order(quantile_proportions)]

# Write that current variable to a new one so we can plot easily
# And make sure it's numeric, not integer or something
data[["current_variable"]] <- as.numeric(data[[variable_name]])

# Get those values as a vector
current_data_vector <- data[[variable_name]]

# Find the quantile breaks
quantiles <- quantile(current_data_vector,
                      probs = quantile_proportions,
                      na.rm = TRUE)
# Drop 0% and 100% if they're in there
quantile_indices <- !(names(quantiles) %in% c("0%", "100%"))
quantiles <- quantiles[quantile_indices]

paste("The quantile breaks are")
paste0(names(quantiles), ": ", round(quantiles, digits = 1))

# Write in the quantile that the values belong in, starting with all of them in 100%
data[["Quantile"]] <- "100%"
# Then in order from largest to smallest, assign the rest of the quantiles
for (current_quantile in names(quantiles)[length(quantiles):1]) {
  data[["Quantile"]][data[["current_variable"]] <= quantiles[current_quantile]] <- current_quantile
}

quantile_names <- c(names(quantiles), "100%")

data[["Quantile"]] <- factor(data[["Quantile"]],
                             levels = quantile_names)

# Plot!
quantile_plot <- ggplot(data = data) +
  geom_histogram(aes(y = current_variable,
                     fill = Quantile),
                 binwidth = 1) +
  scale_fill_manual(values = current_palette) +
  geom_hline(yintercept = quantiles,
             size = 1.5,
             color = "gray50") +
  labs(x = "Count of plots",
       y = variable_friendly_name) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray95")) +
  xlim(0, NA) +
  ylim(0, NA) +
  coord_flip()

quantile_plot

plotly::ggplotly(quantile_plot)


paste0("The distribution of values for the indicator across ", sum(benchmark_results_summary), " plots, broken into ", length(quantiles) + 1, " quantiles. ",
       paste0(paste0(paste0(names(quantiles), " of plots have a value <= "),
                     round(quantiles, digits = 1),
                     collapse = ", "),
              ", and 100% of plots have a value <= ", round(max(current_data_vector, na.rm = TRUE), digits = 1)))
       
       
       # Now time to do this with the theoretical benchmark
       relationship <- "<="
       benchmark_value <- 30
       benchmark_string <- paste(relationship, benchmark_value)
       
       evaluation_strings <- paste0(data$current_variable, benchmark_string)
       strings_evaluated <- sapply(X = evaluation_strings,
                                   FUN = function(X){
                                     eval(parse(text = X))
                                   })
       benchmark_results <- rep("Not Meeting",
                                times = length(strings_evaluated))
       benchmark_results[strings_evaluated] <- "Meeting"
       data[["benchmark_results"]] <- benchmark_results
       
       benchmark_results_summary <- table(benchmark_results)
       
       # In case all the plots fell into one category
       meeting_count <- benchmark_results_summary["Meeting"]
       if (is.null(meeting_count) | is.na(meeting_count)) {
         meeting_count <- 0
       }
       not_meeting_count <- benchmark_results_summary["Not Meeting"]
       if (is.null(not_meeting_count) | is.na(not_meeting_count)) {
         not_meeting_count <- 0
       }
       
       percent_meeting <- round(100 * meeting_count / sum(benchmark_results_summary),
                                digits = 1)
       percent_not_meeting = 100 - percent_meeting
       
       # Plot!
       benchmark_plot <- ggplot(data = data) +
         geom_histogram(aes(y = current_variable,
                            fill = benchmark_results),
                        binwidth = 1) +
         scale_fill_manual(values = current_palette) +
         geom_hline(yintercept = benchmark_value,
                    size = 1.5,
                    color = "gray50") +
         labs(x = "Count of plots",
              y = variable_friendly_name,
              fill = "Benchmark Status") +
         theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = "gray95")) +
         xlim(0, NA) +
         ylim(0, NA) +
         coord_flip()
       
       benchmark_plot
       
       plotly::ggplotly(benchmark_plot)
       
       
       test <- paste0("The distribution of values for the indicator across ", sum(benchmark_results_summary), " plots classified as meeting or not meeting the benchmark of ",
                      benchmark_string, ".")
       test2 <- paste0("Of the ", sum(benchmark_results_summary), " plots, ", percent_meeting, "% (", meeting_count, " plots) met the benchmark and ",
                       percent_not_meeting, "% (", not_meeting_count, " plots) did not meet the benchmark.")
       