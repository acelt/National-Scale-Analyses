# benchmark_histograms
Creating histograms of indicator values with benchmarks

This is a Shiny app powered by R and intended to help users to visualize the distribution of their data in order to decide benchmarks.

Benchmarks are expressed as inequalities to evaluate the data against and categorize the values as either "meeting" or "not meeting" the inequality. In land management, benchmarks are used to convert from continuous values (e.g. the percent cover at a sampling location provided by vegetation) into condition categories. Because benchmarks can be applied according to the ecological potential of a site, this makes comparisons between very different locations possible. For example, to compare the amount of bare ground between a desert site and a grassland site, using a single benchmark would be inappropriate because the ecological potential leading to the amount of bare ground is different; separate ecosystem-appropriate benchmarks would be applied to each, converting from the continuous values which have very different meanings due to the difference in ecological potential into directly comparable condition categories (e.g. "meeting" and "not meeting").

This tool breaks down the distribution of values for data into quantiles so that the user can get a feel for where natural benchmark breaks might be appropriate. It also allows the user to define a benchmark and see the results of applying it to their data.
