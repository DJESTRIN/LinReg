# Script Title: utils
# Author: DJ Estrin
# Date: 07-31-2024
# Description: Common utility functions for R

#Libraries
# library(dplyr)


# Functions
# Function for standard error
stderr <- function(x) {sd(x)/sqrt(length(x))} 

# Function for aggregation. Define the function
get_mean_sem <- function(df, indep_vars, order,stop) {
    # Double check variables are in dataframe
    if (!all(indep_vars %in% colnames(df))) {stop("Some of the specified independent variables are not in the dataframe.")}
    if (!all(order %in% colnames(df))) {stop("Some of the specified order variables are not in the dataframe.")}
    if (length(indep_vars) == 0) {stop("Indep_vars should not be empty.")}
    if (length(order) == 0) {stop("Order list should not be empty.")}

    #Loop over elements of order
    aggregated_data <- df
    copy_order <- order
    for (i in seq_along(copy_order)) {
        if (i==stop){ 
            cat("Stopping at: ", order[i], "\n")
            break
        }
        order <- order[-i] #remove current element
        aggregated_data <- aggregated_data %>% group_by(across(all_of(indep_vars))) %>% summarize(across(all_of(order), mean, na.rm = TRUE), .groups = 'drop')}

    return(aggregated_data)
}


