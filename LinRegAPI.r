#Dependencies
library(optparse)

# Source scripts from LinReg repo
source("utils.r")
source("gghelper.r")
source("LinReg.r")

# Command line options:
CL_inputs <- list(
  make_option(c("--csv_file"), type = "character", default = NULL, help = "Path to the CSV file"),
  make_option(c("--xlsx_file"), type = "character", default = NULL, help = "Path to the CSV file"),
  make_option(c("--sql_database"), type = "character", default = NULL, help = "Path to the CSV file")
)

# Create pipeline function
main <- function(file_path){
    cat('Read File path')
    # Read the file and put into dataframe
    # Clean dataframe and get the summary data => heirarchy of dataframe is needed. 
    # Run model on data from data frame -> model formula needed, else use AIC values or model comparisons to find best fit model?
    # Generate general graphs for model performance
    # Generate general graphs for data
    # Save model results and graphs into common document markdown?
    # Save the object (attributes) as a file
}

# Parse command-line options
opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)

# Access the options
csv_file <- opts$csv_file
xlsx_file <- opts$xlsx_file
sql_database <- opts$sql_database

# Determine if datawas given
if (is.null(csv_file) && is.null(xlsx_file) && is.null(sql_database)) {
  stop("Error: path to data is required. No csv, xlsx or sql database files were given")} 
else {
main(csv_file)
}

#