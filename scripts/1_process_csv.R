# Script Name : 1_process_csv.R
# Author      : Michael Parkin
# Email       : mdparkin@gmail.com
# Created     : 2025-09-15
# Revised     : 2025-10-26
#
# Description :
#   Run this script to process the unzipped CSV files, creating a new dataframe
#   with only the required fields for analysis
#
# Usage :
#   source("1_process_csv.R") within R
#
# Dependencies :
#   - readr to read in the CSV files
#   - Various tidyverse packages for processing
#
# Notes :
#   - The dataframe will be saved as a new dated RDS file named
#     [date]-data-processed.rds in the data_out folder

# Setup ----

# Required libraries
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# Local data folder in path
data_in_path <- "data_in"

# Local data folder out path
data_out_path <- "data_out"

# List of CSV files to process
csv_files <- list.files(path = data_in_path,
                        pattern = "\\.csv$",
                        full.names = TRUE)

# Main ----
data <- read_csv(csv_files, progress = TRUE)

# Remove unnecessary fields
data_processed <- data %>% 
  select(-c(id, created, updated))

# Save the new dataframe as an RDS file
saveRDS(data_processed,
        str_c(data_out_path, '/', today(), '-data-processed.rds'))
