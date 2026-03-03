# Script Name : 0_fetch_latest_data.R
# Author      : Michael Parkin
# Email       : mdparkin@gmail.com
# Created     : 2025-09-15
# Revised     : 2025-10-26
#
# Description :
#   Run this script to download and unzip the CSV zip for the latest version
#   of the Data Citation Corpus Data File from Zenodo
#
# Usage :
#   source("0_fetch_latest_data.R") within R
#
# Dependencies :
#   - httr2 for fetching Zenodo API metadata and downloading
#   - Various tidyverse packages for processing
#
# Notes :
#   - The CSV zip file and corresponding CSV files will be placed and retained
#     in a folder named 'data_in' within the workspace

# Setup ----

# Required libraries
library(httr2)
library(jsonlite)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

# Zenodo record ID (first version)
record_id <- "11196859"

# Zenodo API URL
api_url <- "https://zenodo.org/api/records/"

# Zenodo URL redirect to latest version
latest_redirect <- "/versions/latest"

# Local data folder path
data_path <- "data_in"
if (!dir.exists(data_path)) dir.create(data_path)

# Functions ----

# Fetch latest record metadata (as parsed JSON list)
fetch_latest_record_meta <- function(id){
  url <- str_c(api_url, id, latest_redirect)
  resp <- request(url) %>% 
    req_perform()
  if (resp$status != 200) {
    stop("Failed to fetch latest metadata for record: HTTP status ", resp$status)
  }
  resp_body_json(resp, simplifyVector = TRUE)
}

# Download a file
download_file <- function(file_url, destfile, overwrite = FALSE) {
  if (file.exists(destfile) && !overwrite) {
    message("File already exists, skipping: ", destfile)
    return(destfile)
  }
  message("Downloading: ", file_url, " -> ", destfile)
  req <- request(file_url)
  resp <- req %>% 
    req_perform(path = destfile)
  if (resp$status != 200) {
    stop("Failed to download file: HTTP status ", resp$status)
  }
  destfile
}

# Main ----
message("Script starting")

# Fetch Zenodo metadata for latest version of dataset
message("Fetching latest Zenodo record")
meta <- fetch_latest_record_meta(record_id)

# Gather Zenodo files information
files_info <- meta$files
if (is.null(files_info) || length(files_info) == 0) {
  stop("No files found in the Zenodo record metadata")
}

# Filter for the CSV zip file(s)
target_files <- files_info %>%
  unnest_wider(links) %>% 
  filter(str_detect(self, "csv.zip"))

if (nrow(target_files) == 0) {
  stop("The CSV zip file(s) could not be found")
}

# Download CSV file(s)
message("Downloading CSV zip file(s)")
target_files %>%
  mutate(dest = file.path(data_path, key)) %>%
  walk2(.x = .$self, .y = .$dest,
    .f = ~ download_file(.x, .y), overwrite = FALSE)

message("All files downloaded for record ", 
        meta$metadata$title, " ", meta$metadata$version,
        " published on ", meta$metadata$publication_date)

# Remove existing CSV files
existing_files_to_remove <- list.files(data_path, pattern = ".csv$",
                                       recursive = FALSE, full.names = TRUE)

if (length(existing_files_to_remove) > 0) {
  message("Removing existing CSV files")
  unlink(existing_files_to_remove)
  message("Existing CSV files removed")
} 

# Unzip CSV file(s)
files_to_unzip <- list.files(data_path, pattern = ".zip$",
                             recursive = FALSE, full.names = TRUE)

if (length(files_to_unzip) > 0) {
  message("Unzipping file(s)")
  for (z in files_to_unzip){
    unzip(z, exdir = data_path)
  }
  message("File(s) unzipped")
}

# Delete zip file(s)
zip_files_to_remove <- list.files(data_path, pattern = ".zip$",
                                  recursive = FALSE, full.names = TRUE)

if (length(zip_files_to_remove) > 0) {
  message("Removing zip files")
  unlink(zip_files_to_remove)
  message("Zip files removed")
} 

message("End of script")
