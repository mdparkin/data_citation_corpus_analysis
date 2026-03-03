# Script Name : 3_top_aff_metadata.R
# Author      : Michael Parkin
# Email       : mdparkin@gmail.com
# Created     : 2025-11-05
# Revised     : 2025-11-05
#
# Description :
#   Run this script to generate a CSV file containing data citations from
#   selected affiliations (via ROR ID) and include OpenAlex subject
#   classifications
#
# Usage :
#   Having already generated the processed RDS file via 1_process_csv.R, 
#   source("3_top_aff_metadata.R") within R
#
# Dependencies :
#   - Various tidyverse packages for processing
#   - httr2 for querying the OpenAlex API
#
# Notes :
#   - Querying the OpenAlex API takes some time (~0.5 hours);
#     could look to perform the requests in parallel to save time

# Setup ----

# Required libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(httr2)

# Local data folder out path
data_out_path <- "data_out"

# Set ROR IDs of interest
ror_chosen <- tibble(
  ROR = c('00jmfr291', 
          '013q1eq08', 
          '052gg0110'),
  affiliation = c("University of Michigan",
                  "Fudan University",
                  "University of Oxford")
  )

# Set date range of interest
year_lower <- ymd('2013-01-01')
year_upper <- ymd('2024-12-31')

# Identify processed RDS file
rds_file <- list.files(path = data_out_path,
                       pattern = "\\.rds$",
                       full.names = TRUE)

# Functions ----
openalex_subjects <- function(doi){
  message("Fetching OpenAlex subjects for ", doi)
  req <- request(str_c("https://api.openalex.org/works/", doi,
                       "?select=topics"))
  resp <- tryCatch(
    req_perform(req),
    error = function(e) return(NULL)
  )
  if (is.null(resp)){
    warning("Failed to fetch OpenAlex metadata for record: ", doi)
    return(NA)
  }
  if (resp$status_code != 200){
    stop("Failed to fetch OpenAlex metadata for record: ", doi, ". HTTP status ", 
         resp$status_code)
  }
  meta <- tibble(topic = resp_body_json(resp)$topics) %>% 
    unnest_wider(topic)
  if (length(meta) < 1){
    message("No OpenAlex topics found for: ", doi)
    return(NULL)
  }
  meta <- meta %>% 
    unnest_longer(subfield) %>% 
    unnest_longer(field) %>%
    unnest_longer(domain) %>%
    filter(subfield_id == "display_name",
           field_id == "display_name",
           domain_id == "display_name")
  tbl <- tibble(
    openAlexTopics = str_c(unique(meta$display_name), collapse = '; '),
    openAlexSubfields = str_c(unique(meta$subfield), collapse = '; '),
    openAlexFields = str_c(unique(meta$field), collapse = '; '),
    openAlexDomains = str_c(unique(meta$domain), collapse = '; ')
    )
  return(tbl)
  }

openalex_ror_to_id <- function(ror){
  message("Fetching OpenAlex internal ID for RORID:", ror)
  req <- request(str_c("https://api.openalex.org/institutions/ror:", ror,
                       "?select=lineage"))
  resp <- req_perform(req)
  if (resp$status_code != 200){
    stop("Failed to fetch OpenAlex metadata for record: ", ror, ". HTTP status ", 
         resp$status_code)
  }
  openalex_id <- resp_body_json(resp)$lineage[[1]] %>% 
    str_split_1('/') %>% 
    tail(1) %>% 
    tolower()
  return(as.character(openalex_id))
}

openalex_pub_stats <- function(ror, min_date, max_date){
  message("Fetching publication counts for RORID:", ror)
  openalex_id <- openalex_ror_to_id(ror)
  years <- year(min_date):year(max_date)
  combined <- tibble()  # create empty tibble
  for (y in years){
    req <- request(str_c("https://api.openalex.org/works?group_by=type&per_page=200&filter=authorships.institutions.lineage:", 
                         openalex_id, ",publication_year:", y))
    resp <- req_perform(req)
    if (resp$status_code != 200){
      stop("Failed to fetch OpenAlex counts for RORID:", ror, " in year ", y, ". HTTP status ", 
           resp$status_code)
    }
    meta <- tibble(pubYear = y,
                   meta = resp_body_json(resp)$group_by) %>% 
      unnest_wider(meta) %>% 
      filter(key_display_name %in% c('article', 'review', 'preprint')) %>% 
      select(pubYear, 'publicationType' = key_display_name, count)
    combined <- combined %>% 
      bind_rows(meta)
  }
  return(combined)
  }

# Main ----

## Data citations with OpenAlex subjects ---- 
data <- readRDS(rds_file)

# Filter for affiliations of interest via ROR ID, then by date range
data_ror <- data %>% 
  filter(!is.na(affiliationsROR)) %>% 
  filter(!is.na(publishedDate), publishedDate >= year_lower,
         publishedDate <= year_upper) %>% 
  mutate(affiliationsROR = str_extract_all(affiliationsROR, 
                                           "0[a-z|0-9]{6}[0-9]{2}")) %>% 
  unnest_longer(affiliationsROR) %>% 
  filter(affiliationsROR %in% ror_chosen$ROR)

rm(data) # Free some memory
  
# Add ROR affiliation name and OpenAlex subjects
openalex_data <- data_ror %>% 
  left_join(., ror_chosen, 
            by = c('affiliationsROR' = 'ROR')) %>% 
  mutate(openAlex = map(publication, openalex_subjects)) %>%
  filter(!is.na(openAlex)) %>% 
  unnest_wider(openAlex) %>% 
  select(c(dataset, publication, publishedDate, source, affiliation,
         subjects, openAlexTopics, openAlexSubfields, openAlexFields, 
         openAlexDomains))

# Save the new subjects dataframe as a CSV file
write_csv(openalex_data,
          str_c(data_out_path, '/', today(), '-openAlex-subjects.csv'))

## OpenAlex publication stats for chosen affiliations ----
pub_stats <- ror_chosen %>% 
  mutate(stats = pmap(list(ROR, year_lower, year_upper),
         openalex_pub_stats)) %>% 
  unnest(stats)

# Save the publication counts as a CSV file
write_csv(pub_stats,
          str_c(data_out_path, '/', today(), '-openAlex-pub-counts.csv'))
