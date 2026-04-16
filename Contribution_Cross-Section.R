#===========================================================
# BUILD CROSS-SECTION OF HARVEST DATA IN NAZI REICH 1939
#===========================================================
rm(list = ls())

library(tidyverse)
library(readODS)
library(janitor)
library(cli)
library(sf)
library(fuzzyjoin)

# Read hand collected Kreis names
kreis_names <- c(
  "1939_1940_1941_1943/R 3102_10967/kreisnamen_kartoffeln.ods",
  "1939_1940_1941_1943/R 3102_10967/kreisnamen_gerste_hafer.ods",
  "1939_1940_1941_1943/R 3102_10967/kreisnamen_mais.ods",
  "1939_1940_1941_1943/R 3102_10967/kreisnamen_roggen_weizen.ods",
  "1939_1940_1941_1943/R 3102_10967/kreisnamen_zuckerrüben.ods"
)

kartoffeln_kreise <- read_ods(kreis_names[1]) %>% rename(Seite = `...5`)
gerste_kreise <- read_ods(kreis_names[2])  %>% rename(Seite = `...5`)
mais_kreise <- read_ods(kreis_names[3])  %>% rename(Seite = `...5`)
roggen_kreise <- read_ods(kreis_names[4])  %>% rename(Seite = `...5`)
rueben_kreise <- read_ods(kreis_names[5])  %>% rename(Seite = `...5`)

# List all files extracted from scans
files <- list.files("1939_1940_1941_1943/R 3102_10967/cropped/output/", pattern = "csv", full.names = T)

# Find all files with many low confidence numbers
# files_conf <- files[grepl("confidence", files)]
# 
# data_conf <- map(files_conf, \(x) read_csv(x) %>% 
#               clean_names() %>% 
#               mutate(file_name = str_extract(x, "R_.*\\.csv")) %>% 
#               mutate_all(\(x) ifelse(x < 0.01, NA, x)) %>% 
#               # Filter if any column is below threshold
#               filter(if_any(where(is.numeric), ~ . < 60)))
# low_conf <- map(data_conf, \(x) dim(x)[1]) %>% unlist()
# data_conf[low_conf > 10]
# -> This only shows columns with many hyphens

# Drop all names with confidence
files <- files[!grepl("confidence", files)]
files

# Read files
# Map is a function to efficiently loop over an argument
# It loops over files, where every item of files is defined as x
# We first load the csv file and we then clean the variable names
data <- map(files, \(x) read_csv(x, col_types = cols(.default = col_character())) %>% 
              clean_names() %>% 
              mutate(file_name = str_extract(x, "R_.*\\.csv")))

# Add page count
data <- map(1:length(data), \(x) data[[x]] %>% 
              mutate(page_nr = x) %>% 
              mutate_all(as.character))

# Check dimensions
dims <- data.frame(ncols = map(data, \(x) dim(x)[2]) %>% unlist()) %>% 
  mutate(file_name = str_extract(files, "R_.*\\.csv"),
         row_nr = row_number())
dims

# Cleaning
data <- map(data, \(x) x %>% 
              mutate_all(trimws))

map(data, \(x) x %>% distinct(file_name))

# MATCH KREIS NAMES WITH DATA ON PAGE NUMBER
roggen <- data[1:31] %>% bind_rows()
gerste <- data[32:62] %>% bind_rows()
mais <- data[63:93] %>% bind_rows()
kartoffeln <- data[94:124] %>% bind_rows()
zuckerrueben <- data[125:155] %>% bind_rows()

# Loop over all pages
glue_data_to_kreis_names = function(df, df_kreise, page_bind_anyways = NULL) {
  
  pages <- unique(df$page_nr) # Number of pages
  
  kreis_data <- map(pages, \(page) {
    # Get Kreisnamen per page
    kreise_pg <- df_kreise %>% filter(Seite == page)
    # Get Data per page
    data_pg <- df %>% filter(page_nr == page)
    # Page
    file_name = data_pg$file_name[1]
    
    # If number of rows match, bind columns
    if (dim(kreise_pg)[1] == dim(data_pg)[1]) {
      cli_alert_success("Number of Kreise and data rows match for page {page} with name {file_name}")
      df <- bind_cols(kreise_pg, data_pg)
    }
    # If number of rows do not match, return NULL
    if (dim(kreise_pg)[1] != dim(data_pg)[1]) {
      cli_alert_danger("Number of Kreise and data rows do not match for page {page} with name {file_name}")
      cli_text(" -> # Kreise {dim(kreise_pg)[1]} vs. # Data {dim(data_pg)[1]}")
      df <- NULL
      
      if(page %in% page_bind_anyways) {
        cli_alert_warning("Binding data anyways filling NAs in bottom")
        kreise_pg <- kreise_pg %>% mutate(id = row_number())
        data_pg <- data_pg %>% mutate(id = row_number())
        df <- full_join(kreise_pg, data_pg, by = "id") %>% select(-id)
      }
      
    }
    return(df)
  })
  return(kreis_data)
}

# ROGGEN 
roggen <- glue_data_to_kreis_names(roggen, roggen_kreise)

roggen <- roggen %>% 
  bind_rows() %>% 
  select(-page_nr) %>% 
  # Add names
  set_names(c("provinz", "regierungsbezirk", "typ", "kreis", "page",
              "winter_rye_ha", "winter_rye_dz_ha", "winter_rye_dz", 
              "summer_rye_ha", "summer_rye_dz_ha", "summer_rye_dz", 
              "winter_wheat_ha", "winter_wheat_dz_ha", "winter_wheat_dz", 
              "summer_wheat_ha", "summer_wheat_dz_ha", "summer_wheat_dz", 
              "file_name")) 


# GERSTE
gerste <- gerste %>% 
  mutate(page_nr = as.numeric(page_nr) - 31)

gerste <- glue_data_to_kreis_names(gerste, gerste_kreise)

gerste <- gerste %>% 
  bind_rows() %>% 
  select(-page_nr) %>% 
  # Add names
  set_names(c("provinz", "regierungsbezirk", "typ", "kreis", "page",
              "emmer_ha", "emmer_dz_ha", "emmer_dz", 
              "winter_barley_ha", "winter_barley_dz_ha", "winter_barley_dz", 
              "summer_barley_ha", "summer_barley_dz_ha", "summer_barley_dz", 
              "oats_ha", "oats_dz_ha", "oats_dz", 
              "file_name")) 
# MENGGETREIDE & MAIS
mais <- mais %>% 
  mutate(page_nr = as.numeric(page_nr) - 62)

mais <- glue_data_to_kreis_names(mais, mais_kreise, page_bind_anyways = 13)

mais <- mais %>% bind_rows() %>% 
  select(-page_nr) %>% 
  # Add names
  set_names(c("provinz", "regierungsbezirk", "typ", "kreis", "page",
              "winter_mixed_grains_ha", "winter_mixed_grains_dz_ha", "winter_mixed_grains_dz", 
              "summer_mixed_grains_ha", "summer_mixed_grains_dz_ha", "summer_mixed_grains_dz", 
              "grain_maize_ha", "grain_maize_dz_ha", "grain_maize_dz", 
              "buckwheat_ha", "buckwheat_dz_ha", "buckwheat_dz", 
              "file_name")) 

# KARTOFFELN
# Match em
kartoffeln <- kartoffeln %>% 
  mutate(page_nr = as.numeric(page_nr) - 93)
kartoffeln

kartoffeln <- glue_data_to_kreis_names(kartoffeln, kartoffeln_kreise)

# Stack all pages on top of each other
kartoffeln <- kartoffeln %>% 
  bind_rows() %>% 
  select(-page_nr) %>% #Seitenzahl nur zum zsmführen relevant; dann gelöscht
  # Add names
  set_names(c("provinz", "regierungsbezirk", "typ", "kreis", "page",
              "other_grains_or_legumes_ha",
              "potatoes_late_ha", "potatoes_late_dz_ha", "potatoes_late_dz", 
              "potatoes_common_early_ha", "potatoes_chitted_early_ha", "potatoes_early_ha", 
              "potatoes_early_dz_ha", "potatoes_early_dz",
              "file_name"))  

# RUEBEN
zuckerrueben 
zuckerrueben <- zuckerrueben %>% 
  mutate(page_nr = as.numeric(page_nr) - 124)

zuckerrueben <- glue_data_to_kreis_names(zuckerrueben, rueben_kreise)
zuckerrueben 

zuckerrueben <- zuckerrueben %>%
  bind_rows() %>% 
  select(-page_nr) %>% #Seitenzahl nur zum zsmführen relevant; dann gelöscht
  # Add names
  set_names(c("provinz", "regierungsbezirk", "typ", "kreis", "page",
              "sugar_beets_ha", "sugar_beets_dz_ha", "sugar_beets_dz", "sugar_beets_seeds_ha",  
              "feed_beets_ha", "feed_beets_dz_ha", "feed_beets_dz", "feed_beets_seeds_ha",  
              "rutabaga_ha", "rutabaga_dz_ha", "rutabaga_dz", "rutabaga_seeds_ha",  
              "file_name"))  

harvest_1943 <- list(kartoffeln, roggen, gerste, mais, zuckerrueben)

save(harvest_1943, file = "1939_1940_1941_1943/R 3102_10967/harvest_1943_raw.rda")
