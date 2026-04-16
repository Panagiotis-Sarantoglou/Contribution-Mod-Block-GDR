#===========================================================
# MATCHING OF KREIS NAMES BETWEEN HARVEST DATA TOPICS
#===========================================================
1940
# Workflow: 
rm(list = ls())

library(tidyverse)
library(readODS)
library(janitor)
library(cli)
library(sf)
library(fuzzyjoin)
library(leaflet)

# Functions
source(file = "1939_1940_1941_1943/matching_helper_functions.R")

# HARVEST DATA
#==================
load("1939_1940_1941_1943/R 3102_10926/harvest_1940_raw.rda")


# DUPLICATE KREIS NAMES IN DIFFERENT PROVINCES
# Duplicate names in districts
harvest_1940[[1]] %>% group_by(kreis) %>% filter(n() > 1) %>% print(n = 500)
harvest_1940[[2]] %>% group_by(kreis) %>% filter(n() > 1) %>% print(n = 500)
harvest_1940[[3]] %>% group_by(kreis) %>% filter(n() > 1) %>% print(n = 500)
harvest_1940[[4]] %>% group_by(kreis) %>% filter(n() > 1) %>% print(n = 500)
# -> Will be cleaned by hand!

# Bind all Kreisnames together
kreis_names <- bind_rows(
  select(harvest_1940[[1]], provinz, regierungsbezirk, kreis),
  select(harvest_1940[[2]], provinz, regierungsbezirk, kreis),
  select(harvest_1940[[3]], provinz, regierungsbezirk, kreis),
  select(harvest_1940[[4]], provinz, regierungsbezirk, kreis)) %>% 
  clean_district_name(df = ., kreis_var = "kreis") %>% 
  arrange(kreis_string) %>% 
  # Some cleaning
  mutate(kreis_string = trimws(gsub("hansestadt", "", kreis_string))) %>%
  group_by(kreis_string) %>%
  mutate(nr = n()) %>% 
  ungroup() %>% 
  mutate(kreis_clean = kreis_string) 

kreis_names %>% 
  select(-provinz) %>% 
  filter(nr != 4) %>% 
  print(n = 500)
kreis_names <- kreis_names %>% 
  mutate(kreis_clean = case_when(
    kreis_string == "aurich ostfriesland" ~ "aurich",
    kreis_string == "bartenstein" ~ "bartenstein ostpr",
    kreis_string == "dramburg" ~ "dramsburg",
    kreis_string == "friedberg" & regierungsbezirk == "Hessen" ~ "friedberg hessen",
    kreis_string == "gmünd" & regierungsbezirk == "Jagstbezirk" ~ "gmünd jagstbezirk",
    kreis_string == "lindau a d isar" ~ "landau a d isar",
    kreis_string == "gießen" ~ "giessen",
    kreis_string == "gießen stadt" ~ "giessen stadt",
    kreis_string == "waidhofen an der thaya" ~ "waidhofen an der moldau",
    TRUE ~ kreis_string
  )) %>% 
  group_by(kreis_clean) %>% 
  mutate(nr = n()) %>% 
  ungroup()

kreis_names %>% 
  select(-provinz) %>% 
  filter(nr != 4) %>% 
  print(n = 500)

# UNIFORMIZE PROVINZ AND REGIERUNGSBEZIRK OVER CLEAN NAMES
kreis_names <- kreis_names %>% 
  select(data_kreis_clean = kreis_clean, data_kreis_orig = kreis, 
         data_provinz_orig = provinz, data_regierungsbezirk_orig = regierungsbezirk) %>% 
  group_by(data_kreis_clean) %>% 
  mutate(id = row_number()) %>%
  mutate(data_regierungsbezirk = ifelse(id == 1, data_regierungsbezirk_orig, NA), 
         data_provinz = ifelse(id == 1, data_provinz_orig, NA)) %>%
  fill(c(data_regierungsbezirk, data_provinz), .direction = "downup") %>% 
  ungroup() %>% 
  distinct(data_kreis_clean, data_provinz, data_regierungsbezirk,
           data_provinz_orig, data_regierungsbezirk_orig, data_kreis_orig) 
kreis_names

# DEFINE STADT & LAND
kreis_names_stadt <- kreis_names %>% 
  distinct(data_kreis_clean,  data_regierungsbezirk) %>% 
  mutate(data_kreis_clean_stadt = trimws(gsub("\\bstadt\\b", "", data_kreis_clean))) %>% 
  group_by(data_kreis_clean_stadt) %>% 
  mutate(nr = n()) %>% 
  mutate(stadt_land = ifelse(grepl("\\bstadt\\b",data_kreis_clean), "stadt", NA_character_)) %>%
  fill(stadt_land, .direction = "downup") %>% 
  ungroup()




# !!! All remaining duplicates are either Stadt or Land
kreis_names_stadt %>% 
  filter(nr > 1) %>% 
  print(n = 500)

kreis_names_stadt <- kreis_names_stadt %>% 
  filter(nr > 1) %>% 
  mutate(data_kreis_clean_stadt_land = ifelse(grepl("\\bstadt\\b",data_kreis_clean) & nr > 1, data_kreis_clean, 
                                              paste0(data_kreis_clean, " land"))) %>% 
  select(data_kreis_clean, data_kreis_clean_stadt_land, data_regierungsbezirk)
kreis_names_stadt %>% 
  print(n = 200)


# Add back to kreis_names
kreis_names <- kreis_names %>% 
  left_join(kreis_names_stadt) %>% 
  mutate(data_kreis_clean = ifelse(is.na(data_kreis_clean_stadt_land), 
                                   data_kreis_clean, data_kreis_clean_stadt_land)) %>% 
  select(-data_kreis_clean_stadt_land) %>% 
  rename(provinz = data_provinz_orig, regierungsbezirk = data_regierungsbezirk_orig, kreis = data_kreis_orig) 


# LINK BACK TO HARVEST TO UNIFORMIZE DISTRICT NAMES
# Sanity checks
anti_join(harvest_1940[[1]], kreis_names)
anti_join(harvest_1940[[2]], kreis_names)
anti_join(harvest_1940[[3]], kreis_names)
anti_join(harvest_1940[[4]], kreis_names)

harvest_1940[[1]] <- inner_join(harvest_1940[[1]], kreis_names) %>% 
  select(-provinz, -regierungsbezirk, -kreis, -typ, -page, -file_name) %>%
  select(matches("data"), names(.)) 
harvest_1940[[2]] <- inner_join(harvest_1940[[2]], kreis_names) %>% 
  select(-provinz, -regierungsbezirk, -kreis, -typ, -page, -file_name) %>%
  select(matches("data"), names(.)) 
harvest_1940[[3]] <- inner_join(harvest_1940[[3]], kreis_names) %>% 
  select(-provinz, -regierungsbezirk, -kreis, -typ, -page, -file_name) %>%
  select(matches("data"), names(.)) 
harvest_1940[[4]] <- inner_join(harvest_1940[[4]], kreis_names) %>% 
  select(-provinz, -regierungsbezirk, -kreis, -typ, -page, -file_name) %>%
  select(matches("data"), names(.)) 

#Joining with `by = join_by(provinz, regierungsbezirk, kreis)`
harvest_1940[[1]] %>% 
  distinct(data_kreis_clean) 


harvest_1940 <- harvest_1940[[1]] %>% 
  full_join(harvest_1940[[2]]) %>% 
  full_join(harvest_1940[[3]]) %>% 
  full_join(harvest_1940[[4]]) 

harvest_1940 %>% dim()
harvest_1940 %>% distinct(data_kreis_clean) %>% dim()

harvest_1940 %>% 
  arrange(data_kreis_clean) %>% 
  group_by(data_kreis_clean) %>% 
  filter(n() > 1) %>% 
  print(n = 200)

save(harvest_1940, file = "1939_1940_1941_1943/R 3102_10926/harvest_1940_clean_names.rda")
