#=================
# HEKTARERTRAEGE
#=================
# Objective: Clean Hektarertragsdaten
# Authors: Pana & Moritz

# Main tasks: 
# Clean the table data contained in "getreide_1966-1973.pdf"

# Delete all objects in your console
rm(list = ls())

# Load libraries
require(tidyverse) # Super Funktionen Sammlung 
require(sf) # Fuer Karten und Geodaten
require(janitor) # Um Daten zu saeubern
require(broom) # Um regressionsoutput in datenform zu bringen

# Shapefile mit dt. Landkreisen 1953
#------------------------------------------
# Laden mit dem sf package
ddr_brd <- read_sf("./shapefile_1973/Germany_1973_v.1.0.shp") 
# Clean names with function from janitor package
ddr_brd <- clean_names(ddr_brd)
# Visualisieren von BRD und DDR in 1953, Faerbung ist Groesse des Kreisesplot(ddr_brd[,"area"])
# In Deutschland hat jede Gemeinde einen Code mit 8 Stellen (Wiki: Allgemeiner Gemeindeschluessel, kurz AGS)
# Die ersten 5 Stellen sind fuer die Kreise, die ersten 2 fuer Bundeslaender
# Berlin ist 11, die andern Oestlichen Bundeslaender haben 12 bis 16
# Kodiere Variable mit Bundesland Schluesseln 
ddr_brd <- ddr_brd %>%  mutate(bula_kenn = substr(kreis_kenn, 0,2))
# Nur die DDR
ddr <- ddr_brd %>% filter(bula_kenn %in% c(11:16)) 
plot(ddr[,"bula_kenn"])

# In der DDR wurde allerdings in anderes Schluesselsystem benutzt. Es gab auch die Bundeslaender nicht mehr. 
# Das System kommt aus der BRD und wurde in diesem Shapefile vom Max Planck Inst. rueckwaertsgewandt auf die DDR uebertragen. 

# FRAGE: SOLLEN WIR STADT UND LAND ZUSAMMEN FUEHREN?
# Manche Kreise haben die gleichen Namen sollen aber Standt und Land sein. Mergen! 
# ddr %>% group_by(gen) %>% filter(n()>1)
# ddr %>% filter(gen == "Neubrandenburg") %>% ggplot()+ geom_sf()
# ddr %>% filter(gen == "Suhl") %>% ggplot()+ geom_sf()
# ddr %>% filter(gen == "Cottbus") %>% ggplot()+ geom_sf()
# 
# # Merge sf polyons grouped by gen
# ddr <- ddr %>% 
#   group_by(gen, kreis_id) %>% 
#   summarise(area = sum(area), 
#             perimeter = max(perimeter), 
#             bula_kenn = first(bula_kenn)) %>% 
#   ungroup() %>% 
#   st_make_valid()
# ddr %>% filter(gen == "Neubrandenburg") %>% ggplot()+ geom_sf()
# ddr %>% filter(gen == "Suhl") %>% ggplot()+ geom_sf()

# Speichern aller Kreisnamen in einem data frame 
glimpse(ddr)
kreise <- ddr %>% st_set_geometry(NULL) %>% select(bula_kenn, gen) %>% arrange(bula_kenn)

# Das sind alle Kreise in der DDR in 1953
kreise %>% print(n=300)
# Koenntest Du die Kreisnamen weiter unten damit harmonisieren? Wird gemacht!

# Mit dieser Funktion kannst Du einen beliebigen Kreis auswaehlen und visualisieren
visualize_kreis = function(kreis) {
  ddr %>%
    mutate(label = ifelse(gen == kreis, gen, NA_character_)) %>%
    ggplot(aes(label = label, color = bula_kenn)) + geom_sf() + geom_sf_label() + theme_void()
}
visualize_kreis("Fürstenberg")

# SPREADSHEETS 
#-----------------
# Load all files in the directory
file_names <- list.files(path = "./output_1973") 
# Sort using integers inside string
file_names <- file_names[order(as.integer(gsub("\\D", "", file_names)))]
file_names

# Each file corresponds to one page of the original PDF.
# The files ending with confidence contain a measure of how confident the digiiization was.
file_names_conf <- file_names %>% str_subset("confidence") 
file_names_data <- file_names %>% str_subset("1973.csv") 

# Lade alle csv Dateien in eine Liste mit jeder Spalte als character
data <- map(file_names_data, ~ read_csv(paste0("./output_1973/", .x), 
                                        col_types = cols(.default = col_character())))

# data ist eine liste mit dataframes mit dimensionen:
map(data, ~dim(.x))




# STRUKTUR DES DOKUMENTES
#=========================
# Seiten 1-2 wurden nicht digitalisiert
# Seiten 3-4 sind pro Bezirk
# Seiten 5-ende sind pro Kreis
# Jeweils immer zwei Seiten: Einmal Weizen & Co und dann Kartoffeln & Co

# Get all even numbers in the range 5-30
evens <- function(x) subset(x, x %% 2 == 0)
even_range = evens(1:30)

# odd numbers in the range 5-30
odd_range = even_range - 1
odd_range

getreide <- data[odd_range]
kartoffeln <- data[even_range]
rm(data)

# GETREIDE
#---------
# Schauen wir uns zuerst Weizen an. 
map(getreide, ~ dim(.x))

# Erstes Blatt mit Bezirksergebnissen nehmen wir gar nicht
getreide[[1]] <- NULL


# Variablennamen 
getreide_namen <- c(
  # Davon weizen, roggen, gerste, gerste? einschliesslich Koernermais
  "getreide_1966_1970", "getreide_1972", "getreide_1973",  
  "winterweizen_1972", "winterweizen_1973", "winterrogen_1972", "winterrogen_1973", 
  "wintergerste_1972", "wintergerste_1973", "sommergerste_1972", "sommergerste_1973", 
  "winterraps_1966_1970", "winterraps_1972", "winterraps_1973") %>% 
  paste0(., "_dtha") %>% 
  c("kreis", .)

# Ersten 5 Zeilen abschneiden
getreide <- map(getreide, ~.x %>% slice(-c(1:5))) 

# Namen drauf und dann alles zusammenkleben
getreide <- map(getreide, ~.x %>% set_names(getreide_namen)) 

# Alles zusammenkleben
getreide <- bind_rows(getreide) 

# Jeden Bindestrich als missing
getreide <- getreide %>% mutate(across(everything(), ~ifelse(trimws(.x) == "-", NA_character_, .x)))

# Kommas durch punkte ersetzen
getreide <- getreide %>% mutate(across(everything(), ~gsub("\\,", ".", .x)))

# Hier und da gibt es noch zb quotation marks
getreide <- getreide %>% mutate(across(everything(), ~gsub('\\"', "", .x)))

# Alle vars als numerisch
getreide <- getreide %>% type_convert()

# Buchstaben noch weg
getreide <- getreide %>% mutate(across(getreide_1966_1970_dtha:winterraps_1973_dtha, ~gsub('[a-zA-Z]', "", .x)))

# Und wenn bindestrich irgendwo dann weg
getreide <- getreide %>% mutate(across(getreide_1966_1970_dtha:winterraps_1973_dtha, 
                                       ~ifelse(grepl("\\-", .x), NA_character_, .x)))
getreide <- getreide %>% type_convert()
# Voila!

# Schmeissen wir die Bezirk insgesamt zeilen raus
getreide <- getreide %>% filter(!grepl("bezirk|insgesamt|DDR", kreis, ignore.case = T))

# Keine Leerzeichen um das wort herum 
getreide <- getreide %>% mutate(kreis = trimws(kreis))

# Checken ob Getreide einzigartige namen hat? 
getreide %>% distinct(kreis)

# KARTOFFELN ETC
#---------------
# Schauen wir uns zuerst Weizen an. 
map(kartoffeln, ~ dim(.x))

# Erste spalte wegschmeissen im ersten  
# kartoffeln[[1]] <- kartoffeln[[1]][,-1]
# map(kartoffeln, ~ dim(.x))

# Erstes Blatt mit Bezirksergebnissen nehmen wir gar nicht
kartoffeln[[1]] <- NULL

# Variablennamen 
kartoffel_namen <- c(
  "kartoffeln_1966_1970", "kartoffeln_1972", "kartoffeln_1973",  
  "zuckerrueben_1966_1970", "zuckerrueben_1972", "zuckerrueben_1973",  
  "futterhackfruechte_1972", "futterhackfruechte_1973",  
  "gruen_silomais_1972", "gruen_silomais_1973",  
  "wiesenheu_1966_1970", "wiesenheu_1972", "wiesenheu_1973",  
  "feldfutterpflanzen_1972", "feldfutterpflanzen_1973") %>%   # Ohne Mais
  paste0(., "_dtha") %>% 
  c("kreis", .)


# Ersten 5 Zeilen abschneiden
map(kartoffeln, ~ head(.x))
kartoffeln <- map(kartoffeln, ~.x %>% slice(-c(1:4))) 

# Namen drauf und dann alles zusammenkleben
kartoffeln <- map(kartoffeln, ~.x %>% set_names(kartoffel_namen)) %>% bind_rows() 

# Jeden Bindestrich als missing
kartoffeln <- kartoffeln %>% mutate(across(everything(), ~ifelse(trimws(.x) == "-", NA_character_, .x)))

# Kommas durch punkte ersetzen
kartoffeln <- kartoffeln %>% mutate(across(everything(), ~gsub("\\,", ".", .x)))

# Alle vars als numerisch
kartoffeln <- kartoffeln %>% type_convert()

# Hier und da gibt es noch zb quotation marks
kartoffeln <- kartoffeln %>% mutate(across(everything(), ~gsub('\\"|\\*', "", .x)))

# Alle vars als numerisch
kartoffeln <- kartoffeln %>% type_convert()

# Buchstaben noch weg
kartoffeln <- kartoffeln %>% mutate(across(kartoffeln_1966_1970_dtha:feldfutterpflanzen_1973_dtha, ~gsub('[a-zA-Z]', "", .x)))

# Und wenn bindestrich irgendwo dann weg
kartoffeln <- kartoffeln %>% mutate(across(kartoffeln_1966_1970_dtha:feldfutterpflanzen_1973_dtha, 
                                           ~ifelse(grepl("\\-", .x), NA_character_, .x)))
kartoffeln <- kartoffeln %>% type_convert()

# Rest forcen wir als NA
kartoffeln <- kartoffeln %>% mutate(across(kartoffeln_1966_1970_dtha:feldfutterpflanzen_1973_dtha, 
                                           ~ as.numeric(.)))
# Schmeissen wir die Bezirk insgesamt zeilen raus
kartoffeln <- kartoffeln %>% filter(!grepl("bezirk|bazirk|insgesamt|DDR|D D R", kreis, ignore.case = T))

# Keine Leerzeichen um das wort herum 
kartoffeln <- kartoffeln %>% mutate(kreis = trimws(kreis))

# Kreis checken
kartoffeln %>% distinct(kreis)
# HARMONISIERUNG DER KREISNAMEN
#-------------------------------
# Alle Namen sammeln aus den Daten
kreis_schreibweisen <- data.frame(kreis_ertragsdaten = unique(getreide$kreis, kartoffeln$kreis)) %>% as_tibble()

# Korrespondenz zum shapefile
kreis_schreibweisen <- ddr %>% 
  distinct(gen) %>%
  mutate(kreis_shapefile = gen) %>% 
  left_join(kreis_schreibweisen, ., by = c("kreis_ertragsdaten" = "gen")) 
kreis_schreibweisen

# Korrespondenz zu ertragsdaten
ddr %>% 
  distinct(gen) %>%
  mutate(kreis_shapefile = gen) %>% 
  anti_join(., kreis_schreibweisen, ., by = c("gen"="kreis_ertragsdaten")) %>% 
  arrange(gen) %>%
  print(n=100)

# Dort wo kreis_shapefile missing ist muessen wir den richtigen Namen eintragen
kreis_schreibweisen %>% print(n = 300)
kreis_schreibweisen %>% filter(is.na(kreis_shapefile)) %>% print(n = 300)

# Hier sind die richtigen Namen
ddr %>% group_by(gen) %>% filter(n()>1)
ddr %>% distinct(gen)
ddr %>% select(gen) %>% View()

# Korrespondenz bitte per Hand matchen! 
kreis_schreibweisen <- kreis_schreibweisen %>% 
  mutate(kreis_shapefile = case_when(
    kreis_ertragsdaten == "Noubrandenburg" ~ "Neubrandenburg",
    # Wenn Frankfurt in dem Namen ist
    grepl("Frankfurt", kreis_ertragsdaten) ~ "Frankfurt an der Oder",
    kreis_ertragsdaten == "Hallo" ~ "Halle",
    kreis_ertragsdaten == "Leipsig" ~ "Leipzig",
    kreis_ertragsdaten == "Harl-Marx-Stadt" ~ "Karl-Marx-Stadt",
    kreis_ertragsdaten == "Hauptstadt Berlin" ~ "Berlin Ost",
    kreis_ertragsdaten == "Grevesmillen" ~ "Grevesmühlen",
    kreis_ertragsdaten == "Ribnitz" ~ "Ribnitz-Damgarten",
    kreis_ertragsdaten == "Rugen" ~ NA_character_,
    kreis_ertragsdaten == "Butzow" ~ "Bützow",
    kreis_ertragsdaten == "Liibz" ~ "Lübz",
    kreis_ertragsdaten == "Demnin" ~ "Demmin",
    kreis_ertragsdaten == "Noustrelitz" ~ "Neustrelitz",
    kreis_ertragsdaten == "Robol" ~ "Röbel/Müritz",
    kreis_ertragsdaten == "UeckermUnde" ~ "Ueckermünde",
    kreis_ertragsdaten == "Juterbog" ~ "Jüterbog",
    kreis_ertragsdaten == "Konigs Wusterhausen" ~ "Königs-Wusterhausen",
    kreis_ertragsdaten == "Luckenvalde" ~ "Luckenwalde",
    kreis_ertragsdaten == "Angermunde" ~ "Angermünde",
    kreis_ertragsdaten == "Eisenhüttenstadt" ~ "Fürstenberg", # 
    kreis_ertragsdaten == "Furstenwalde" ~ "Fürstenwalde",
    kreis_ertragsdaten == "Liebenwerda" ~ "Bad Liebenwerda",
    kreis_ertragsdaten == "Lubben" ~ "Lübben",
    kreis_ertragsdaten == "Kalbe/N" ~ "Kalbe (Milde)",
    kreis_ertragsdaten == "Klotze" ~ "Klötze",
    kreis_ertragsdaten == "Tangerhutte" ~ "Tangerhütte",
    kreis_ertragsdaten == "Bad Langensalza" ~ NA_character_,
    kreis_ertragsdaten == "Muhlheusen" ~ "Mühlhausen",
    kreis_ertragsdaten == "Sommerda" ~ "Sömmerda",
    kreis_ertragsdaten == "Erfurt-Stadt" ~ "Erfurt",
    kreis_ertragsdaten == "poßneck" ~ "Pößneck",
    kreis_ertragsdaten == "Bad Salsungen" ~ "Bad Salzungen",
    kreis_ertragsdaten == "Neuhaus" ~ "Neuhaus am Rennweg",
    kreis_ertragsdaten == "Schmallcalden" ~ "Schmalkalden",
    kreis_ertragsdaten == "Dippoldisvalde" ~ "Dippoldiswalde",
    kreis_ertragsdaten == "Gorlitz" ~ "Görlitz",
    kreis_ertragsdaten == "GroBenhain" ~ "Großenhain",
    kreis_ertragsdaten == "Lobau" ~ "Löbau",
    kreis_ertragsdaten == "Wer&au" ~ "Werdau",
    kreis_ertragsdaten == "Hohenstein/E." ~ "Hohenstein-Ernstthal",
    kreis_ertragsdaten == "Floha" ~ "Flöha",
    kreis_ertragsdaten == "Brand-E." ~ "Brand-Erbisdorf",
    kreis_ertragsdaten == "Rugen" ~ "Rügen",
    kreis_ertragsdaten == "Brandenburg" ~ "Brandenburg-Land",
    kreis_ertragsdaten == "Bad Langensalza" ~ "Langensalza",
    TRUE ~ kreis_shapefile
  ))

kreis_schreibweisen %>% filter(is.na(kreis_shapefile)) %>% print(n = 300)
kreis_schreibweisen
kreis_schreibweisen <- kreis_schreibweisen %>% 
  filter(!is.na(kreis_shapefile)) 
kreis_schreibweisen

# An die Ertragsdaten dranhaengen
kartoffeln <- inner_join(kreis_schreibweisen, kartoffeln, by = c("kreis_ertragsdaten" = "kreis")) %>% 
  select(-kreis_ertragsdaten) %>% 
  rename(kreis = kreis_shapefile)
kartoffeln

getreide <- inner_join(kreis_schreibweisen, getreide, by = c("kreis_ertragsdaten" = "kreis")) %>% 
  select(-kreis_ertragsdaten) %>% 
  rename(kreis = kreis_shapefile)

# Kreiskennzahl (AGS) dranhaengen
kartoffeln <- ddr %>% 
  select(gen, kreis_kenn) %>% 
  inner_join(kartoffeln, ., by = c("kreis" = "gen")) %>% 
  select(kreis, kreis_kenn, everything())

# Da fuer Cottbust Stadt und Land nicht unterschieden wird gibt es die jetzt zwei mal
kartoffeln %>% group_by(kreis) %>% filter(n()>1)

getreide <- ddr %>% 
  select(gen, kreis_kenn) %>% 
  inner_join(getreide, ., by = c("kreis" = "gen")) %>% 
  select(kreis, kreis_kenn, everything())

getreide %>% group_by(kreis) %>% filter(n()>1)
getreide

# Ertragsdaten zusammen bringen auf Ebene der gecleanten Kreisnahmen
ertraege <- full_join(getreide, kartoffeln)
ertraege %>% group_by(kreis) %>% filter(n()>1)

save(ertraege, file = "hektarertraege_1973.Rda")
