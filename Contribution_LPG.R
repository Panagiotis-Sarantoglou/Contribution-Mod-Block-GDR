#======
# LPGs 
#=======
# Objective: Build cross-section of LPG development in 1953 from digitized files
# Authors: Pana & Moritz

# Main tasks: 
# Harmonize variable names
# Add names of Kreise when missing
# Bring all Bezirke together into one spreadsheet
# Alle Variablen zu doubles, ausser den Kreisnamen (die sind character)

# Delete all objects in your console
rm(list = ls())

# 2do: Suhl bind_cols to left_joins

# Load libraries
require(tidyverse) # Super Funktionen Sammlung 
require(sf) # Fuer Karten und Geodaten
require(readxl) # Um Excel zu lesen
require(janitor) # Um Daten zu saeubern
require(glue)
require(broom) # Um regressionsoutput in datenform zu bringen

# Shapefile mit dt. Landkreisen 1953
#------------------------------------------
# Laden mit dem sf package
ddr_brd <- read_sf("data/shapefile_1953/Germany_1953_v.1.0.shp") 
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
visualize_kreis("Jüterbog")


# SPREADSHEETS 
#-----------------
# Load all files in the directory
file_names <- list.files(path = "./data/1953-06-30/spreadsheets/", full.names = T) 
file_names

file_lpgs <- list.files(path = "./data/1953-06-30/fehlende_typen/spreadsheets", 
                        pattern = "csv", full.names = T) 
# Nur die files in denen uebernommen steht im Namen und die csv sind
file_lpgs <- file_lpgs[grepl("bernommen", file_lpgs)]

#Um LPGs einzufügen
library(readr)
library(dplyr)

# VARIABLE NAMES
#------------------
var_names <- c("kreis", "mitglieder_insg", "landarbeiter", "landarbeiter_fam", 
               "neubauern", "neubauern_fam", "altbauern", "altbauern_fam", "sonst_berufe", 
               "frauen_insg", "betriebe_insg", "betriebe_2ha", "betriebe_5ha", "betriebe_10ha", 
               "betriebe_15ha", "betriebe_20ha", "betriebe_groesser20ha", "gen_flaechen_insg", 
               "gen_flaechen_acker", "gen_flaechen_wiesen", "gen_flaechen_weiden", 
               "gen_flaechen_forsten", "ges_betreu_technisch", "ges_betreu_agronomisch", 
               "ges_betreu_tieraerztlich",  "gen_vieh_pferde", "gen_vieh_rinder_insg", 
               "gen_vieh_rinder_kuehe", "gen_vieh_rinder_zugtiere", "gen_vieh_schweine", "gen_vieh_schafe",
               "ind_flaechen_insg", "ind_flaechen_acker", "ind_flaechen_weiden", "ind_flaechen_forsten", "ind_vieh_pferde", 
               "ind_vieh_rinder_insg", "ind_vieh_rinder_zugtiere", "ind_vieh_schweine", "ind_vieh_schafe") 

#---------
# HALLE
#---------
# vollständig
read_excel(file_names[1]) 
halle <- read_excel(file_names[1], skip = 4) 

#Kommentar Pana: ehemaliges LPGs alle hier rausgenommen, alles zusammen kommt unten
# Auswahl relevanten Spalten
halle <- halle[,c(1,3:19,21,23,25,27,29:38,40,42,44,46:50)] 

#Namen der Spalten anpassen
names(halle) <- var_names 

# Korrektur der Kreisnamen
halle <- halle %>% 
  mutate(kreis = case_when( 
    kreis == "Grafenhausen" ~ "Gräfenhainichen", 
    kreis == "Saalekreis" ~ "Saalkreis", 
    kreis == "Oned Limburg" ~ "Quedlinburg", 
    kreis == "Orierfurt" ~ "Querfurt", 
    kreis == "Rosslan" ~ "Roßlau", 
    kreis == "Weissenfels" ~ "Weißenfels", 
    kreis == "Reiff" ~ "Zeitz",
    kreis == "Dessau (Stadt)" ~ "Dessau", 
    kreis == "Halle (Stadt)" ~ "Halle/Saale", 
    TRUE ~ as.character(kreis) 
  )) 
halle <- halle %>% mutate(across(where(is.character), ~na_if(., "-"))) 
halle <- halle  %>% type_convert()  
halle$bezirk <- "Halle" 

#Nachträglich einfügen der fehlenden LPGs
#Einlesen der Daten
file_lpgs[4]
halle_typen <- read_delim(file_lpgs[4], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>%
  mutate(datum = as.Date(datum, format = "%d.%m.%y")) %>% 
  mutate(kreis = ifelse(kreis == "Rosslau", "Roßlau", kreis))

anti_join(halle, halle_typen)
anti_join(halle_typen, halle)

halle <- inner_join(halle, halle_typen)

# Koenntest Du fuer diese zwei Variablen checken ob die Werte richtig sind? 
halle$ind_flaechen_insg[1:2] == c(461, 533)
halle$gen_flaechen_insg[1:2] == c(5065, 10615)
# Pana: sind richtig!

# Das hier wird unser data frame in dem wir alles sammeln
lpgs <- halle
glimpse(lpgs)

#---------
# Potsdam   
#---------
# vollständig
read_excel(file_names[2]) 
potsdam <- read_excel(file_names[2],  skip = 4) 

# Die Anzahl der LPG Typen ziehen wir raus und kleben die spaeter wieder dran
potsdam_typen <- potsdam[,c(2:5)]

# Umbenennen
names(potsdam_typen) <- c("lpg_typ_alle", "lpg_typ_1", "lpg_typ_2", "lpg_typ_3")

# Ersetze in allen Variablen - durch NA
potsdam_typen <- potsdam_typen  %>% mutate(across(where(is.character), ~na_if(., "-")))  
potsdam_typen <- potsdam_typen  %>% type_convert()

# Die restlichen Variablen
potsdam <- potsdam[,c(1,6:22,24,26,28,30,32:41,43,45,47,49:53)] 
potsdam <- potsdam %>% set_names(var_names) 
potsdam <- potsdam %>% 
  mutate(kreis = case_when( 
    kreis == 1 ~ "Belzig", 
    kreis == 2 ~ "Brandenburg-Land", 
    kreis == 3 ~ "Gransee", 
    kreis == 4 ~ "Jüterbog", 
    kreis == 5 ~ "Königs-Wusterhausen", 
    kreis == 6 ~ "Kyritz", 
    kreis == 7 ~ "Luckenwalde", 
    kreis == 8 ~ "Nauen", 
    kreis == 9 ~ "Neuruppin", 
    kreis == 10 ~ "Potsdam-Land", 
    kreis == 11 ~ "Pritzwalk", 
    kreis == 12 ~ "Oranienburg", 
    kreis == 13 ~ "Rathenow", 
    kreis == 14 ~ "Wittstock", 
    kreis == 15 ~ "Zossen", 
    kreis == 16 ~ "Brandenburg/Havel", 
    kreis == 17 ~ "Bezirk Potsdam insges", 
    TRUE ~ as.character(kreis) 
  )) 
potsdam <- potsdam %>% mutate(across(where(is.character), ~na_if(., "-"))) 
potsdam <- type_convert(potsdam) 
potsdam <- potsdam %>%  mutate(bezirk = "Potsdam") 

# Die LPG Typen ankleben an den Rest, der Reihenfolge nach
potsdam <- bind_cols(potsdam, potsdam_typen)

# Am Ende kleben wir das vertikal auf einander mit den Daten aus Halle. 
# Da die Variablen "lpg_typ_1", "lpg_typ_2", "lpg_typ_3" noch nicht in Halle sind bleibt das erstmal NA. 
lpgs <- bind_rows(lpgs, potsdam) 

#---------
# Rostock 
#---------
# vollständig
read_excel(file_names[3]) 
rostock <- read_excel(file_names[3], skip = 4) 

# Anzahl der LPG Typen
rostock_typen <- rostock[,c(2:5)]
# Ersetze in allen Variablen - durch NA
rostock_typen <- rostock_typen  %>% mutate(across(where(is.character), ~na_if(., "-")))  
rostock_typen <- rostock_typen  %>% type_convert()

# Vorsicht!!! Hier ist die Reihenfolge anders als in Potsdam. 
names(rostock_typen) <- c("lpg_typ_1", "lpg_typ_2", "lpg_typ_3", "lpg_typ_alle")

# Die restlichen Variablen
rostock <- rostock[,c(1,6:22,24,26,28,30,32:41,43,45,47,49:53)] 
names(rostock) <- var_names 
rostock <- rostock %>% 
  mutate(kreis = case_when( 
    kreis == 1 ~ "Bad Doberan", 
    kreis == 2 ~ "Bergen", 
    kreis == 3 ~ "Ribnitz-Damgarten", 
    kreis == 4 ~ "Putbus",  
    kreis == 5 ~ "Greifswald", 
    kreis == 6 ~ "Grevesmühlen", 
    kreis == 7 ~ "Grimmen", 
    kreis == 8 ~ "Rostock-Land", 
    kreis == 9 ~ "Stralsund-Land", 
    kreis == 10 ~ "Wismar-Land", 
    kreis == 11 ~ "Wolgast",  
    kreis == 12 ~ "Rostock", 
    kreis == 13 ~ "Bezirk Rostock insges", 
    TRUE ~ as.character(kreis) 
  )) 
rostock <- rostock %>% mutate(across(where(is.character), ~na_if(., "-"))) 
rostock <- type_convert(rostock) 
rostock <- rostock %>%  mutate(bezirk = "Rostock") 

# Mit den anderen Variablen zusammen binden
rostock <- bind_cols(rostock, rostock_typen)
lpgs <- bind_rows(lpgs, rostock)

# Dates
lpgs <- lpgs %>% mutate(datum = dmy(datum))

#---------------
#Neubrandenburg
#----------------
# vollständig 
file_names[4]
read_excel(file_names[4]) 
neubrandenburg <- read_excel(file_names[4], skip = 4) 

#Anmerkung Pana: auch hier habe ich alles rausgenommen, der Code von unten reicht tatsächlich
neubrandenburg <- neubrandenburg[c(1:15),c(1,3:19,21,23,25,27,29:38,40,42,44,46:50)]  
#nur die ersten 15 Zeilen benutzt; Typ 1 ignoriert 
names(neubrandenburg) <- var_names 
neubrandenburg <- neubrandenburg %>%  mutate(bezirk = "Neubrandenburg") 
neubrandenburg$sonst_berufe <- as.double(neubrandenburg$sonst_berufe)
neubrandenburg <- neubrandenburg %>% 
  mutate(kreis = case_when( 
    kreis == "Röbel" ~ "Röbel/Müritz",
    kreis == "Insgesamt" ~ "Bezirk Neubrandenburg insges", 
    TRUE ~ as.character(kreis) 
  ))

#Nachträglich einfügen der fehlenden LPGs
#Einlesen der Daten
neubrandenburg_typen <- read_delim(file_lpgs[7], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>%
  mutate(datum = as.Date(datum, format = "%d.%m.%y")) 

# Check mismatch
anti_join(neubrandenburg, neubrandenburg_typen)
neubrandenburg <- left_join(neubrandenburg, neubrandenburg_typen)

lpgs <- bind_rows(lpgs, neubrandenburg) 

#---------------
# Schwerin 
#---------------
# vollständig
file_names[5]
read_excel(file_names[5]) 
schwerin <- read_excel(file_names[5], skip = 4) 
schwerin <- schwerin[c(1:12),c(1:18,20,22,24,26,28:37,39,41,43,45:49)] 
names(schwerin) <- var_names 
schwerin <- schwerin %>% 
  mutate(kreis = case_when( 
    kreis == 1 ~ "Bützow", 
    kreis == 2 ~ "Gadebusch", 
    kreis == 3 ~ "Güstrow", 
    kreis == 4 ~ "Hagenow", 
    kreis == 5 ~ "Ludwigslust", 
    kreis == 6 ~ "Lübz", 
    kreis == 7 ~ "Parchim", 
    kreis == 8 ~ "Perleberg", 
    kreis == 9 ~ "Schwerin-Land", 
    kreis == 10 ~ "Sternberg", 
    kreis == 11 ~ "Schwerin", 
    kreis == 12 ~ "Bezirk Schwerin insges", 
    TRUE ~ as.character(kreis) 
  )) 
schwerin <- schwerin %>% mutate(across(where(is.character), ~na_if(., "-"))) 
schwerin <- type_convert(schwerin) 
schwerin <- schwerin %>%  mutate(bezirk = "Schwerin") 

#Nachträglich einfügen der fehlenden LPGs
#Einlesen der Daten
schwerin_typen <- read_delim(file_lpgs[8], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>% 
  #Datum nachträglich als solches umformatieren (geht nicht anders)
  mutate(datum = as.Date(datum, format = "%d.%m.%y"))


anti_join(schwerin, schwerin_typen)
anti_join(schwerin_typen, schwerin)
schwerin <- inner_join(schwerin_typen, schwerin)

lpgs <- bind_rows(lpgs, schwerin)

#---------------
# Magdeburg 
#---------------
# vollständig
read_excel(file_names[6]) 
magdeburg <- read_excel(file_names[6], skip = 4) 
magdeburg <- magdeburg[,c(1:18,20,22,24,26,28:37,39,41,43,45:49)] 
names(magdeburg) <- var_names 
magdeburg <- magdeburg %>% 
  mutate(kreis = case_when( 
    kreis == "Coburg" ~ "Loburg",
    kreis == "Kalbe / Milde" ~ "Kalbe (Milde)",
    kreis == "Bezirksergebnis" ~ "Bezirk Magdeburg insges", 
    TRUE ~ as.character(kreis) 
  )) 
magdeburg <- magdeburg %>% mutate(across(where(is.character), ~na_if(., "-"))) 
magdeburg <- type_convert(magdeburg) 
magdeburg <- magdeburg %>%  mutate(bezirk = "Magdeburg") 

#Nachträglich einfügen der fehlenden LPGs
#Einlesen der Daten
magdeburg_typen <- read_delim(file_lpgs[6], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>% 
  #Datum nachträglich als solches umformatieren (geht nicht anders)
  mutate(datum = as.Date(datum, format = "%d.%m.%y"))

anti_join(magdeburg, magdeburg_typen)
anti_join(magdeburg_typen, magdeburg)
magdeburg <- inner_join(magdeburg_typen, magdeburg)

lpgs <- bind_rows(lpgs, magdeburg) 






#---------------
# Karl-Marx-Stadt 
#---------------
# vollständig 
read_excel(file_names[7]) 
karl_marx_stadt <- read_excel(file_names[7], skip=4) 

# Anzahl der LPG Typen
karl_marx_stadt_typen <- karl_marx_stadt[c(1:25),c(2:4)]
# Ersetze in allen Variablen - durch NA
karl_marx_stadt_typen <- karl_marx_stadt_typen  %>% mutate(across(where(is.character), ~na_if(., "-")))  
karl_marx_stadt_typen <- karl_marx_stadt_typen  %>% type_convert()
names(karl_marx_stadt_typen) <- c("lpg_typ_1", "lpg_typ_2", "lpg_typ_3")

karl_marx_stadt <- karl_marx_stadt[c(1:25),c(1,5:21,23,25,27,29,31:40,42,44,46,48:52)] 
names(karl_marx_stadt) <- var_names 
karl_marx_stadt <- karl_marx_stadt %>% 
  mutate(kreis = case_when( 
    kreis == 1 ~ "Annaberg", 
    kreis == 2 ~ "Aue", 
    kreis == 3 ~ "Auerbach", 
    kreis == 4 ~ "Brand-Erbisdorf", 
    kreis == 5 ~ "Karl-Marx-Stadt-Land",  
    kreis == 6 ~ "Flöha", 
    kreis == 7 ~ "Freiberg", 
    kreis == 8 ~ "Glauchau", 
    kreis == 9 ~ "Stollberg", 
    kreis == 10 ~ "Hainichen", 
    kreis == 11 ~ "Hohenstein-Ernstthal", 
    kreis == 12 ~ "Marienberg", 
    kreis == 13 ~ "Oelsnitz", 
    kreis == 14 ~ "Plauen-Land", 
    kreis == 15 ~ "Reichenbach", 
    kreis == 16 ~ "Rochlitz", 
    kreis == 17 ~ "Schwarzenberg", 
    kreis == 18 ~ "Klingenthal", 
    kreis == 19 ~ "Werdau", 
    kreis == 20 ~ "Zschopau", 
    kreis == 21 ~ "Zwickau-Land", 
    kreis == 22 ~ "Karl-Marx-Stadt", 
    kreis == 23 ~ "Plauen", 
    kreis == 24 ~ "Zwickau", 
    kreis == 25 ~ "Bezirk Karl-Marx-Stadt insges", 
    TRUE ~ as.character(kreis) 
  )) 
karl_marx_stadt <- karl_marx_stadt %>% mutate(across(where(is.character), ~na_if(., "-")))  
karl_marx_stadt <- type_convert(karl_marx_stadt) 
karl_marx_stadt <- karl_marx_stadt %>%  mutate(bezirk = "Karl-Marx-Stadt") 
karl_marx_stadt <- bind_cols(karl_marx_stadt, karl_marx_stadt_typen)

#Zusammenaddieren für typ_alle
karl_marx_stadt %>% select(matches("typ")) # Checken
karl_marx_stadt <- karl_marx_stadt %>% mutate(lpg_typ_1 = ifelse(is.na(lpg_typ_1) & (!is.na(lpg_typ_2 ) | !is.na(lpg_typ_3)),  0, lpg_typ_1))
karl_marx_stadt <- karl_marx_stadt %>% mutate(lpg_typ_3 = ifelse(is.na(lpg_typ_3) & (!is.na(lpg_typ_1 ) | !is.na(lpg_typ_2)),  0, lpg_typ_3))
karl_marx_stadt <- karl_marx_stadt %>% mutate(lpg_typ_2 = ifelse(is.na(lpg_typ_2) & (!is.na(lpg_typ_1 ) | !is.na(lpg_typ_3)),  0, lpg_typ_2))
karl_marx_stadt <- karl_marx_stadt %>% mutate(lpg_typ_alle = lpg_typ_1 + lpg_typ_2 + lpg_typ_3)
karl_marx_stadt %>% select(matches("typ")) # Checken

#Wieder die 0en zu NAs
karl_marx_stadt %>% select(matches("typ"))
karl_marx_stadt <- karl_marx_stadt %>%
  mutate(
    lpg_typ_1 = na_if(lpg_typ_1, 0),
    lpg_typ_2 = na_if(lpg_typ_2, 0),
    lpg_typ_3 = na_if(lpg_typ_3, 0)
  )

lpgs <- bind_rows(lpgs, karl_marx_stadt) 

#---------------
# Gera 
#---------------
# vollständig
read_excel(file_names[8]) 
gera <- read_excel(file_names[8], skip=4) 
gera <- gera[,c(1:18,20,22,24,26,28:37,39,41,43,45:49)] 
names(gera) <- var_names 
gera <- gera %>%        
  mutate(kreis = case_when( 
    kreis == 1 ~ "Eisenberg", 
    kreis == 2 ~ "Gera-Land", 
    kreis == 3 ~ "Jena-Land", 
    kreis == 4 ~ "Lobenstein", 
    kreis == 5 ~ "Pößneck", 
    kreis == 6 ~ "Rudolstadt", 
    kreis == 7 ~ "Saalfeld", 
    kreis == 8 ~ "Schleiz", 
    kreis == 9 ~ "Stadtroda", 
    kreis == 10 ~ "Zeulenroda", 
    kreis == 11 ~ "Greiz", 
    kreis == 12 ~ "Gera", 
    kreis == 13 ~ "Bezirk Gera insges",  
    TRUE ~ as.character(kreis) 
  )) 
gera <- gera %>% mutate(across(where(is.character), ~na_if(., "-")))  
gera <- type_convert(gera) 
gera <- gera %>%  mutate(bezirk = "Gera")  

#Nachträglich einfügen der fehlenden LPGs
#Einlesen der Daten
gera_typen <- read_delim(file_lpgs[3], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>% 
  #Datum nachträglich als solches umformatieren (geht nicht anders)
  mutate(datum = as.Date(datum, format = "%d.%m.%y"))

# Check
anti_join(gera, gera_typen)
anti_join(gera_typen, gera)
gera <- inner_join(gera, gera_typen)

lpgs <- bind_rows(lpgs, gera) 

#---------------
# Frankfurt 
#---------------
# vollständig
read_excel(file_names[9]) 
frankfurt <- read_excel(file_names[9], skip=4) 

# Anzahl der LPG Typen
frankfurt_typen <- frankfurt[,c(2:5)]
# Ersetze in allen Variablen - durch NA
frankfurt_typen <- frankfurt_typen  %>% mutate(across(where(is.character), ~na_if(., "-")))  
frankfurt_typen <- frankfurt_typen  %>% type_convert()
names(frankfurt_typen) <- c("lpg_typ_1", "lpg_typ_2", "lpg_typ_3", "lpg_typ_alle")

frankfurt <- frankfurt[,c(1,6:22,24,26,28,30,32:41,43,45,47,49:53)] 
names(frankfurt) <- var_names 
frankfurt <- frankfurt %>% 
  mutate(kreis = case_when( 
    kreis == 1 ~ "Angermünde", 
    kreis == 2 ~ "Beeskow", 
    kreis == 3 ~ "Bernau", 
    kreis == 4 ~ "Eberswalde", 
    kreis == 5 ~ "Bad Freienwalde", 
    kreis == 6 ~ "Fürstenberg", 
    kreis == 7 ~ "Fürstenwalde", 
    kreis == 8 ~ "Seelow", 
    kreis == 9 ~ "Strausberg", 
    kreis == 10 ~ "Frankfurt an der Oder", 
    kreis == 11 ~ "Bezirk Frankfurt insges", 
    TRUE ~ as.character(kreis) 
  )) 
frankfurt <- frankfurt %>% mutate(across(where(is.character), ~na_if(., "-"))) 
frankfurt <- type_convert(frankfurt) 
frankfurt <- frankfurt %>%  mutate(bezirk = "Frankfurt")
frankfurt <- bind_cols(frankfurt, frankfurt_typen)
lpgs <- bind_rows(lpgs, frankfurt) 

#---------------
# Erfurt 
#---------------
# vollständig 
file_names[12]
read_excel(file_names[12]) 
erfurt <- read_excel(file_names[12], skip=4) 
erfurt %>% print(n = 40)
#Kommentar Pana: da die Zahlen für alle LPGs nur vom Oktober vorhanden sind, habe 
#ich die bisherigen Zahlen von lpg_typ_alle rausgenommen, da sie nicht übereinstimmen
# Kommentar Moritz: 
# Hier haben die Leute bei dem data entry keinen Zwischenraum gelassen zwischen den 
# Bezirkssummen pro Typ in Linien 7-9 und den Kreisdaten. Super erkannt hier, dass wir 
# den Kram ab Zeile 8 einschliesslich brauchen. Weg mit Zeilen 1-7: 
erfurt <- erfurt[-c(1:7),]

# Schmeissen wir alle Spalten raus in denen alle Zellen missing sind
erfurt <- erfurt[,colSums(is.na(erfurt))<nrow(erfurt)]

# Auch die Anzahl der LPG Typen
erfurt <- erfurt[,-2]
names(erfurt) <- var_names

# Jetzt koennen wir die Namen korregieren
erfurt <- erfurt %>%  
  mutate(kreis = case_when(  
    kreis == 8 ~ "Apolda",  
    kreis == 9 ~ "Arnstadt",  
    kreis == 10 ~ "Eisenach",  
    kreis == 11 ~ "Erfurt-Land",  
    kreis == 12 ~ "Gotha",  
    kreis == 13 ~ "Heiligenstadt",  
    kreis == 14 ~ "Langensalza",  
    kreis == 15 ~ "Mühlhausen",  
    kreis == 16 ~ "Nordhausen",
    kreis == 17 ~ "Sömmerda", 
    kreis == 18 ~ "Sondershausen",
    kreis == 19 ~ "Weimar-Land",  
    kreis == 20 ~ "Worbis", 
    kreis == 21 ~ "Erfurt", 
    kreis == 22 ~ "Weimar",  
    kreis == 23 ~ "Bezirk Erfurt insges", 
    TRUE ~ as.character(kreis)  
  )) 
erfurt <- erfurt %>% mutate(across(where(is.character), ~na_if(., "-"))) 
erfurt <- type_convert(erfurt) 
erfurt <- erfurt %>%  mutate(bezirk = "Erfurt") 

#Nachträglich einfügen der fehlenden LPGs
#Einlesen der Daten
erfurt_typen <- read_delim(file_lpgs[2], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>% 
  #Datum nachträglich als solches umformatieren (geht nicht anders)
  mutate(datum = as.Date(datum, format = "%d.%m.%y"))

anti_join(erfurt, erfurt_typen)
anti_join(erfurt_typen, erfurt)
erfurt <- inner_join(erfurt, erfurt_typen)

lpgs <- bind_rows(lpgs, erfurt) 

#---------------
# Leipzig 
#---------------
# vollständig
read_excel(file_names[13]) 
leipzig <- read_excel(file_names[13], skip=4) 

leipzig <- leipzig[,c(1,3:19,21,23,25,27,29:38,40,42,44,46:50)] 
names(leipzig) <- var_names 
leipzig <- leipzig %>% 
  mutate(kreis = case_when( 
    is.na(kreis) ~ "Bezirk Leipzig insges", 
    kreis == "Land" ~ "Leipzig-Land",  
    TRUE ~ as.character(kreis) 
  )) 
leipzig <- leipzig %>% mutate(across(where(is.character), ~na_if(., "-"))) 
leipzig <- type_convert(leipzig) 
leipzig <- leipzig %>%  mutate(bezirk = "Leipzig") 

#Nachträglich einfügen der fehlenden LPGs
#Einlesen der Daten
leipzig_typen <- read_delim(file_lpgs[5], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>% 
  #Datum nachträglich als solches umformatieren (geht nicht anders)
  mutate(datum = as.Date(datum, format = "%d.%m.%y"))

# Check
anti_join(leipzig_typen, leipzig)
anti_join(leipzig, leipzig_typen)
leipzig <- inner_join(leipzig, leipzig_typen)


lpgs <- bind_rows(lpgs, leipzig) 

#---------------
# Dresden  
#---------------
# vollständig
read_excel(file_names[14]) 
dresden <- read_excel(file_names[14],skip=4) 

#LPGs einfügen
dresden_lpgs <- read_delim(file_lpgs[1], col_types = cols(
  kreis = col_character(),
  bezirk = col_character(),
  lpg_typ_alle = col_double(),
  lpg_typ_1 = col_double(),
  lpg_typ_2 = col_double(),
  lpg_typ_3 = col_double(),
  datum = col_character()  # Erst als Charakter einlesen
)) %>% 
  # Datumsspalte jetzt erst richtig konvertieren; anders gehts nicht
  mutate(datum = as.Date(datum, format = "%d.%m.%y"))

# Normale Variablen aussuchen und benennen
dresden <- dresden[c(1:17),c(1:18,20,22,24,26,28:37,39,41,43,45:49)] 
names(dresden) <- var_names 
dresden <- dresden %>% 
  mutate(kreis = case_when( 
    kreis == 1 ~ "Bautzen", 
    kreis == 2 ~ "Bischofswerda", 
    kreis == 3 ~ "Dippoldiswalde", 
    kreis == 4 ~ "Dresden-Land", 
    kreis == 5 ~ "Freital", 
    kreis == 6 ~ "Görlitz-Land", 
    kreis == 7 ~ "Großenhain", 
    kreis == 8 ~ "Kamenz", 
    kreis == 9 ~ "Löbau", 
    kreis == 10 ~ "Meißen", 
    kreis == 11 ~ "Niesky", 
    kreis == 12 ~ "Pirna", 
    kreis == 13 ~ "Riesa", 
    kreis == 14 ~ "Sebnitz", 
    kreis == 15 ~ "Zittau", 
    kreis == 16 ~ "Dresden", 
    kreis == 17 ~ "Bezirk Dresden insges", 
    TRUE ~ as.character(kreis) 
  )) 
dresden <- dresden %>% mutate(across(where(is.character), ~na_if(., "-"))) 
dresden <- type_convert(dresden) 
dresden <- dresden %>%  mutate(bezirk = "Dresden") 

# Check
anti_join(dresden, dresden_lpgs)
anti_join(dresden_lpgs, dresden)
dresden <- inner_join(dresden, dresden_lpgs)

lpgs <- bind_rows(lpgs, dresden) 

#---------------
# Cottbus
#---------------
# vollständig
read_excel(file_names[10]) 
cottbus <- read_excel(file_names[10],skip=4) 

# Anzahl der LPG Typen
cottbus_typen <- cottbus[c(1:15),c(2:5)]
# Ersetze in allen Variablen - durch NA
cottbus_typen <- cottbus_typen  %>% mutate(across(where(is.character), ~na_if(., "-")))  
cottbus_typen <- cottbus_typen  %>% type_convert()
names(cottbus_typen) <- c("lpg_typ_alle", "lpg_typ_1", "lpg_typ_2", "lpg_typ_3")

cottbus <- cottbus[c(1:15),c(1,6:22,24,26,28,30,32:41,43,45,47,49:53)]
names(cottbus) <- var_names
cottbus <- cottbus %>% 
  mutate(kreis = case_when( 
    kreis == "Calan" ~ "Calau",
    kreis == "Luckan" ~ "Luckau",
    kreis == "Bezirk Insgesamt Davon:" ~ "Bezirk Cottbus insges", 
    TRUE ~ as.character(kreis) 
  )) 
cottbus <- cottbus %>% mutate(across(where(is.character), ~na_if(., "-"))) 
cottbus <- type_convert(cottbus) 
cottbus <- cottbus %>%  mutate(bezirk = "Cottbus") 
cottbus$ges_betreu_technisch <- gsub("/.*", "", cottbus$ges_betreu_technisch)
cottbus$ges_betreu_agronomisch <- gsub("/.*", "", cottbus$ges_betreu_agronomisch)
cottbus$ges_betreu_tieraerztlich <- gsub("/.*", "", cottbus$ges_betreu_tieraerztlich)
cottbus$ges_betreu_technisch <- as.double(cottbus$ges_betreu_technisch)
cottbus$ges_betreu_agronomisch <- as.double(cottbus$ges_betreu_agronomisch)
cottbus$ges_betreu_tieraerztlich <- as.double(cottbus$ges_betreu_tieraerztlich)
cottbus <- cottbus %>% mutate(ges_betreu_agronomisch = ifelse(kreis == "Bad Liebenwerda", 28, ges_betreu_agronomisch))
cottbus <- bind_cols(cottbus, cottbus_typen)
lpgs <- bind_rows(lpgs,cottbus)

#---------------
# Suhl
#---------------
# vollständig
read_excel(file_names[11])
suhl <- read_excel(file_names[11],skip=4)

# Anzahl der LPG Typen
suhl_typen <- suhl[,c(2:4)]
# Ersetze in allen Variablen - durch NA
suhl_typen <- suhl_typen  %>% mutate(across(where(is.character), ~na_if(., "-")))  
suhl_typen <- suhl_typen  %>% type_convert()
names(suhl_typen) <- c("lpg_typ_1", "lpg_typ_2", "lpg_typ_3")

suhl <- suhl[,c(1,5:21,23,25,27,29,31:40,42,44,46,48:52)]
names(suhl) <- var_names
suhl <- suhl %>% 
  mutate(kreis = case_when( 
    kreis == 1 ~ "Bad Salzungen", 
    kreis == 2 ~ "Hildburghausen", 
    kreis == 3 ~ "Ilmenau", 
    kreis == 4 ~ "Neuhaus am Rennweg", 
    kreis == 5 ~ "Meiningen", 
    kreis == 6 ~ "Schmalkalden", 
    kreis == 7 ~ "Sonneberg", 
    kreis == 8 ~ "Suhl", 
    kreis == 9 ~ "Bezirk Suhl insges",
    TRUE ~ as.character(kreis) 
  )) 
suhl <- suhl %>% mutate(across(where(is.character), ~na_if(., "-"))) 
suhl <- type_convert(suhl)
suhl$ges_betreu_technisch <- gsub("/.*", "", suhl$ges_betreu_technisch)
suhl$ges_betreu_agronomisch <- gsub("/.*", "", suhl$ges_betreu_agronomisch)
suhl$ges_betreu_tieraerztlich <- gsub("/.*", "", suhl$ges_betreu_tieraerztlich)
suhl <- type_convert(suhl)
suhl <- suhl %>%  mutate(bezirk = "Suhl") 
suhl <- bind_cols(suhl, suhl_typen)

# Fuellen der NAs und addieren von den Typen
suhl %>% select(matches("typ")) # Checken
suhl <- suhl %>% mutate(lpg_typ_1 = ifelse(is.na(lpg_typ_1) & (!is.na(lpg_typ_2 ) | !is.na(lpg_typ_3)),  0, lpg_typ_1))
suhl <- suhl %>% mutate(lpg_typ_2 = ifelse(is.na(lpg_typ_2) & (!is.na(lpg_typ_1 ) | !is.na(lpg_typ_3)),  0, lpg_typ_2))
suhl <- suhl %>% mutate(lpg_typ_3 = ifelse(is.na(lpg_typ_3) & (!is.na(lpg_typ_1 ) | !is.na(lpg_typ_2)),  0, lpg_typ_3))
suhl <- suhl %>% mutate(lpg_typ_alle = lpg_typ_1 + lpg_typ_2 + lpg_typ_3)
suhl %>% select(matches("typ")) # Checken

#Die 0en wieder zu NAs
suhl %>% select(matches("typ"))
suhl <- suhl %>%
  mutate(
    lpg_typ_1 = na_if(lpg_typ_1, 0),
    lpg_typ_2 = na_if(lpg_typ_2, 0),
    lpg_typ_3 = na_if(lpg_typ_3, 0)
  )

# Auf die anderen Bezirke draufpacken
lpgs <- bind_rows(lpgs,suhl)


# LPG TYPEN
lpgs <- lpgs %>% 
  mutate(across(matches("lpg_typ"), ~ ifelse(is.na(.x), 0, .x))) %>% 
  mutate(datum = if_else(is.na(datum), dmy("30-06-1953"), datum))

# Sort
lpgs <- lpgs %>% select(kreis, bezirk, names(.))

# Throw out totals
lpgs %>% filter(grepl("insges", kreis)) %>% select(kreis)
lpgs <- lpgs %>% filter(!grepl("insges", kreis))


# Anspielen der 1953 Shapefile codes
lpgs <- ddr %>% 
  st_set_geometry(NULL) %>% 
  select(kreis = gen, from_code = kreis_kenn) %>% 
  inner_join(lpgs, .) %>% 
  mutate(from_year = 1953) %>% 
  select(kreis, from_code, from_year, names(.)) %>% 
  as_tibble()

# Save
#---------------------------------------------------------------------------------
save(lpgs, file = "data/1953-06-30/lpgs.rda")
save(lpgs, file = "/home/moritz/wolke/research/lpg/data/raw/collectivization/lpgs.rda")


# Codebook
#---------------------------------------------------------------------------------
codebook <- c(
  "kreis" = "Kreisname in 1953 Max Planck Shapefile",
  "from_code" = "Kreis Code in 1953 Max Planck Shapefile",
  "bezirk" = "DDR Bezirk",
  "lpg_typ_alle" = "Anzahl der LPGs",
  "lpg_typ_1" = "Anzahl der LPGs Typ 1",
  "lpg_typ_2" = "Anzahl der LPGs Typ 2",
  "lpg_typ_3" = "Anzahl der LPGs Typ 3",
  "datum" = "Datum"
)
codebook <- data.frame(variables = names(codebook), labels = codebook)

write_csv(codebook, file = "data/1953-06-30/lpgs_codebook.csv")
glimpse(lpgs)
