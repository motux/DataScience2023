library(httr)
library(tibble)
library(jsonlite)
library(dplyr)
library(tidyverse)

# - PARAMETRI -
# rangeYears intervallo di anni in cui scarica i dati dei SET, parte sempre dall'anno scorso e va indietro di rangeYears
# Esempio 10 Anno corrente 2023 parte dal 2022 e torna indietro fino al 2013 (compreso)
rangeYears = 10

str_filter_params_start ="{'pageSize':500,'pageNumber':%i,'year':'"
str_filter_params_end = "'}"
# API BRICKSET
# Scarico dei dati dal API di brickset.com
# Per informazioni: https://brickset.com/article/52664/api-version-3-documentation
# https://brickset.com/api/v3.asmx
str_login_url = "https://brickset.com/api/v3.asmx/login?"
str_getYears_url = "https://brickset.com/api/v3.asmx/getYears?"
str_getSets_url = "https://brickset.com/api/v3.asmx/getSets?"

# Lista configurazione api login
# apiKey: valore copiato in fase di registrazione ed abilitazione iniziale
# https://brickset.com/tools/webservices/requestkey
# username e password sono quelle per accedere al sito
li_login_config = list(
  apiKey="",
  username="",
  password=""
)

#Import del prezzo delle scatole NA da importazione brickset

tb_importSetsPrice = as_tibble(read.csv(file = "tot_sets_null.csv",header = TRUE,sep = ";",dec = ".",stringsAsFactors = FALSE)) %>%
  select(setID,availability,LEGOCom.DE.retailPrice)

# - FUNZIONI -
# FUNZIONE PRINCIPALE API Brickset
# Restituisce sempre un tibble in base al url che chiami
# ARGUMETS
# url: indirizzo https del API di brickset
# get_list: lista di configurazione
# RETURN
# Un tibble contente i dati altrimenti due colonne, primo valore KO e secondo valore la risposta della request

get_tb_api = function (url, get_list){
  request = GET(url,query = get_list)
  
  if (request$status_code == 200){
    response = content(request, as = "text", type = "application/json", encoding = "UTF-8")
    json_response = fromJSON(response, flatten = TRUE) %>% as_tibble()
  } else {
    json_response = tibble( Request = "KO", Result = request$status_code )
  }
  
  return(json_response)
}


# INIZIO SCRIPT

# CHIAMATA API BRICKSET: login fondamentale per la HASH, altrimenti non puoi fare le altre chiamate
# https://brickset.com/api/v3.asmx?op=login
tb_login = get_tb_api(str_login_url, li_login_config)
# Lista di configurazione api Years per scaricare il numero di set per anno
# apiKey: valore copiato in fase di registrazione ed abilitazione iniziale
# https://brickset.com/tools/webservices/requestkey
li_getYears_config = list(
  apiKey=li_login_config$apiKey,
  theme=""
)
# CHIAMATA API BRICKSET:  getYears
# https://brickset.com/api/v3.asmx?op=getYears
tb_getYears = get_tb_api(str_getYears_url, li_getYears_config)

# Creazione di una lista anno->numero set usciti
nYears = length(tb_getYears$years$year)
li_getYears = list()
for (i in 1:nYears) {
  # read the setCount (Numero di set usciti nell'anno)
  li_getYears[tb_getYears$years$year[i]] = tb_getYears$years$setCount[i]
}
# Creazione della stringa params per API getsets

## Stringa di partenza con l'ultimo anno
str_filter_year = as.character(names(li_getYears[2]))
tot_set = li_getYears[[2]];

## Costruzione stringa filtro degli anni inferiore all'ultimo anno per il numero rangeYears
for (i in 3:(1+rangeYears)){
  str_lastYear_range =as.character(as.integer(names(li_getYears[i])))
  str_filter_year = paste(str_lastYear_range,str_filter_year,sep=",")
  tot_set = tot_set +  li_getYears[[i]]
}

pageNumber=as.integer(tot_set / 500)+1

tb_tot_sets = tibble()

for (p in 1:pageNumber){
  str_filter_params = paste(sprintf(str_filter_params_start,p),str_filter_year,str_filter_params_end,sep = "")
  li_getSets_config = list(
    apiKey=li_login_config$apiKey,
    userHash=tb_login$hash,
    params=str_filter_params
  )
  # # CHIAMATA API BRICKSET: getSets
  # https://brickset.com/api/v3.asmx?op=getSets
  # filter(category != 'Gear', theme != 'Collectable Minifigures', pieces >0, !(is.na(LEGOCom.DE.retailPrice))) %>%
  # mutate(ppDE = as.double(LEGOCom.DE.retailPrice,4) / as.double(pieces,4) ))
  tb_getSets = get_tb_api(str_getSets_url, li_getSets_config)
  
  
  if (tb_getSets$status[1] != "error"){
    tb_tot_sets = rbind (tb_tot_sets, tb_getSets$sets %>%
                               select(setID, number, name, year, theme, themeGroup, subtheme, category, packagingType, availability, image.thumbnailURL, bricksetURL, ageRange.min, minifigs, pieces, LEGOCom.DE.retailPrice, LEGOCom.DE.dateFirstAvailable, lastUpdated, collections.ownedBy, collections.wantedBy) %>%
                               filter(!availability %in% c('Promotional','Not sold') & !theme %in% c('Collectable Minifigures','Books') & !subtheme %in% c('Magazine Gift','Magazine gift','Gift with Purchase') & pieces > 0) %>%
                               mutate(minifigs = ifelse(is.na(minifigs),0,minifigs)) %>%
                               left_join(tb_importSetsPrice, by='setID') %>%
                               rename(retailPrice.api = LEGOCom.DE.retailPrice.x, retailPrice.cus = LEGOCom.DE.retailPrice.y, availability.cus = availability.y, availability.api = availability.x, dateFirstAvailable = LEGOCom.DE.dateFirstAvailable) %>%
                               filter(!availability.cus %in% c('Promotional','Not sold')) %>%
                               arrange(year,number) %>%
                               mutate(pp = ifelse(is.na(retailPrice.api),ifelse(is.na(retailPrice.cus),0,as.double(retailPrice.cus,4) / as.double(pieces,4)),as.double(retailPrice.api,4) / as.double(pieces,4)))
    )
  } else {
    view(tb_getSets)
    break
  }
}

tb_tot_sets_NA = tb_tot_sets %>%
  filter(pp == 0) %>%
  arrange(year,number)

tb_sets_PP = tb_tot_sets %>%
  select( number, name, year, theme, themeGroup, subtheme, category,packagingType, availability.api, ageRange.min, minifigs, pieces, retailPrice.api, retailPrice.cus, pp, collections.ownedBy, collections.wantedBy) %>%
  mutate(retailPrice = ifelse(is.na(retailPrice.api),retailPrice.cus,retailPrice.api)) %>%
  rename(availability = availability.api, ageRange = ageRange.min )

col_order = c("number","name","year","theme","themeGroup","subtheme","category","packagingType","availability","ageRange", "minifigs", "pieces", "retailPrice", "pp", "collections.ownedBy", "collections.wantedBy")
tb_sets_PP = tb_sets_PP[, col_order]

write.csv(tb_tot_sets_NA, "tot_tibble_sets_na.csv")
write.csv(tb_sets_PP, "tot_tibble_sets.csv")
view(tb_tot_sets)

