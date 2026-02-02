rm(list = ls())
##  muss noch integriert werden
library(tidyverse)
#library(healthiar)
library(readxl)
library(janitor)
#library(purrr)

read_metadata <- function(path) {
  readxl::read_excel(path) |>
    dplyr::mutate(quelle = factor(quelle),
                  kartierungsumfang = factor(kartierungsumfang))
}

read_colnames <- function(path) {
  readr::read_csv(path, col_names = FALSE, show_col_types = FALSE) |>
    dplyr::pull(1)
}

read_one_dataset <- function(meta_row) {
  col_names <- read_colnames(meta_row$spaltennamen)
  readxl::read_excel(
    path  = meta_row$pfad,
    sheet = meta_row$tabellenblatt,
    skip  = meta_row$zeilen_weglassen,
    n_max = meta_row$zeilen_gesamt,
    col_names = col_names
  ) %>%
    mutate(geoschluessel_stellen = meta_row$geoschluessel_stellen)
}
## Expositionsdaten schön machen

lang_machen <- function(data) {
  data %>%
    select(contains("gemeinde") |
             contains("belasteter") | contains("geoschluessel")) %>%
    setNames(str_replace(names(.), "_bis_[0-9]*", "")) %>%
    pivot_longer(
      starts_with("anzahl"),
      names_sep = "_ab_",
      names_to = c("metric", "l_untergrenze"),
      values_to = "exponierte"
    ) %>%
    mutate(
      l_untergrenze = as.numeric(l_untergrenze),
      l_central = l_untergrenze + 2,
      .keep = "unused"
    ) %>%
    mutate(
      kreis = str_sub(
        gemeinde_kennziffer,
        start = geoschluessel_stellen - 5L,
        end = geoschluessel_stellen - 3L
      )
    ) %>%
    mutate(metric = str_remove(metric, "anzahl_belasteter_")) %>%
    # mutate(metric = str_replace_all(metric, "l_night", "lnight")) %>%
    # pivot_wider(names_from = metric, values_from = exponierte) %>%
    replace_na(list(exponierte = 0))
}

meta<-
  "./data/MetadatenExpositionsdaten.xlsx" %>%
  read_metadata()

for (i in 1:nrow(meta)) {
  metadatazeile <- meta[i, ]
  cat("Lese ",paste(metadatazeile),", Sheet",metadatazeile$tabellenblatt,"....\n")
  ds <- read_one_dataset(metadatazeile) 
  cat("Habe",nrow(ds)," Zeilen gelesen.")
  ds<-ds %>%
    mutate(gemeinde_kennziffer=as.numeric(gemeinde_kennziffer)) %>% 
    lang_machen() %>%
    mutate(
      bl_id        = metadatazeile$bl_id,
      quelle            = metadatazeile$quelle,
      kartierungsumfang = metadatazeile$kartierungsumfang
    )
  cat("... und56 ins lange Format formatiert.\n")
  if (i == 1) {
    data <- ds
    cat(" dataframe namens data geschaffen; hat",nrow(data),"Zeilen.\n")
  } else {
    data <- bind_rows(data, ds)
    cat(" und angehängt. Insgesamt sind es jetzt",nrow(data),"Zeilen.\n")
  }
  
}


anzahl_expositionen<-data %>% 
  group_by(name_stadt_gemeinde,quelle, kartierungsumfang, metric) %>% 
  summarise(total_exponierte = sum(exponierte),n=n())
