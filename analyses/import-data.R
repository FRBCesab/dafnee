## Import CESAB data (from local Zotero) ----

cesab <- get_cesab_data()


## Alternative (import cleaned data frame) ----
# 
# cesab <- read.table(here::here("data", "derived-data",
#                                "cesab-publications.txt"),
#                     header = TRUE)


## Import NCEAS data (from raw file) ----

nceas <- get_nceas_data()


## Alternative (import cleaned data frame) ----
# 
# nceas <- read.table(here::here("data", "derived-data",
#                                "nceas-publications.txt"),
#                     header = TRUE)


## Merge data ----

refs <- rows_bind(nceas, cesab)

