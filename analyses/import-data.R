## Import CESAB data (from local Zotero) ----

# cesab <- get_cesab_data()


## Alternative (import cleaned data frame) ----

cesab <- read.table(here::here("data", "derived-data",
                               "cesab-publications.txt"),
                    header = TRUE)


## Import NCEAS data (from raw file) ----

# nceas <- get_nceas_data()


## Alternative (import cleaned data frame) ----

nceas <- read.table(here::here("data", "derived-data",
                               "nceas-publications.txt"),
                    header = TRUE)


## Import SDIV data (from raw file) ----

# sdiv <- get_sdiv_data()


## Alternative (import cleaned data frame) ----

sdiv <- read.table(here::here("data", "derived-data",
                              "sdiv-publications.txt"),
                    header = TRUE)


## Merge data ----

refs <- rows_bind(nceas, cesab, sdiv)


## Get journals ----

journals <- data.frame("journal" = sort(unique(refs$"journal")))


## Export list of journals ----

write.table(journals, here::here("outputs", "list_of_journals.txt"), 
            row.names = FALSE, sep = "\t")
