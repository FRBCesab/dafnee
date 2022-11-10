## Get CESAB data (from local Zotero) ----

# get_data(path = "~/Documents/Zotero", collection = "CESAB")


## Import CESAB papers metadata ----

cesab <- read.table(here::here("data", "raw-data", "cesab-publications.txt"), 
                    header = TRUE)


## Check for duplicates ----

any(duplicated(cesab$"title"))


## Select articles ----

cesab <- cesab[cesab$"category" == "journalArticle", ]


## Get journals names ----

cesab_journals_n <- sort(table(cesab$"journal"))
cesab_journals   <- sort(names(cesab_journals_n))
