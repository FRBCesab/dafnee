## Import list of journals ----

journals <- read.delim(here::here("outputs", "list_of_journals.txt"), 
                       header = TRUE)

## Read Dafnee data ----

daf <- read.csv(here::here("data", "raw-data", 
                           "DAFNEE-surveyed-journals-2023-01-10.csv"))

colnames(daf) <- tolower(colnames(daf))
daf <- daf[ , -1]
daf <- daf[order(daf$"journal"), ]


## Create merging keys ----

journals$"parser" <- tolower(journals$"journal")
journals$"parser" <- gsub("[[:punct:]]", " ", journals$"parser")
journals$"parser" <- gsub("\\s+", " ", journals$"parser")
journals$"parser" <- gsub(" online", " ", journals$"parser")
journals$"parser" <- trimws(journals$"parser")

journals[which(journals$"parser" == "am nat"), "parser"]  <- "american naturalist"
journals[which(journals$"parser" == "the american naturalist"), "parser"]  <- "american naturalist"
journals[which(journals$"parser" == "biorxiv"), "parser"] <- "bioarxiv"
journals[which(journals$"parser" == "canadian journal of zoology revue canadienne de zoologie"), "parser"] <- "canadian journal of zoology"
journals[which(journals$"parser" == "comparative biochemistry and physiology part a"), "parser"] <- "comparative biochemistry and physiology"
journals[which(journals$"parser" == "ecol lett"), "parser"]  <- "ecology letters"
journals[which(journals$"parser" == "ecological applications supplement the science of marine reserves"), "parser"] <- "ecological applications"
journals[which(journals$"parser" == "ecological modelling special issue placing fisheries in their ecosystem context"), "parser"] <- "ecological modelling"
journals[which(journals$"parser" == "ecology special issue phylogenetic approaches to community ecology"), "parser"] <- "ecology"
journals[which(journals$"parser" == "ecology amp amp society"), "parser"] <- "ecology and society"
journals[which(journals$"parser" == "ecology amp amp amp society"), "parser"] <- "ecology and society"
journals[which(journals$"parser" == "ecosystems special feature on discontinuous structure in ecological systems"), "parser"] <- "ecosystems"
journals[which(journals$"parser" == "elementa"), "parser"] <- "elementa science of the anthropocene"
journals[which(journals$"parser" == "eos transactions american geophysical union"), "parser"] <- "eos"
journals[which(journals$"parser" == "glob chang biol"), "parser"] <- "global change biology"
journals[which(journals$"parser" == "insect conservation and diversity"), "parser"] <- "insect conservation diversity"
journals[which(journals$"parser" == "journal of the geological society"), "parser"] <- "journal of the geological society of london"
journals[which(journals$"parser" == "nat genet"), "parser"] <- "nature genetics"
journals[which(journals$"parser" == "nature clim change"), "parser"] <- "nature climate change"
journals[which(journals$"parser" == "nature ecology amp evolution"), "parser"] <- "nature ecology evolution"
journals[which(journals$"parser" == "nature ecology amp amp evolution"), "parser"] <- "nature ecology evolution"
journals[which(journals$"parser" == "neues jahrbuch fur geologie und palaontologie abhandlungen"), "parser"] <- "neues jahrbuch fur geologie und palaontologie"
journals[which(journals$"parser" == "neues jahrbuch fur geologie und palaontologie monatshefte"), "parser"] <- "neues jahrbuch fur geologie und palaontologie"
journals[which(journals$"parser" == "people and nature"), "parser"] <- "people nature"
journals[which(journals$"parser" == "philosophical transactions of the royal society of london b biological sciences"), "parser"] <- "philosophical transactions of the royal society of london series b biological sciences"
journals[which(journals$"parser" == "proceedings of the royal society of london b biological sciences"), "parser"] <- "proceedings of the royal society b biological sciences"
journals[which(journals$"parser" == "proceedings of the royal society of london series b biological sciences"), "parser"] <- "proceedings of the royal society b biological sciences"
journals[which(journals$"parser" == "plos biol"), "parser"] <- "plos biology"
journals[which(journals$"parser" == "pnas"), "parser"] <- "proceedings of the national academy of sciences of the usa"
journals[which(journals$"parser" == "proceedings of the national academy of sciences"), "parser"] <- "proceedings of the national academy of sciences of the usa"
journals[which(journals$"parser" == "trends ecol evol"), "parser"] <- "trends in ecology and evolution"

daf$"parser" <- tolower(daf$"journal")
daf$"parser" <- gsub("[[:punct:]]", " ", daf$"parser")
daf$"parser" <- gsub("\\s+", " ", daf$"parser")
daf$"parser" <- trimws(daf$"parser")

matching     <- length(which(journals$"parser" %in% daf$"parser"))
non_matching <- length(which(!(journals$"parser" %in% daf$"parser")))

100 * matching / (matching + non_matching)

daf <- daf[!duplicated(daf$"parser"), ]
daf <- daf[!duplicated(daf$"parser"), ]

journals <- merge(journals, daf[ , -1], by = "parser", all.x = TRUE, all.y = FALSE)

papers <- read.csv2(here::here("data", "derived-data", "articles_scopus_corrected.csv"))

papers <- merge(papers, journals, by = "journal", all = FALSE)

write.table(papers, here::here("outputs", "final_papers_list.txt"), 
            row.names = FALSE, sep = "\t")
