#' Import and clean raw NCEAS data
#' 
#' @noRd

get_nceas_data <- function() {
  
  tab <- read.csv(here::here("data", "raw-data", "nceas_products.csv"))
  
  
  ## Select Journal articles ----
  
  tab <- tab[tab$"type" == "Journal Article", ]
  
  
  ## Select by status ----
  
  tab <- tab[tab$"status" %in% c("published", "in press"), ]
  
 
  ## Get unique articles ----
  
  articles_id <- sort(unique(tab$"product_oid"))
  
  
  ## Merge authors by articles ----
  
  authors <- unlist(lapply(articles_id, function(article_id) {
    dat <- tab[tab$"product_oid" == article_id, ]
    dat <- dat[order(dat$"author_num", decreasing = FALSE), ]
    
    paste0(paste0(dat$"last_name", ", ", dat$"first_name"), collapse = " ; ")
  }))
  
  authors <- data.frame("product_oid" = articles_id, "author" = authors)
  
  
  ## Remove duplicated rows ----
  
  tab <- tab[!duplicated(tab$"product_oid"), ]
  
  
  ## Add authors ----
  
  tab <- merge(tab, authors, by = "product_oid", all = TRUE)
  
  
  ## Final table ----
  
  tab <- data.frame("collection" = "NCEAS",
                    "year"       = tab$"publish_year",
                    "title"	     = tab$"title",
                    "author"     = tab$"author",
                    "journal"    = tab$"journal",
                    "doi"        = tab$"series_number")
  
  
  ## Clean data ----
  
  tab$"doi" <- gsub("https://doi.org/", "", tab$"doi")
  tab$"doi" <- gsub("http://doi.org/", "", tab$"doi")
  tab$"doi" <- gsub("https://dx.doi.org/", "", tab$"doi")
  tab$"doi" <- gsub("http://dx.doi.org/", "", tab$"doi")
  tab$"doi" <- gsub("ttps://doi.org/", "", tab$"doi")
  tab$"doi" <- gsub("doi.org/", "", tab$"doi")
  tab$"doi" <- gsub("doi:", "", tab$"doi")
  tab$"doi" <- gsub("%2F", "/", tab$"doi")
  
  pos <- grep("http", tab$"doi")
  if (length(pos)) tab[pos, "doi"] <- NA
  
  tab$"year" <- gsub("in press", "", tab$"year", ignore.case = TRUE)
  
  for (i in seq_len(ncol(tab))) {
    tab[ , i] <- gsub("^\\s|\\s$", "", tab[ , i])
    tab[ , i] <- gsub("^\\s+", " ", tab[ , i])
    pos <- which(tab[ , i] == "")
    if (length(pos)) tab[pos, i] <- NA
  }
  
  
  tab$"year" <- as.numeric(tab$"year")
  
  
  ## Remove duplicates ----
  
  tab <- tab[!duplicated(tab$"title"), ]
  
  
  ## Export ----
  
  filename <- file.path("data", "derived-data", "nceas-publications.txt")
  
  write.table(tab, filename, sep = "\t", row.names = FALSE)
  
  invisible(tab)
}
