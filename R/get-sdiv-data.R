#' Import and clean raw sDiv data
#' 
#' @noRd

get_sdiv_data <- function() {
  
  tab <- readLines(here::here("data", "raw-data", "sdiv_pubs-28_11_2022.txt"))
  tab <- tab[tab != ""]
  
  
  ## Format references ----
  
  starts <- c(grep("^Reference Type: ", tab), length(tab))
  
  for (i in 1:(length(starts) - 1)) {
    
    ## Extract chunk ----
    
    content <- tab[starts[i]:(starts[i + 1] - 1)]
    
    
    ## Extract field names ----
    
    fields  <- lapply(content, function(x) {
      out <- regmatches(x, regexpr("^([[:alpha:]]+\\s?){1,}:", x))
      ifelse(length(out), out, NA)  
    })
    
    fields <- unlist(fields)
    
    
    ## Add missing field names ----
    
    pos <- which(is.na(fields))
    
    if (length(pos) > 0) {
      for (j in pos) {
        fields[j] <- fields[j - 1]
      }
    }
    
    
    ## Clean field names ----
    
    field_s <- tolower(gsub("\\s", "_", fields))
    field_s <- gsub(":", "", field_s)
    field_s <- trimws(field_s)
    
    
    ## Extract field values ----
    
    content <- gsub("^([[:alpha:]]+\\s?){1,}:", "", content)
    content <- trimws(content)
    names(content) <- field_s
    
    
    ## Merge duplicated fields (e.g. keywords) ----
    
    pos <- which(duplicated(field_s))
    
    if (length(pos)) {
      
      dup_field_s <- unique(field_s[pos])
      
      dup_content <- NULL
      
      for (j in 1:length(dup_field_s)) {
        dup_content[j] <- paste0(content[which(field_s == dup_field_s[j])], 
                                 collapse = ", ")
        names(dup_content)[j] <- dup_field_s[j]
      }
      
      content <- content[!(names(content) %in% dup_field_s)]
      content <- c(content, dup_content)
    }
    
    
    ## Convert to data frame ----
    
    content <- data.frame(t(content))
    
    if (i == 1) {
      refs <- content  
    } else {
      refs <- dplyr::bind_rows(refs, content)
    }
  }
  
  
  ## Select articles ----
  
  refs <- refs[refs$"reference_type" == "Journal Article", ]
  
  
  ## Final table ----
  
  refs <- data.frame("collection" = "SDIV",
                     "year"       = refs$"year",
                     "title"	    = refs$"title",
                     "author"     = refs$"author",
                     "journal"    = refs$"journal",
                     "doi"        = refs$"doi")
  
  
  ## Clean data ----
  
  refs$"doi" <- gsub("https://doi.org/", "", refs$"doi")
  refs$"doi" <- gsub("http://doi.org/", "", refs$"doi")
  refs$"doi" <- gsub("https://dx.doi.org/", "", refs$"doi")
  refs$"doi" <- gsub("http://dx.doi.org/", "", refs$"doi")
  refs$"doi" <- gsub("ttps://doi.org/", "", refs$"doi")
  refs$"doi" <- gsub("doi.org/", "", refs$"doi")
  refs$"doi" <- gsub("doi:", "", refs$"doi")
  refs$"doi" <- gsub("%2F", "/", refs$"doi")
  
  for (i in seq_len(ncol(refs))) {
    refs[ , i] <- gsub("^\\s|\\s$", "", refs[ , i])
    refs[ , i] <- gsub("^\\s+", " ", refs[ , i])
    pos <- which(refs[ , i] == "")
    if (length(pos)) refs[pos, i] <- NA
  }
  
  
  refs$"year" <- as.numeric(refs$"year")
  
  
  ## Remove duplicates ----
  
  refs <- refs[!duplicated(refs$"title"), ]
  
  
  ## Export ----
  
  filename <- file.path("data", "derived-data", "sdiv-publications.txt")
  
  write.table(refs, filename, sep = "\t", row.names = FALSE)
  
  invisible(refs)
}
