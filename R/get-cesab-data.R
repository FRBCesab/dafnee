#' Import CESAB data from Zotero local database
#' 
#' @noRd

get_cesab_data <- function(path = "~/Documents/Zotero", collection = "CESAB", 
                           filename = "") {
  
  ## Check args ----
  
  if (is.null(collection)) {
    stop("Argument 'collection' is required", call. = FALSE)
  }
  
  if (!is.character(collection) || length(collection) != 1) {
    stop("Argument 'collection' must be of length 1", call. = FALSE)
  }
  
  if (!(collection %in% c("CESAB", "SDIV", "NCEAS"))) {
    stop("Argument 'collection' must be one of 'CESAB', 'SDIV', 'NCEAS'",
         call. = FALSE)
  }
  
  
  ## Get data ----
  
  refs <- zoteror::get_zotero_data(path)
  
  
  ## Clean data ----
  
  refs <- refs[ , -c(1, 2)]
  colnames(refs)[1] <- "library"
  
  
  ## Select collection ----
  
  refs <- refs[refs$"collection" == collection, ]
  rownames(refs) <- NULL
  
  
  ## Remove duplicates ----
  
  refs <- refs[!duplicated(refs$"title"), ]
  
  
  ## Select articles ----
  
  refs <- refs[refs$"category" == "journalArticle", ]
  
  
  ## Export data ----
  
  if (filename == "") filename <- file.path("data", "derived-data", 
                                            paste0(tolower(collection), 
                                                   "-publications.txt"))
  
  write.table(refs, filename, sep = "\t", row.names = FALSE)
  
  invisible(refs)
}
