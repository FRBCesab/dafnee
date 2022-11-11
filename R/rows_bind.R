#' Bind rows of multiple data.frame with different columns (case sensitive)
#' @noRd

rows_bind <- function(...) {
  
  datas <- list(...)
  
  if (length(datas) < 2) {
    stop("Please provide at least two data.frame", call. = FALSE)
  }
  
  lapply(datas, function(x) {
    if (!is.data.frame(x)) {
      stop("Please provide only data.frame", call. = FALSE)
    }
    invisible(NULL)
  })
  
  
  for (i in 2:length(datas)) {
    
    common_cols <- colnames(datas[[1]])[colnames(datas[[1]]) %in% 
                                          colnames(datas[[i]])]
    
    if (length(common_cols) == 0) {
      stop("No column in common found", call. = FALSE)
    }
    
    datas[[1]] <- rbind(datas[[1]][ , common_cols, drop = FALSE], 
                        datas[[i]][ , common_cols, drop = FALSE])
  }
  
  datas[[1]]
}
