#' Standardize output of a table to an xlsx sheet 
#' 
write_tbl_to_xlsx <- function(x, wb, sheetName=NA_character_) {

  exprx <- rlang::enexpr(x)
  if (is.na(sheetName)) {sheetName <- rlang::as_string(exprx)}
  
    if (! sheetName %in% names(wb)) {
      openxlsx::addWorksheet(wb, sheetName)
    }
  
  openxlsx::writeData(wb, sheetName, x, 2, 2)
  invisible(TRUE)
}

