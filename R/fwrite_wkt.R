#' data.table::fwrite + wkt
#'
#' Converts a data.table to a CSV with geometry stored as WKT. Ready to upload to EE.
#'
#' CRS must be 4326.
#'
#' Warning, this function overwrites files a little boldly...
#'
#' @param outpath character; name of output folder for local assets
#' @param coords character vector; column names of coordinates in EPSG 4326.
#' @param DT data.table
#' @param name name of dataset
#'
#' @return
#'
#' Path to output file for passing to upload_asset, and writes out a CSV.
#' @export
#'
#' @examples
fwrite_wkt <- function(DT, outpath, name, coords = c('x_long', 'y_lat')) {
  check_col(DT, coords[1])
  check_col(DT, coords[2])
  
  out <- paste0(file.path(outpath, name), '.csv')
  
  DT[, WKT := wk::as_wkt(wk::xy(.SD[[1]], .SD[[2]])), .SDcols = coords]
  
  data.table::fwrite(DT, out)
  out
}


#' check col
#' @author Alec L. Robitaille
#' @param DT data.table
#' @param col column name
#' @param arg argument name
#' @param extra extras
check_col <- function(DT = NULL, col = NULL, arg = NULL, extra = NULL) {
  
  if (is.null(arg)) {
    it <- col
  } else {
    it <- paste0(arg, " ('", col, "')")
  }
  
  if (!(col %in% colnames(DT))) {
    stop(paste0(it, ' column not found in DT', extra))
  }
}
