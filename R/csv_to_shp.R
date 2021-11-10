#' CSV to Shapefile for Earth Engine Upload
#' 
#' @author Alec L. Robitaille
#' @param out_path character; name of output folder for local assets
#' @param coords character vector; column names of coordinates in EPSG 4326.
#' @param DT data.table
#' @param name name of dataset
#' @export
#' @examples
csv_to_shp <- function(DT, out_path, name, coords = c('x_long', 'y_lat')) {
  check_col(DT, coords[1])
  check_col(DT, coords[2])
  
  out <- paste0(file.path(out_path, name), '.shp')
  
  if (DT[is.na(get(coords[1])), .N] > 0 | DT[is.na(get(coords[2])), .N] > 0) {
    warning('dropping rows with NAs in coordinate columns')
  }
  
  st_write(
    st_as_sf(
      DT,
      coords = coords
    ),
    out
  )
  
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
