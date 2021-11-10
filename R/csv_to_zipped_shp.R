#' CSV to zipped Shapefile for Earth Engine Upload
#' 
#' @author Alec L. Robitaille
#' @param out_path character; name of output folder for local assets
#' @param coords character vector; column names of coordinates in EPSG 4326.
#' @param DT data.table
#' @param name name of dataset
#' @export
#' @examples
csv_to_zipped_shp <- function(DT, out_path, name, coords = c('x_long', 'y_lat')) {
  check_col(DT, coords[1])
  check_col(DT, coords[2])
  
  out_dir <- file.path(out_path, name)
  out_zip <- paste0(out_dir, '.zip')
  out_shp <- paste0(file.path(out_dir, name), '.shp')
  
  if (!dir.exists(out_dir)) dir.create(out_dir)
  
  
  if (DT[is.na(get(coords[1])), .N] > 0 | DT[is.na(get(coords[2])), .N] > 0) {
    warning('dropping rows with NAs in coordinate columns')
    DT <- DT[!(is.na(get(coords[1])) | is.na(get(coords[2])))]
  }
  
  st_write(
    st_as_sf(
      DT,
      coords = coords
    ),
    out_shp,
    append = FALSE
  )
  
  zip::zip(
    out_zip,
    out_dir
  )
  
  unlink(out_dir, recursive = TRUE)
  
  out_zip
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
