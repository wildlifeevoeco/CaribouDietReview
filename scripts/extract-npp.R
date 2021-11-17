# Extract NPP
# Alec L. Robitaille


# Packages
library(data.table)
library(sf)
library(zip)


# Functions
source('R/csv_to_zipped_shp.R')


# Data
DT <- fread('output/clean-data.csv')


# Variables
out_path <- 'output'
name <- 'clean-data'
coords <- c('longitude', 'latitude')


# Shorten column names manually (too long for shapefiles)
long_names <- setdiff(colnames(DT), coords)
short_names <- abbreviate(long_names)
setnames(DT, long_names, short_names)


# Convert CSV to a zipped shapefile for Earth Engine upload
zip_path <- csv_to_zipped_shp(DT, out_path, name, coords)

# Extract with EE (EE/Extract-NPP.js)
# ...

# Load output
npp <- fread('output/clean-data-npp.csv')

# Replace names
setnames(npp, short_names, long_names)
setnames(DT, short_names, long_names)

# Remove extra columns
npp[, `.geo` := NULL]
npp[, `system:index` := NULL]

# Why NAs?
npp[is.na(Npp), why := 'NA in NPP image']

# Merge back onto full data (with NAs in coordinates)
DT[is.na(longitude) | is.na(latitude), why := 'NA in coordinates']

# Using forbs, graminoid, etc for matching duplicated observations
#  within study*author*year*season*subspecies
m <- merge(
  npp,
  DT,
  all = TRUE,
  by = intersect(colnames(npp), colnames(DT))
)

m2 <- m[, mean(Npp, na.rm = T), by = c("author_yr", "season", "Subspecies", 
                                       "data type", "sympatric_ungulates", 
                                       "lichen", "vascular", "graminoid")]

setnames(m2, "V1", "Npp")

fwrite(m2, "output/clean-data-npp2.csv")
