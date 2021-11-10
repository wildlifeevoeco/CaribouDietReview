# Extract NPP
# Alec L. Robitaille


# Packages
library(data.table)
library(sf)
library(zip)


# Functions
source('R/csv_to_zipped_shp.R')


# Data
DT <- fread('input/lat.csv')


# Variables
out_path <- 'output'
name <- 'lat_ee'
coords <- c('longitude', 'latitude')


# Shorten column names manually (too long for shapefiles)
long_names <- grep('year', colnames(DT), value = TRUE)
short_names <- gsub('_of|_data_collection', '', long_names)
setnames(DT, long_names, short_names)


# Convert CSV to a zipped shapefile for Earth Engine upload
zip_path <- csv_to_zipped_shp(DT, out_path, name, coords)

# Extract with EE (EE/Extract-NPP.js)
# ...

# Load output
npp <- fread('output/lat-ee-npp.csv')

# Replace names
setnames(npp, short_names, long_names)

# Remove extra columns
npp[, `.geo` := NULL]
npp[, `system:index` := NULL]

# Why NAs?
npp[is.na(Npp), why := 'NA in NPP image']

# Merge back onto full data (with NAs in coordinates)
DT[is.na(longitude) | is.na(latitude), why := 'NA in coordinates']

