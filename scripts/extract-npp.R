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
