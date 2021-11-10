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


# Convert CSV to a zipped shapefile for Earth Engine upload
csv_to_zipped_shp(DT, out_path, name, coords)
