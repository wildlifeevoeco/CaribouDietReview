# Extract NPP
# Alec L. Robitaille


# Packages
library(data.table)
library(wk)


# Functions
source('R/fwrite_wkt.R')


# Data
DT <- fread('input/lat.csv')


# Variables
out_path <- 'output'
name <- 'lat_ee'
coords <- c('longitude', 'latitude')


# Convert to CSV with WKT for lat/long
fwrite_wkt(DT, out_path, name, coords)