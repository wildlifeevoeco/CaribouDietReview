# Extract NPP
# Alec L. Robitaille


# Packages
library(data.table)
library(wk)

# Data
DT <- fread('input/lat.csv')

# Variables
out_path <- 'output'
name <- 'lat_ee'
coords <- c('long', 'lat')

# Convert to CSV with WKT for lat/long
fwrite_wkt(DT, out_path, name, coords)