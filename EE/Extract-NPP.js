// Caribou foraging: extract NPP
// Alec Robitaille
// November 2021

 
// Functions ===================================================================
// Function to grab date from image and add it as a band
function addDates(img) {
  var date = img.date();
  return img.addBands(ee.Image([date.get('year')]).rename(['year'])).float();
}


// Function to sample an image in each region of supplied geometry
function sampleregions (im) {
	return(im.reduceRegions(points, ee.Reducer.mean(), 30)
           .copyProperties(im));
}

// Images ======================================================================
// MOD17A3HGF.006: Terra Net Primary Production Gap-Filled Yearly Global 500m
var npp = ee.ImageCollection('MODIS/006/MOD17A3HGF');

// Features ====================================================================
var points = ee.FeatureCollection('users/robitalec/WEEL/Caribou-foraging/lat_ee');

// Process images ==============================================================
npp = npp.map(addDates);


// Sample images ===============================================================
// Sample images using our geometry
var sample = npp.map(sampleregions)
                .flatten();


// Check output ================================================================
print(sample.limit(10));
print(sample.size());

// Export ======================================================================
Export.table.toDrive({
  collection: sample,
  description: 'lat-ee-npp',
  folder: 'Caribou-foraging'
});
