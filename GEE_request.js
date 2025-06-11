// Setting the boundaries of the region of interest
var polygon = ee.Geometry.Polygon(
  [[
    [0.9600256058726764,48.22193326486002],  
    [4.179019746497676,48.22193326486002],  
    [4.179019746497676,49.60092572450123], 
    [0.9600256058726764,49.60092572450123], // Vertex 4
    [0.9600256058726764,48.22193326486002]   // Closing the polygon
  ]]
);
// Select the image from the collection and filter it by date
var dataset = ee.ImageCollection('MODIS/061/MOD13Q1')
                  .filter(ee.Filter.date('2022-05-15', '2024-10-01'));
// Select the NDVI band
var ndvi = dataset.select('NDVI');
// Clip the NDVI image to the polygon boundaries
var clippedNdvi = ndvi.map(function(image) {
  return image.clip(polygon);  // Clip each image in the collection to the polygon
});
// Visualization parameters for NDVI
var ndviVis = {
  min: -2000,
  max: 8000,
  palette: ['grey', 'white', 'green']
};
// Center the map on the polygon and zoom to a higher level
Map.centerObject(polygon, 13);
// Add the clipped NDVI layer to the map
Map.addLayer(clippedNdvi, ndviVis, 'Clipped NDVI');

// Export each image in the ImageCollection
clippedNdvi.evaluate(function(images) {
  images.features.forEach(function(image, index) {
    // Creating an ee.Image object from the image ID
    var img = ee.Image(image.id).toInt16();
    // Retrieving the date
    var imageDate = ee.Date(image.properties['system:time_start']).format('YYYY_MM_dd').getInfo();
    // Export the image
    Export.image.toDrive({
      image: img,  
      description: 'MODIS_NDVI_IDF_250_'+ imageDate,  // name of the output file
      folder: 'GEE_Exports',  // folder on Google Drive
      fileNamePrefix: 'MODIS_NDVI_IDF_250_'+ imageDate,  
      region: polygon,  // region for clipping (optionnal here ?)
      scale: 250,  // resolution in meters
      crs: 'EPSG:4326',  
      maxPixels: 1e8  // number of pixels to export
    });
  });
});

// Exporting a single image
//Export.image.toDrive({
//   description: 'MODIS_NDVI_IDF_250_'+ imageDate,  // name of the output file
//  folder: 'GEE_Exports',  // folder on Google Drive
//  fileNamePrefix: 'MODIS_NDVI_IDF_250_'+ imageDate,  
//  region: polygon,  // region for clipping (optionnal here ?)
//  scale: 250,  // pixel resolution (in meters)
//  crs: 'EPSG:4326',  
//  maxPixels: 1e8  // Max number of pixels to export
//});


// FOR LANDSAT2  NDVI -> 30M

// Setting the boundaries of the region of interest
var polygon = ee.Geometry.Polygon(
  [[
    [0.9600256058726764,48.22193326486002],  
    [4.179019746497676,48.22193326486002],  
    [4.179019746497676,49.60092572450123], 
    [0.9600256058726764,49.60092572450123], // Vertex 4
    [0.9600256058726764,48.22193326486002]   // Closing the polygon
  ]]
);

// Select the image from the collection and filter it by date
var dataset = ee.ImageCollection('LANDSAT/COMPOSITES/C02/T1_L2_8DAY_NDVI')
                  .filter(ee.Filter.date('2022-05-15', '2024-10-01'));
// Select the NDVI band
var ndvi = dataset.select('NDVI');
// Clip the NDVI image to the polygon boundaries
var clippedNdvi = ndvi.map(function(image) {
  return image.clip(polygon);  // Clip each image in the collection to the polygon
});
print('NDVI Collection:', clippedNdvi);
// Visualization parameters for NDVI
var ndviVis = {
  min: -1,
  max: 1,
  palette: ['grey', 'white', 'green']
};
// Center the map on the polygon and zoom to a higher level
Map.centerObject(polygon, 13);
// Add the clipped NDVI layer to the map
Map.addLayer(clippedNdvi, ndviVis, 'Clipped NDVI');

// Export each image in the ImageCollection
clippedNdvi.evaluate(function(images) {
  images.features.forEach(function(image, index) {
    // Creating an ee.Image object from the image ID
    var img = ee.Image(image.id).toFloat();
    // Retrieving the date
    var imageDate = ee.Date(image.properties['system:time_start']).format('YYYY_MM_dd').getInfo();
    // Export the image
    Export.image.toDrive({
      image: img,  
      description: 'LANDSAT_NDVI_IDF_30_'+ imageDate,  // name of the output file
      folder: 'GEE_Exports',  // folder on Google Drive
      fileNamePrefix: 'LANDSAT_NDVI_IDF_30_'+ imageDate, 
      region: polygon,  // region for clipping (optionnal here ?)
      scale: 30,  // resolution in meters
      crs: 'EPSG:4326',  
      maxPixels: 1e9  // number of pixels to export
    });
  });
});

// OBTAINING THE RGE Alti AT 5M RESOLUTION
// Load the RGE Alti 5m dataset
var rge_alti5 = ee.Image("projects/sat-io/open-datasets/IGN_RGE_Alti_5m");

// Define bounding box around Paris region
var bbox = ee.Geometry.Polygon([
  [
    [1.495868998215708, 48.16770955224539],
    [3.484394388840708, 48.16770955224539],
    [3.484394388840708, 49.26222436212999],
    [1.495868998215708, 49.26222436212999],
    [1.495868998215708, 48.16770955224539]
  ]
]);

// Clip the image to the bbox
var clipped_rge = rge_alti5.clip(bbox);

// Center map display
Map.centerObject(bbox, 8);
Map.addLayer(clipped_rge, {}, 'RGE Alti 5m clipped');

// Export as GeoTIFF
Export.image.toDrive({
  image: clipped_rge,
  description: 'RGE_Alti5m_Clipped',
  scale: 5,
  region: bbox,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});
