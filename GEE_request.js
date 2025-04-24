// Setting the boundaries of the region of interest
var polygon = ee.Geometry.Polygon(
  [[
    [2.328717645633982, 48.81328561163421],  
    [2.39369147681074, 48.81328561163421],  
    [2.39369147681074, 48.835549858691415], 
    [2.328717645633982, 48.835549858691415], // Vertex 4
    [2.328717645633982, 48.81328561163421]   // Closing the polygon
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
      description: 'Clipped_NDVI_Paris_13_'+ imageDate,  // name of the output file
      folder: 'GEE_Exports',  // folder on Google Drive
      fileNamePrefix: 'clipped_ndvi_'+ imageDate,  
      region: polygon,  // region for clipping (optionnal here ?)
      scale: 250,  // resolution in meters
      crs: 'EPSG:4326',  
      maxPixels: 1e8  // number of pixels to export
    });
  });
});

// Exporting a single image
//Export.image.toDrive({
//  image: clippedNdvi,  // The image to export
//  description: 'Clipped_NDVI_Paris13',  // A name for the exported file
//  folder: 'GEE_Exports',  // (Optional) Folder in your Google Drive
//  fileNamePrefix: 'clipped_ndvi',  // (Optional) Prefix for the filename
//  //region: polygon,  // region to export (clipping based on the polygon)
//  scale: 250,  // pixel resolution (in meters)
//  crs: 'EPSG:4326',  
//  maxPixels: 1e8  // Max number of pixels to export
//});


// FOR LANDSAT2  NDVI -> 30M

// Setting the boundaries of the region of interest
var polygon = ee.Geometry.Polygon(
  [[
    [2.328717645633982, 48.81328561163421],  
    [2.39369147681074, 48.81328561163421],  
    [2.39369147681074, 48.835549858691415], 
    [2.328717645633982, 48.835549858691415], // Vertex 4
    [2.328717645633982, 48.81328561163421]   // Closing the polygon
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
      description: 'Clipped_NDVI_Paris_13_Landsat'+ imageDate,  // name of the output file
      folder: 'GEE_Exports',  // folder on Google Drive
      fileNamePrefix: 'clipped_ndvi_Landsat'+ imageDate,  
      region: polygon,  // region for clipping (optionnal here ?)
      scale: 30,  // resolution in meters
      crs: 'EPSG:4326',  
      maxPixels: 1e9  // number of pixels to export
    });
  });
});
