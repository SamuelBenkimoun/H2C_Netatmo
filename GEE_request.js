//GEE SCRIPT TO EXTRACT NDVI VALUES FROM MODIS IMAGERY AT 250M, FOR A SAMPLE REGION IN PARIS
// Setting the boundaries of the region of interest (rectangle)
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
                  .filter(ee.Filter.date('2022-05-31', '2024-10-01'));
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
      fileNamePrefix: 'clipped_ndvi_',  
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
