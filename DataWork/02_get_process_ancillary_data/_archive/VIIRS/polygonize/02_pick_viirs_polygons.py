# Save VIIRs Polygon as Pickle Object
# Done for Faster Loading

viirs_gdf = gpd.read_file(os.path.join(DROPBOX_DIRECTORY, 'Data', 'VIIRS', 'FinalData', 'viirs_annual_polygon.geojson'))
viirs_gdf.to_pickle(os.path.join(DROPBOX_DIRECTORY, 'Data', 'VIIRS', 'FinalData', 'viirs_annual_polygon.pkl'))
