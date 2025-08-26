# Air-photo-mapping
Data and code supporting the publication "Travers-Smith et al. (2025). Long-term vegetation change across the Canadian forest-tundra ecotone using historic air photos and satellite time-series."

Datasets:
01_transect_plots.csv = 300m grid cells used in photo interpretation. Attributes for air photo id and georeference accuracy (RMS), historical percent cover of forest, shrub, graminoid/herbaceous, bare ground and water land cover types, percent change in land cover, proportion of each cell covered by Landsat structural change classes, treeline position (distance and whether cell is above or below the treeline). Percent cover vales represent categorical ranges described in percent_cover_class_codes.txt. 

02_pixel_samples.csv = 30m pixels sampled to assess agreement between photo interpreted change in forest and shrub expansion with Landsat classification. Change is binary [0,1] for no increase vs increase in cover. 

photo_plots.shp = shapefile of the locations of each photo interpretation grid cell and attributes from "transect_plots.csv".   

Code: 
R-code provided replicates all key results and figures from the publication. 

01_summary_statistcs = summarize change in 300m photo interpretation grids and compare historic percent cover of forest, shrub and herbaceous vegetation with Landsat classes. 

02_glmm = Generalized linear mixed effects models to calculate the probability of forest and shrub cover increases relative to treeline position. 

03_pixel_correlations = Confusion matrices for 30m pixel level correlation between photo interpretation and Landsat classes.  

