Readme for Marine R code 3
This code opens the original marine climate models and Cumulative Impacts model, masks out uncertain model values,  standardizes the data to a common grid, and uses the original data and common grid to calculate marine indicators.

Code 3a marine data import:
Input and output descriptions (with the "data names" used in the R code) and save locations (and "file name" examples) 
Inputs (with R code names): 
	NetCDFs: Original Model NetCDF files for salinity, temperature and pH (e.g. "NEP_SST_45"); 
	CI model: Original Cumulative Impact model (e.g. "CI")  
	Masks: Polygon Mask shape files (e.g. "NEP_mask")
	MAZ: Polygons of marine adaptive zones ("MAZ")
Input locations (with file names): 
	NetCDFs: located in model specific folders in 0_data_climate (e.g. "NEP36_MonthlyData.gdb")  
	CI model:OneDrive - DFO-MPO\0_data_spatial\Cumulative_Impacts_Pacfic_Canada.gdb
	Masks: located in model specific folders in 0_data_climate (e.g. "NEP_mask.shp")
	MAZ: OneDrive - DFO-MPO\0_data_spatial\MAZ\MAZ_Final.shp"

Outputs: 
	3a1/Surface data: Spatial data frames of each models monthly surface estimates of each variable with centroid points (e.g. 'NEP_SST'); 
	3a2/CI points: CI centroid points ("CI_points")
	3a3/Subset surface data: 3a1 masked to only include points with high confidence inside the EEZ and has a additional column (compared to containing the MAZ Acronym each point falls withing (e.g. "NEP_SST_sub")
Output locations
	3a1/Surface data: Saved as .gdb to model specific folders in 0_data_climate (e.g. "NEP36_MonthlyData.gdb") 
	3a2/CI points: "\OneDrive - DFO-MPO\0_data_spatial\CumulativeImpacts\CI_points.gdb"
	3a3/Subset surface data: All saved to "OneDrive - DFO-MPO\0_data_climate\Standardized_Marine_data\" (e.g. "BCCM_SSPH_sub.gdb")

Code description: 
The first part of this code opens NEP 36, SSC, and BCCM NetCDFs, extracts monthly averages of surface variables for the historic and future time periods, and recombines the monthly averages in new spatial data frames with surface data (3a1). The surface spatial data frames (3a1) have point data representing cell centroids from the original NetCDF files. The surface variables included are SST and SSS for all three models and SSpH for NEP36 and BCCM. The column names specify the variable/time period/month following a naming convention of Variable_TimePeriod_Month. For example "SST_H_06" column refers to the SST estimate for the Historic period for the month of June. The column named "SSPH_F_03" refers to SSpH in the future time period for March. 

SSC data required further formatting steps due to the zeros included in the original data. When mapped, the SSC points that fell on land had zeros across all monthly average columns. These zeros were turned into "NA" values so the interpolation in code 3b would not incorporate these values.  

Time Period description 
NEP36: historic 1986-2005, future 2046-2065
BCCM: historic 1981-2010, future 2041-2070
SSC: historic 1986-2005, future 2046-2065

This code also opens and extracts centroid data for the Cumulative Impacts model (CI) and saves it as a spatial data frame (3a2/CI points). 
     CI field name descriptions: 
	MarineAREA:       the area within a planning unit grid cell that is covered by water (i.e. land excluded). 
	UNIT_ID:          This unique identifier is equivalent to the PU_ID that corresponds to a management planning unit, one square kilometer in size, used by DFO Oceans Management. 
	Cumul_Impact_all: cumulative impact score representing impacts from all activities and all habitats in each PU grid cell.
	Cumul_Impact_sp:  cumulative impact score representing impacts from all activities in the shallow pelagic habitat in each PU grid cell.
	Cumul_Impact_dp:  cumulative impact score representing impacts from all activities in the deep pelagic habitat in each PU grid cell.
	Cumul_Impact_bh:  cumulative impact score representing impacts from all activities in the benthic habitat in each PU grid cell.
	Cumul_Impact_eg:  cumulative impact score representing impacts from all activities in the eelgrass habitat in each PU grid cell.
	Cumul_Impact_sr:  cumulative impact score representing impacts from all activities in the sponge reef habitat in each PU grid cell.
	Cumul_Impact_kp:  cumulative impact score representing impacts from all activities in the kelp forest habitat in each PU grid cell.

The second part of code spatially masks the model/variable data frames created in the first portion (3a1) to create spatially subsetted data frames that only include points with high confidence in modeled values (3a3). These subsetted data frames are joined to Marine Adaptive Zone polygons so that the resulting spatial dataframes (3a3) have a column called "MAZ_Acrony" containing the MAZ acronym that each point overlaps with. The masks used are specific to each model and were adapted in QGIS from the original mask polygons that came with each model. Each mask file contains a polygon that encompasses all the models' points that have high confidence and are within BC's EEZ. See mask description in powerpoint for more information on how and why they were adapted.

     MAZ acronym description:
	GStr:     Georgia Strait
	WVI:      Vancouver Island Coastal Current 
	SFj:      Johnstone & Queen Charlotte Straits
	HStr:     Hecate Strait & Queen Charlotte Sound
	WQCI:     West Graham Island
	NQCI:     North Graham Island
	NSKEst:   Nass & Skeena Estuary
	Offshore: Offshore

Code 3a CMIP import: 
Inputs: 
	CMIP 5 NetCDFs: Oringal CMIP 5 NetCDFs with specific scenarios (e.g. "CMIP_45_SST")
Input locations:
	CMIP 5 NetCDFs: \houtmann\OneDrive - DFO-MPO\0_data_climate\Marine_CMIP5
Outputs: 

Description: 
This code opens the CMIP 5 data, puts the data into useful spatial data frames that are summarized by different groups. Data was downloaded from https://climate-scenarios.canada.ca/index.php?page=sea-surface-data for 3 different surface variables (SSpH, SST, SSS), 3 different scenarios (historic, RCP 4.5, RCP 8.5), four different models (ensemble (median, min, max), CanESM2, IPSL-CM5A-LR, MPI-ESM-LR), with monthly for every year within the scenarios year range. The year range for future scenarios (RCP 4.5 and 8.5) is 2006-2100, while the historic scenario had monthly estimates between 1900-2005. 

The code starts by opening the models and reforming the data into dataframes with the "RCMIP5" package. Each unique combination of model, variable, and scenario are made into data frames so there is a total of 18 data frames per variable (6 models (including the ensemble median, min, and max models) * 3 scenarios). These data frames only include years 2040-2070, data points between 210-240 degrees of longitude, 45-60 degrees of latitude, and have columns for year, month, season, model, and scenario. The values for temperature are converted from kelvin to celcius during the data frame creation as well. 

Seasonal definitition:
Winter = Jan-March
Spring = April - June
Summer = July - September
Fall = Oct - December  

After the dataframes are formed, they are bound together using "bind_rows" into larger data frames specific to each variable, so all SST data are in one dataframe. Currently, one data frame has the main CMIP5 models CMIP5_SST= Ensemble median, CanESM2, IPSL-CM5A-LR, MPI-ESM-LR, and another has the ensemble model median, min, and max models. These larger data frames are turned into spatial data frames with the "st_as_sf" function and pointing to the "lat" and "lon" columns for coordinates and specifying that the coordinates are in crs=4326. After, the spatial data frames are converted to crs= 3005 to remain consistent with the other Marine R codes. 
TO CONTINUE... 

Code 3b: Standarizing marine data 
Input and output descriptions (with the "data names" used in the R code) and save locations (and "file name" examples) 
Inputs (with R code names): 
	3a1/Surface data: Spatial data frames of each models monthly surface estimates of each variable with centroid points (e.g. NEP_SST); 
	3a2/CI points: CI centroid points ("CI_points")
	Masks: Polygon Mask shape files (e.g. "NEP_mask")
	MAZ: Polygons of marine adaptive zones ("MAZ")
Input locations (with file names): 
	3a1/Surface data: Saved as .gdb to model specific folders in 0_data_climate (e.g. "NEP36_MonthlyData.gdb") 
	3a2/CI points: "\OneDrive - DFO-MPO\0_data_spatial\CumulativeImpacts\CI_points.gdb"
	Masks: located in model specific folders in 0_data_climate (e.g. "NEP_mask.shp") and the EEZ layer is located in 0_data_spatial/BC_EEZ/BC_EEZ.shp
	MAZ: OneDrive - DFO-MPO\0_data_spatial\MAZ\MAZ_Final.shp"
Outputs: 
	3b1/Standardized Surface data: Standarized vector grids with model surface data and CI data, cropped to the extent of each model, and contain a column referencing the MAZ each cell overlaps with (e.g. "NEP_SST_cropped")
Output locations: 
	3b1/Standardized Surface data: Saved to "\OneDrive - DFO-MPO\0_data_climate\Standardized_Marine_data" (e.g. "NEP_SST_cropped")

This code takes the spatial data frames with all model points created in the first part of code (3a1), interpolates the points to a standard grid, crop the grids to the extent of the model using masks, and join the resulting grids to MAZ polygons. The 3b code follows these steps: 
1) Load the surface data (3a1), CI points(3a2), and masks/extent polygons   
2) Load an interpolation function ("point2rast") and associated function ("nnfit") from PACEA
3) Interpolate the surface data using the nearest neighbor interpolation "point2rast" function. Specify parameters so that a maximum of 4 neighbors are considered, cell size is equal to that of the original netCDF file (see below), and the extent is similar to that of the original NetCDF points.

Resolution of original models:
	BCCM:  3km 	
	NEP36: 3km
	SSC:   0.5km
	CI:    1km

4) Resample the interpolated surfaces to a standarized grid based on the BCCM model.The BCCM model was used as the standard as it has the broadest resolution (3km). The nearest neighbour method was used in the resample function because we wanted local values to have the greatest influence. 
5) The resampled surface data are masked with model-specific masks and converted into a spatial feature object. These objects are named with and ending of "_cropped" (e.g. "NEP_SST_cropped"). 
6) The "_cropped" spatial feature objects are joined to MAZ polygons so they gain a new column called "MAZ_Acrony" containing the MAZ acronmym that each grid cell overlaps. 
7) Save the "_cropped" data to "\OneDrive - DFO-MPO\0_data_climate\Standardized_Marine_data" (e.g. "NEP_SST_cropped.gdb")

Code 3c: Marine indicators
Inputs: 
	3a3/Subset surface data: 3a1 masked to only include points with high confidence inside the EEZ and has a additional column (compared to containing the MAZ Acronym each point falls withing (e.g. "NEP_SST_sub")
Input locaitons: 
	3a3/Subset surface data and CI data: All saved to "OneDrive - DFO-MPO\0_data_climate\Standardized_Marine_data\" (e.g. "BCCM_SSPH_sub.gdb")
Outputs: No outputs other than the graphs and maps generated in the code
Output locations: Not saved anywher 
 
The subsetted data created in 3a are used to calculate marine indicators based on MAZs. These files already have a MAZ column, and have been masked to the area that high certainty in the modeled results. Indicators may change, but currently, there is Spring difference, Mean spring projected temperature, Decadal Rate of change, mean CI score, Projected absolute values, Historic annual average, z score, and seasonality. Each indicator was calculated for each variable/model combination. 
Spring difference: 1) caluculated spring average for future and historic senarios, 2) subtracted the future spring average from the historic spring average 3) calculated the mean difference for each variable for each MAZ, 4) merge data back into a table to present results
Mean spring projected temperature: 1) calcuated Spring average for the future scenario, 2) summarize projected values by calculating the mean projected values for each MAZ, 3) merge data back into a table to present results


