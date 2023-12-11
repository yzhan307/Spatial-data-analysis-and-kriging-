# Spatial-data-analysis-and-kriging-
Spatial data analysis and data interpolation 

This project utilizes the Argo dataset from location 2, containing 2500 observations across 7 variables. 7 variables include longitude, latitude, temperature, salinity, pressure (depth), profile number, and distance from a reference location. We filtered the dataset focus to the top surface level (0m to 600m depth), which result in a subset of 1357 observations by 7 variables. There are no missing or null values.

Data preparation:
The spatial domain refers to the geographical region covered by the data. In    this case, the spatial domain is represented by the Latitude and Longitude values. The data appears to cover multiple spatial locations in the ocean, with varying coordinates. Spatial domain: Latitude: [-6.4910 ,7.2560] from south 6.4910 to north 7.2560; Longitude: [-167.5,177.3] from eastern 177.3 to 180, then to western 167.5 (converted negative longitude value into positive by adding 360)
 	 

Training set includes 1226 observations; Test set has 131 observations. In this dataset, the 'profileNumber' attribute exhibits 50 distinct values, each associated with 50 corresponding observations. To establish our test set, we employed a random sample process to extract 5 float values from the 50 unique 'profileNumber' group. The remaining 45 floats data form our training set. Following are maps of training set and test set locations with salinity values: 
