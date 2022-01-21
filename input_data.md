# INPUT DATA and processes
 
### LAND COVER
 **Land Cover:** The land cover proceed from LC Copernicus 100m 2019 dicrete layer. The LC has been reclassified in 6 classes. 
 The LC has been clipped using the EU27
 
### 1- LPD
 **Land Productivity Dynamics:** the LPD come from the Layer "LPS_change_opt4" and reclassified in 2 classes (1: decreasing land productivity, 0: same class and increasing).
 The LPD is based on the SumNDVI.
 
### 2- BUILTUP
 **Builtp:** source JRC, Builtup global, clipped using EU27. Stratified >Median
 
### 3- POPULATION
 **Builtp:** source JRC, Builtup global, clipped using EU27. Stratified >Median
 
### 4- RUSLE
 **Erosion:** source JRC ESDAC Rusle 2015 original 100m, resampled to 1Km and clipped using EU27. Stratified >Median
 
### 5- TREE LOSS
 **Tree Loss:** source Hansen Dataset. [surce code](https://code.earthengine.google.com/c0a3bb99851351a2a8b9343aa35d42aa)
 
### 6- WIND EROSION
 **Wind Erosion:** source JRC ESDAC 1resampled to 1Km and clipped using EU27. Stratified >Median
 
### 7- SOIL ACIDIFICATION
 **Acidification:** source JRC Carlo Rega and M.Luisa Paracchini. Described in MAES publication [here](https://publications.jrc.ec.europa.eu/repository/handle/JRC120383). for a description see the annex to MAES. Stratified >Median
 
### 8- GROSS NITROGEN BALANCE
 **Nitrogen:** source JRC Carlo Rega and M.Luisa Paracchini. Described in MAES publication [here](https://publications.jrc.ec.europa.eu/repository/handle/JRC120383). for a description see the annex to MAES. Stratified >Median
 
### 9- WATER STRESS
 **Baseline Water Stress:** source [Aqueduct](https://www.wri.org/data/aqueduct-global-maps-30-data).
 Dataset "Overall water risk" only classes "Medium-High", "High" and "Extremely High"
 
### 10- FIRES
 **Fires:** source MODIS 500m, see the [GEE Code](https://code.earthengine.google.com/e017edbd1f14539bbad15912bb409015)
 
### 11- IRRIGATION ON BWS
 **Irrigation on BWS:** source Corine Land Cover and Water Stress Aqueduct. 

### 12- GROUNDWATER DECLINE
 **Groundwater decline:** source [Aqueduct](https://www.wri.org/data/aqueduct-global-maps-30-data).
