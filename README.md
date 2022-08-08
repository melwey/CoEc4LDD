# CoEc4LDD
paper on Convergence of Evidence in Europe (EU) for Land Degradation and Development special issue

# Data
Data of Convergence of Evidence (CoE) for Europe are prepared by transforming input variables into boolean variables. Details on sources and transformation are available in [input_data.md](/input_data.md). Some variables are stratified by land cover classes, using code in [stratification.R(/stratification.R)]
Median values by land cover classes for the stratified variables are stored in [medians.md](/medians.md)

# Data analysis
Main code for data analysis is available in [CoE_Europe_analysis.R](/CoE_Europe_analysis.R)

We summarise and plot the outputs from the CoE on Europe 
- by administrative level (NUTS3 to NUTS0)
- by land cover class
- by Land Productivity Dynamics trend

We compare CoE results in different agricultural managements in [agricultural_management.r](/agricultural_management.r)

We also compare CoE and Trends.Earth results in [comp_trendsearth.R](/comp_trendsearth.R)
 
