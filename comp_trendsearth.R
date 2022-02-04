## Comparison with trends.earth outputs
# run Coe_Europe_analysis.R first
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(ggtern)

# compare our results with trends.earth:
# TO DO 
# - reproject sdg15.3 to 3035 by nearest neighbour
# - zonal stat at NUTS3 level (area degrading, stable and improving)
# load Trends.Earth output
sdg15 <- rast("../TrendsEarthEurope/sdg-15-3-1-summary_shape-lon10.462lat52.447_baseline_sdg.tif")
# r_zones from previous analysis
rzones_eu27 <- rast("./temp/rzones_eu27.tif ")
# reproject,resample and mask to match zones
sdg15_3035 <- project(sdg15,
                      rzones_eu27, 
                      method = "near", # nearest neighbour
                      mask = TRUE,
                      filename = "./temp/sdg15_3035.tif", overwrite=TRUE)
names(sdg15_3035) <- c("sdg15_3", "pop", "lpd")

# zonal stat: freq of each value in each zone
# in layer sdg15_3 (first), values are -1, 0 or 1 and -32768 (no data)
# count them individually for the whole area
sdg15_freq <- terra::freq(sdg15_3035$sdg15_3)
# by nuts3 (not very efficient...)
sdg15_nuts_impr <- terra::zonal(sdg15_3035$sdg15_3, rzones_eu27, 
                           fun = function(x){sum(x == 1)})
sdg15_nuts_stab <- terra::zonal(sdg15_3035$sdg15_3, rzones_eu27, 
                                fun = function(x){sum(x == 0)})
sdg15_nuts_degr <- terra::zonal(sdg15_3035$sdg15_3, rzones_eu27, 
                                fun = function(x){sum(x == -1)})
sdg15_nuts_ndat <- terra::zonal(sdg15_3035$sdg15_3, rzones_eu27, 
                                fun = function(x){sum(x == -32768)})
# merge results
sdg_nuts <- bind_rows (list(Improving = sdg15_nuts_impr,
                            Stable = sdg15_nuts_stab,
                            Degrading = sdg15_nuts_degr,
                            NoData = sdg15_nuts_ndat),
                       .id = "LandStatus")
# join with soi nuts mean
# load soi_nuts_m created in CoE_Europe_analysis.R
load("./temp/soi_nuts_m.rdata")
# also needs zones_eu_27 to have link between ID and NUTS_ID
zones_eu27_sf <- st_read("./temp/zones_eu27.geojson")
nuts_id <- zones_eu27_sf %>% as.data.frame() %>% dplyr::select(ID, NUTS_ID)
# make sdg_nuts wider, with sdg 15.3 values as columns
sdg_nuts_wide <- sdg_nuts %>% pivot_wider(names_from = LandStatus, values_from = sdg15_3)
soi_sdg_nuts <-soi_nuts_m %>% 
  left_join(nuts_id) %>%
  left_join(sdg_nuts_wide)

# display comparison as ternary plot
# position on the triangle represents proportions of area of nuts with degr, stab and impr
# colour scale represents median number of issues.
# t <- ggtern(data = soi_sdg_nuts, aes(Improving, Stable, Degrading)) +
#   geom_point(aes(colour = median), size = .5) +
#   scale_colour_viridis_b(name = "Median # issues", breaks = 0:max(soi_nuts_m$median))
# ggsave("./fig/soi_m_sdg_tern.png")
# citation(package = 'ggtern')

# points are not well scattered: not enough improving
# try 2D, but need to scale by nuts
soi_sdg_nuts_pc <- soi_sdg_nuts %>%
  rowwise()%>%
  mutate(sum_sdg = sum(c_across(Improving:NoData))) %>%
  mutate(across(Improving:NoData, function(x){round(x/sum_sdg*100)})) %>%
  ungroup() %>% 
  pivot_longer(cols = Improving:Degrading,
               names_to = "LandStatus",
               values_to = "%Area") %>%
  mutate(LandStatus = factor(LandStatus, levels = c("Improving", "Stable", "Degrading")))
# jitter plot + trend
ggplot(data = soi_sdg_nuts_pc,
       mapping = aes(x = median, y = `%Area`, colour = LandStatus)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  xlab("Median # issues") + 
  ylab("Percentage area within NUTS3") + 
  scale_colour_brewer(palette = "Dark2")
ggsave(filename = "./fig/soi_sdg_nuts_jitter.png", height = 6, width = 8)
# There is a certain agreement between the two methods:
# NUTS3 with a larger proportion of Degrading tend to have more coinciding issues
# NUTS3 with a larger proportion of Improving tend to have less issues.
# The trend is however not so strong.
# A similar trend is seen for the proportion of Stable land.
# The proportion of Stable land as calculated by Trends.Earth indicator SDG 15.3
# is above 50% in most NUTS3. We here show however that most of these NUTS3
# are affected by at least three coinciding issues on half of their area (Median number of issues).
# 
# We hence urge countries to include other indicators of land status and trends
# in their land degradation assessment.

citation(package = 'ggplot2')
citation(package = 'terra')
citation(package = 'sf')
