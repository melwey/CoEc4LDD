# Analysis of sum of issues and individual issues distribution in study area.
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rasterVis)
wd <- getwd()

# set path to convergence data (issues)
path2data <- "../CoE_Europe_OUTPUT/coe_jan2022"

restart <- TRUE
if (restart) {
  file.remove("./temp")
  file.remove("./temp")
}
if (!dir.exists("./temp")){dir.create("./temp")}
if (!dir.exists("./fig")){dir.create("./fig")}

# # DO NOT RUN
# # to start from scratch, execute:
# # empty dir temp
# tempfiles <- dir(path = "./temp")
# apply(tempfiles, file.remove)
# restart<- TRUE
# # END DO NOT RUN

## Define study area and zones
#The outputs of the convergence of evidence are summarised at administrative (or other) level.

# get official NUTS areas
if (!"eurostat" %in% installed.packages()[, 1]) {
  install.packages("eurostat")
}

if (!file.exists("./temp/zones_eu27.geojson")) {
  library(eurostat)
  reg_area3 <- get_eurostat("reg_area3") %>%
    filter(time == as.Date("2016-01-01")) %>%
    dplyr::select(geo, landuse, values) %>%
    # landuse: TSA (total surface area) = TOTAL
    #          TLA (total land area) = L0008
    pivot_wider(id_cols = geo,
                names_from = landuse,
                values_from = values) %>%
    rename(TSA = TOTAL, TLA = L0008)
  
  # load NUTS level 3 polygons in ETRS89 from EUROSTAT gisco API
  # Map Scale = Raster resolution (in meters) * 2 * 1000
  # 2e6 doesn't exist so take 1:1000000
  url_zones <-
    "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_3035_LEVL_3.geojson"
  zones <- geojsonsf::geojson_sf(url_zones)
  # cite as © EuroGeographics for the administrative boundaries
  # crs not properly assigned
  st_crs(zones) <- st_crs(3035)
  
  # restrict to EU27
  clip <- read_sf("../EU_dissolve/EU_dissolve.shp") %>%
    st_transform(crs = 3035)
  zones_eu27 <- zones %>%
    # EU27
    dplyr::filter(
      CNTR_CODE %in% c(
        "BE",
        "BG",
        "CZ",
        "DK",
        "DE",
        "EE",
        "IE",
        "EL",
        "ES",
        "FR",
        "HR",
        "IT",
        "CY",
        "LV",
        "LT",
        "LU",
        "HU",
        "MT",
        "NL",
        "AT",
        "PL",
        "PT",
        "RO",
        "SI",
        "SK",
        "FI",
        "SE"
      )
    ) %>%
    # remove oversea territories !!! Cyprus is missing!!!
    sf::st_intersection(clip) %>%
    # add back missing CY
    add_row(zones %>% filter(CNTR_CODE == "CY")) %>%
    # add ID
    mutate(ID = row_number()) %>%
    # add area from eurostat
    dplyr::left_join(reg_area3, by = c("NUTS_ID" = "geo"))
  # export clip
  st_write(zones_eu27, dsn = "./temp/zones_eu27.geojson")
  # reload file as SpatVect
  zones_eu27 <- vect(zones_eu27)
  rm(zones, clip)
} else {
  zones_eu27 <-
    vect(st_read("./temp/zones_eu27.geojson"))
}


## Load CoE output data
# Assuming all issues have been processed, so that they :
#   - have the same equal area geographical coordinate system
# - have the same extent
# - are all dummy variables with values of 0 and 1
# - have a consistent definition for nodata

# We compute the sum of issues ignoring the no data values, i.e. no data are considered as 0. The resulting layer has integer values from 0 to (theoretically) 14, where 0 corresponds to true 0 or no data. All other values correspond to at least their number of coinciding issues.

coe_files <- list.files( 
  path = path2data,
  pattern = "*.tif$",
  full.names = TRUE)

# define band order
band_order <-  c(
  "fires1" = "Fires",
  "treeloss_issue" = "Tree loss",
  "nitrogen_R" = "Nitrogen surplus",
  "eutrofication_issue_R" = "Eutrophication",
  "acidification_issue_R" = "Soil acidification",
  "water_stress_issue" = "Water stress",
  #"irrigationOnBWS" = "Irrigation under water stress",
  "groundwater_decline" = "Groundwater table decline",
  #"hhi" = "Household income",
  "rusle_issue_R" = "Water erosion",
  "wind_issue_R" = "Wind erosion",
  "lpd_issue" = "LPD change",
  "population_issue_R" = "Population",
  "popchange_R" = "Population change",
  "builtup_issue" = "Built-up",
  "soilmicro_R" = "Soil respiratory quotient"
)
# coe_files <- list.files( 
#   path = path2data,
#   pattern = "*.vrt",
#   full.names = TRUE)

# # treeloss must be rounded
# treeloss <- rast(file.path(path2data,"treeloss.tif"))
# treeloss_bin <- app(treeloss, fun = function(x)(round(x)), filename = "./temp/treeloss.tif", wopt = list(datatype = "INT1U"))
# # rename original file
# file.rename(from = file.path(path2data, "treeloss.tif"), to = file.path(path2data, "treeloss_fede.tif"))
# # copy rounded file
# file.copy(from = "./temp/treeloss.tif", to = file.path(path2data, "treeloss.tif"))

# coe_list <- lapply(coe_files, function(x){rast(x)})

if(exists("coe_stack")) {rm(coe_stack)}

tryCatch(
  coe_stack <- rast(file.path(path2data, paste0(names(band_order), ".tif")))
)
if (!exists("coe_stack")) {
  print("data harmonization needed")
  ### sort out extent etc.
  # bypass by creating vrt
  file.create("./temp/buildvrtInputFiles.txt")
  write(file.path(path2data,names(band_order), ".tif"),"./temp/buildvrtInputFiles.txt")
  # set target extent to something reasonable, e.g. that of zones_eu27
  expres <- paste("gdalbuildvrt -resolution user -te", paste(as.vector(round(ext(zones_eu27)[c(1,3,2,4)])), collapse = " "), "-tr 1000 1000 -separate -r nearest -input_file_list", file.path(path2data, "buildvrtInputFiles.txt"), file.path(path2data, "coe_stack.vrt"))
  system(expres)
  coe_stack <- writeRaster(rast(file.path(path2data, "coe_stack.vrt")),
                           datatype = "INT1U",
                           filename = "./temp/coe_stack.tif")
}

# print(coe_stack)

# set band names.
names(coe_stack) <- names(band_order)

# get resolution
reso = res(coe_stack)
# multiplication factor to get area in sqkm from cell count
mf <- reso[1] * reso[2] * 1e-6

# sum of issues
if (!file.exists("./temp/coe_soi.tif") | restart){
  coe_soi <- terra::app(coe_stack, fun = sum, na.rm = TRUE, overwrite=TRUE, filename = "./temp/coe_soi.tif") 
} else {
  coe_soi <- rast("./temp/coe_soi.tif")
}
# 0 are true 0 or no data

# rasterize study area : needed to couple zones with masks 
if (restart) {
  if(file.exists("./temp/rzones_eu27.tif")){file.remove("./temp/rzones_eu27.tif")}
  system.time(
    rzones_eu27 <- zones_eu27 %>% 
      rasterize(coe_soi, field = "ID", filename = "./temp/rzones_eu27.tif")
  )
} else {
  rzones_eu27<- rast("./temp/rzones_eu27.tif")
}
# user  system elapsed 
# 0.812   0.161   1.119  
system.time(
  stat_nuts <- terra::freq(rzones_eu27) %>% as.data.frame()
)
areas_nuts <- stat_nuts %>%
  left_join(as.data.frame(zones_eu27) %>% dplyr::select(ID, NUTS_ID, CNTR_CODE, URBN_TYPE, MOUNT_TYPE, COAST_TYPE), by = c("value" = "ID"))

# plot soi
coe_soi_masked <- terra::mask(coe_soi, mask = rzones_eu27, filename = "./temp/coe_soi_masked.tif")
gplot(coe_soi_masked) + 
  geom_raster(aes(fill = value)) + 
  scale_fill_viridis_b(name = "# issues", 
                       breaks = 0:max(soi_nuts_m$median),
                       na.value="white")  +
  coord_equal() +
  theme_void()
ggsave(filename = "./fig/soi_map.png", height = 8, width = 8)  


## Zonal statistics
# Within each administrative unit, we summarize the information:
#   - area affected by a co-occurrence of issues
# - area affected by each issue
# 
# We look at this in different land cover classes and in function of land productivity dynamics.

### Sum of issues
# we add LC, lpd and aridity to zones
if (restart){
  # Land cover reclassied as 6 classes as used for stratification
  LC <- rast("../CoE_Europe_OUTPUT/LC/LandCover_3035.tif") %>%
    # resample to match zones
    project(rzones_eu27, method = "near", filename = "./temp/LC.tif", overwrite=TRUE)
  
  # change in LPD between reference period (2000-2015) and reporting (2004-2019)
  LPD <- rast("../CoE_Europe_OUTPUT/LPD_sumNDVI/lpd_change_option_4.tif") %>%
    # reproject
    project(rzones_eu27, method = "near", filename = "./temp/LPD.tif", overwrite=TRUE)
  # takes much more time than LC... possibly difference in encoding?
  
  # aridity: problem: doesn't cover all land; would introduce no data
  # so i drop it from the analysis
  
  # sum raster
  comb <- function(x, y, z){return(x + y * 1e4 + z * 1e5)}
  combined_zones <- terra::lapp(sds(rzones_eu27, LC, LPD), 
                                fun = comb,
                                filename = "./temp/combined_zones.tif",
                                overwrite=TRUE)
  soi_zones <- terra::lapp(sds(combined_zones, coe_soi),
                           fun = function(x,y) {return(x + floor(y) *1e6)},
                           filename = "./temp/soi_zones.tif",
                           overwrite=TRUE)
  
  # Issues
  # zonal sum: sum of cells is equivalent to area in sq.km because we are in an equal area projection with 1000 m resolution AND data are 1 or 0
  system.time(
    stat_issues <- terra::zonal(coe_stack, combined_zones, fun = "sum", na.rm = TRUE)
  )
  #    user  system elapsed 
  # 48.277   1.925  51.902
  # i would like to have a way to know how much data
  system.time(
    stat_issues_data <- terra::zonal(coe_stack, combined_zones, fun = function(x, ...){sum(!is.na(x))})
  )
  #   user  system elapsed 
  # 99.005  11.661 115.369 
  tmp <- stat_issues_data %>% summarise(across(where(is.integer), sum))
  # number of cells with data values depends on issue. Max is 4102215 for those defined everywhere, then 4094610 for lpd, then 4092673, for 5 issues. Min is treeloss because only on forest cells? This reflects on coe sum of issues: 0 are true 0 or no data
  # fires1 4102215. Defined everywhere
  # treeloss_issue 3824392. Defined only where there are trees.
  # nitrogen_R 3070983. Only in agric.
  # eutrofication_issue_R 3862217. almost everywhere. some no data coming from resampling?
  # acidification_issue_R 3750614.some huge holes in france, finland, cyprus and a bit in spain. plus all coast lines
  # water_stress_issue 4102215. defined everywhere
  # groundwater_decline 4102215. defined everywhere
  # rusle_issue_R 3716056. defined only in agric?
  # wind_issue_R 939524. Defined in very few areas. Check original data decription.
  # lpd_issue 4094610.
  # population_issue_R 4092424. defined everywhere (except sometimes on coastline)
  # popchange_R 4085514. same.
  # builtup_issue 4092673. same
  # soilmicro_R 3122597. not defined in urban/peri-urban. Not in mountains.
  
  # Sum of issues
  system.time(
    stat_soi <- terra::freq(soi_zones) %>% as.data.frame()
  )
  # 
  sum(stat_soi$count)
  # 4102190, coming from the LPD.
  # so we can safely use the areas from soi to analyse the individual issues.
  
  # Prepare data for further analysis
  issues_areas <- stat_issues %>%
    mutate(LPD = floor(ID * 1e-5)) %>%
    mutate(LC = floor((ID - LPD*1e5)*1e-4)) %>%
    mutate(ID = ID - LPD*1e5 - LC*1e4) %>%
    left_join(as.data.frame(zones_eu27) %>% dplyr::select(ID, NUTS_ID, CNTR_CODE, URBN_TYPE, MOUNT_TYPE, COAST_TYPE))
  # I'm missing the total cell count with data at this stage...
  # save data to disk
  issues_areas %>% write.csv(file = "./temp/issues_areas.csv")
  
  issues_data_areas <- stat_issues_data %>%
    mutate(LPD = floor(ID * 1e-5)) %>%
    mutate(LC = floor((ID - LPD*1e5)*1e-4)) %>%
    mutate(ID = ID - LPD*1e5 - LC*1e4) %>%
    left_join(as.data.frame(zones_eu27) %>% dplyr::select(ID, NUTS_ID, CNTR_CODE, URBN_TYPE, MOUNT_TYPE, COAST_TYPE))
  save(issues_data_areas, file = "./temp/issues_data_areas")
  
  
  soi_areas <- stat_soi %>%
    mutate(soi = floor(value * 1e-6)) %>%
    mutate(LPD = floor((value - soi*1e6) * 1e-5)) %>%
    mutate(LC = floor((value - soi*1e6 - LPD*1e5 )*1e-4)) %>%
    mutate(ID = value - soi*1e6 - LPD*1e5 - LC*1e4) %>%
    left_join(as.data.frame(zones_eu27) %>% dplyr::select(ID, NUTS_ID, CNTR_CODE, URBN_TYPE, MOUNT_TYPE, COAST_TYPE))
  # save data to disk
  soi_areas %>% write.csv(file = "./temp/soi_areas.csv")
} else{
  issues_areas <- readr::read_csv("./temp/issues_areas.csv")
  soi_areas <- readr::read_csv("./temp/soi_areas.csv")
}
max_soi <- coe_soi@ptr$range_max
min_soi <- coe_soi@ptr$range_min
first_issue <- names(band_order)[1]
last_issue <- names(band_order)[length(band_order)]

# We visualize the tabular data as bar charts. Data are available at all NUTS levels.

# sum of issues
soi_eu <- soi_areas %>%
  group_by(soi) %>%
  summarise(area = sum(count)) %>%
  mutate(cum_area = cumsum(area)/sum(area)*100)

p <- ggplot(soi_eu, aes(x = soi, y = area)) + geom_col()
p
# more than half of the (continental) EU has at least 3 coinciding issues.
ggsave(filename = "./fig/soi_eu.png")
#fig.cap = "Frequency of number of coinciding issues for continental EU27."}

# Half of the (continental) EU has at least 2 coinciding issues. Maybe this graph can be skipped because it is also in the next fig \@ref(fig:soicntrplot).

soi_cntr <- soi_areas %>%
  group_by(soi, CNTR_CODE) %>%
  summarise(area = sum(count)) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0) %>%
  # sort countries alphabetically
  arrange(CNTR_CODE) %>%
  # # remove is.na(CNTR_CODE) # there shouldn't be any NA
  # dplyr::filter(!is.na(CNTR_CODE)) %>%
  # add EU27 total
  bind_rows(soi_eu %>% dplyr::select(-cum_area) %>% mutate(CNTR_CODE = "EU27") %>% pivot_wider(names_from = soi, values_from = area, values_fill = 0))
knitr::kable(soi_cntr, caption = "Areas of each EU country affected by a given number of coinciding issues.")

# Countries do present a different distribution of the number of coinciding issues. 

tmp <- soi_cntr %>%
  # compute total mapped area by country
  rowwise() %>%
  mutate(cntr_area = sum(c_across(where(is.numeric)))) 
areas_cntr <- tmp %>% 
  dplyr::select(CNTR_CODE, cntr_area)%>%
  ungroup()
soi_cntr_pc <- tmp %>%
  mutate(across(where(is.numeric), ~round(.x/cntr_area*100))) %>%
  ungroup() %>%
  dplyr::select(-cntr_area) %>%
  # cntr_code to factor to fix order (alphabetical)
  mutate(CNTR_CODE = factor(CNTR_CODE, levels = pull(tmp, CNTR_CODE))) %>%
  # to long table
  pivot_longer(cols = where(is.numeric), names_to = "soi", values_to = "area")
ggplot(soi_cntr_pc,
       aes(x = as.numeric(soi), y = area)) +
  geom_col() +
  facet_wrap(~CNTR_CODE, ncol = 7) +
  xlab("# coinciding issues") + 
  ylab("Percentage of country area")
ggsave(filename = "./fig/soi_cntr.png", width = 8, height = 6)
# fig.cap = "Frequency of number of coinciding issues for EU27 countries (excluding oversea territories)."

# mean, median, mode by country
soi_cntr_m <- soi_cntr_pc %>%
  mutate(soixarea = as.numeric(soi) * area) %>%
  group_by(CNTR_CODE) %>%
  arrange(as.numeric(soi), by_group = TRUE) %>%
  # mutating expressions are computed within groups
  mutate(cumarea = cumsum(area)) %>%
  summarise(mean = sum(soixarea)/100, 
            mode = soi[area == max(area)][1], 
            q1 = soi[cumarea == min(cumarea[cumarea>=25])],
            median = soi[cumarea == min(cumarea[cumarea>=50])],
            q3 = soi[cumarea == min(cumarea[cumarea>=75])]) %>%
  ungroup() %>% 
  mutate(across(mode:q3, as.numeric))

# fig.cap = "Distribution statistics on number of coinciding issues for EU27 countries. Small black dot is arithmetic mean; cross (x) is mode; red dot is median; vertical line ends are first and fourth quartiles."}
lim_mean <- soi_cntr_m %>% arrange(mean) %>% dplyr::select(CNTR_CODE) %>% unlist()
names(lim_mean) <-NULL
soicntrmplot <- ggplot(soi_cntr_m %>% mutate(CNTR_CODE = factor(CNTR_CODE, levels = lim_mean)), aes(x = CNTR_CODE)) +
  geom_pointrange(mapping = aes(y = median, ymin = q1, ymax = q3), colour = "red", show.legend = TRUE) +
  geom_point(mapping = aes(y = mean),show.legend = TRUE) +
  geom_point(mapping = aes(y = mode), shape = "x", size = 4, show.legend = TRUE) +
  ylab("# coinciding issues") + 
  xlab("Countries") 
soicntrmplot

ggsave(filename = "./fig/soi_cntr_m.png", width = 8, height = 6)

# We can distinguish groups of countries. Five countries have at most one issue on half the area analysed (\@ref(fig:soicntrmplot) median, red dot): Finland (FI), Sweden (SE), Latvia (LV), Estonia (EE) and Croatia (HR). The bar charts of Finland and Sweden are similar (Fig. \@ref(fig:soicntrplot)). Their distribution's peak (mode, x in Fig.\@ref(fig:soicntrmplot)) is at one single issue. At the other extreme, six countries have more half of their mapped area subject to five or more coinciding issues: CZ, LU, DE, BE, DK, NL. 
# Half of the continental EU27 (without maritime territories) analysed here has at least threeß coinciding issues. 25% of the area has at least two issues (lower end of vertical line), while another 25% of the area has at least four coinciding issues (upper end of vertical line). The most frequent number of coinciding issues is 3 (mode, x). On average, there are 3.5 issues (mean, black dot).

# map of soi median at NUT3 level
soi_nuts <- soi_areas %>% 
  group_by(soi, NUTS_ID) %>%
  summarise(area = sum(count)) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0)
tmp <- soi_nuts %>%
  # compute total mapped area by country
  rowwise() %>%
  mutate(nuts_area = sum(c_across(where(is.numeric)))) 
areas_nuts <- tmp %>% 
  dplyr::select(NUTS_ID, nuts_area)%>%
  ungroup()
soi_nuts_pc <- tmp %>%
  mutate(across(where(is.numeric), ~round(.x/nuts_area*100))) %>%
  ungroup() %>%
  dplyr::select(-nuts_area) %>%
  # to long table
  pivot_longer(cols = where(is.numeric), names_to = "soi", values_to = "area")
# soi_nut_pc has 1152*12=13824 entries
# add expectation stats
soi_nuts_m <- soi_nuts_pc %>%
  # transform soi into numeric
  mutate(soi = as.numeric(soi)) %>%
  # basis to compute mean: soi times area
  mutate(soixarea =  soi * area) %>%
  # computations need to be made within groups
  group_by(NUTS_ID) %>%
  # sort soi values in each group to compute cumulative area, used to compute quartiles
  arrange(as.numeric(soi), by_group = TRUE) %>%
  # mutating expressions are computed within groups
  mutate(cumarea = cumsum(area)) %>%
  # summarise expressions are computed within groups
  summarise(mean = sum(soixarea)/100, 
            # if 2 soi have same area, use lowest soi.
            mode = soi[area == max(area)][1], 
            # first quartile is when at least 25% of area is covered
            q1 = soi[cumarea == min(cumarea[cumarea>=25])][1],
            median = soi[cumarea == min(cumarea[cumarea>=50])][1],
            q3 = soi[cumarea == min(cumarea[cumarea>=75])][1]) %>%
  ungroup()
save(soi_nuts_m, file = "./temp/soi_nuts_m.rdata")
# join with vect
soi_nuts_vect <- terra::merge(zones_eu27, soi_nuts_m) 
soi_nuts_sf <- sf::st_as_sf(soi_nuts_vect) # does not work with sf version 0.9-6

#plot(soi_nuts_vect, "median", lty = "blank", col = hcl.colors(8, "viridis"))

ggplot(soi_nuts_sf) +
    geom_sf(aes(fill = median), colour = NA) +
    scale_fill_viridis_b(name = "", breaks = 0:max(soi_nuts_m$median)) 

ggsave(filename = "./fig/soi_nuts.png", width = 6, height = 6)


#In the next section, we look at which are the most frequent issues.

## Issues
issues_eu <- issues_areas %>%
  # to long table
  pivot_longer(!!first_issue : !!last_issue, names_to = "issue", values_to = "area") %>%
  group_by(issue) %>%
  summarise(area = sum(area)) %>%
  # sort by area
  arrange(area)

p <- ggplot(issues_eu, aes(x = factor(issue, levels = pull(issues_eu,issue)), y = area)) + 
  geom_col() +
  coord_flip()+ 
  xlab("") 
p
ggsave(filename = "./fig/issues_eu.png")

issues_cntr <- issues_areas %>%
  group_by(CNTR_CODE) %>%
  summarise(across(!!first_issue : !!last_issue, sum)) %>%
  # sort countries alphabetically
  arrange(CNTR_CODE) %>%
  # add EU27 total
  bind_rows(issues_eu %>% mutate(CNTR_CODE = "EU27") %>% pivot_wider(names_from = issue, values_from = area, values_fill = 0)) %>%
  # rearrange columns
  dplyr::select(c("CNTR_CODE", pull(issues_eu,issue)))
knitr::kable(issues_cntr, 
             caption = "Areas [sq.km] of each EU country affected by a given issue.",
             col.names = c("Country code", band_order[pull(issues_eu,issue)])
)

#fig.cap="Percentage of area in EU27 countries subject to land change issues.
issues_cntr_pc <- issues_cntr %>%
  left_join(areas_cntr) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~round(.x/cntr_area*100))) %>%
  ungroup() %>%
  dplyr::select(-cntr_area) %>%
  # cntr_code to factor to fix order
  mutate(CNTR_CODE = factor(CNTR_CODE, levels = pull(issues_cntr, CNTR_CODE))) %>%
  # to long table
  pivot_longer(where(is.numeric), names_to = "issue", values_to = "area") %>%
  # set issue as factor, with levels sorted by increasing area affected at EU scale
  mutate(issue = factor(issue, levels = pull(issues_eu,issue)))
  

ggplot(issues_cntr_pc,
       aes(x = issue, y = area)) + # for geom_text # , label = area
  geom_col() +
  #geom_text() +
  facet_wrap(~CNTR_CODE, ncol = 7) +
  scale_x_discrete(limits = pull(issues_eu,issue), labels = band_order ) +
  xlab("") + ylab ("Percentage of country area" ) +
  coord_flip()
ggsave(filename = "./fig/issues_cntr.png", height = 9, width = 8)

# table cntr
issues_cntr_tbl <- issues_cntr_pc %>%
  pivot_wider(names_from = issue, values_from = area) %>%
  bind_rows(issues_cntr) %>%
  arrange(CNTR_CODE)
readr::write_csv(issues_cntr_tbl, "./temp/issues_cntr_tbl.csv")
# export to csv and format in excel
tmp <- issues_cntr_pc %>%
  pivot_wider(names_from = issue, values_from = area)
names(tmp) <- c("Country", band_order[pull(issues_eu,issue)])
readr::write_csv(tmp, "./temp/issues_cntr_pc_tbl.csv")
readr::write_csv(issues_cntr, "./temp/issues_cntr.csv")

# EU27
issues_eu %>% filter(issue == "eutrofication_issue_R")
issues_cntr_pc %>% filter(CNTR_CODE == "EU27" & issue == "eutrofication_issue_R") %>% dplyr::select(area) 

# We map the percentage of each NUTS3 in which each issue occurs.

# map of issue percentage at NUT3 level
issues_nuts <- issues_areas %>%
  group_by(NUTS_ID) %>%
  summarise(across(!!first_issue : !!last_issue, sum)) 

issues_nuts_pc <- issues_nuts %>%
  left_join(areas_nuts) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~round(.x/nuts_area*100))) %>%
  ungroup() %>%
  dplyr::select(-nuts_area) #%>%
  # # to long table
  # pivot_longer(where(is.numeric), names_to = "issue", values_to = "area") %>%
  # # set issue as factor, with levels sorted by increasing area affected at EU scale
  # mutate(issue = factor(issue, levels = pull(issues_eu,issue)))

# join with vect
# soi_nuts_sf <- sf::st_as_sf(soi_nuts_sf) # does not work with sf version 0.9-6 and I can't update it
# i can load zones_eu27 as sf
zones_eu27_sf <- st_read("./temp/zones_eu27.geojson")
issues_nuts_vect <- terra::merge(zones_eu27_sf, issues_nuts_pc) 

#plot(issues_nuts_vect, "area", lty = "blank", col = hcl.colors(8, "viridis"))
# dev.new(width=12, height=10, noRStudioGD = TRUE)
# par(mfrow=c(4,7))
# for (issue in issues_eu$issue) {
#   plot(issues_nuts_vect, issue, lty = "blank", col = hcl.colors(8, "viridis"), main = band_order[grepl(issue, names(band_order))])
# }
# ggplot needs an sf
# use 2 different colour scales: 1 for issues reaching 100%, 1 for those going only up to 50
# Fires up to 35, irrigation on BWS 36, lpd 70, 
for (issue in issues_eu$issue) {
  var <- sym(issue)
  p <- ggplot(issues_nuts_vect) +
    geom_sf(aes(fill = !!var), colour = NA) +
    scale_fill_viridis_b(name = "% Area") +
    labs(title = band_order[grepl(issue, names(band_order))]) 
  ggsave(file.path("./fig", paste("map", issue, "nuts.png", sep = "_")), p,
         units = "cm", width = 15, height = 18)
}

# Among the land change issues analysed in this study, eutrophication is the most prominent one in the EU 27. Overall, it affects 1,928,013 km^2 (`r `issues_eu %>% filter(issue == "eutrofication_issue")`), i.e. 47 % (`r issues_cntr_pc %>% filter(CNTR_CODE == "EU27" & issue == "eutrofication_issue") %>% dplyr::select(area) `) of the area mapped. It is dominant (>75% area) in many EU27 administrative units (NUTS3). Sweden, Finland, Ireland and Cyprus are exceptions.

## TO DO ?: correlations of % area affected by various issues 
# library(GGally)
# GGally::ggpairs(issues_nuts_pc, columns = 2:14)
# not really interesting
# pairs(issues_nuts_pc[,-1], pch = 20, cex = .5)
# Only pop and builtup are highly correlated (0.96022875)
# fires and treeloss 0.47
# acidification and nitrogen 0.53
# acidif and eutroph 0.4
# acidif and rusle -0.5
# acidif and pop 0.53
# acidif and builtup 0.49
# cor(issues_nuts_pc[,-1])

# We can also look at the data, under different perspectives.

### Highlight on arid and semi-arid areas
# need to find appropriate dataset... Aridity index may not be the best? See papers from ETH/IIASA guy

### Dominant land use/cover classes (end of period)

# continental overview of soi and issues by land cover
lc_classes <- tibble(LC = as.numeric(0:6), LC_name = factor(c("Other", "Grassland", "Cropland", "Forest", "Urban", "Wetlands", "Bare" ), levels = c("Grassland", "Cropland", "Forest", "Urban", "Wetlands", "Bare", "Other", "EU27")))
soi_lc <- soi_areas %>%
  group_by(soi, LC) %>%
  summarise(area = sum(count)) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0) %>%
  # add LC names
  left_join(lc_classes) %>%
  # rearrange columns
  relocate(LC_name, .after = LC) %>%
  # add EU27 total
  bind_rows(soi_eu %>% dplyr::select(-cum_area, ) %>% mutate(LC_name = factor("EU27")) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0)) %>%
  # add total by class
  rowwise() %>%
  mutate(total = sum(c_across(!!as.character(min_soi) : !!as.character(max_soi)))) %>%
  arrange(LC_name)
knitr::kable(soi_lc %>% dplyr::select(-LC), 
             caption = "Areas of each braod land cover class affected by a given number of coinciding issues.",
             col.names = c("Land Cover class", names(soi_lc)[-1][-1]))

# fig.cap = "Frequency of number of coinciding issues in the EU27 by land cover class (excluding oversea territories)."}
soi_lc_pc <- soi_lc %>%
  mutate(across(!!as.character(min_soi) : !!as.character(max_soi), ~round(.x/total*100))) %>%
  ungroup() %>%
  dplyr::select(-total, -LC) %>%
  # to long table
  pivot_longer(cols = where(is.numeric), names_to = "soi", values_to = "area")
ggplot(soi_lc_pc %>% dplyr::filter(! LC_name %in% c("Other", "EU27")),
       aes(x = as.numeric(soi), y = area)) +
  geom_col() +
  facet_wrap(~LC_name, ncol = 3) +
  xlab("# coinciding issues") +
  ylab("Percentage area within LC class")
ggsave(filename = "./fig/soi_lc.png", width = 8, height = 6)

# plot soi mean, median, mode by LC class
soi_lc_m <- soi_lc_pc %>%
  mutate(soixarea = as.numeric(soi) * area) %>%
  group_by(LC_name) %>%
  arrange(as.numeric(soi), by_group = TRUE) %>%
  # mutating expressions are computed within groups
  mutate(cumarea = cumsum(area)) %>%
  summarise(mean = sum(soixarea)/100, 
            mode = soi[area == max(area)][1], 
            q1 = soi[cumarea == min(cumarea[cumarea>=25])][1],
            median = soi[cumarea == min(cumarea[cumarea>=50])][1],
            q3 = soi[cumarea == min(cumarea[cumarea>=75])][1]) %>%
  ungroup() %>% 
  mutate(across(mode:q3, as.numeric))

# fig.cap = "Distribution statistics on number of coinciding issues for land cover classes in the EU27. Small black dot is arithmetic mean; cross (x) is mode; red dot is median; vertical line ends are first and fourth quartiles."}
lim_mean <- soi_lc_m %>% arrange(mean) %>% dplyr::select(LC_name) %>% unlist()
names(lim_mean) <-NULL
soilcmplot <- ggplot(soi_lc_m %>% mutate(LC_name = factor(as.character(LC_name), levels = lim_mean)), aes(x = LC_name)) +
  geom_pointrange(mapping = aes(y = median, ymin = q1, ymax = q3), colour = "red", show.legend = TRUE) +
  geom_point(mapping = aes(y = mean),show.legend = TRUE) +
  geom_point(mapping = aes(y = mode), shape = "x", size = 4, show.legend = TRUE) +
  ylab("# coinciding issues") + 
  xlab("Land cover") 

soilcmplot
ggsave(filename = "./fig/soi_lc_m.png", width = 8, height = 6)

# On average, Wetlands, Bare and Other areas present less coinciding land change issues then other land covers. Cropland and Urban areas present the most numerous coinciding pressures.

issues_lc <- issues_areas %>%
  # add LC_name
  left_join(lc_classes ) %>%
  group_by(LC_name) %>%
  summarise(across(!!first_issue : !!last_issue, sum)) %>%
  # arrange by land cover class
  arrange(LC_name) %>%
  # rearrange columns
  dplyr::select(c("LC_name", pull(issues_eu,issue)))
knitr::kable(issues_lc, 
             caption = "Areas [sq.km] under specific land cover affected by a given issue.",
             col.names = c("Land Cover class", band_order)
)

# fig.cap="Percentage of area in EU27 subject to land change issues by land cover class."
issues_lc_pc <- issues_lc %>%
  left_join(soi_lc %>% dplyr::select(LC_name, total)) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~round(.x/total*100))) %>%
  ungroup() %>%
  dplyr::select(-total) %>%
  # to long table
  pivot_longer(where(is.numeric), names_to = "issue", values_to = "area") %>%
  # set issue as factor, with levels sorted by increasing area affected at EU scale
  mutate(issue = factor(issue, levels = pull(issues_eu,issue))) %>%
  # add info on stratification
  mutate(is_strat = if_else(issue %in% c("population_issue_R", 
                                         "rusle_issue_R", 
                                         "wind_issue_R", 
                                         "eutrofication_issue_R", 
                                         "nitrogen_R", 
                                         "acidification_issue_R", 
                                         "soilmicro_R",
                                         "popchange_R"), 
                            true = "Stratified", false = "Not stratified"))
  
ggplot(issues_lc_pc,
       aes(x = issue, y = area, fill = is_strat)) +
  geom_col() +
  facet_wrap(~LC_name, ncol = 4) +
  scale_x_discrete(limits = pull(issues_eu,issue), labels = band_order ) +
  scale_fill_brewer(palette = "Set2",
                    name = "") +
  xlab("") + ylab("Percentage of area") +
  coord_flip() +
  theme(legend.position=c(0.9, 0.3))
ggsave(filename = "./fig/issues_lc.png", width = 8, height = 6)

# table
issues_lc_tbl <- issues_lc_pc %>%
  dplyr::select(-is_strat) %>%
  pivot_wider(names_from = LC_name, values_from = area) %>%
  bind_rows(issues_lc %>% 
              pivot_longer (where(is.numeric), names_to = "issue", values_to = "area") %>%
              pivot_wider(names_from = LC_name, values_from = area)) %>%
  mutate(issue = factor(issue, levels = pull(issues_eu,issue))) %>%
  arrange(desc(issue))

knitr::kable(issues_lc_tbl, 
             caption = "Areas under specific land cover affected by a given issue. First line in sq.km; Second line in percentage of total area within land cover class",
             col.names = c("Land Cover class", band_order)
)
# export to csv
readr::write_csv(issues_lc_tbl, "./temp/issues_lc_tbl.csv")
             

# Depending on how the threshold is defined, analysing the issue distribution per land cover class makes more or less sense.
# For all stratified variables, the criteria is that all values greater than the median within each land cover class are to be considered an issue (value_issue = 1). Therefore, the area affected by a specific stratified issue can't exceed 50~%. If the criteria would have included the median, this wouldn't be the case.
# The actual result depends on the values distribution, i.e. how many grid cells are effectively equal to the median and on how skewed the distribution is. If the median value in a particular land cover class has a high frequency, the result might be far less than 50%. This is why, we look at all issues.
# 
# Eutrophication affects most of cropland and urban areas (54 and 56 % respectively). In grassland, forests and wetlands, it reaches more than 40 %. Only bare and other areas are less affected, with 32 and 12 % respectively.
# In some classes eutrophication is greater than 50% and in others it is less. The criteria is hence difficult to understand
# 
# Water erosion affects equally grassland and cropland (49 %) and slightly less forest (45 %). It is a concern on a quarter of urban and bare areas, while it only affects around 9 % of wetlands and other areas.
# 
# Nitrogen balance on the landscape can only be considered an issue in croplands because this is how the original data was defined.
# 
# Water stress equally affects grassland, cropland and urban areas (37, 36 and 38 %). It is a concern in a quarter of the forest areas and 10% of the wetlands.
# 
# Soil acidification is an issue on 33 % of croplands, 35 % of urban areas, 21 % of forests and 15% of wetlands.
# 
# Unsurprisingly, the increase in built-up is an issue in most urban areas (68 %), but it also concerns a large proportion of areas dominated by cropland, forests and grassland areas (33, 16 and 16 % respectively).
# 
# It is important to note that the land cover used here and all the layers used to determine the issues were resampled at 1 km

### Land productivity dynamics

# continental overview of soi and issues by lpd
lpd_classes <- tibble(LPD = as.numeric(1:3), LPD_name = factor(c("Decreasing", "Stable", "Increasing"), levels = c("Decreasing", "Stable", "Increasing")))
soi_lpd <- soi_areas %>%
  group_by(soi, LPD) %>%
  summarise(area = sum(count)) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0) %>%
  # add LPD names
  left_join(lpd_classes) %>%
  # rearrange columns
  relocate(LPD_name, .after = LPD) %>%
  # add EU27 total
  bind_rows(soi_eu %>% dplyr::select(-cum_area, ) %>% mutate(LPD_name = factor("EU27")) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0)) %>%
  # add total by class
  rowwise() %>%
  mutate(total = sum(c_across(!!as.character(min_soi) : !!as.character(max_soi)))) %>%
  arrange(LPD_name)
knitr::kable(soi_lpd %>% dplyr::select(-LPD), 
             caption = "Areas of each land productivity dynamics (LPD) class affected by a given number of coinciding issues.",
             col.names = c("LPD", names(soi_lpd)[-1][-1]))

#"Frequency of number of coinciding issues in the EU27 by land productivity dynamics class (excluding oversea territories)."
soi_lpd_pc <- soi_lpd %>%
  mutate(across(!!as.character(min_soi) : !!as.character(max_soi), ~round(.x/total*100))) %>%
  ungroup() %>%
  dplyr::select(-total, -LPD) %>%
  # to long table
  pivot_longer(cols = where(is.numeric), names_to = "soi", values_to = "area")
ggplot(soi_lpd_pc %>% dplyr::filter(! LPD_name %in% c("Other", "EU27")),
       aes(x = as.numeric(soi), y = area)) +
  geom_col() +
  facet_wrap(~LPD_name, ncol = 3) +
  xlab("# coinciding issues") +
  ylab("Percentage area within LPD class")
ggsave(filename = "./fig/soi_lpd.png", width = 8, height = 3.5)

# soi mean, median, mode by lpd class
soi_lpd_m <- soi_lpd_pc %>%
  mutate(soixarea = as.numeric(soi) * area) %>%
  group_by(LPD_name) %>%
  arrange(as.numeric(soi), by_group = TRUE) %>%
  # mutating expressions are computed within groups
  mutate(cumarea = cumsum(area)) %>%
  summarise(mean = sum(soixarea)/100, 
            mode = soi[area == max(area)][1], 
            q1 = soi[cumarea == min(cumarea[cumarea>=25])][1],
            median = soi[cumarea == min(cumarea[cumarea>=50])][1],
            q3 = soi[cumarea == min(cumarea[cumarea>=75])][1]) %>%
  ungroup() %>% 
  mutate(across(mode:q3, as.numeric))

# fig.cap = "Distribution statistics on number of coinciding issues for land productivuty dynamics classes in the EU27. Small black dot is arithmetic mean; cross (x) is mode; red dot is median; vertical line ends are first and fourth quartiles."
lim_mean <- soi_lpd_m %>% arrange(mean) %>% dplyr::select(LPD_name) %>% unlist()
names(lim_mean) <-NULL
soilpdmplot <- ggplot(soi_lpd_m %>% mutate(LC_name = factor(as.character(LPD_name), levels = lim_mean)), aes(x = LPD_name)) +
  geom_pointrange(mapping = aes(y = median, ymin = q1, ymax = q3), colour = "red", show.legend = TRUE) +
  geom_point(mapping = aes(y = mean),show.legend = TRUE) +
  geom_point(mapping = aes(y = mode), shape = "x", size = 4, show.legend = TRUE) +
  ylab("# coinciding issues") + 
  xlab("Land productivity dynamics change") 

soilpdmplot
ggsave(filename = "./fig/soi_lpd_m.png", width = 8, height = 6)

# are fires in wetland due to groundwater decline?
# add the 2 layers

# Areas with both decreasing LPD are mostly (media, red dot) associated with four other land change issues (need to subtract 1 from decreasing). Areas with stable and increasing LPD tend to have one issue less.

# issues by LPD
issues_lpd <- issues_areas %>%
  # add LC_name
  left_join(lpd_classes ) %>%
  group_by(LPD_name) %>%
  summarise(across(!!first_issue : !!last_issue, sum)) %>%
  # arrange by land cover class
  arrange(LPD_name) %>%
  # rearrange columns
  dplyr::select(c("LPD_name", pull(issues_eu,issue)))
knitr::kable(issues_lpd, 
             caption = "Areas [sq.km] with specific LPD affected by a given issue.",
             col.names = c("Land Productivity Dynamics", band_order),
             format = "html"
)

# fig.cap="Percentage of area in EU27 subject to land change issues by LPD class.
issues_lpd_pc <- issues_lpd %>%
  left_join(soi_lpd %>% dplyr::select(LPD_name, total)) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~round(.x/total*100))) %>%
  ungroup() %>%
  dplyr::select(-total) %>%
  # to long table
  pivot_longer(where(is.numeric), names_to = "issue", values_to = "area") %>%
  # set issue as factor, with levels sorted by increasing area affected at EU scale
  mutate(issue = factor(issue, levels = pull(issues_eu,issue)))
  
ggplot(issues_lpd_pc %>% filter(issue != "lpd_issue"),
       aes(x = issue, y = area, fill = LPD_name)) +
  geom_col(position = "dodge") +
  # facet_wrap(~LPD_name, ncol = 3) +
  scale_x_discrete(
    limits = pull(issues_eu %>% filter(issue != "lpd_issue"),issue),
    labels = band_order 
    ) +
  scale_fill_brewer(palette = "RdYlBu", name = "LPD change") +
  xlab("") + ylab("Percentage of area within LPD change class") +
  theme_dark() +
  coord_flip()
ggsave(filename = "./fig/issues_lpd.png", width = 8, height = 6)

### Conventional ag
### Organic ag 
### Regenerative ag
# couldn't find data for these ....

