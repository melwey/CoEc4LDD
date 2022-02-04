# agriculture structure compared to CoE
# load nuts levels
# download.file("https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2021.xlsx", destfile = "./temp/nuts.xlsx")
# NUTS_ID are constructed as:
# 00 1 2 3
# so to get NUTS1 and NUTS2 levels I need to get rid of the fifth and fourth characters

# load soi_nuts_m
nuts123 <- nuts_id %>%
  as.data.frame() %>%
  dplyr::select(NUTS_ID) %>%
  mutate(NUTS0 = substr(NUTS_ID, 1, 2),
         NUTS1 = substr(NUTS_ID, 1, 3),
         NUTS2 = substr(NUTS_ID, 1, 4),
         NUTS3 = NUTS_ID)

# load soi_areas
soi_areas <- readr::read_csv("./tmp/soi_areas.csv")

max_soi <- soi_areas$soi%>%max()
min_soi <- soi_areas$soi%>%min()

# NUTS1
soi_nuts1 <- soi_areas %>% 
  left_join(nuts123) %>%
  group_by(soi, NUTS1) %>%
  summarise(area = sum(count)) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0) %>%
  # compute total mapped area by country
  rowwise() %>%
  mutate(nuts1_area = sum(c_across(!!as.character(min_soi) : as.character(!!max_soi)))) %>%
  ungroup()

soi_nuts1_pc <- soi_nuts1 %>%
  rowwise() %>%
  mutate(across(!!as.character(min_soi) : as.character(!!max_soi), ~round(.x/nuts1_area*100))) %>%
  ungroup() %>%
  dplyr::select(-nuts1_area) %>%
  # to long table
  pivot_longer(cols = where(is.numeric), names_to = "soi", values_to = "area")

# add expectation stats
soi_nuts1_m <- soi_nuts1_pc %>%
  # transform soi into numeric
  mutate(soi = as.numeric(soi)) %>%
  # basis to compute mean: soi times area
  mutate(soixarea =  soi * area) %>%
  # computations need to be made within groups
  group_by(NUTS1) %>%
  # sort soi values in each group to compute cumulative area, used to compute quartiles
  arrange(soi, by_group = TRUE) %>%
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

# NUTS2
soi_nuts2 <- soi_areas %>% 
  left_join(nuts123) %>%
  group_by(soi, NUTS2) %>%
  summarise(area = sum(count)) %>%
  pivot_wider(names_from = soi, values_from = area, values_fill = 0) %>%
  # compute total mapped area by country
  rowwise() %>%
  mutate(nuts2_area = sum(c_across(!!as.character(min_soi) : as.character(!!max_soi)))) %>%
  ungroup()

soi_nuts2_pc <- soi_nuts2 %>%
  rowwise() %>%
  mutate(across(!!as.character(min_soi) : as.character(!!max_soi), ~round(.x/nuts2_area*100))) %>%
  ungroup() %>%
  dplyr::select(-nuts2_area) %>%
  # to long table
  pivot_longer(cols = where(is.numeric), names_to = "soi", values_to = "area")

# add expectation stats
soi_nuts2_m <- soi_nuts2_pc %>%
  # transform soi into numeric
  mutate(soi = as.numeric(soi)) %>%
  # basis to compute mean: soi times area
  mutate(soixarea =  soi * area) %>%
  # computations need to be made within groups
  group_by(NUTS2) %>%
  # sort soi values in each group to compute cumulative area, used to compute quartiles
  arrange(soi, by_group = TRUE) %>%
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

# load eurostat data
library(eurostat)
# Organic crop area by agricultural production methods and crops (from 2012 onwards) [ORG_CROPAR] 
org_cropar <- get_eurostat("org_cropar") %>%
  # year 2019 (2020 is incomplete)
  filter(time == as.Date("2019-01-01")) %>%
  # Fully converted and transition
  filter(agprdmet == "TOTAL") %>%
  # Percentage of total utilised agricultural area
  filter(unit == "PC_UAA") %>%
  # Utilised agricultural area excluding kitchen gardens
  filter(crops == "UAAXK0000")

# only available at country level
ggplot(soi_cntr_m %>% inner_join(org_cropar, by = c("CNTR_CODE" = "geo")),
       mapping = aes(x = median, y = values)) +
  geom_jitter() +
  geom_smooth(method = "glm", formula = y~x, se = FALSE)+
  xlab("Median # coinciding issues in NUTS0") + 
  ylab("Percentage UAA under organic ag.") 
ggsave(filename = "./fig/soi_cntr_m_vs_org_croppar.png", height = 4, width = 6)
# caption: Figure soi_cntr_m_vs_org_croppar Utilised agricultural area (UAA) fully converted and under conversion to organic agriculture (source: EUROSTAT) in EU27 countries for the year 2019 as a function of the median number of coinciding issues.
# lm results
soi_org_lm <- lm(values ~ median , data = soi_cntr_m %>% inner_join(org_cropar, by = c("CNTR_CODE" = "geo")))
with(summary(soi_org_lm), r.squared)


# Utilised agricultural area (UAA) managed by low-, medium- and high-input farms (source: FADN)
aei_ps_inp <- get_eurostat("aei_ps_inp") %>%
  filter(
    # year 2018 (2019 is incomplete)
    time == as.Date("2018-01-01"),
    # [PC_AREA] Percentage of area
    unit == "PC_AREA"
    # indic_ag %in% c("HIGH_INP", "LOW_INP", "MED_INP")
    )

ggplot(soi_nuts2_m %>% inner_join(aei_ps_inp, by = c("NUTS2" = "geo")),
       mapping = aes(x = median, y = values, colour = indic_ag)) +
  geom_jitter() +
  geom_smooth(method = "glm", formula = y~x, se = FALSE)+
  xlab("Median # coinciding issues in NUTS2") + 
  ylab("Percentage UAA") +
  scale_colour_discrete(name = "Management", labels = c("HIGH_INP" = "High input", "MED_INP" = "Medium input", "LOW_INP" = "Low input"))
ggsave(filename = "./fig/soi_nuts2_m_vs_aei_ps_inp.png", height = 6, width = 8)
# caption: Figure soi_nuts2_m_vs_aei_ps_inp Utilised agricultural area (UAA) managed by low-, medium- and high-input farms (source: FADN/EUROSTAT) in NUTS2 administrative units for the year 2018 as a function of the median number of coinciding issues.
# lm results
soi_inp_glm <- glm(values ~ median %in% indic_ag, data = soi_nuts2_m %>% inner_join(aei_ps_inp, by = c("NUTS2" = "geo")))
with(summary(soi_inp_glm), 1 - deviance/null.deviance)



