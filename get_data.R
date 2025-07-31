source('R code/libs.R')
# setwd("")
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

options(scipen=10000)

# note this takes a while to load, you can also download the file onto your computer as a csv
# replace the line `read.socrata` with this code #read.csv("C:/Users/barboza-salerno.1/Downloads/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2022_release.csv") %>%
# Brittany and Kelly run each line of code to understand what it is doing on each iteration!!
cdc_places <-
  read.socrata("https://data.cdc.gov/resource/cwsq-ngmh.json?") %>%
  dplyr::filter(stateabbr == 'OH' & countyname == "Franklin" & year == 2021) %>%
  dplyr::mutate(x = gsub("[c()]", " ", geolocation.coordinates)) %>%
  separate(x,into= c("X","Y"), sep=",") %>%
  dplyr::select(
    GEOID = locationname,
    category = category,
    measure = measure,
    value = data_value,
    lci = low_confidence_limit,
    uci = high_confidence_limit,
    pop = totalpopulation, X, Y
  ) %>%
  dplyr::mutate(
    X = as.numeric(as.character(X)), # KARLA!!
    Y = as.numeric(as.character(Y)),
    value = as.numeric(as.character(value)),
    lci = as.numeric(as.character(lci)),
    uci = as.numeric(as.character(uci))
  ) %>%
  # Note the pivot_wider is critical here, it comes in long format which is unusable
  pivot_wider(names_from = c(category:measure), values_from = c(value:uci)) %>% 
  dplyr::select(GEOID, `value_Disability_Mobility disability among adults aged >=18 years`,
                `value_Disability_Any disability among adults aged >=18 years`,
                `value_Disability_Self-care disability among adults aged >=18 years` ,
                `value_Disability_Vision disability among adults aged >=18 years`,
                `value_Disability_Independent living disability among adults aged >=18 years`,
                `value_Disability_Hearing disability among adults aged >=18 years`,
                `value_Health Outcomes_Depression among adults aged >=18 years`, X, Y) %>%
  dplyr::rename(mobility = `value_Disability_Mobility disability among adults aged >=18 years`,
                selfcare = `value_Disability_Self-care disability among adults aged >=18 years` ,
                any = `value_Disability_Any disability among adults aged >=18 years` ,
                vision = `value_Disability_Vision disability among adults aged >=18 years`,
                indepliv = `value_Disability_Independent living disability among adults aged >=18 years`,
                hearing = `value_Disability_Hearing disability among adults aged >=18 years`,
                depression = `value_Health Outcomes_Depression among adults aged >=18 years` ) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") -> cdc_places_sf

# and there we have it, one line of code
######################################### Get ACS data for 2021 OH census tracts
# Note, the CDC data portal has these data listed as SDOH here
# https://data.cdc.gov/500-Cities-Places/SDOH-Measures-for-Census-Tract-ACS-2017-2021/e539-uadk/explore/query/SELECT%0A%20%20%60year%60%2C%0A%20%20%60stateabbr%60%2C%0A%20%20%60statedesc%60%2C%0A%20%20%60countyname%60%2C%0A%20%20%60countyfips%60%2C%0A%20%20%60locationname%60%2C%0A%20%20%60datasource%60%2C%0A%20%20%60category%60%2C%0A%20%20%60measure%60%2C%0A%20%20%60data_value_unit%60%2C%0A%20%20%60data_value_type%60%2C%0A%20%20%60data_value%60%2C%0A%20%20%60moe%60%2C%0A%20%20%60totalpopulation%60%2C%0A%20%20%60locationid%60%2C%0A%20%20%60categoryid%60%2C%0A%20%20%60measureid%60%2C%0A%20%20%60datavaluetypeid%60%2C%0A%20%20%60short_question_text%60%2C%0A%20%20%60geolocation%60%2C%0A%20%20%60%3A%40computed_region_skr5_azej%60%2C%0A%20%20%60%3A%40computed_region_hjsp_umg2%60/page/filter
# but they include it in long format as above and so its actually easier to download it directly from the census website

options(tigris_use_cache = TRUE)
# census_api_key("") which you get here: https://api.census.gov/data/key_signup.html
census_api_key("00d9b11e773673571eaeb53e6dfb80ca5eeafac3", overwrite = TRUE, install = T)
variable.list.5.year.acs.2021 <- load_variables(2021, "acs5", cache = TRUE)

variable.list.5.year.acs.2021 %>%
  {if (knitr::is_html_output()) datatable(., filter = 'top', rownames = F) else .}

var2021 <- load_variables(2021,"acs1/subject")

age21 <- c(age85plus="S0101_C01_019", 
           age80_84="S0101_C01_018",
           age75_79="S0101_C01_017",
           age70_74="S0101_C01_016",
           age65_69="S0101_C01_015",
           age60_64="S0101_C01_014",
           age55_59="S0101_C01_013",
           age50_54="S0101_C01_012",
           age45_49="S0101_C01_011",
           age40_44="S0101_C01_010",
           age35_39="S0101_C01_009",
           age30_34="S0101_C01_008",
           age25_29="S0101_C01_007",
           age20_24="S0101_C01_006",
           age15_19="S0101_C01_005",
           age10_14="S0101_C01_004",
           age5_9="S0101_C01_003",
           newborn_4="S0101_C01_002")

# Note, the function appends an "E" and "M" to each variable (Estimate, Margin of Error)
ohio_tract_ages_2021 <- get_acs(
  geography = "tract", county = "Franklin", 
  variables = age21, 
  summary_var = "S0101_C01_001",
  year = 2021, state = "OH", output = "wide"
  ) %>% 
  dplyr::select(ends_with("E"), summary_est, GEOID) %>%
  arrange(as.numeric(GEOID)) 

ohio_tracts_housing_2021 <- get_acs(
  state = "OH", 
  county = "Franklin", 
  geography = "tract", 
  variables = "B25105_001", year = 2021,
  geometry = TRUE
  ) %>%
  rename(median_housing_costs = estimate)

############################################### We need the Franklin county boundary file
st_read("https://gist.github.com/8DancingElephants/0090b05d2fa6ce42a1b6715a770837a8#file-ohcounties-json") %>%
  filter(COUNTY_SEA == "COLUMBUS") %>%
  st_set_crs(4326) %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") -> ohio

border <- st_union(ohio)

#######################################################################
############################################################# Get Redfin/Zillow Walkability Scores
cdc_places_sf$lat <- st_coordinates(cdc_places_sf)[, 2]
cdc_places_sf$lon <- st_coordinates(cdc_places_sf)[, 1]

mywalkscore <- function(x, y) {
  walk <- mapply(function(x1,y1) getWS(x1,y1,"39e05cbe5dd1860041cf909acb366618")$walkscore, x, y)
  unlist(walk)
}

df1 <- cdc_places_sf %>% select(lon, lat) %>%
  dplyr::mutate(score = walkscore::walkscore(., apikey = "39e05cbe5dd1860041cf909acb366618"))

df2<- cbind(df1, cdc_places_sf)
cdc_places <- cbind(cdc_places, df1)
#write.csv(df2, "../dep-walkability/data/walkability/walkability_SOM.csv")

cdc_places_sf$walkscore <- df2$score$walkscore
cdc_places_sf$walkdesc <- df2$score$description
cdc_places_sf$bikescore <- df2$score$bike.score
cdc_places_sf$bikedesc <- df2$score$bike.description
######################################################################
#url = "https://edg.epa.gov/EPADataCommons/public/OA/WalkabilityIndex.zip"
library(crsuggest)

walkies <- st_read("D:/projects/dep-walkability/dep-walkability/data/walkability/NationalWalkabilityI_Project1.shp") %>%
  st_set_crs(4326) %>%
  st_transform("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

rivers <- linear_water(state = "Ohio", "Franklin", class="sf")
water <- area_water(state = "Ohio", "Franklin", class = "sf")
rd <- roads(state = "Ohio", "Franklin", class = "sf") %>%
  filter(RTTYP == "I")

#################################################### the data is at the CBG level, aggregate UP to census tract
#################################################### and left  join to PLACEs data
walkies_CT <- walkies %>%
  unite("GEOID", STATEFP:TRACTCE, sep = "", remove = FALSE) %>%
  dplyr::select(GEOID, NatWalkInd) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarize(NatWalkInd = mean(NatWalkInd)) %>%
  mutate(
  across(where(is.numeric), ~replace_na(., replace = mean(., na.rm = TRUE))))

cdc_places_sf_nogeo <- cdc_places_sf %>% st_drop_geometry()
walkies_CT <- walkies_CT %>%  left_join(cdc_places_sf_nogeo)

ggplot() +
  geom_sf(data = walkies_CT, color = "white", size = 0.0725, aes(fill = NatWalkInd)) +
  geom_sf(data = rd, color = "white", size = 2, alpha = 1/10) +
  geom_sf(data = rd, color = "white", size = 1, alpha = 1/5) +
  geom_sf(data = rd, color = "white", size = 1/2, alpha = 1/2) +
  geom_sf(data = rivers, color = "lightblue", size = 1/2, alpha = 1/2) +
  geom_sf_label(
    data = ohio, aes(label = ""), size = 4, lineheight = 0.875,
    label.padding = unit(0.05, "lines"), label.size = 0, fill = "white"
  ) +
  scale_fill_viridis_c(
    direction = -1, limits = c(0,20), name = "Walkability Index",
    breaks = seq(0, 20, 5), labels = c("0 (Not at all walkable)", 5, 10, 15, "20 (Very walkable)")
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Walkability Index for Franklin County, Ohio, Ohio Census Tracts",
    caption = "Data sources: EPA DataCommons Walkability Index\nCenters for Disease Control Places Data"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.85, 0.25)) +
  theme(legend.title = element_text( hjust = 0.5)) +
  theme(legend.box.background = element_rect(color = "#2b2b2b", fill = "white")) + theme_void()

############################################## Merge ACS data on ages and housing
# Note you cannot merge two spatial datasets together with sf functions, so drop the geometry
# before doing the merge or get a nasty error

ohio_tract_ages_2021 <- ohio_tract_ages_2021 %>% st_drop_geometry()
ohio_tracts_housing_2021 <- ohio_tracts_housing_2021 %>% st_drop_geometry()

walkies_CT <- walkies_CT %>% left_join(ohio_tract_ages_2021) %>% left_join(ohio_tracts_housing_2021)

############### SAVE data we DONT want to run this again!!
#st_write(walkies_CT, "data/walkability/final-dep-walkability-dat.shp")
#write.csv(walkies_CT %>% st_drop_geometry(), "data/walkability/final-dep-walkability-dat.csv")

#final_df <- read.csv("data/walkability/final-dep-walkability-dat.csv") %>% mutate(GEOID = as.character(GEOID))
######################################### merge ADI and slope
dat1 <- st_read("D:/cleaned_network_franklin_slopes_merged_places_adi_walkability.geojson")
dat1 <- dat1 %>% 
  group_by(GEOID) %>% 
  select(GEOID, slope, length, ADI) %>%  
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

quantile(dat1$ADI, c(.25, .50, .75), na.rm=TRUE)

dat1 <- dat1 %>%
   mutate(ADI_Q = case_when(
     ADI <= 84.67209    ~  "Q25",
     (ADI > 84.67209    & ADI <= 98.63148 )  ~ "Q50",
     (ADI > 98.63148    & ADI <= 114.48766 )  ~ "Q75",
     ADI >= 114.48766  ~ "Q100",
   )) %>% left_join(final_df)

dat1$ADI_Q <- factor(dat1$ADI_Q, levels = c("Q25", "Q50", "Q75", "Q100"))

walkies_CT <- walkies_CT %>% st_drop_geometry()

dat_final <- dat1 %>% 
  select(GEOID, slope, length, ADI, ADI_Q) %>% 
  left_join(walkies_CT) %>% select(-bikedesc, -bikescore)
  
write.csv(dat_final %>% st_drop_geometry(), "data/walkability/final-dep-walkability-ADI-dat.csv")

############################### make table

(table1 <- 
    dat_final %>% mutate(age65plus = (age85plusE + age80_84E + age75_79E + age70_74E + age65_69E)/summary_est) %>%
  st_drop_geometry() %>%
  tbl_summary(
    by = ADI_Q,
    include = c(
      walkscore, 
      vision, 
      depression, 
      NatWalkInd, 
      age65plus, 
      mobility, 
      any, 
      selfcare, 
      vision, 
      indepliv, 
      hearing, 
      ADI, 
      length, 
      slope), 
    missing = "no",  
    label = list(walkscore ~ "WalkScore Premium",
      depression ~ "Depression (%)",
      vision ~ "Vision (%)")) %>%
  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() ) %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "/projects/dep-walkability/dep-walkability/output/tableone.docx")


(table2 <- 
    walkies_CT_sf_clust %>% 
    st_drop_geometry() %>%
    tbl_summary(
      by = fcluster,
      include = c(
        walkscore, 
        vision, 
        depression, 
        NatWalkInd, 
        age65plus, 
        mobility, 
        any, 
        selfcare, 
        vision, 
        indepliv, 
        hearing, 
        ADI, 
        length, 
        slope), 
      missing = "no",  
      label = list(walkscore ~ "WalkScore Premium",
                   depression ~ "Depression (%)",
                   vision ~ "Vision (%)")) %>%
    add_n() %>% # add column with total number of non-missing observations
    modify_header(label = "**Variable**") %>% # update the column header
    bold_labels() ) %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "D:/projects/dep-walkability/dep-walkability/output/tabletwo.docx")

