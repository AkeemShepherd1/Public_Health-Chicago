## <cleaning>----
## <base_processing>----
vr_data <- data %>%
  # Make the names lowercase
  clean_names() %>%
  mutate(
    primary_type = ifelse(victimization_primary == "HOMICIDE", "FATAL SHOOTING", "NON-FATAL SHOOTING"),
    fatal = victimization_primary == "HOMICIDE",
    # Clean date columns
    date = mdy_hms(date),
    year = year(as_date(date)),
    # tag month of observation (in 3-letter format "Jan", "Feb" and so on)
    month_text = month.abb[month(date)] %>% factor(levels = month.abb),
    # Instead of renaming rd_no = case_number
    rd_no = case_number,
    # Used to check how many are dropped by `drop_na(latitude)`
    na_latitude = is.na(latitude),
    # Again, for viz purposes, create a generic date of the first of the month
    # for each shooting (this is what we group_by later to summarize/count
    # by CCA)
    month_year = floor_date(as_date(date), "month") # generic date threw an error
  ) %>%
  # Note - for this analysis (and consistent with previous analyses we've done
  # using the VR dashboard dataset, just grab the GV records)
  filter(gunshot_injury_i == "YES") %>%
  # Because we want to be able to map incidents, drop any where latitude
  # is na. The number of dropped observations is 4 as of 1/7/2025
  walk(print(nrow(.))) %>% 
  drop_na(latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  # Deduplicate (drops 0 records as of 1/7/2025)
  distinct(case_number, unique_id, .keep_all = TRUE) %>% 
  walk(print(nrow(.))) %>% 
  st_transform(3435)


write.csv(vr_data, "~/Documents/GitHub_personal/data/vr_data.csv", row.names = FALSE)

## <total_shootings21>----
head(vr_data$updated)

vr_data <- vr_data %>%
  mutate(
    # from vr_data extract (latitude, as numeric
    lat = as.numeric(str_extract(updated, "(?<=\\().+?(?=,)")),
    # from vr_data extract ,longitude) as numeric
    lon = as.numeric(str_extract(updated, "(?<=,).+?(?=\\))"))
  ) 

# convert vr_data to a spatial dataframe using function "st_as_sf" from the sf package
vr_sf <- st_as_sf(
  vr_data,
  # use "wkt=" if column is formatted as Point(lat, long)
  wkt = "updated",
  crs = 4326
)

# check to make sure vr_sf is spatial and a dataframe
class(vr_sf)

# check geometry column in vr_sf
st_geometry(vr_sf)


## <total_shootings25>----

# goal: build a df that includes a subset of all data corresponding to year 2025
#       from vr_sf dataset.
#       join the new dataframe with the boundary file.
#       In the joined file, aggregate total shootings and drop geometric data. 
#       left join this file to the original boundary file by community area.


# filter vr_sf to represent 2021 based on date and time
vr_2025subset <- vr_sf[ # in the vr_sf dataframe column case_number, select all values 
  # corresponding to the first hour of 2025 to the last hour of 2025
  vr_sf$case_number >= as.POSIXct("2025-01-01 00:00:00") &
    vr_sf$case_number <= as.POSIXct("2025-12-31 23:59:59"), ]

# plot
#mapview(vr_2025subset["updated"])

# check the EPSG (these numbers should match)
#st_crs(vr_2025subset)
#st_crs(boundary_file)

# join the files
shootings_by_CA <- st_join(
  vr_2025subset,
  boundary_file,
  join = st_within
)

# aggreate shootings by community area
shootings_by_CA <- shootings_by_CA %>%
  st_drop_geometry() %>%
  count(ward, name = "total shootings") %>%
  rename(shootings = 'total shootings',
         community = 'ward')

#shootings_by_CA <- shootings_by_CA %>%
#rename(shootings = 'total shootings',
#community = 'ward')

# join shootings by_CA to boundary map
community_areas_map <- boundary_file %>%
  left_join(shootings_by_CA, by = "community") %>%
  mutate(shootings = replace_na(shootings, 0)) %>%
  # convert column type to integer for plot capabilities
  mutate(area_num_1 = as.integer(area_num_1))


## <per_chg_24_25>----
# MAP: of 77 community areas with a gradient fill showing the % change in 
# shootings between 2024 and 2025

data2024_25 <- vr_data[vr_data$case_number >= as.POSIXct("2023-01-01 00:00:00") &
                         vr_data$case_number <= as.POSIXct("2025-12-31 23:59:59"), ]

data2024_25$case_number <- substr(data2024_25$case_number, 1, 4) 


shootings_perchng <- data2015_25 %>%
  filter(incident_primary == "YES") %>%        
  group_by(ward, case_number) %>%
  summarise(
    incident_primary = sum(incident_primary == "YES", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = case_number,
    values_from = incident_primary,
    names_prefix = "shootings_",
    values_fill = 0
  )

# compute percent change for 24_25
shootings_perchng <- shootings_perchng %>%
  mutate(
    pct_change_24_25 = case_when(
      shootings_2024 == 0 & shootings_2025 == 0 ~ 0,
      shootings_2024 == 0 & shootings_2025 > 0 ~ NA_real_,  # undefined / infinite
      TRUE ~ ((shootings_2025 - shootings_2024) / shootings_2024) * 100
    )
  )

# subset ward and percent change from the shootings_perchng

subset_shootings_perchng <- shootings_perchng %>%
  select(ward, pct_change_24_25) %>%
  rename(community = ward)

# left_join subset_shooting_perchng with boundary file for spatial viz

joined_shootings_perchng <- boundary_file %>%
  left_join(subset_shootings_perchng, by = "community") %>%
  mutate(pct_change_24_25 = replace_na(pct_change_24_25, 0))

# bin percent change values for a cleaner visualization
joined_shootings_perchng <- joined_shootings_perchng %>%
  mutate(
    pct_bin = case_when(
      pct_change_24_25 <= -100 ~ "Large Decrease",
      pct_change_24_25 <= -50 ~ "Moderate Decrease",
      pct_change_24_25 < -30    ~ "Small Decrease",
      pct_change_24_25 == 0   ~ "No Change",
      pct_change_24_25 <= 80  ~ "Small Increase",
      pct_change_24_25 <= 100  ~ "Moderate Increase",
      TRUE                  ~ "Large Increase"
    )
  )

# order bins for plotting
joined_shootings_perchng <- joined_shootings_perchng %>%
  mutate(
    pct_bin = factor(
      pct_bin,
      levels = c(
        "Large Decrease",
        "Moderate Decrease",
        "Small Decrease",
        "No Change",
        "Small Increase",
        "Moderate Increase",
        "Large Increase"
      ),
      ordered = TRUE
    )
  )


## <total_pop_by_community_area>----
# all community total population code lay below. 
# community level population data did not contain name of area or area num, but did contain
# zip_code. Because of this, I used a real estate dataset that held columns containing 
# both zip_codes, and area names and matched those to population dataset based on zip_codes while i
# including community area names

# Citywide population counts 
citywide_tot_pop <- Chicago_population_data %>%
  filter(Geography.Type == "Citywide")

# Zipcode population counts
Chicago_population_data <- Chicago_population_data %>%
  filter(Geography.Type != "Citywide") %>%
  rename(zipcode = Geography) %>%
  select(-starts_with("Geography.Type"))

# change zipcode colum type from character to integer to support join later
Chicago_population_data <- Chicago_population_data %>%
  mutate(zipcode = as.integer(zipcode))

# ===========================
# Fin_Chicago_population_dataset was created using "COLS df that came from a 
# columns_needed" dataset that contained all community areas with corresponding zipcodes
# and area codes. The dataset 'COLS' is a housing dataset mined from Chicago Open Data, 
# and was slected specifically for columns area number, community area name, and zipcodes.
# The Final Chicago Population file was created with the "COLS" and raw Chicago 
# population data that came from Chicago Open data, which has total population variables
# for 2018 to 2021 by Zipcode.

# Note: the 'columns_needed' dataset did not contain area codes and correpsonding community area 
# and zip codes for the following areas: 12, 20, 47, 52, 57, 59, 64, 72, 74, 75, 76.
# I would later add these manually using 2023 population estimates provided by 
# Chicago Metropolitan Agency for Planning (CMAP), .

Cols <- Cols %>%
  select(Community.Area.Name,
         Community.Area.Number,
         Zip.Code) %>%
  rename(community = Community.Area.Name,
         area_num = Community.Area.Number,
         zipcode = Zip.Code)

Cols <- Cols %>%
  group_by(community, zipcode) %>%
  summarise(total = sum(area_num, na.rm = TRUE))

Cols <- Cols %>%
  select(-starts_with("total"))

# create Final Chicago Populaiton Dataset
Fin_Chicago_population_data <- Cols %>% 
             left_join(Chicago_population_data, by = "zipcode") 
# ============

Chicago_pop2021 <- Fin_Chicago_population_data %>%
  filter(Year == "2021")

Chicago_pop2021 <- Chicago_pop2021 %>%
  select(community, zipcode, Year, Population...Female, Population...Male) %>%
  mutate(
    Population...Female = as.numeric(gsub(",", "", Population...Female)),
    Population...Male = as.numeric(gsub(",", "", Population...Male)),
    population = Population...Female + Population...Male) %>%
  select(-starts_with("zipcode")) %>%
  select(-starts_with("Year"))

Chicago_pop2021<- Chicago_pop2021 %>%
  group_by(community) %>%
  summarise(total_pop = sum(population, na.rm = TRUE)) 

Chicago_pop2021 <- Chicago_pop2021 %>%
  mutate(across(where(is.character), toupper))

Chicago_pop2021 <- boundary_file %>%
  left_join(Chicago_pop2021, by = "community")

# these are filled in with population estimates for 2023, 
# according to https://cmap.illinois.gov/data/community-data-snapshots/
# the Chicago_pop_2021 dataset is essentially frankenstined, where total populations
# represent mostly 2021 data from Chicago Open data and 2023 data from cmap (cmap used to sub
# NAs-NAs in the case area created due to lack of representation in the COLS dataset for community area
# values: 12, 20, 47, 52, 57, 59, 64, 72, 74, 75, 76)

Chicago_pop2021 <- Chicago_pop2021 %>%
  mutate(
    total_pop = case_when(
      is.na(total_pop) & community == "FOREST GLEN" ~ 19517,
      is.na(total_pop) & community == "HERMOSA" ~ 22776,
      is.na(total_pop) & community == "BURNSIDE" ~ 2148,
      is.na(total_pop) & community == "EAST SIDE" ~ 22722,
      is.na(total_pop) & community == "RIVERDALE" ~ 7536,
      is.na(total_pop) & community == "ARCHER HEIGHTS" ~ 14021,
      is.na(total_pop) & community == "MCKINLEY PARK" ~ 15443,
      is.na(total_pop) & community == "CLEARING" ~ 24924,
      is.na(total_pop) & community == "BEVERLY" ~ 19570,
      is.na(total_pop) & community == "MOUNT GREENWOOD" ~ 18553,
      is.na(total_pop) & community == "MORGAN PARK" ~ 21325,
      is.na(total_pop) & community == "OHARE" ~ 14004,
      TRUE ~ total_pop
    )
  )

# using Chicago_pop2021 get population percentages by community area to the 
# nearest hundredth 


Chicago_pop2021 <- Chicago_pop2021 %>%
  mutate(
    Percent_pop = round(total_pop / sum(total_pop) * 100, 2)
  )


  

## <shootings_by_CCA_pop_capita>----
# join shootings for year 2025 to CCA population 2021 boundary file
shootings_per_cap <- Chicago_pop2021 %>%
  left_join(shootings_by_CA, by = "community") %>%
  mutate(shootings = ifelse(is.na(shootings), 0, shootings))
# For this analysis should I drop NAs, which is an indication for areas
# without shootings for year 2025 (convert the NAs to 0)

# shootings/population X 100000 to get shootings by CCA

shootings_per_cap <- shootings_per_cap %>%
  mutate(
    shootings_per_100k = (shootings / total_pop) * 100000
  ) %>%
  mutate(
shootings_per_100k = round(shootings_per_100k, 1)
)

## <dosage_based_analysis>----
# filter the data set to exclude all CVI-orgs that no longer exist
# layer cvi map boundaries frame with community areas frame
# first check crs 
st_crs(cvi_map_boundaries_raw) # 3435
st_crs(community_areas_map) # 4326
# because these frames have different EPSG, transform one cvi_map_boundaries_raw
# to match community_areas_map to projected EPSG
cvi_map_boundaries_raw <- st_transform(cvi_map_boundaries_raw, 3435)
community_areas_map <- st_transform(community_areas_map, 3435)

intersection_layer <- st_intersection(community_areas_map, cvi_map_boundaries_raw)

intersection_layer <- intersection_layer %>%
  select(-c(program_type, boundary_end_date)) %>%
  filter(current_map == "TRUE")

intersection_layer <-
  st_collection_extract(intersection_layer, "POLYGON")
write.csv(intersection_layer, "File_to_match.csv")

  
# To make a dosage based analysis possible, the model needs
# data set that has 'community_area', 'population','2025_shootings',
# and 'outreach orgs'.
# step one: build the data set by joining shootings_by_CA file to community_areas_map file.
#   join the population by community area file to shootings with cvi_map_boundaries.

dosage_analysis <-  community_areas_map %>%
  left_join(shootings_by_CA, by = "community") 

# check CRS
st_crs(dosage_analysis) #3435
st_crs(Chicago_pop2021) #4326

# match CRS/ESPG of data sets
Chicago_pop2021 <- st_transform(
  Chicago_pop2021,
  st_crs(dosage_analysis)
)

# join
dosage_analysis <- dosage_analysis %>%
  st_join(Chicago_pop2021, by = "community") 

dosage_analysis <- dosage_analysis %>%
  rename(community=community.x)


dosage_analysis <- dosage_analysis %>%
  st_join(intersection_layer, by = "community")

# Now that there's a dosage data set, proceed to building the model in analysis.R.


