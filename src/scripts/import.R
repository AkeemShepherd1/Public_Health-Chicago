
# data <- read.csv("~/Documents/GitHub_personal/Public_Health-Chicago/data/Violence_Reduction_-_Victims_of_Homicides_and_Non-Fatal_Shootings_20260106.csv")
# columns_needed <- read.csv("~/Documents/GitHub_personal/Public_Health-Chicago/data/Affordable_Rental_Housing_Developments_20260203.csv", header = TRUE)

# import file through path
vr_data <- read.csv("~/Documents/GitHub_personal/Public_Health-Chicago/data/VR data/vr_data.csv", 
                    header = TRUE, 
                    sep = ",", 
                    row.names = NULL)

#import boundary file using st_read which requires the "sf" package and converts file to an sf object
boundary_file <- st_read("~/Documents/GitHub_personal/Public_Health-Chicago/data/VR data/Boundaries - Community Areas_20260126/geo_export_72abcdaa-ed06-4240-95a9-1ac31523b938.shp")

# import Chicago_population data
Chicago_population_data <- read.csv("~/Documents/GitHub_personal/Public_Health-Chicago/data/VR data/Chicago_Population_Counts_20260202.csv",
                                    header = TRUE)
bq_auth()

# import cvi boundary shape_file from BQ
cvi_map_boundaries_raw <- bq_project_query("n3-main",
                                           "SELECT * FROM `n3-main.spatial.cvi_spatial_boundaries`") %>%
  bq_table_download() %>%
  st_as_sf(wkt = "geometry") %>%
  st_set_crs(4326) %>%
  # Project CCAs into projected coordinate system for consistent analysis
  st_transform(3435)

# there is a cleaner version of the 
chicago_areas <- st_read("~/Documents/GitHub_personal/Public_Health-Chicago/data/VR data/Boundaries - 2015-2025 shootings/community_areas.shp")
Cols <- read.csv("~/Documents/GitHub_personal/Public_Health-Chicago/data/Affordable_Rental_Housing_Developments_20260203.csv")
