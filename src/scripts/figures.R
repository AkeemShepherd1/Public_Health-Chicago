## <figures>----
## <shootings2021>----
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

# check geometry column in vr_sf; this should not be NULL
st_geometry(vr_sf)


# filter vr_sf to represent 2021 based on date and time
vr_subset <- vr_sf[ # in the vr_sf dataframe column case_number, select all values 
  # corresponding to the first hour of 2021 to the last hour of 2021
  vr_sf$case_number >= as.POSIXct("2021-01-01 00:00:00") &
                       vr_sf$case_number <= as.POSIXct("2021-12-31 23:59:59"), ]

# plot
mapview(vr_subset["updated"])
mapview(cvi_map_boundaries_raw["geometry"])
mapview()
# What have shootings trends been like over the past 10 years? (Have they gone up? Down?) Try one or more of:
# LINE GRAPH: total citywide shootings for each year from 2016-2025

## <line_graph>----

# filter the dataset to represent the past 10 years
data2016_25 <- vr_data[vr_data$case_number >= as.POSIXct("2016-01-01 00:00:00") &
  vr_data$case_number <= as.POSIXct("2025-12-31 23:59:59"), ]


data2016_25$case_number <- substr(data2016_25$case_number, 1, 4) 
  

line_viz <- data2016_25 %>%
  mutate(case_number = as.integer(case_number)) %>%   # make year numeric
  group_by(case_number) %>%
  summarise(incident_primary = n(), .groups = "drop") %>%
  arrange(case_number)                            # order by year

ggplot(line_viz, aes(x = case_number, y = incident_primary)) +
  geom_line(color = "purple") +
  geom_point(color = "purple", size = 1) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_vline(xintercept = 2015, linewidth = 0.2) +
  scale_x_continuous(breaks = line_viz$case_number) +
  labs(
    title = "Total Shooting Incidents by Year in Chicago",
    x = "Year",
    y = "Counts"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", 
                                  size = 10, 
                                  family = "Times New Roman"),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        axis.title.x = element_text(size = 6,
                                    face = "italic"),
        axis.title.y = element_text(size = 6,
                                    face = "italic")
)

## <bar_graph>----
# BAR CHART: % change in total shootings for each year over time, using 2015 as the "start" year

data2015_25 <- vr_data[vr_data$case_number >= as.POSIXct("2015-01-01 00:00:00") &
                         vr_data$case_number <= as.POSIXct("2025-12-31 23:59:59"), ]

data2015_25$case_number <- substr(data2015_25$case_number, 1, 4) 

# Aggregate total incidents by year
per_chg <- data2015_25 %>%
  mutate(year = as.integer(case_number)) %>%
  filter(year >= 2015) %>%    # only 2015 to current
  group_by(year) %>%
  summarise(incidents = n(), .groups = "drop") %>%
  arrange(year) %>%
  mutate(
    pct_change = (incidents / lag(incidents) - 1) * 100
  )

per_chg <- per_chg %>%
  filter(year!=2015)

#barchart 
ggplot(per_chg, aes(x = year, y = pct_change)) +
  geom_col(fill = "purple") +
  geom_label(
    aes(
      label = sprintf("%.1f%%", pct_change)), 
      vjust = ifelse(per_chg$pct_change >= 0, -0.02, 1), 
      size = 2.5
    ) +  
  geom_hline(yintercept = 0, linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1), 
                     limits = c(-60,60)) +
  theme_classic() + 
  labs(
    title = "Year-Over-Year Percent Change in Shooting Incidents in Chicago",
    x = "year",
    y = "%"
  ) +
  scale_x_continuous(breaks = per_chg$year) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 10,
                                  family = "Times New Roman"),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 9,
                                    face = "italic"),
        axis.title.y = element_text(size = 9,
                                    face = "italic")

)


## <shootings2025byCA>----
# MAP: of 77 community areas with a gradient fill showing the total 
# number of shootings in 2025
names(community_areas_map)
# plot
map_plot <- ggplot(community_areas_map) +
  geom_sf(aes(fill = shootings), color = "black", linewidth = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "purple", #009 -LOOKS NICE and #56b
    name = "Counts: 0 - 158",
    breaks = c(120,80,40,0)
    ) +
labs(
    title = "Total Shootings (2025)",
    subtitle = "Chicago Community Areas",
    caption = "Source: Violence Reduction Dashboard"
  ) +
  theme_minimal()


map_plot +
  #geom_sf_text(
    #aes(label = paste0(area_num_1)),
    #size = 1,
    #color = "black"
  #) +
  theme_void() +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme(legend.title = element_text(face = "italic",
                              size = 6,
                              family = "Times New Roman"),
        legend.text = element_text(face = "italic",
                              size = 6,
                              family = "Times New Roman"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 10,
                                  family = "Times New Roman"),
        plot.subtitle = element_text(face = "bold",
                                     size = 8,
                                     family = "Times New Roman"),
        plot.caption = element_text(face = "italic",
                                    size = 5),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")
)

## <%chngshootings24-25>----
# MAP: of 77 community areas with a gradient fill showing the % change in 
# shootings between 2024 and 2025

# names(joined_shootings_perchng)

# plot
map_plot <- ggplot(joined_shootings_perchng) +
  geom_sf(aes(fill = pct_bin), color = "black", linewidth = 0.1) +
  scale_fill_brewer(palette = "PuOr",
                    name = "Percent Changes") +
    #low = "white",
    #high = "purple", #009 -LOOKS NICE and #56b
    #name = "-100 - 900%",
    #breaks = c(750,500,250,0,-100)
  #) +
  labs(
    title = "Year-Over-Year Percent Change in Shootings (2024-2025)",
    subtitle = "Chicago Community Areas",
    caption = "Source: Violence Reduction Dashboard"
  ) 


map_plot +
  #geom_sf_text(
    #aes(label = paste0(area_num_1)),
    #size = 1,
    #color = "black"
  #) +
  theme_void() +
  theme(legend.title = element_text(face = "italic",
                                    size = 6,
                                    family = "Times New Roman",
                                    margin = margin(b=10)),
        legend.text = element_text(face = "italic",
                                   size = 6,
                                   family = "Times New Roman"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 10,
                                  family = "Times New Roman"),
        plot.subtitle = element_text(face = "bold",
                                     size = 8,
                                     family = "Times New Roman"),
        plot.caption = element_text(face = "italic",
                                    size = 5),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm")
  )


## <total_population>----

mapplot <- ggplot(Chicago_pop2021) +
  geom_sf(aes(fill = Percent_pop), color = "black", linewidth = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "purple", #009 -LOOKS NICE and #56b
    name = "",
    #breaks = c(750,500,250,0,-100)
  ) +
  labs(
    title = "Total Population Estimates (2021)",
    subtitle = "Chicago Community Areas",
    caption = "Source: Chicago Open Data, Census, & Chicago Metropolitan Agency for Planning (CMPA)"
  ) +
  theme_minimal()

mapplot +
  #geom_sf_text(
    #aes(label = paste0(area_num_1)),
    #size = 1.5,
    #color = "black"
  #) +
  theme_void() +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 10,
                                  family = "Times New Roman"),
        plot.subtitle = element_text(face = "bold",
                                     size = 8,
                                     family = "Times New Roman"),
        plot.caption = element_text(face = "italic",
                                    size = 5)
  )

## <100K_cap_shooting>----
mapplot <- ggplot(shootings_per_cap) +
  geom_sf(aes(fill = shootings_per_100k), color = "black", linewidth = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "purple", #009 -LOOKS NICE and #56b
    name = "shootings [0 - 358]",
    breaks = c(350,250,150,50)
  ) +
  labs(
    title = "Shootings per 100k capita",
    subtitle = "Chicago Community Areas",
    caption = "Source: Chicago Open Data, Census, & Chicago Metropolitan Agency for Planning (CMPA)"
  ) +
  theme_minimal()

mapplot +
  #geom_sf_text(
  #aes(label = paste0(area_num_1)),
  #size = 1.5,
  #color = "black"
  #) +
  theme_void() +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme(#legend.position = "none",
        legend.title = element_text(face = "italic",
                                    size = 6),
        legend.text = element_text(face = "italic",
                                   size = 6),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(0.3, "cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 10,
                                  family = "Times New Roman"),
        plot.subtitle = element_text(face = "bold",
                                     size = 8,
                                     family = "Times New Roman"),
        plot.caption = element_text(face = "italic",
                                    size = 5)
  )






## <CVI in CCA Map> ----
cvi_in_cca <- st_intersection(
  st_union(community_areas_map), # union ensures we clip only within Chicago
  cvi_map_boundaries_raw
) %>%
  st_collection_extract("POLYGON")
#filter(!st_is_empty(.))

ggplot() +
  # Base layer: CCAs boundaries
  geom_sf(data = community_areas_map,
          fill = "grey95",  # light fill for context
          color = "white") + # boundaries visible
  # Overlay layer: shaded CVI areas
  geom_sf(data = cvi_in_cca,
          fill = "blue",    # shaded color
          alpha = 0.5,      # semi-transparent
          color = NA) +     # no borders for overlay
  theme_void()  
## <Dosage analysis>----
mapplot <- ggplot(dosage1, aes(x = org, y = shootings)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "Number of Organizations",
    y = "Shootings",
    title = "Relationship Between CVI Dosage and Shootings"
  )

mapplot +
  theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 10,
                                  family = "Times New Roman"),
       
  )
