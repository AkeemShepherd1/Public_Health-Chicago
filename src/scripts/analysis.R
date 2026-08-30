# shootings by capita. Try to do by per 100k
# If you join the data set that contains population counts by community area
# with shooting totals for year 2025; you can get shootings per community area capita. this
# would be the total shootings divided by the total population per community area.

# Using the CVI boundary file from Source: CORNERS
# map CCAs where CVI orgs are active. The goal is to compare CVI boundaries
# w/ non-CVI boundaries for a dasage-based analysis on shootings.
# things to consider: there are other variables that affect shootings
#                     besides CVI-interventions

# to see some potential changes/history of CVI:
# Using organizations that have existed earlier in CVI to see changes in shootings overtime.

# dosage
names(dosage_analysis)
# define dosage columns
dosage <- dosage_analysis %>%
  select(c(org,
           community.x,
           shootings.x,
           area_numbe.y,
           total_pop,
           Percent_pop
           )
         )
dosage <- dosage %>%
  st_drop_geometry() %>%
  rename(community = community.x,
         shootings = shootings.x,
         area_numbe = area_numbe.y)


# restructure data set for dosage analysis
dosage_ <- dosage %>%
  group_by(community) %>%
  summarise(
    org = n(),
    shootings = first(shootings),
    total_pop = first(total_pop),
    Percent_pop = first(Percent_pop),
    area_numbe = first(area_numbe)
  )


# lowest org count is 2; highest org count is 25
dosage1 <- dosage_ %>%
  mutate(
    dosage_level = case_when(
      org <= 9 ~ "LOW",
      org > 9 & org <= 15 ~ "MEDIUM",
      org > 15 ~ "HIGH"
    )
  )

# run a correlation test to check relationship between dosage
# and shootings
cor(dosage1$org, dosage1$shootings)
# 0.5700792
# this correlation coefficient is moderate, as dosage increase 
# shooting incidents tend to increase. org counts and shootings 
# move together statistically.

# linear regression between org and total_pop with shootings
lm(shootings ~ org + total_pop, data = dosage1)
#Coefficients:
  #(Intercept)    org    total_pop  
# -2.795e+00    2.380e-01    7.451e-05  

# visualize in figures.R


# see how this graph changes with shootings normalized by population
# dosage1 <- dosage1 %>%
  # mutate(shooting_rate = shootings / total_pop * 10000)
#####
# data limitations: If data mapping CVI org creation/establishment is available
# overtime, I would have been able to test the relationships of changes 
# CVI orgs established in community areas and its affects on shootings overtime.
#
