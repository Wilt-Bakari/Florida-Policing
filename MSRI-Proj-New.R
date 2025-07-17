# install.packages("readxl")
# install.packages("usmap")
# install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
# install.packages('tinytex', repos = "http://cran.us.r-project.org")
# devtools::install_github("UrbanInstitute/urbnmapr")
# install.packages("urbanmapr")
# install.packages("tidycensus")
# install.packages("viridis")
# install.packages("sf")
# install.packages("pals")


# Load all required packages
library(ggplot2)      # For visualizations
library(dplyr)        # For data manipulation
library(plotly)       # For interactive plots
library(readr)        # For data loading
library(tidyr)        # For data reshaping
library(scales)       # For better axis formatting
library(readxl)
library(tidyverse)
library(lubridate)
library(urbnmapr)
library(rmarkdown)
library(tidycensus)
library(mapview)
library(leaflet)
library(viridis)
library(pals)
library(sf)

fl_pop_tposed <- read.csv("MSRI-Proj/FL_2016_pop_tposed.csv")
fl_pop <- read.csv("MSRI-Proj/FL_2016_pop.csv")
fl_pop_changed <- read.csv("MSRI-Proj/FL_2016_pop_changed.csv")

# Reorder following the value of another column:
fl_pop_gen <- fl_pop %>%
  mutate(Race = fct_reorder(Race, Free)) %>%
  ggplot( aes(x=Race, y=Free)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
fl_pop_gen

fl_pop_incar <- fl_pop %>%
  mutate(Race = fct_reorder(Race, Incarcerated)) %>%
  ggplot( aes(x=Race, y=Incarcerated)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
fl_pop_incar

fl_pop_prop_race <- fl_pop %>%
  mutate(Prop = (Incarcerated/(Incarcerated+Free))*100) %>%
  mutate(Race = fct_reorder(Race, Prop)) %>%
  ggplot( aes(x=Race, y=Prop)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
fl_pop_prop_race

fl_pop_prop_gen <- fl_pop %>%
  mutate(Prop_Gen = (Incarcerated/(sum(Incarcerated)))*100) %>%
  mutate(Race = fct_reorder(Race, Prop_Gen)) %>%
  ggplot( aes(x=Race, y=Prop_Gen)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
fl_pop_prop_gen


################################################################################

fl_pop_counties <- read_xlsx("MSRI-Proj/FL_pop_counties.xlsx") %>%
  mutate(`ASIAN/HAW/PACISL` = ASIAN + `HAW/PACISL`,
         OTHER = MULTI + `INDIAN/ALAS`) %>%
  select(-c(ASIAN,MULTI,`INDIAN/ALAS`,`HAW/PACISL`))

counties <- usmap::us_map(regions = "counties")

plt_fl_pop_counties_white<- fl_pop_counties %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("COUNTIES" = "county")) %>%
  mutate(WHITE = fl_pop_counties$WHITE)

ggplot() +
  geom_sf(plt_fl_pop_counties_white,
          mapping = aes(fill = WHITE, geometry = geom),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) + scale_fill_viridis_c() + 
  labs(fill = "Population",
       title = paste0("White Population of Florida in 2016- by County"))

################################################################################

plt_fl_pop_counties_black<- fl_pop_counties %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("COUNTIES" = "county")) %>%
  mutate(`BLACK(NH)` = fl_pop_counties$`BLACK(NH)`)

ggplot() +
  geom_sf(plt_fl_pop_counties_black,
          mapping = aes(fill = `BLACK(NH)`, geometry = geom),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) + scale_fill_viridis_c() + 
  labs(fill = "Population",
       title = paste0("Black Population of Florida in 2016- by County"))

################################################################################

plt_fl_pop_counties_hispanic<- fl_pop_counties %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("COUNTIES" = "county")) %>%
  mutate(`HISPANIC/LATINO` = fl_pop_counties$`HISPANIC/LATINO`)

ggplot() +
  geom_sf(plt_fl_pop_counties_hispanic,
          mapping = aes(fill = `HISPANIC/LATINO`, geometry = geom),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) + scale_fill_viridis_c() + 
  labs(fill = "Population",
       title = paste0("Hispanic/Latino Population of Florida in 2016- by County"))

################################################################################

plt_fl_pop_counties_asianhawpac<- fl_pop_counties %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("COUNTIES" = "county")) %>%
  mutate(`ASIAN/HAW/PACISL` = fl_pop_counties$`ASIAN/HAW/PACISL`)

ggplot() +
  geom_sf(plt_fl_pop_counties_asianhawpac,
          mapping = aes(fill = `ASIAN/HAW/PACISL`, geometry = geom),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) + scale_fill_viridis_c() + 
  labs(fill = "Population",
       title = paste0("Asian, Hawaiian, and Pacific Islander Population of Florida in 2016- by County"))

################################################################################

plt_fl_pop_counties_other<- fl_pop_counties %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("COUNTIES" = "county")) %>%
  mutate(`OTHER` = fl_pop_counties$`OTHER`)

ggplot() +
  geom_sf(plt_fl_pop_counties_other,
          mapping = aes(fill = `OTHER`, geometry = geom),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) + scale_fill_viridis_c() + 
  labs(fill = "Population",
       title = paste0("Other (Indian/Alaskan Native/Mixed) Population of Florida in 2016- by County"))

################################################################################

fl_traffic_stops <- read_rds("MSRI-Proj/FL_traffic_stops.rds")
fl_traffic_stops_clean <- fl_traffic_stops %>%
  filter(year(date) == 2016) %>%
  select(c(date,county_name,subject_race,subject_sex,officer_race,outcome))

################################################################################

plt_fl_stops_counties <- fl_traffic_stops_clean %>%
  group_by(county_name) %>%
  summarise(count_stops = n(), .groups = "drop") %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("county_name" = "county"))

ggplot() +
  geom_sf(plt_fl_stops_counties,
          mapping = aes(fill = `count_stops`, geometry = geom),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) + scale_fill_viridis_c() + 
  labs(fill = "Stops",
       title = paste0("Traffic Stops in 2016")
  )

################################################################################

plt_fl_stops_race_counties <- fl_traffic_stops_clean %>%
  group_by(county_name,subject_race,outcome) %>%
  summarise(count_stops = n(), .groups = "drop") %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("county_name" = "county"))
plt_fl_stops_race_topfive <- plt_fl_stops_race_counties[plt_fl_stops_race_counties$fips %in% c("12086", "12099", "12095", "12011", "12057"), ]
ggplot(plt_fl_stops_race_topfive,aes(fill=subject_race,y=count_stops,x=outcome)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ county_name) +
  coord_flip()

################################################################################

plt_fl_citations_counties <- fl_traffic_stops_clean %>%
  filter(outcome == "arrest") %>%
  group_by(county_name) %>%
  summarise(count_stops = n(), .groups = "drop") %>%
  right_join(counties %>% filter(full == "Florida"),
             by = c("county_name" = "county"))

ggplot() +
  geom_sf(plt_fl_citations_counties,
          mapping = aes(fill = `count_stops`, geometry = geom),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) + scale_fill_viridis_c() + 
  labs(fill = "Stops",
       title = paste0("Traffic Stops Resulting in Citations in 2016")
  )

################################################################################
years = 2010:2020
my_vars <- c("B19013_001")

income_data_counties <- map_dfr(years, function(y) {
  get_acs(
    geography = "county",  # Example: county level
    state = "FL",           # Example: California
    variables = my_vars,
    year = y,
    survey = "acs5",       # 5-year ACS estimates
    geometry = FALSE        # No spatial data
  ) %>%
    mutate(year = y) %>%  # Add a year column
    mutate(NAME = gsub(", Florida","",NAME)) %>%
    right_join(counties %>% filter(full == "Florida"),
               by = c("NAME" = "county"))  # Add a year column
})

income_data_counties_2010 <- income_data_counties %>%
  filter(year == 2010)
income_data_counties_2011 <- income_data_counties %>%
  filter(year == 2011)
income_data_counties_2012 <- income_data_counties %>%
  filter(year == 2012)
income_data_counties_2013 <- income_data_counties %>%
  filter(year == 2013)
income_data_counties_2014 <- income_data_counties %>%
  filter(year == 2014)
income_data_counties_2015 <- income_data_counties %>%
  filter(year == 2015)
income_data_counties_2016 <- income_data_counties %>%
  filter(year == 2016)
income_data_counties_2017 <- income_data_counties %>%
  filter(year == 2017)
income_data_counties_2018 <- income_data_counties %>%
  filter(year == 2018)
income_data_counties_2019 <- income_data_counties %>%
  filter(year == 2019)
income_data_counties_2020 <- income_data_counties %>%
  filter(year == 2020)

income_data_counties_2010_spatial <- st_as_sf(income_data_counties_2010)
income_data_counties_2011_spatial <- st_as_sf(income_data_counties_2011)
income_data_counties_2012_spatial <- st_as_sf(income_data_counties_2012)
income_data_counties_2013_spatial <- st_as_sf(income_data_counties_2013)
income_data_counties_2014_spatial <- st_as_sf(income_data_counties_2014)
income_data_counties_2015_spatial <- st_as_sf(income_data_counties_2015)
income_data_counties_2016_spatial <- st_as_sf(income_data_counties_2016)
income_data_counties_2017_spatial <- st_as_sf(income_data_counties_2017)
income_data_counties_2018_spatial <- st_as_sf(income_data_counties_2018)
income_data_counties_2019_spatial <- st_as_sf(income_data_counties_2019)
income_data_counties_2020_spatial <- st_as_sf(income_data_counties_2020)

m10 <- mapview::mapview(income_data_counties_2010_spatial, zcol='estimate',
                       layer.name='Estimated Median Income',
                       legend = FALSE,
                       col.regions = viridis
)
m11 <- mapview::mapview(income_data_counties_2011_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m12 <- mapview::mapview(income_data_counties_2012_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m13 <- mapview::mapview(income_data_counties_2013_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m14 <- mapview::mapview(income_data_counties_2014_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m15 <- mapview::mapview(income_data_counties_2015_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m16 <- mapview::mapview(income_data_counties_2016_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m17 <- mapview::mapview(income_data_counties_2017_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m18 <- mapview::mapview(income_data_counties_2018_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m19 <- mapview::mapview(income_data_counties_2019_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)
m20 <- mapview::mapview(income_data_counties_2020_spatial, zcol='estimate',
                        layer.name='Estimated Median Income',
                        legend = FALSE,
                        col.regions = viridis
)

pal_continuous <- colorNumeric(
  palette = viridis(400),
  domain = sort(income_data_counties$estimate,decreasing = FALSE)
)


# ggplot() +
#   geom_sf(income_data_2016,
#           mapping = aes(fill = `estimate`, geometry = geom),
#           color = "white", size = 0.05) +
#   coord_sf(datum = NA) + scale_fill_viridis_c() + 
#   labs(fill = "Income",
#        title = paste0("Income of Florida in 2016")
#   )