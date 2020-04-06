# Title: Extract and Munge
# Author: Tim Essam
# Date: 2020-04-05
# Notes:


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(sf)
library(glitr)


# GLOBALS -----------------------------------------------------------------

  data_in <- "Data"
  data_out <- "Dataout"
  gis <- "GIS"
  
  
  states <- c("Virginia", "District of Columbia", "Maryland")
  
  prinf <- function(df) {
    df %>% print(., n = Inf)
  }




# LOAD AND MUNG -----------------------------------------------------------

link <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

df_geo <- st_read(file.path(gis, "Virginia_Cities_and_Counties.shp"), stringsAsFactors = FALSE)

df <- read_csv(link) %>% filter(state %in% states) %>% 
  arrange(county, date) %>% 
  group_by(state, county) %>% 
  mutate(max_cases = max(cases)) %>% 
  ungroup()


va <- df_geo %>% 
  rename(fips = STCOFIPS) %>% 
  left_join(., df %>% filter(state == "Virginia"), by = c("fips")) 


df %>% count(county, state) %>% arrange(county) %>% prinf


# PLOT --------------------------------------------------------------------


ggplot(va, aes(x = date, y = cases, group = paste(county), 
               colour = state)) + geom_line(colour = "#D3D3D3") +
  geom_line(data = df_arl, aes(x = date, y = cases), colour = "#505050") +
  ggrepel::geom_label_repel(data = df_arl, aes(label = label)) +
  scale_y_log10() + si_style()


va %>% 
  filter(state == "Virginia", max_cases > 50) %>% 
  mutate(county_sort = fct_reorder(county, cases, .fun = max)) %>% 
  ggplot(., aes(x = date, y = county_sort, fill = cases)) + 
  geom_tile(colour = "white", size = 0.25) + scale_fill_viridis_c() +
  si_style_nolines()

df_arl <- va %>% filter(county == "Arlington") %>% 
  mutate(label = if_else(date == max(date), cases, NA_real_))

va %>% ggplot() +
  geom_sf(aes(fill = max_cases), colour = "white", size = 0.25) +
  scale_fill_viridis_c(option = "D", na.value = "#d3d3d3") + 
  theme_void() +
  si_style_nolines()




  

  