# Title: Extract and Munge
# Author: Tim Essam
# Date: 2020-04-05
# Notes:


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(glitr)
library(here)


# GLOBALS -----------------------------------------------------------------

  data_in <- "Data"
  data_out <- "Dataout"
  gis <- "GIS"
  image <- "Images"
  
  
  
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
  ungroup() %>% 
  group_by(county, state) %>% 
  mutate(daily_cases = cases - lag(cases, order_by = date)) %>% 
  ungroup


va <- df_geo %>% 
  rename(fips = STCOFIPS) %>% 
  right_join(., df %>% filter(state %in% c("Virginia", "District of Columbia")), by = c("fips")) 


df %>% count(county, state) %>% arrange(county) %>% prinf

df_arl <- va %>% filter(cases > 10000) %>% 
  mutate(label = if_else(date == max(date), paste(cases, county), NA_character_))


# PLOT --------------------------------------------------------------------

date_min <- df %>% summarise(min = min(date, na.rm = TRUE)) %>% pull()
date_max <- df %>% summarise(max = max(date, na.rm = TRUE) + 10) %>% pull()

ggplot(va, aes(x = date, y = cases, group = paste(county), 
               colour = state)) + geom_line(colour = "#D3D3D3") +
  geom_line(data = df_arl, aes(x = date, y = cases), colour = "#505050") +
  ggrepel::geom_label_repel(data = df_arl, aes(label = label), nudge_x = 1, force = 10) +
  scale_y_log10() + si_style() +
  expand_limits(x = as.Date(c(date_min, date_max)))


va %>% 
  filter(max_cases > 50) %>% 
  mutate(county_sort = fct_reorder(county, cases, .fun = max)) %>% 
  ggplot(., aes(x = date, y = county_sort, fill = log(cases))) + 
  geom_tile(colour = "white", size = 0.25) + 
  scale_fill_viridis_c() +
  si_style_nolines()



# va %>% ggplot() +
#   geom_sf(aes(fill = max_cases), colour = "white", size = 0.25) +
#   scale_fill_viridis_c(option = "D", na.value = "#d3d3d3") + 
#   theme_void() +
#   si_style_nolines()

facet_newline <- function(string) {
  paste(string, "\n")
} 
  

# daily cases
va %>% filter(county %in% c("Arlington", "Fairfax", 
  "District of Columbia", "Alexandria city")) %>% 
  ggplot(aes(x = date, y = daily_cases)) +
  geom_area(aes(fill = county), alpha = 0.85) +
  geom_line(colour = "white", size = 0.25) +
    facet_wrap(~county, scale = "free_y",
        labeller = labeller(county = facet_newline)) +
  geom_smooth(color = old_rose, alpha = 0.5, se = FALSE) +
  si_style_ygrid() +
  scale_fill_si(palette = "siei",)+
  theme(legend.position = "none") +
  labs(title = "COVID-19 CASES by Day", y = NULL)


ggsave(here(image, paste0("COVID_DC_Region_Cases_", Sys.Date(), ".pdf")), last_plot(), 
       device = "pdf",
       height = 8.5,
       width = 11)


  

  