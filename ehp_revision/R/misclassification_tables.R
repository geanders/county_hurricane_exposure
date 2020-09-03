library(hurricaneexposure)
library(hurricaneexposuredata)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(viridis)
library(dendextend)
library(ComplexHeatmap)
library(circlize)
library(stringr)
library(xtable)

data(closest_dist)

calc_jaccard <- function(x){
  x_mat <- x %>%
    dplyr::select(-fips) %>%
    dplyr::mutate_all(funs(as.numeric(.))) %>%
    as.matrix()
  jaccard <- proxy::simil(t(x_mat), method = "jaccard")
  out <- combn(colnames(x_mat), 2) %>%
    t() %>%
    as_tibble(.name_repair = "unique") %>%
    dplyr::rename(metric_1 = `...1`,
                  metric_2 = `...2`) %>%
    dplyr::mutate(jaccard = as.vector(jaccard))
  return(out)
}

calc_metric_1_not_2 <- function(x){
  x_mat <- x %>%
    dplyr::select(-fips) %>%
    dplyr::mutate_all(funs(as.numeric(.))) %>%
    as.matrix()
  met_1_not_2 <- function(i, j){
    x <- x_mat[ , i]
    y <- x_mat[ , j]
    if(sum(x | y) == 0) return(NA)
    a <- sum(x & y)
    b <- sum(x & !y)
    c <- sum(!x & y)
    metric <- b / (a + b + c)
    return(metric)
  }
  out_metric <- outer(1:5, 1:5, FUN = Vectorize(met_1_not_2))
  out <- combn(colnames(x_mat), 2) %>%
    t() %>%
    as_tibble(.name_repair = "unique") %>%
    dplyr::rename(metric_1 = `...1`,
                  metric_2 = `...2`) %>%
    dplyr::mutate(met_1_not_2 = as.vector(t(out_metric))[t(upper.tri(out_metric))])
  return(out)
}


calc_metric_2_not_1 <- function(x){
  x_mat <- x %>%
    dplyr::select(-fips) %>%
    dplyr::mutate_all(funs(as.numeric(.))) %>%
    as.matrix()
  met_2_not_1 <- function(i, j){
    x <- x_mat[ , i]
    y <- x_mat[ , j]
    if(sum(x | y) == 0) return(NA)
    a <- sum(x & y)
    b <- sum(x & !y)
    c <- sum(!x & y)
    metric <- c / (a + b + c)
    return(metric)
  }
  out_metric <- outer(1:5, 1:5, FUN = Vectorize(met_2_not_1))
  out <- combn(colnames(x_mat), 2) %>%
    t() %>%
    as_tibble(.name_repair = "unique") %>%
    dplyr::rename(metric_1 = `...1`,
                  metric_2 = `...2`) %>%
    dplyr::mutate(met_2_not_1 = as.vector(t(out_metric))[t(upper.tri(out_metric))])
  return(out)
}

my_fips <- unique(closest_dist$fips)

distance_data <- county_distance(counties = my_fips, start_year = 1996, end_year = 2011,
                                 dist_limit = 100) %>%
  select(storm_id, fips) %>%
  dplyr::mutate(distance = TRUE)

rain_data <- county_rain(counties = my_fips, start_year = 1996, end_year = 2011,
                         rain_limit = 75, dist_limit = 500) %>%
  select(storm_id, fips) %>%
  dplyr::mutate(rain = TRUE)

wind_data <- county_wind(counties = my_fips, start_year = 1996, end_year = 2011,
                         wind_limit = 17.4) %>%
  select(storm_id, fips) %>%
  dplyr::mutate(wind = TRUE)

flood_data <- county_events(counties = my_fips, start_year = 1996, end_year = 2011,
                            event_type = "flood") %>%
  select(storm_id, fips) %>%
  dplyr::mutate(flood = TRUE)

tornado_data <- county_events(counties = my_fips, start_year = 1996, end_year = 2011,
                              event_type = "tornado") %>%
  select(storm_id, fips) %>%
  dplyr::mutate(tornado = TRUE)

misclass <- distance_data %>%
  full_join(rain_data, by = c("storm_id", "fips")) %>%
  full_join(wind_data, by = c("storm_id", "fips")) %>%
  full_join(flood_data, by = c("storm_id", "fips")) %>%
  full_join(tornado_data, by = c("storm_id", "fips")) %>%
  mutate_at(c("distance", "rain", "wind", "flood", "tornado"),
            funs(ifelse(is.na(.), FALSE, .))) %>%
  dplyr::mutate(any_exposure = any(distance, rain, wind, flood, tornado)) %>%
  dplyr::group_by(storm_id) %>%
  dplyr::mutate(counties_exposed = sum(any_exposure)) %>%
  ungroup() %>%
  dplyr::filter(counties_exposed >= 250)

misclass_rain <- misclass %>% 
  mutate(dist_rain = distance & rain, 
         dist_no_rain = distance & !rain, 
         no_dist_rain = !distance & rain) %>% 
  group_by(storm_id) %>% 
  summarize(dist_rain = sum(dist_rain), 
            dist_no_rain = sum(dist_no_rain), 
            no_dist_rain = sum(no_dist_rain)) %>% 
  ungroup() %>% 
  mutate(tot = dist_rain + dist_no_rain + no_dist_rain, 
         neither = length(my_fips) - tot) %>% 
  select(-tot)

misclass_rain %>% 
  arrange(neither) %>%  
  rename(
    `Storm` = storm_id,
    `Exposed for both distance metric and rain metric` = dist_rain,
    `Exposed for distance metric but unexposed for rain metric` = dist_no_rain,
    `Exposed for rain metric but unexposed for distance metric` = no_dist_rain,
    `Unexposed for both distance metric and rain metric` = neither
  ) %>% 
  mutate(Storm = str_replace(Storm, "-", " ("), 
         Storm = paste0(Storm, ")")) %>% 
  mutate_at(2:5, prettyNum, big.mark = ",") %>% 
  xtable(align = "ccp{3cm}p{3cm}p{3cm}p{3cm}",
          caption = "Caption. Limited to storms with at least 250 counties assessed as exposed based on at least one exposure metric considered in this study. Numbers are out of 2,396 counties in the study area (states in the eastern half of the US). Exposure assessment is based on the thresholds given in Table 1 of the main text. The Jaccard index shown in Figure [x] of the main text is based on numbers in the second through fourth columns (the value in the second column divide by the sum of numbers in the second through fourth columns.  Storms are ordered based on the number of counties exposed to at least one of these two exposure metrics.).") %>% 
  print(include.rownames = FALSE, booktabs = TRUE, 
         only.contents = FALSE,
         file = "ehp_revision/tables/rain_misclass.tex", 
        caption.placement = "top")

misclass_wind <- misclass %>% 
  mutate(dist_wind = distance & wind, 
         dist_no_wind = distance & !wind, 
         no_dist_wind = !distance & wind) %>% 
  group_by(storm_id) %>% 
  summarize(dist_wind = sum(dist_wind), 
            dist_no_wind = sum(dist_no_wind), 
            no_dist_wind = sum(no_dist_wind)) %>% 
  ungroup() %>% 
  mutate(tot = dist_wind + dist_no_wind + no_dist_wind, 
         neither = length(my_fips) - tot) %>% 
  select(-tot)

misclass_wind %>% 
  arrange(neither) %>%  
  rename(
    `Storm` = storm_id,
    `Exposed for both distance metric and wind metric` = dist_wind,
    `Exposed for distance metric but unexposed for wind metric` = dist_no_wind,
    `Exposed for wind metric but unexposed for distance metric` = no_dist_wind,
    `Unexposed for both distance metric and wind metric` = neither
  ) %>% 
  mutate(Storm = str_replace(Storm, "-", " ("), 
         Storm = paste0(Storm, ")")) %>% 
  mutate_at(2:5, prettyNum, big.mark = ",") %>% 
  xtable(align = "ccp{3cm}p{3cm}p{3cm}p{3cm}",
         caption = "Caption. Limited to storms with at least 250 counties assessed as exposed based on at least one exposure metric considered in this study. Numbers are out of 2,396 counties in the study area (states in the eastern half of the US). Exposure assessment is based on the thresholds given in Table 1 of the main text. The Jaccard index shown in Figure [x] of the main text is based on numbers in the second through fourth columns (the value in the second column divide by the sum of numbers in the second through fourth columns). Storms are ordered based on the number of counties exposed to at least one of these two exposure metrics.") %>% 
  print(include.rownames = FALSE, booktabs = TRUE, 
        only.contents = FALSE,
        file = "ehp_revision/tables/wind_misclass.tex", 
        caption.placement = "top")


misclass_flood <- misclass %>% 
  mutate(dist_flood = distance & flood, 
         dist_no_flood = distance & !flood, 
         no_dist_flood = !distance & flood) %>% 
  group_by(storm_id) %>% 
  summarize(dist_flood = sum(dist_flood), 
            dist_no_flood = sum(dist_no_flood), 
            no_dist_flood = sum(no_dist_flood)) %>% 
  ungroup() %>% 
  mutate(tot = dist_flood + dist_no_flood + no_dist_flood, 
         neither = length(my_fips) - tot) %>% 
  select(-tot)

misclass_flood %>% 
  arrange(neither) %>%  
  rename(
    `Storm` = storm_id,
    `Exposed for both distance metric and flood metric` = dist_flood,
    `Exposed for distance metric but unexposed for flood metric` = dist_no_flood,
    `Exposed for flood metric but unexposed for distance metric` = no_dist_flood,
    `Unexposed for both distance metric and flood metric` = neither
  ) %>% 
  mutate(Storm = str_replace(Storm, "-", " ("), 
         Storm = paste0(Storm, ")")) %>% 
  mutate_at(2:5, prettyNum, big.mark = ",") %>% 
  xtable(align = "ccp{3cm}p{3cm}p{3cm}p{3cm}",
         caption = "Caption. Limited to storms with at least 250 counties assessed as exposed based on at least one exposure metric considered in this study. Numbers are out of 2,396 counties in the study area (states in the eastern half of the US). Exposure assessment is based on the thresholds given in Table 1 of the main text. The Jaccard index shown in Figure [x] of the main text is based on numbers in the second through fourth columns (the value in the second column divide by the sum of numbers in the second through fourth columns). Storms are ordered based on the number of counties exposed to at least one of these two exposure metrics.") %>% 
  print(include.rownames = FALSE, booktabs = TRUE, 
        only.contents = FALSE,
        file = "ehp_revision/tables/flood_misclass.tex", 
        caption.placement = "top")

misclass_tornado <- misclass %>% 
  mutate(dist_tornado = distance & tornado, 
         dist_no_tornado = distance & !tornado, 
         no_dist_tornado = !distance & tornado) %>% 
  group_by(storm_id) %>% 
  summarize(dist_tornado = sum(dist_tornado), 
            dist_no_tornado = sum(dist_no_tornado), 
            no_dist_tornado = sum(no_dist_tornado)) %>% 
  ungroup() %>% 
  mutate(tot = dist_tornado + dist_no_tornado + no_dist_tornado, 
         neither = length(my_fips) - tot) %>% 
  select(-tot)

misclass_tornado %>% 
  arrange(neither) %>%  
  rename(
    `Storm` = storm_id,
    `Exposed for both distance metric and tornado metric` = dist_tornado,
    `Exposed for distance metric but unexposed for tornado metric` = dist_no_tornado,
    `Exposed for tornado metric but unexposed for distance metric` = no_dist_tornado,
    `Unexposed for both distance metric and tornado metric` = neither
  ) %>% 
  mutate(Storm = str_replace(Storm, "-", " ("), 
         Storm = paste0(Storm, ")")) %>% 
  mutate_at(2:5, prettyNum, big.mark = ",") %>% 
  xtable(align = "ccp{3cm}p{3cm}p{3cm}p{3cm}",
         caption = "Caption. Limited to storms with at least 250 counties assessed as exposed based on at least one exposure metric considered in this study. Numbers are out of 2,396 counties in the study area (states in the eastern half of the US). Exposure assessment is based on the thresholds given in Table 1 of the main text. The Jaccard index shown in Figure [x] of the main text is based on numbers in the second through fourth columns (the value in the second column divide by the sum of numbers in the second through fourth columns). Storms are ordered based on the number of counties exposed to at least one of these two exposure metrics.") %>% 
  print(include.rownames = FALSE, booktabs = TRUE, 
        only.contents = FALSE,
        file = "ehp_revision/tables/tornado_misclass.tex", 
        caption.placement = "top")
