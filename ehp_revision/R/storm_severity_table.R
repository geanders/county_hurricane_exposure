library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(xtable)
library(stringr)

my_fips <- unique(closest_dist$fips)

distance_exposure <- county_distance(counties = my_fips, start_year = 1988,
                                     end_year = 2018, dist_limit = 100) %>%
        select(storm_id, fips) %>%
        mutate(exposed = TRUE,
               storm_id = as.character(storm_id))

dist_sum <- distance_exposure %>%  
  group_by(storm_id) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(n_storms = n() / (2018 - 1988 + 1),
            n_exposures = sum(n) / (2018 - 1988 + 1),
            med = median(n),
            p25 = quantile(n, 0.25),
            p75 = quantile(n, 0.75),
            max = max(n),
            which_max = storm_id[which.max(n)])

rain_exposure <- county_rain(counties = my_fips,
                             start_year = 1988, end_year = 2011,
                             rain_limit = 75, dist_limit = 500) %>%
        select(storm_id, fips) %>%
        mutate(exposed = TRUE,
               storm_id = as.character(storm_id))
rain_sum <- rain_exposure %>%  
  group_by(storm_id) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(n_storms = n() / (2011 - 1988 + 1),
            n_exposures = sum(n) / (2011 - 1988 + 1),
            med = median(n),
            p25 = quantile(n, 0.25),
            p75 = quantile(n, 0.75),
            max = max(n),
            which_max = storm_id[which.max(n)])

wind_exposure <- county_wind(counties = my_fips, start_year = 1988,
                             end_year = 2018, wind_limit = 17.4) %>%
        select(storm_id, fips) %>%
        mutate(exposed = TRUE,
               storm_id = as.character(storm_id))
wind_sum <- wind_exposure %>%  
  group_by(storm_id) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(n_storms = n() / (2018 - 1988 + 1),
            n_exposures = sum(n) / (2018 - 1988 + 1),
            med = median(n),
            p25 = quantile(n, 0.25),
            p75 = quantile(n, 0.75),
            max = max(n),
            which_max = storm_id[which.max(n)])

flood_exposure <- county_events(counties = my_fips,
                                start_year = 1996, end_year = 2018,
                                event_type = "flood") %>%
        select(storm_id, fips) %>%
        mutate(exposed = TRUE,
               storm_id = as.character(storm_id))
flood_sum <- flood_exposure %>%  
  group_by(storm_id) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(n_storms = n() / (2018 - 1996 + 1),
            n_exposures = sum(n) / (2018 - 1996 + 1),
            med = median(n),
            p25 = quantile(n, 0.25),
            p75 = quantile(n, 0.75),
            max = max(n),
            which_max = storm_id[which.max(n)])

tornado_exposure <- county_events(counties = my_fips,
                                  start_year = 1988, end_year = 2018,
                                  event_type = "tornado") %>%
        select(storm_id, fips) %>%
        mutate(exposed = TRUE,
               storm_id = as.character(storm_id))
tornado_sum <- tornado_exposure %>%  
  group_by(storm_id) %>% 
  count() %>% 
  ungroup() %>%
  summarize(n_storms = n() / (2018 - 1988 + 1),
            n_exposures = sum(n) / (2018 - 1988 + 1),
            med = median(n),
            p25 = quantile(n, 0.25),
            p75 = quantile(n, 0.75),
            max = max(n),
            which_max = storm_id[which.max(n)])

dist_sum2 <- distance_exposure %>% 
  tbl_df() %>% 
  mutate(year = str_extract(storm_id, "[0-9].+")) %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(mean_exp = mean(n),
            p25_exp = quantile(n, prob = 0.25),
            p75_exp = quantile(n, prob = 0.75))
rain_sum2 <- rain_exposure %>% 
  tbl_df() %>% 
  mutate(year = str_extract(storm_id, "[0-9].+")) %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(mean_exp = mean(n),
            p25_exp = quantile(n, prob = 0.25),
            p75_exp = quantile(n, prob = 0.75))
wind_sum2 <- wind_exposure %>% 
  tbl_df() %>% 
  mutate(year = str_extract(storm_id, "[0-9].+")) %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(mean_exp = mean(n),
            p25_exp = quantile(n, prob = 0.25),
            p75_exp = quantile(n, prob = 0.75))
flood_sum2 <- flood_exposure %>% 
  tbl_df() %>% 
  mutate(year = str_extract(storm_id, "[0-9].+")) %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(mean_exp = mean(n),
            p25_exp = quantile(n, prob = 0.25),
            p75_exp = quantile(n, prob = 0.75))
tornado_sum2 <- tornado_exposure %>% 
  tbl_df() %>% 
  mutate(year = str_extract(storm_id, "[0-9].+")) %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  summarize(mean_exp = mean(n),
            p25_exp = quantile(n, prob = 0.25),
            p75_exp = quantile(n, prob = 0.75))

total_sum <- bind_rows(bind_cols(dist_sum, dist_sum2),
                       bind_cols(rain_sum, rain_sum2),
                       bind_cols(wind_sum, wind_sum2),
                       bind_cols(flood_sum, flood_sum2),
                       bind_cols(tornado_sum, tornado_sum2)
                       ) %>% 
  mutate(metric = c("Distance", "Rain", "Wind", "Flood", "Tornado"),
         n_storms = round(n_storms, 1),
         n_exposures = round(n_exposures),
         sum = paste(round(med), " (", round(p25), ", ", round(p75), ")", sep = ""),
         max_storm = paste(str_replace(which_max, "-", ", "),
                           " (", max, ")", sep = ""),
         years = c("1988--2018", "1988--2011", "1988--2018",
                   "1996--2018", "1988--2018"),
         yearly_sum = paste(round(mean_exp), " (", round(p25_exp), 
                            ", ", round(p75_exp), ")", sep = ""),
         metric = paste0(metric, " (", years, ")")) %>% 
  select(metric, yearly_sum, max_storm)

total_sum %>% 
  slice(2:5) %>% 
  rename(`Metric (years available)` = metric,
         `Mean of county exposures per year (interquartile range)` = yearly_sum,
         `Tropical cyclone with most counties exposed (# exposed counties)` = max_storm) %>%
  xtable(align = "cp{4.5cm}p{4.5cm}p{4.5cm}",
         caption = "Summary statistics for the number of county tropical cyclone exposures under each metric.",
         label = "tab:exposuresummaries",
         digits = 0) %>%
  print(include.rownames = FALSE, booktabs = TRUE, 
        only.contents = FALSE,
        file = "tables/stormseverity.tex", 
        caption.placement = "top")

