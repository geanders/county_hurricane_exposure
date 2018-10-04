library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(xtable)
library(stringr)

my_fips <- unique(closest_dist$fips)
distance_exposure <- county_distance(counties = my_fips, start_year = 1988,
                                     end_year = 2015, dist_limit = 100) %>%
        select(storm_id, fips) %>%
        mutate(distance_exposed = TRUE,
               storm_id = as.character(storm_id))
rain_exposure <- county_rain(counties = my_fips,
                             start_year = 1988, end_year = 2011,
                             rain_limit = 75, dist_limit = 500) %>%
        select(storm_id, fips) %>%
        mutate(rain_exposed = TRUE,
               storm_id = as.character(storm_id))
wind_exposure <- county_wind(counties = my_fips, start_year = 1988,
                             end_year = 2015, wind_limit = 15) %>%
        select(storm_id, fips) %>%
        mutate(wind_exposed = TRUE,
               storm_id = as.character(storm_id))
flood_exposure <- county_events(counties = my_fips,
                                start_year = 1996, end_year = 2015,
                                event_type = "flood") %>%
        select(storm_id, fips) %>%
        mutate(flood_exposed = TRUE,
               storm_id = as.character(storm_id))
tornado_exposure <- county_events(counties = my_fips,
                                  start_year = 1996, end_year = 2015,
                                  event_type = "tornado") %>%
        select(storm_id, fips) %>%
        mutate(tornado_exposed = TRUE,
               storm_id = as.character(storm_id))
total_exposure <- distance_exposure %>%
        full_join(rain_exposure, by = c("storm_id", "fips")) %>%
        full_join(wind_exposure, by = c("storm_id", "fips")) %>%
        full_join(flood_exposure, by = c("storm_id", "fips")) %>%
        full_join(tornado_exposure, by = c("storm_id", "fips")) %>%
        gather(metric, exposed,
               -storm_id, -fips, na.rm = FALSE) %>%
        mutate(exposed = ifelse(is.na(exposed), FALSE, exposed))

total_exposure %>%
        group_by(storm_id, metric) %>%
        dplyr::summarize(n_exposed = sum(exposed)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(metric) %>%
        dplyr::summarize(median_exposure2 = median(n_exposed),
                         iqr_exposure = paste(quantile(n_exposed, c(0.25, 0.75)),
                                              collapse = ", "),
                         median_exposure = paste0(median_exposure2, " (",
                                                  iqr_exposure, ")"),
                         max_exposure = max(n_exposed),
                         which_max_exposure = storm_id[which.max(n_exposed)],
                         which_max_exposure = gsub("-", ", ", which_max_exposure),
                         max_exposure = paste0(which_max_exposure, " (",
                                               max_exposure, ")")) %>%
        arrange(desc(median_exposure2)) %>%
        dplyr::select(-iqr_exposure, -which_max_exposure, -median_exposure2) %>%
        mutate(metric = str_replace(metric, "_exposed", ""),
               metric = str_to_title(metric)) %>%
        rename(`Metric` = metric,
               `Median number of exposed counties (Interquartile range)` = median_exposure,
               `Storm with most counties exposed (# exposed counties)` = max_exposure) %>%
        xtable(align = "cp{1cm}p{3cm}p{3cm}") %>%
        print(include.rownames = FALSE, booktabs = TRUE, hline.after = c(0, 5),
              only.contents = TRUE,
              file = "tables/stormseverity.tex")
