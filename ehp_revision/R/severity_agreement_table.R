library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(xtable)
library(stringr)
library(psych)

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

storm_severity <- total_exposure %>%
        mutate(metric = str_replace(metric, "_exposed", ""),
               metric = str_to_title(metric)) %>%
        group_by(storm_id, metric) %>%
        summarize(exposed_counties = sum(exposed)) %>%
        spread(metric, exposed_counties) %>%
        ungroup()

storm_cor <- cor(storm_severity[, c("Distance", "Rain", "Wind",
                                    "Flood", "Tornado")],
                 method = "kendall")

storm_cor <- corr.test(storm_severity[ , c("Distance", "Rain", "Wind", "Flood", "Tornado")], method = "kendall", adjust = "bonferroni")$r
storm_cor <- apply(storm_cor, 1, function(x){ sprintf("%.2f", x)})
row.names(storm_cor) <- c("Distance", "Rain", "Wind", "Flood", "Tornado")
for(i in 1:nrow(storm_cor)){
        for(j in 1:ncol(storm_cor)){
                if(i <= j){storm_cor[i, j] <- "-"}
        }
}
storm_cor[2:5, ] %>%
        xtable() %>%
        print(booktabs = TRUE, only.contents = TRUE,
              file = "tables/severityagreement.tex")
