library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(choroplethr)
data(closest_dist)

my_fips <- unique(closest_dist$fips)
dist <- county_distance(counties = my_fips, start_year = 1988,
                                     end_year = 2015, dist_limit = 100) %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 28) %>%
        mutate(metric = "distance")

rain <- county_rain(counties = my_fips, start_year = 1988,
                             end_year = 2011, dist_limit = 500,
                             rain_limit = 75) %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 24) %>%
        mutate(metric = "rain")

wind <- county_wind(counties = my_fips, start_year = 1988,
                             end_year = 2015, wind_limit = 15) %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 28) %>%
        mutate(metric = "wind")

flood <- county_events(counties = my_fips,
                                start_year = 1996, end_year = 2015,
                                event_type = "flood") %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 20) %>%
        mutate(metric = "flood")

tornado <- county_events(counties = my_fips,
                                start_year = 1996, end_year = 2015,
                                event_type = "tornado") %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 20) %>%
        mutate(metric = "tornado")

our_fips <- hurricaneexposure:::get_eastern_map() %>% dplyr::select(fips) %>% dplyr::distinct() %>%
        dplyr::mutate(fips = as.numeric(fips))
all_exposures <- bind_rows(dist, rain, wind, flood, tornado) %>%
        mutate(n_exposures = n_exposures * 10,
               fips = as.numeric(fips)) %>%
        right_join(our_fips, by = "fips") %>%
        tidyr::spread(metric, n_exposures, fill = 0) %>%
        tidyr::gather(metric, n_exposures, -fips)

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::mutate(fips = as.numeric(fips)) %>%
        dplyr::left_join(all_exposures, by = "fips") %>%
        dplyr::mutate(metric = factor(metric,
                                      levels = c("distance", "rain", "wind", "flood", "tornado"),
                                      labels = c("Distance-based metric", "Rain-based metric",
                                                 "Wind-based metric", "Flood-based metric",
                                                 "Tornado-based metric"))) %>%
        dplyr::filter(!is.na(metric))
out_data$value <- cut(out_data$n_exposures, c(0,
                                                    1,
                                                    2,
                                                    3,
                                                    4,
                                                    5,
                                                    6,
                                                    7,
                                                    8,
                                                    100),
                            labels = c("0-1", "1-2",
                                       "2-3", "3-4",
                                       "4-5", "5-6",
                                       "6-7", "7-8", "8+"),
                            include.lowest = TRUE, right = FALSE)


out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
                              ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                            fill = ~ value, color = ~ value),
                              size = 0.2) +
        ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                              "georgia", "florida", "alabama", "kentucky",
                                              "tennessee", "maryland", "west virginia",
                                              "district of columbia", "pennsylvania",
                                              "new jersey", "delaware", "mississippi",
                                              "louisiana", "texas", "oklahoma", "arkansas",
                                              "new york", "connecticut", "rhode island",
                                              "massachusetts", "new hampshire", "vermont",
                                              "maine", "kansas", "missouri", "iowa", "michigan",
                                              "illinois", "ohio", "wisconsin", "indiana"),
                         colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
        ggplot2::theme_void() +
        ggplot2::coord_map() +
        viridis::scale_fill_viridis(name = "Hits per\ncounty per\ndecade",
                                    discrete = TRUE, direction = -1, option = "A") +
        viridis::scale_color_viridis(name = "Hits per\ncounty per\ndecade",
                                     discrete = TRUE, direction = -1, option = "A") +
        facet_wrap(~ metric) +
        theme(legend.position = c(.85, .3),
              legend.direction = "horizontal",
              legend.key.size = unit(0.8, "lines"),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(colour = "black", fill = "white")) # +
        # ggtitle("Average county storm exposures per decade by metric")

storm <- "Ivan-2004"

distance_data <- filter_storm_data(storm = storm,
                                   output_vars = c("fips", "storm_dist")) %>%
        dplyr::mutate_(distance = ~ storm_dist <= 100) %>%
        dplyr::select(fips, distance) %>%
        dplyr::tbl_df()

rain_data <- filter_storm_data(storm = storm,
                               days_included = c(-2, -1, 0, 1),
                               include_rain = TRUE,
                               output_vars = c("fips", "tot_precip",
                                               "storm_dist")) %>%
        dplyr::mutate_(rain = ~ tot_precip >= 75 &
                               storm_dist <= 500) %>%
        dplyr::select(fips, rain) %>%
        dplyr::tbl_df()

wind_data <- filter_wind_data(storm = storm, wind_source = "modeled",
                              output_vars = c("fips", "vmax_sust")) %>%
        `colnames<-`(c("fips", "wind_value")) %>%
        dplyr::mutate_(wind = ~ wind_value >= 15) %>%
        dplyr::select(fips, wind) %>%
        dplyr::tbl_df()

storm_year <- gsub("*.+-", "", storm)
counties <- hurricaneexposuredata::closest_dist %>%
        dplyr::filter_(~ storm_id == storm) %>%
        dplyr::select_(quote(fips), quote(storm_dist))

flood_data <- county_events(counties = counties$fips,
                            start_year = storm_year,
                            end_year = storm_year,
                            event_type = "flood") %>%
        dplyr::filter_(~ storm_id == storm) %>%
        dplyr::select_(quote(fips)) %>%
        dplyr::mutate_(event = ~ 1) %>%
        dplyr::right_join(counties, by = "fips") %>%
        dplyr::mutate_(event = ~ !is.na(event)) %>%
        dplyr::rename_(flood = ~ event) %>%
        dplyr::select_(quote(-storm_dist))

tornado_data <- county_events(counties = counties$fips,
                              start_year = storm_year,
                              end_year = storm_year,
                              event_type = "tornado") %>%
        dplyr::filter_(~ storm_id == storm) %>%
        dplyr::select_(quote(fips)) %>%
        dplyr::mutate_(event = ~ 1) %>%
        dplyr::right_join(counties, by = "fips") %>%
        dplyr::mutate_(event = ~ !is.na(event)) %>%
        dplyr::rename_(tornado = ~ event) %>%
        dplyr::select_(quote(-storm_dist))


ivan_data <- distance_data %>%
        full_join(rain_data, by = "fips") %>%
        full_join(wind_data, by = "fips") %>%
        full_join(flood_data, by = "fips") %>%
        full_join(tornado_data, by = "fips") %>%
        gather(key = metric, value = Exposed, -fips) %>%
        mutate(metric = factor(metric,
                               levels = c("distance", "rain", "wind", "flood", "tornado"),
                               labels = c("Distance-based metric", "Rain-based metric",
                                          "Wind-based metric", "Flood-based metric",
                                          "Tornado-based metric")))

out_data2 <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(ivan_data, by = "fips")
out2 <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data2,
                              ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                            fill = ~ Exposed, color = ~ Exposed),
                              size = 0.2) +
        ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                              "georgia", "florida", "alabama", "kentucky",
                                              "tennessee", "maryland", "west virginia",
                                              "district of columbia", "pennsylvania",
                                              "new jersey", "delaware", "mississippi",
                                              "louisiana", "texas", "oklahoma", "arkansas",
                                              "new york", "connecticut", "rhode island",
                                              "massachusetts", "new hampshire", "vermont",
                                              "maine", "kansas", "missouri", "iowa", "michigan",
                                              "illinois", "ohio", "wisconsin", "indiana"),
                         colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
        ggplot2::theme_void() +
        ggplot2::coord_map() +
        viridis::scale_fill_viridis(discrete = TRUE, begin = 0.75,
                                    end = 1, option = "A", direction = -1,
                                    name = "", labels = c("Unexposed", "Exposed")) +
        viridis::scale_color_viridis(discrete = TRUE, begin = 0.75, direction = -1,
                                     end = 1, option = "A",
                                     name = "", labels = c("Unexposed", "Exposed")) +
        facet_wrap(~ metric, ncol = 3) +
        theme(legend.position = c(.85, .3),
              legend.text = element_text(size = 7),
              strip.background = element_rect(colour = "black", fill = "white")) # +
       #  ggtitle("Counties exposured to Hurricane Ivan (2004) based on different exposure metrics")

out2 <- map_tracks(storm, plot_object = out2)

pdf("figures/averageexposure.pdf", width = 9, height = 11)
grid.arrange(out, out2, ncol = 1)
dev.off()

pdf("figures/averageexposureonly.pdf", width = 9, height = 5)
print(out)
dev.off()

pdf("figures/ivanonly.pdf", width = 9, height = 5)
print(out2)
dev.off()

my_fips <- unique(closest_dist$fips)
dist <- county_distance(counties = my_fips, start_year = 1996,
                        end_year = 2011, dist_limit = 100) %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 16) %>%
        mutate(metric = "distance")

rain <- county_rain(counties = my_fips, start_year = 1996,
                    end_year = 2011, dist_limit = 500,
                    rain_limit = 75) %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 16) %>%
        mutate(metric = "rain")

wind <- county_wind(counties = my_fips, start_year = 1996,
                    end_year = 2011, wind_limit = 15) %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 16) %>%
        mutate(metric = "wind")

flood <- county_events(counties = my_fips,
                       start_year = 1996, end_year = 2011,
                       event_type = "flood") %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 16) %>%
        mutate(metric = "flood")

tornado <- county_events(counties = my_fips,
                         start_year = 1996, end_year = 2011,
                         event_type = "tornado") %>%
        group_by(fips) %>%
        dplyr::summarize(n_exposures = n() / 16) %>%
        mutate(metric = "tornado")

our_fips <- hurricaneexposure:::get_eastern_map() %>% dplyr::select(fips) %>% dplyr::distinct() %>%
        dplyr::mutate(fips = as.numeric(fips))
all_exposures <- bind_rows(dist, rain, wind, flood, tornado) %>%
        mutate(n_exposures = n_exposures * 10,
               fips = as.numeric(fips)) %>%
        right_join(our_fips, by = "fips") %>%
        tidyr::spread(metric, n_exposures, fill = 0) %>%
        tidyr::gather(metric, n_exposures, -fips)

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::mutate(fips = as.numeric(fips)) %>%
        dplyr::left_join(all_exposures, by = "fips") %>%
        dplyr::mutate(metric = factor(metric,
                                      levels = c("distance", "rain", "wind", "flood", "tornado"),
                                      labels = c("Distance-based metric", "Rain-based metric",
                                                 "Wind-based metric", "Flood-based metric",
                                                 "Tornado-based metric"))) %>%
        dplyr::filter(!is.na(metric))
out_data$value <- cut(out_data$n_exposures, c(0,
                                              1,
                                              2,
                                              3,
                                              4,
                                              5,
                                              6,
                                              7,
                                              8,
                                              100),
                      labels = c("0-1", "1-2",
                                 "2-3", "3-4",
                                 "4-5", "5-6",
                                 "6-7", "7-8", "8+"),
                      include.lowest = TRUE, right = FALSE)


out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
                              ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                            fill = ~ value, color = ~ value),
                              size = 0.2) +
        ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                              "georgia", "florida", "alabama", "kentucky",
                                              "tennessee", "maryland", "west virginia",
                                              "district of columbia", "pennsylvania",
                                              "new jersey", "delaware", "mississippi",
                                              "louisiana", "texas", "oklahoma", "arkansas",
                                              "new york", "connecticut", "rhode island",
                                              "massachusetts", "new hampshire", "vermont",
                                              "maine", "kansas", "missouri", "iowa", "michigan",
                                              "illinois", "ohio", "wisconsin", "indiana"),
                         colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
        ggplot2::theme_void() +
        ggplot2::coord_map() +
        viridis::scale_fill_viridis(name = "Hits per\ncounty per\ndecade",
                                    discrete = TRUE, direction = -1,
                                    option = "A") +
        viridis::scale_color_viridis(name = "Hits per\ncounty per\ndecade",
                                     discrete = TRUE, direction = -1,
                                     option = "A") +
        facet_wrap(~ metric) +
        theme(legend.position = c(.85, .3),
              legend.direction = "horizontal",
              legend.key.size = unit(0.8, "lines"),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(colour = "black", fill = "white")) +
        ggtitle("Average county storm exposures per decade by metric")

pdf("figures/averageexposureonlysupp.pdf", width = 9, height = 5.5)
print(out)
dev.off()
