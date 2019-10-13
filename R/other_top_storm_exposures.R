library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

storm <- "Floyd-1999"

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
        dplyr::mutate_(wind = ~ wind_value >= 17.4) %>%
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


storm_data <- distance_data %>%
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

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")
# out_data <- out_data %>% 
#         dplyr::filter(metric != "Distance-based metric")

out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
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
        viridis::scale_color_viridis(discrete = TRUE, begin = 0.75,
                                     end = 1, option = "A", direction = -1,
                                     name = "", labels = c("Unexposed", "Exposed")) +
        # ggplot2::scale_fill_manual(name = "",
        #                            values = c("white", "dodgerblue"),
        #                            labels = c("Unexposed", "Exposed")) +
        facet_wrap(~ metric, ncol = 5) +
        theme(legend.position="none")

floyd <- map_tracks(storm, plot_object = out) +
        ggtitle("Floyd (1999)")

storm <- "Ike-2008"

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
        dplyr::mutate_(wind = ~ wind_value >= 17.4) %>%
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


storm_data <- distance_data %>%
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

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")

# out_data <- out_data %>% 
#         dplyr::filter(metric != "Distance-based metric")

out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
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
        viridis::scale_color_viridis(discrete = TRUE, begin = 0.75,
                                     end = 1, option = "A", direction = -1,
                                     name = "", labels = c("Unexposed", "Exposed")) +
        # ggplot2::scale_fill_manual(name = "",
        #                            values = c("white", "dodgerblue"),
        #                            labels = c("Unexposed", "Exposed")) +
        facet_wrap(~ metric, ncol = 5) +
        theme(legend.position="none")

ike <- map_tracks(storm, plot_object = out) +
        ggtitle("Ike (2008)")



storm <- "Katrina-2005"

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
        dplyr::mutate_(wind = ~ wind_value >= 17.4) %>%
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


storm_data <- distance_data %>%
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

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")

# out_data <- out_data %>% 
#         dplyr::filter(metric != "Distance-based metric")

out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
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
        viridis::scale_color_viridis(discrete = TRUE, begin = 0.75,
                                     end = 1, option = "A", direction = -1,
                                     name = "", labels = c("Unexposed", "Exposed")) +
        # ggplot2::scale_fill_manual(name = "",
        #                            values = c("white", "dodgerblue"),
        #                            labels = c("Unexposed", "Exposed")) +
        facet_wrap(~ metric, ncol = 5) +
        theme(legend.position="none")

katrina <- map_tracks(storm, plot_object = out) +
        ggtitle("Katrina (2005)")

storm <- "Dennis-2005"

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
        dplyr::mutate_(wind = ~ wind_value >= 17.4) %>%
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


storm_data <- distance_data %>%
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

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")

# out_data <- out_data %>% 
#         dplyr::filter(metric != "Distance-based metric")

out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
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
        viridis::scale_color_viridis(discrete = TRUE, begin = 0.75,
                                     end = 1, option = "A", direction = -1,
                                     name = "", labels = c("Unexposed", "Exposed")) +
        # ggplot2::scale_fill_manual(name = "",
        #                            values = c("white", "dodgerblue"),
        #                            labels = c("Unexposed", "Exposed")) +
        facet_wrap(~ metric, ncol = 5) +
        theme(legend.position="none")

dennis <- map_tracks(storm, plot_object = out) +
        ggtitle("Dennis (2005)")

pdf("figures/othertopstorms.pdf", width = 10, height = 7)
grid.arrange(floyd, ike, katrina, dennis, ncol = 1)
dev.off()

