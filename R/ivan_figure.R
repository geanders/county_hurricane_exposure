library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

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

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(ivan_data, by = "fips")
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
        viridis::scale_fill_viridis(discrete = TRUE, begin = 0.15,
                                    end = 0.98, option = "B",
                                    name = "", labels = c("Unexposed", "Exposed")) +
        viridis::scale_color_viridis(discrete = TRUE, begin = 0.15,
                                     end = 0.98, option = "B",
                                     name = "", labels = c("Unexposed", "Exposed")) +
        # ggplot2::scale_fill_manual(name = "",
        #                            values = c("white", "dodgerblue"),
        #                            labels = c("Unexposed", "Exposed")) +
        facet_wrap(~ metric, ncol = 3) +
        theme(legend.position=c(.8,.15))

out <- map_tracks(storm, plot_object = out)

pdf("figures/ivanexposure.pdf", width = 9, height = 6)
print(out)
dev.off()


