library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

storm <- "Frances-2004"

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


storm_data <- rain_data %>%
        full_join(wind_data, by = "fips") %>%
        gather(key = metric, value = Exposed, -fips) %>%
        mutate(metric = factor(metric,
                               levels = c("rain", "wind"),
                               labels = c("Rain-based metric", "Wind-based metric")))

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")
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
        facet_wrap(~ metric, ncol = 3) +
        theme(legend.position = "none")

out_a <- map_tracks(storm, plot_object = out) +
        ggtitle(paste0(str_split(storm, "-")[[1]][1], " (", str_split(storm, "-")[[1]][2], ")"))

###

storm <- "Hugo-1989"

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


storm_data <- rain_data %>%
        full_join(wind_data, by = "fips") %>%
        gather(key = metric, value = Exposed, -fips) %>%
        mutate(metric = factor(metric,
                               levels = c("rain", "wind"),
                               labels = c("Rain-based metric", "Wind-based metric")))

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")
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
        facet_wrap(~ metric, ncol = 3) +
        theme(legend.position = "none")

out_b <- map_tracks(storm, plot_object = out) +
        ggtitle(paste0(str_split(storm, "-")[[1]][1], " (", str_split(storm, "-")[[1]][2], ")"))


###

storm <- "Isabel-2003"

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


storm_data <- rain_data %>%
        full_join(wind_data, by = "fips") %>%
        gather(key = metric, value = Exposed, -fips) %>%
        mutate(metric = factor(metric,
                               levels = c("rain", "wind"),
                               labels = c("Rain-based metric", "Wind-based metric")))

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")
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
        facet_wrap(~ metric, ncol = 3) +
        theme(legend.position = "none")

out_c <- map_tracks(storm, plot_object = out) +
        ggtitle(paste0(str_split(storm, "-")[[1]][1], " (", str_split(storm, "-")[[1]][2], ")"))

###

storm <- "Isidore-2002"

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


storm_data <- rain_data %>%
        full_join(wind_data, by = "fips") %>%
        gather(key = metric, value = Exposed, -fips) %>%
        mutate(metric = factor(metric,
                               levels = c("rain", "wind"),
                               labels = c("Rain-based metric", "Wind-based metric")))

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::left_join(storm_data, by = "fips")
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
        facet_wrap(~ metric, ncol = 3) +
        theme(legend.position = "none")

out_d <- map_tracks(storm, plot_object = out) +
        ggtitle(paste0(str_split(storm, "-")[[1]][1], " (", str_split(storm, "-")[[1]][2], ")"))


pdf("figures/extentdisagreement.pdf", width = 9, height = 5)
grid.arrange(out_a, out_d, out_b, out_c, ncol = 2)
dev.off()
