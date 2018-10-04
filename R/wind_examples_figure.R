library(hurricaneexposuredata)
library(hurricaneexposure)
library(ggplot2)
library(viridis)

modeled_winds <- data.table::data.table(hurricaneexposuredata::storm_winds) %>%
        filter(storm_id %in% c("Sandy-2012", "Ike-2008")) %>%
        select(fips, vmax_sust, storm_id) %>%
        mutate(wind = cut(vmax_sust, breaks = c(0, 17.4896, 25.7200, 32.9216, 100),
                             include.lowest = TRUE, right = TRUE,
                             labels = c("0-33.9 knots", "34-49.9 knots", "50-63.9 knots", "64+ knots")),
               wind_source = "Modeled")

ext_winds <- data.table::data.table(hurricaneexposuredata::ext_tracks_wind) %>%
        filter(storm_id %in% c("Sandy-2012", "Ike-2008")) %>%
        select(fips, vmax_sust, storm_id) %>%
        mutate(wind = cut(vmax_sust, breaks = c(0, 17.4896, 25.7200, 32.9216, 100),
                             include.lowest = TRUE, right = TRUE,
                             labels = c("0-33.9 knots", "34-49.9 knots", "50-63.9 knots", "64+ knots")),
               wind_source = "Extended Best Tracks")

to_plot <- bind_rows(modeled_winds, ext_winds) %>%
        mutate(wind_source = factor(wind_source, levels = c("Modeled", "Extended Best Tracks")),
               storm_id = factor(storm_id, levels = c("Ike-2008", "Sandy-2012"),
                                 labels = c("Hurricane Ike\n(2008)", "Hurricane Sandy\n(2012)")))


map_dim <- apply(matrix(c(-106.65037, 25.12993, -67.00742, 47.48101),
                        byrow = TRUE, ncol = 2),
                 MARGIN = 2,
                 function(x) range(x) + c(-1, 1) * 2)
tracks <- hurricaneexposuredata::hurr_tracks %>%
        dplyr::select_(~ latitude, ~ longitude, ~ storm_id,
                       ~ date) %>%
        dplyr::filter_(~ as.character(storm_id) %in% c("Sandy-2012", "Ike-2008") &
                               longitude > map_dim[1, 1] &
                               longitude < map_dim[2, 1] &
                               latitude > map_dim[1, 2] &
                               latitude < map_dim[2, 2]) %>%
        dplyr::mutate_(date = ~ lubridate::ymd_hm(date)) %>%
        mutate(storm_id = factor(storm_id, levels = c("Ike-2008", "Sandy-2012"),
                                 labels = c("Hurricane Ike\n(2008)", "Hurricane Sandy\n(2012)")))
splt_tracks <- split(tracks, tracks$storm_id)
full_tracks <- lapply(splt_tracks, hurricaneexposure:::interp_track)
full_tracks <- do.call("rbind", full_tracks)


out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::full_join(to_plot, by = "fips")
out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
                              ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                            fill = ~ wind, color = ~ wind),
                              size = 0.2) +
        scale_fill_viridis(discrete = TRUE, option = "A", direction = -1, name = "Windspeed") +
        scale_color_viridis(discrete = TRUE, option = "A", direction = -1, name = "Windspeed") +
        facet_grid(storm_id ~ wind_source) +
        coord_map() +
        theme_void() +
        ggplot2::geom_path(data = full_tracks,
                           ggplot2::aes_(x = ~ longitude,
                                         y = ~ latitude,
                                         group = ~ storm_id), color = "firebrick") +
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
        theme(legend.position = "top",
              strip.text.y = element_text(angle = 0))

pdf("figures/windexamples.pdf", width = 7, height = 7)
print(out)
dev.off()

modeled_winds <- data.table::data.table(hurricaneexposuredata::storm_winds) %>%
        filter(storm_id %in% c("Katrina-2005", "Ivan-2004")) %>%
        select(fips, vmax_sust, storm_id) %>%
        mutate(wind = cut(vmax_sust, breaks = c(0, 17.4896, 25.7200, 32.9216, 100),
                          include.lowest = TRUE, right = TRUE,
                          labels = c("0-33.9 knots", "34-49.9 knots", "50-63.9 knots", "64+ knots")),
               wind_source = "Modeled")

ext_winds <- data.table::data.table(hurricaneexposuredata::ext_tracks_wind) %>%
        filter(storm_id %in% c("Katrina-2005", "Ivan-2004")) %>%
        select(fips, vmax_sust, storm_id) %>%
        mutate(wind = cut(vmax_sust, breaks = c(0, 17.4896, 25.7200, 32.9216, 100),
                          include.lowest = TRUE, right = TRUE,
                          labels = c("0-33.9 knots", "34-49.9 knots", "50-63.9 knots", "64+ knots")),
               wind_source = "Extended Best Tracks")

to_plot <- bind_rows(modeled_winds, ext_winds) %>%
        mutate(wind_source = factor(wind_source, levels = c("Modeled", "Extended Best Tracks")),
               storm_id = factor(storm_id, levels = c("Katrina-2005", "Ivan-2004"),
                                 labels = c("Hurricane Katrina\n(2005)", "Hurricane Ivan\n(2004)")))


map_dim <- apply(matrix(c(-106.65037, 25.12993, -67.00742, 47.48101),
                        byrow = TRUE, ncol = 2),
                 MARGIN = 2,
                 function(x) range(x) + c(-1, 1) * 2)
tracks <- hurricaneexposuredata::hurr_tracks %>%
        dplyr::select_(~ latitude, ~ longitude, ~ storm_id,
                       ~ date) %>%
        dplyr::filter_(~ as.character(storm_id) %in% c("Katrina-2005", "Ivan-2004") &
                               longitude > map_dim[1, 1] &
                               longitude < map_dim[2, 1] &
                               latitude > map_dim[1, 2] &
                               latitude < map_dim[2, 2]) %>%
        dplyr::mutate_(date = ~ lubridate::ymd_hm(date)) %>%
        mutate(storm_id = factor(storm_id, levels = c("Katrina-2005", "Ivan-2004"),
                                 labels = c("Hurricane Katrina\n(2005)", "Hurricane Ivan\n(2004)")))
splt_tracks <- split(tracks, tracks$storm_id)
full_tracks <- lapply(splt_tracks, hurricaneexposure:::interp_track)
full_tracks <- do.call("rbind", full_tracks)


out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::full_join(to_plot, by = "fips")
out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
                              ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                            fill = ~ wind, color = ~ wind),
                              size = 0.2) +
        scale_fill_viridis(discrete = TRUE, name = "Windspeed") +
        scale_color_viridis(discrete = TRUE, name = "Windspeed") +
        facet_grid(storm_id ~ wind_source) +
        coord_map() +
        theme_void() +
        ggplot2::geom_path(data = full_tracks,
                           ggplot2::aes_(x = ~ longitude,
                                         y = ~ latitude,
                                         group = ~ storm_id), color = "firebrick") +
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
        theme(legend.position = "top",
              strip.text.y = element_text(angle = 0))

pdf("figures/windexamples2.pdf", width = 7, height = 7)
print(out)
dev.off()
