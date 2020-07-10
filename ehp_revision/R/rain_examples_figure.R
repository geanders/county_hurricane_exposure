library(hurricaneexposuredata)
library(hurricaneexposure)
library(ggplot2)
library(sp)
library(maptools)
library(rgeos)

ike_data <- filter_storm_data(storm = c("Ike-2008"), include_rain = TRUE,
                              days_included = c(-2, -1, 0, 1),
                              output_vars = c("storm_id", "fips", "tot_precip", "storm_dist"))
lee_data <- filter_storm_data(storm = c("Lee-2011"), include_rain = TRUE,
                              days_included = c(-2, -1, 0, 1),
                              output_vars = c("storm_id", "fips", "tot_precip", "storm_dist"))
map_data <- bind_rows(ike_data, lee_data) %>%
        mutate(storm_id = factor(storm_id, levels = c("Ike-2008", "Lee-2011"),
                                 labels = c("Hurricane Ike (2008)", "Tropical Storm Lee (2011)")),
               exposed = tot_precip >= 75 & storm_dist <= 500)

map_dim <- apply(matrix(c(-106.65037, 25.12993, -67.00742, 47.48101),
                        byrow = TRUE, ncol = 2),
                 MARGIN = 2,
                 function(x) range(x) + c(-1, 1) * 2)
tracks <- hurricaneexposuredata::hurr_tracks %>%
        dplyr::select_(~ latitude, ~ longitude, ~ storm_id,
                       ~ date) %>%
        dplyr::filter_(~ as.character(storm_id) %in% c("Ike-2008", "Lee-2011") &
                               longitude > map_dim[1, 1] &
                               longitude < map_dim[2, 1] &
                               latitude > map_dim[1, 2] &
                               latitude < map_dim[2, 2]) %>%
        dplyr::mutate_(date = ~ lubridate::ymd_hm(date)) %>%
        mutate(storm_id = factor(storm_id, levels = c("Ike-2008", "Lee-2011"),
                                 labels = c("Hurricane Ike (2008)", "Tropical Storm Lee (2011)")))
splt_tracks <- split(tracks, tracks$storm_id)
full_tracks <- lapply(splt_tracks, hurricaneexposure:::interp_track)
full_tracks <- do.call("rbind", full_tracks)

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::full_join(map_data, by = "fips")

exposed_ike <- out_data %>%
        filter(exposed & storm_id == "Hurricane Ike (2008)") %>%
        select(long, lat, group)
exposed_ike <- split(exposed_ike, exposed_ike$group)
exposed_ike <- lapply(exposed_ike, function(x) Polygon(x[ , c("long", "lat")]))
exposed_ike <- lapply(seq_along(exposed_ike),
                      function(x) Polygons(list(exposed_ike[[x]]), ID = c(1:length(exposed_ike))[x]))
exposed_ike <- SpatialPolygons(exposed_ike, proj4string = CRS("+proj=longlat +datum=WGS84"))
exposed_ike <- spTransform(exposed_ike, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))
exposed_ike <- gBuffer(exposed_ike, byid = TRUE, width = 0)
exposed_ike <- unionSpatialPolygons(exposed_ike, IDs = rep(1, length(exposed_ike)))
exposed_ike <- spTransform(exposed_ike, CRS("+proj=longlat +datum=WGS84"))
exposed_ike <- fortify(exposed_ike) %>%
        mutate(storm_id = "Hurricane Ike (2008)")

exposed_lee <- out_data %>%
        filter(exposed & storm_id == "Tropical Storm Lee (2011)") %>%
        select(long, lat, group)
exposed_lee <- split(exposed_lee, exposed_lee$group)
exposed_lee <- lapply(exposed_lee, function(x) Polygon(x[ , c("long", "lat")]))
exposed_lee <- lapply(seq_along(exposed_lee),
                      function(x) Polygons(list(exposed_lee[[x]]), ID = c(1:length(exposed_lee))[x]))
exposed_lee <- SpatialPolygons(exposed_lee, proj4string = CRS("+proj=longlat +datum=WGS84"))
exposed_lee <- spTransform(exposed_lee, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))
exposed_lee <- gBuffer(exposed_lee, byid = TRUE, width = 0)
exposed_lee <- unionSpatialPolygons(exposed_lee, IDs = rep(1, length(exposed_lee)))
exposed_lee <- spTransform(exposed_lee, CRS("+proj=longlat +datum=WGS84"))
exposed_lee <- fortify(exposed_lee) %>%
        mutate(storm_id = "Tropical Storm Lee (2011)")

a <- bind_rows(exposed_ike, exposed_lee)


out <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = out_data,
                              ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                            fill = ~ tot_precip, color = ~ tot_precip),
                              size = 0.2) +
        scale_fill_viridis(discrete = FALSE, option = "A", direction = -1,
                           name = "Cumulative precipitation (mm)") +
        scale_color_viridis(discrete = FALSE, option = "A", direction = -1,
                            name = "Cumulative precipitation (mm)") +
        facet_wrap(~ storm_id, ncol = 2) +
        theme_void() +
        coord_map() +
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
        ggplot2::geom_polygon(data = a,
                              aes(x = long, y = lat, group = group),
                              color = "cyan", fill = NA, size = 0.5) +
        ggplot2::geom_path(data = full_tracks,
                           ggplot2::aes_(x = ~ longitude,
                                         y = ~ latitude,
                                         group = ~ storm_id), color = "firebrick") +
        theme(legend.position = "top")

pdf("figures/rainexamples.pdf", width = 7, height = 4)
print(out)
dev.off()
