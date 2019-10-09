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
load("data/clean_empower.RData") # Loads dataframe named `medi`

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
                             end_year = 2015, wind_limit = 17.5) %>%
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
        tidyr::gather(metric, n_exposures, -fips) %>%
        left_join(medi, by = "fips") %>%
        mutate(elec_depend_exposure = elec_dependent * n_exposures / 10)

out_data <- hurricaneexposure:::get_eastern_map() %>%
        dplyr::mutate(fips = as.numeric(fips)) %>%
        dplyr::left_join(all_exposures, by = "fips") %>%
        dplyr::mutate(metric = factor(metric,
                                      levels = c("distance", "rain", "wind", "flood", "tornado"),
                                      labels = c("Distance-based metric", "Rain-based metric",
                                                 "Wind-based metric", "Flood-based metric",
                                                 "Tornado-based metric"))) %>%
        dplyr::filter(!is.na(metric))

out_data$value <- cut(out_data$elec_depend_exposure, c(0,
                                                    1,
                                                    10,
                                                    100,
                                                    1000,
                                                    10000),
                            labels = c("0", "1-9", "10-99",
                                       "100-999", "1,000+"),
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
        viridis::scale_fill_viridis(name = "Number of electricity-dependent\nMedicare beneficiaries exposed\nto storms per year", discrete = TRUE) +
        viridis::scale_color_viridis(name = "Number of electricity-dependent\nMedicare beneficiaries exposed\nto storms per year", discrete = TRUE) +
        facet_wrap(~ metric) +
        theme(legend.position = c(.85, .3),
              legend.direction = "vertical",
              legend.key.size = unit(0.8, "lines"),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(colour = "black", fill = "white")) +
        ggtitle("Expected number of electricity-dependent Medicare beneficiaries exposed per year by metric")

top_exposures <- all_exposures %>%
        filter(n_exposures > 0) %>%
        mutate(metric = factor(metric,
                               levels = c("distance", "rain", "wind", "flood", "tornado"),
                               labels = c("Distance-based metric", "Rain-based metric",
                                          "Wind-based metric", "Flood-based metric",
                                          "Tornado-based metric"))) %>%
        group_by(metric) %>%
        top_n(10, wt = elec_depend_exposure) %>%
        mutate(county = stringr::str_to_title(county),
               state = stringr::str_to_upper(state),
               county_name = ifelse(state == "LA", "Parish", "County")) %>%
        unite(county, county, county_name, sep = " ") %>%
        unite(county, county, state, sep = ", ") %>%
        ungroup() %>%
        arrange(metric, elec_depend_exposure) %>%
        mutate(new_ord = factor(1:n()))

library(forcats)
library(viridis)
out2 <- top_exposures %>%
        ggplot(aes(new_ord,
                   elec_depend_exposure,
                   fill = elec_dependent)) +
        geom_col() +
        coord_flip() +
        labs(x = "", y = "Average # of electricity-dependent Medicare beneficiaries exposed per year",
             fill = "# of electricity-dependent\nMedicare beneficiaries\nin the county") +
        scale_fill_viridis(breaks = c(6000, 9000, 12000),
                           labels = c("6,000", "9,000", "12,000")) +
        facet_wrap(~ metric, scales = "free_y", drop = TRUE) +
        scale_x_discrete(breaks = top_exposures$new_ord,
                         labels = top_exposures$county) +
        scale_y_continuous(breaks = c(0, 2000, 4000, 6000),
                           labels = c("0", "2,000", "4,000", "6,000")) +
        theme_classic() +
        theme(legend.position = c(.85, .25),
              legend.direction = "vertical",
              legend.key.size = unit(0.9, "lines"),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_rect(colour = "black", fill = "white"),
              plot.margin=unit(c(1, 1.1, 0.5, 0.5),"cm")) # +
        # ggtitle("Counties with highest storm exposures to electricity-dependent Medicare beneficiaries")

pdf("figures/averageelecdependexposure.pdf", width = 9, height = 5.5)
grid.arrange(out, ncol = 1)
dev.off()

pdf("figures/topelecdependexposure.pdf", width = 9.5, height = 4.5)
grid.arrange(out2, ncol = 1)
dev.off()


