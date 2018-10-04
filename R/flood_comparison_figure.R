library(hurricaneexposuredata)
library(hurricaneexposure)
library(countyfloods)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(viridis)
library(scales)

## Fairfield County, Connecticut 09001
# Baltimore County, MD 24005
## Bergen County, New Jersey 34003
## Montgomery County, Pennsylvania 42091
## Mobile County, Alabama 01097
## DC 11001
## Wake County, North Carolina 37183
## Harris County, TX, 48201
## Fulton County, GA 13121
## Escambia County, FL 12033
# Spartanburg, SC 45083
# Prince William County, VA 51153
# Forrest County, MS 28035

county_fips <- c("37183", "42091", "01097", "13121", "12033", "34003", "48201",
                 "24005", "09001")
county_floods <- county_events(county_fips, start_year = 1996, end_year = 2015, event_type = "flood") %>%
        select(storm_id, fips) %>%
        mutate(flood = TRUE)
county_storms <- county_distance(county_fips, start_year = 1996, end_year = 2015, dist_limit = 500) %>%
        left_join(county_floods, by = c("fips", "storm_id")) %>%
        mutate(flood = ifelse(is.na(flood), FALSE, flood),
               closest_date = ymd(closest_date)) %>%
        dplyr::select(storm_id, fips, closest_date, flood) %>%
        rename(date_0 = closest_date) %>%
        mutate(date_m2 = date_0 - 2,
               date_m1 = date_0 - 1,
               date_1 = date_0 + 1,
               date_2 = date_0 + 2,
               date_3 = date_0 + 3,
               date_4 = date_0 + 4,
               date_5 = date_0 + 5,
               date_6 = date_0 + 6) %>%
        gather(key = lag, value = date, -storm_id, -fips, -flood) %>%
        mutate(lag = str_replace(lag, "date_", ""),
               lag = str_replace(lag, "m", "-"),
               lag = as.numeric(lag))

# county_gages <- get_gages(county_fips, start_date = "1996-01-01", end_date = "2015-12-31")
# gage_q2s <- find_q2(county_gages$site_no)
# county_gages <- full_join(county_gages, gage_q2s, by = "site_no")
# county_flow_data <- get_flow_data(county_gages, start_date = "1996-01-01", end_date = "2015-12-31")
# save(county_gages, file = "data/county_gages.Rdata")
# save(county_flow_data, file = "data/county_flow_data.Rdata")

load("data/county_gages.Rdata")
load("data/county_flow_data.Rdata")

county_gage_data <- county_gages %>%
        filter(!is.na(flood_val)) %>%
        select(site_no, county_cd, flood_val) %>%
        inner_join(county_flow_data, by = "site_no") %>%
        mutate(flood_usgs = discharge > flood_val)

out <- county_gage_data %>%
        mutate(discharge = ifelse(discharge == -999999, NA, discharge)) %>%
        group_by(site_no) %>%
        dplyr::mutate(min_date = min(date),
                      max_date = max(date),
                      n = n()) %>%
        filter(min_date == ymd("1996-01-01") & max_date == ymd("2015-12-31") & n == 7305) %>%
        ungroup() %>%
        right_join(county_storms, by = c("date" = "date", "county_cd" = "fips")) %>%
        filter(lag %in% -2:2) %>%
        group_by(storm_id, county_cd, date) %>%
        mutate(daily_total = sum(discharge)) %>%
        ungroup() %>%
        group_by(storm_id, county_cd, site_no) %>%
        mutate(gage_flooded = any(flood_usgs)) %>%
        ungroup() %>%
        group_by(storm_id, county_cd) %>%
        dplyr::summarize(flood = first(flood),
                         max_total = max(daily_total),
                         perc_flooded = mean(gage_flooded),
                         n_gages = length(unique(site_no))) %>%
        ungroup() %>%
        mutate(county_cd = factor(county_cd, levels = c("37183", "42091", "01097",
                                                        "13121", "12033", "34003",
                                                        "48201", "24005", "09001"),
                                  labels = c("Wake County, NC",
                                             "Montgomery County, PA",
                                             "Mobile County, AL",
                                             "Fulton County, GA",
                                             "Escambia County, FL",
                                             "Bergen County, NJ",
                                             "Harris County, TX",
                                             "Baltimore County, MD",
                                             "Fairfield County, CT")),
               flood = factor(flood, levels = c(FALSE, TRUE),
                              labels = c("No flood event", "Flood event")),
               county_cd = paste0(as.character(county_cd), " (", n_gages, ")"),
               county_cd = factor(county_cd)) %>%
        ggplot(aes(x = flood, y = max_total)) +
        geom_boxplot(color = "darkgray", fill = NA, outlier.alpha = 0, width = 0.3) +
        geom_point(aes(color = perc_flooded), size = 1.7,
                   position = position_jitter(width = 0.1)) +
        facet_wrap(~ county_cd, ncol = 3, scales = "free_x") +
        theme_classic() +
        labs(x = "", y = "Total discharge across county streamflow gages") +
        coord_flip() +
        scale_y_log10() +
        theme(legend.position = "bottom") +
        scale_color_viridis(name = "% streamflow gages over threshold for flooding",
                            breaks = c(0, .5, 1), labels = c("0%", "50%", "100%")) +
        theme(plot.margin = unit(c(5.5, 11, 5.5, 11), "points"))


pdf(file = "figures/floodcomparison.pdf", height = 6, width = 7.5)
print(out)
dev.off()
