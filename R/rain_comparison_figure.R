library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

data("closest_dist")
# library(countyweather) # If pulling data rather than using saved data


plot_county_rain_compare <- function(ex_fips, ex_dir, ex_title,
                                     get_data = FALSE){
        if(get_data){
                write_daily_timeseries(ex_fips, coverage = 0,
                                       date_min = "1988-01-01",
                                       date_max = "2011-12-31",
                                       var = "PRCP",
                                       out_directory = ex_dir,
                                       keep_map = FALSE)
        }

        check_dates <- closest_dist %>%
                dplyr::filter(fips == ex_fips) %>%
                dplyr::select(-storm_dist) %>%
                dplyr::mutate(closest_date = ymd(closest_date)) %>%
                dplyr::rename(day_0 = closest_date) %>%
                dplyr::mutate(fips = as.integer(fips),
                              day_0 = day_0 + days(0),
                              day_b1 = day_0 - days(1),
                              day_b2 = day_0 - days(2),
                              day_a1 = day_0 + days(1)) %>%
                dplyr::select(storm_id, day_b2, day_b1, day_0, day_a1) %>%
                tidyr::gather(key = lag, value = day, -storm_id) %>%
                dplyr::rename(date = day)
        all_dates <- unique(check_dates$date)

        ex_weather <- readRDS(paste0(ex_dir, ex_fips, ".rds"))

        ex_weather <- ex_weather %>%
                dplyr::filter(date %in% all_dates) %>%
                dplyr::right_join(check_dates, by = "date") %>%
                dplyr::group_by(storm_id) %>%
                dplyr::summarize(prcp = sum(prcp),
                                 ave_n = mean(prcp_reporting))

        ex_rain <- county_rain(counties = ex_fips,
                               start_year = 1988, end_year = 2011,
                               rain_limit = 0, dist_limit = 500) %>%
                full_join(ex_weather, by = "storm_id") %>%
                filter(!is.na(prcp) & !is.na(tot_precip)) %>%
                mutate(prcp = prcp / 10) ## Units for countyweather are now 10ths
        ## of millimeters for precipitation

        rain_rho <- cor(ex_rain$tot_precip, ex_rain$prcp, method = "spearman")
        rho <- sprintf("\"Spearman's\" ~ rho == %0.2f", rain_rho)

        ave_n <- round(mean(ex_rain$ave_n, na.rm = TRUE))
        n_storms <- nrow(ex_rain)
        ex_title <- paste0(ex_title, "\n(storms: ", n_storms,
                           "; monitors: ", ave_n, ")")

        plot_range <- range(ex_rain[ , c("prcp", "tot_precip")],
                            na.rm = TRUE)

        ex_plot <- ggplot(ex_rain, aes(x = tot_precip, y = prcp)) +
                geom_hline(aes(yintercept = 75), color = "lightgray") +
                geom_vline(aes(xintercept = 75), color = "lightgray") +
                geom_abline(aes(intercept = 0, slope = 1), color = "gray",
                            alpha = 0.5) +
                geom_point(alpha = 0.5, color = "black") +
                theme_classic() +
                scale_size_continuous(guide = "none") +
                xlab("Rainfall (mm)  from\nNLDAS-2 data") +
                ylab("Rainfall (mm)\nfrom monitors") +
                xlim(plot_range) + ylim(plot_range) +
                ggtitle(ex_title) +
                geom_label(x = max(ex_rain$tot_precip, ex_rain$prcp) * 0.7,
                           y = max(ex_rain$tot_precip, ex_rain$prcp) * 0.1,
                           parse = TRUE, label = as.character(rho),
                           size = 3)
        return(ex_plot)
}

a <- plot_county_rain_compare(ex_fips = "12086", ex_dir = "../DraftExposurePaper/dade_data/data/",
                              ex_title = "Miami-Dade, FL",
                              get_data = FALSE)

b <- plot_county_rain_compare(ex_fips = "48201", ex_dir = "../DraftExposurePaper/harris_data/data/",
                              ex_title = "Harris County, TX",
                              get_data = FALSE)

c <- plot_county_rain_compare(ex_fips = "01097", ex_dir = "../DraftExposurePaper/mobile_data/data/",
                              ex_title = "Mobile County, AL",
                              get_data = FALSE)

d <- plot_county_rain_compare(ex_fips = "22071", ex_dir = "../DraftExposurePaper/orleans_data/data/",
                              ex_title = "Orleans Parish, LA",
                              get_data = FALSE)

e <- plot_county_rain_compare(ex_fips = "13121", ex_dir = "../DraftExposurePaper/fulton_data/data/",
                              ex_title = "Fulton County, GA",
                              get_data = FALSE)

f <- plot_county_rain_compare(ex_fips = "45019", ex_dir = "../DraftExposurePaper/charleston_data/data/",
                              ex_title = "Charleston County, SC",
                              get_data = FALSE)

g <- plot_county_rain_compare(ex_fips = "37183", ex_dir = "../DraftExposurePaper/wake_data/data/",
                              ex_title = "Wake County, NC",
                              get_data = FALSE)

h <- plot_county_rain_compare(ex_fips = "24005", ex_dir = "../DraftExposurePaper/baltimore_data/data/",
                              ex_title = "Baltimore County, MD",
                              get_data = FALSE)

i <- plot_county_rain_compare(ex_fips = "42101", ex_dir = "../DraftExposurePaper/philadelphia_data/data/",
                              ex_title = "Philadelphia County, PA",
                              get_data = FALSE)

pdf(file = "figures/raincomparison.pdf", width = 9, height = 9)
grid.arrange(a, b, c, d, e, f, g, h, i, ncol = 3)
dev.off()

tiff(file = "figures/raincomparison.tiff", width = 700, height = 700)
grid.arrange(a, b, c, d, e, f, g, h, i, ncol = 3)
dev.off()


county_rain_compare_data <- function(ex_fips, ex_dir){

        check_dates <- closest_dist %>%
                dplyr::filter(fips == ex_fips) %>%
                dplyr::select(-storm_dist) %>%
                dplyr::mutate(closest_date = ymd(closest_date)) %>%
                dplyr::rename(day_0 = closest_date) %>%
                dplyr::mutate(fips = as.integer(fips),
                              day_0 = day_0 + days(0),
                              day_b1 = day_0 - days(1),
                              day_b2 = day_0 - days(2),
                              day_a1 = day_0 + days(1)) %>%
                dplyr::select(storm_id, day_b2, day_b1, day_0, day_a1) %>%
                tidyr::gather(key = lag, value = day, -storm_id) %>%
                dplyr::rename(date = day)
        all_dates <- unique(check_dates$date)

        ex_weather <- readRDS(paste0(ex_dir, ex_fips, ".rds"))

        ex_weather <- ex_weather %>%
                dplyr::filter(date %in% all_dates) %>%
                dplyr::right_join(check_dates, by = "date") %>%
                dplyr::group_by(storm_id) %>%
                dplyr::summarize(prcp = sum(prcp),
                                 ave_n = mean(prcp_reporting))

        ex_rain <- county_rain(counties = ex_fips,
                               start_year = 1988, end_year = 2011,
                               rain_limit = 0, dist_limit = 500) %>%
                full_join(ex_weather, by = "storm_id") %>%
                filter(!is.na(prcp) & !is.na(tot_precip)) %>%
                mutate(prcp = prcp / 10) ## Units for countyweather are now 10ths
        ## of millimeters for precipitation

        return(ex_rain)
}

county_rain_compare_data(ex_fips = "12086", ex_dir = "../DraftExposurePaper/dade_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "48201", ex_dir = "../DraftExposurePaper/harris_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "01097", ex_dir = "../DraftExposurePaper/mobile_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "22071", ex_dir = "../DraftExposurePaper/orleans_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "13121", ex_dir = "../DraftExposurePaper/fulton_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "45019", ex_dir = "../DraftExposurePaper/charleston_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "37183", ex_dir = "../DraftExposurePaper/wake_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "24005", ex_dir = "../DraftExposurePaper/baltimore_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))

county_rain_compare_data(ex_fips = "42101", ex_dir = "../DraftExposurePaper/philadelphia_data/data/") %>%
        select(storm_id, tot_precip, prcp) %>%
        mutate(nldas_exp = tot_precip >= 75,
               monitor_exp = prcp >= 75,
               agree = nldas_exp == monitor_exp) %>%
        summarize(n_total = n(),
                  n_agree = sum(agree),
                  n_disagree = sum(!agree),
                  nldas_notmonitor = sum(nldas_exp & !monitor_exp),
                  monitor_notnldas = sum(!nldas_exp & monitor_exp))


county_rain_compare_data(ex_fips = "48201", ex_dir = "../DraftExposurePaper/harris_data/data/") %>%
        arrange(desc(prcp)) %>%
        select(storm_id, tot_precip, prcp) %>%
        slice(1:3)

county_rain_compare_data(ex_fips = "01097", ex_dir = "../DraftExposurePaper/mobile_data/data/") %>%
        arrange(desc(prcp)) %>%
        select(storm_id, tot_precip, prcp) %>%
        slice(1:5)

county_rain_compare_data(ex_fips = "45019", ex_dir = "../DraftExposurePaper/charleston_data/data/") %>%
        arrange(desc(prcp)) %>%
        select(storm_id, tot_precip, prcp) %>%
        slice(1:3)

county_rain_compare_data(ex_fips = "37183", ex_dir = "../DraftExposurePaper/wake_data/data/") %>%
        arrange(desc(prcp)) %>%
        select(storm_id, tot_precip, prcp) %>%
        slice(1:3)
