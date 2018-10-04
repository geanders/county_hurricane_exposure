library(hurricaneexposuredata)
library(dplyr)
library(ggplot2)
library(polycor)
data(storm_winds)
data(ext_tracks_wind)

wind_comp <- storm_winds %>%
        select(fips, vmax_sust, storm_id) %>%
        rename(vmax_sust_model = vmax_sust) %>%
        inner_join(select(ext_tracks_wind, fips, storm_id, vmax_sust),
                  by = c("fips", "storm_id")) %>%
        rename(vmax_sust_ext = vmax_sust) %>%
        mutate(vmax_sust_model_cat = ifelse(vmax_sust_model < 17.4896, 0, NA),
               vmax_sust_model_cat = ifelse(vmax_sust_model >= 17.4896 & vmax_sust_model < 25.7200,
                                            17.4896, vmax_sust_model_cat),
               vmax_sust_model_cat = ifelse(vmax_sust_model >= 25.7200 & vmax_sust_model < 32.9216,
                                            25.7200, vmax_sust_model_cat),
               vmax_sust_model_cat = ifelse(vmax_sust_model >= 32.9216, 32.9216, vmax_sust_model_cat),
               cats_equal = vmax_sust_model_cat == vmax_sust_ext)

fig <- wind_comp %>%
        group_by(storm_id) %>%
        summarize(perc_same = sum(cats_equal) / n(),
                  non_zero = sum(vmax_sust_ext != 0)) %>%
        filter(non_zero > 0) %>%
        ggplot(aes(x = perc_same, y = reorder(storm_id, perc_same), color = non_zero)) +
        geom_point(size = 2) +
        scale_x_continuous(labels = scales::percent) +
        theme_classic() +
        labs(x = "% of counties classified in same wind category\nby modeled storm wind and Extended Best Tracks",
             y = "",
             color = "Number of counties with modeled\nsustained winds of \u2265 34 knots",
             parse = TRUE
             ) +
        viridis::scale_color_viridis(direction = -1, option = "B") +
        theme(legend.position = "bottom")

quartz.save(file = "figures/windcomparison.pdf", type = "pdf", width = 5.25, height = 7)
print(fig)
dev.off()

tiff(filename = "figures/windcomparison.tiff", width = 525, height = 700)
print(fig)
dev.off()

wind_comp_ordinal <- wind_comp %>%
        mutate(vmax_sust_ext = factor(vmax_sust_ext, ordered = TRUE,
                                      labels = c("0-33.9", "34-49.9", "50-63.9", "64+")),
               vmax_sust_model_cat = factor(vmax_sust_model_cat, ordered = TRUE,
                                            labels = c("0-33.9", "34-49.9", "50-63.9", "64+"))) %>%
        group_by(storm_id) %>%
        mutate(non_zero = sum(vmax_sust_ext != 0)) %>%
        ungroup() %>%
        filter(non_zero > 0) %>%
        group_by(storm_id) %>%
        summarize(polychor = polychor(vmax_sust_ext, vmax_sust_model_cat),
                  non_zero = first(non_zero)) %>%
        filter(!is.na(polychor))
wind_comp_ordinal %>%
        ggplot(aes(x = polychor, y = reorder(storm_id, polychor))) +
        geom_point() +
        theme_classic() +
        labs(x ="Polychoric correlation between wind\ncategorization within counties",
             y = "")
