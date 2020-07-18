library(hurricaneexposuredata)
library(dplyr)
library(stringr)
library(ggplot2)
library(polycor)
library(latex2exp)
data(storm_winds)
data(ext_tracks_wind)

wind_comp <- storm_winds %>%
        filter(str_remove(storm_id, ".+\\-") %>%  as.numeric() <= 2018) %>% 
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
        ggplot(aes(x = perc_same, y = reorder(storm_id, perc_same), fill = non_zero)) +
        geom_point(size = 3, shape = 21, color = "black") +
        scale_x_continuous(labels = scales::percent) +
        theme_classic() +
        labs(x = "% of counties classified in same wind category\nby modeled storm wind and based on HURDAT2 wind radii",
             y = "",
             fill = "Number of counties with modeled\n sustained winds of <= 34 knots"
             ) +
        viridis::scale_fill_viridis(direction = -1, option = "B") +
        theme(legend.position = "bottom") + 
        theme(panel.grid.major.y = element_line(colour = "lightgray"))

ggsave("ehp_revision/figures/windcomparison.pdf", fig, width = 5.25, height = 8.5, units = "in")

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

# Check figure for accessibility

library(dichromat)
library(colorspace)
library(gridExtra)

fig_check <- wind_comp %>%
        group_by(storm_id) %>%
        summarize(perc_same = sum(cats_equal) / n(),
                  non_zero = sum(vmax_sust_ext != 0)) %>%
        filter(non_zero > 0) %>%
        ggplot(aes(x = perc_same, y = reorder(storm_id, perc_same), fill = non_zero)) +
        geom_point(size = 3, shape = 21, color = "black") +
        scale_x_continuous(labels = scales::percent) +
        theme_classic() +
        labs(x = "% of counties classified in same wind category\nby modeled storm wind and based on HURDAT2 wind radii",
             y = "",
             fill = "Number of counties with modeled\n sustained winds of <= 34 knots"
        ) +
        theme(legend.position = "bottom") + 
        theme(panel.grid.major.y = element_line(colour = "lightgray"))

a <- fig_check + 
        scale_fill_gradientn(colors = viridis(direction = -1, option = "B", n = 10) %>% 
                                     dichromat(type = "deutan")) + 
        ggtitle("Green-Blind (Deuteranopia)")

b <- fig_check + 
        scale_fill_gradientn(colors = viridis(direction = -1, option = "B", n = 10) %>% 
                                     dichromat(type = "protan")) + 
        ggtitle("Red-Blind (Protanopia)")

c <- fig_check + 
        scale_fill_gradientn(colors = viridis(direction = -1, option = "B", n = 10) %>% 
                                     dichromat(type = "tritan")) + 
        ggtitle("Blue-Blind (Tritanopia)")

d <- fig_check + 
        scale_fill_gradientn(colors = viridis(direction = -1, option = "B", n = 10) %>% 
                                     desaturate()) + 
        ggtitle("Desaturated (Grayscale)")

ggsave("ehp_revision/figures/windcomparison_check1.pdf", 
       grid.arrange(a, b, ncol = 2), 
       width = 10.5, height = 8.5, units = "in")
ggsave("ehp_revision/figures/windcomparison_check2.pdf", 
       grid.arrange(c, d, ncol = 2), 
       width = 10.5, height = 8.5, units = "in")
