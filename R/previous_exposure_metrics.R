library(tibble)
library(stringr)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(ggplot2)
library(viridis)
library(ggthemes)

exp_metrics <- tribble(
        ~ study, ~ area, ~ metric, ~ n,
        "Grabich et al. 2015", "birth outcomes", "wind, distance", "1, 1",
        "Grabich et al. 2016a", "birth outcomes", "damage, distance, wind", "1, 3, 2",
        "Currie and Rossin-Slater 2013", "birth outcomes", "distance, wind", "4, 1",
        "Kinney et al. 2008", "other health", "distance", "1",
        "Czajkowski et al. 2011", "mortality", "distance", "3",
        "Swerdel et al. 2014", "mortality", "damage", "1",
        "McKinney et al. 2011", "mortality", "distance, other", "1, 1",
        "Zandbergen 2009", "tropical cyclone exposure", "distance, wind", "1, 1",
        "Amstadter et al. 2010", "mental health", "flooding, wind, damage, other", "1, 1, 1, 1",
        "Escobedo et al. 2009", "physical damage", "damage", "1",
        "Acierno et al. 2006", "mental health", "distance, damage, wind", "1, 1, 1",
        "Tansel and Sizirici 2010", "physical damage", "distance", "3",
        "Baggerly and Ferretti 2008", "educational outcomes", "other", "1",
        "Zahran et al. 2010", "birth outcomes", "damage, other", "1, 1",
        "Grabich et al. 2016b", "birth outcomes", "wind", "2",
        "Esnard et al. 2011", "tropical cyclone exposure", "wind, distance", "1, 1",
        "Fuller 2014", "educational outcomes", "damage", "1",
        "Lieberman-Cribbin et al. 2017", "mental health", "flooding", "2",
        "Le et al. 2013", "mental health", "other", "1",
        "Mongin et al. 2017", "other health", "damage, flooding", "2, 2",
        "Logan and Xu 2015", "tropical cyclone exposure", "wind, damage, other", "1, 1, 1",
        "Kim et al. 2016", "other health", "other", "1",
        "Kim and Marcouiller 2015", "physical damage", "damage, other", "1, 1",
        "Horney et al. 2016", "tropical cyclone exposure", "damage", "1",
        "Gares and Montz 2014", "tropical cyclone exposure", "distance, other", "1, 1",
        "Mukherjee et al. 2017", "other non-health", "damage", "1",
        "Antipova and Curtis 2015", "birth outcomes", "distance, damage", "1, 1",
        "Domino et al. 2003", "other health", "damage", "1",
        "Rosenheim et al. 2018", "other health", "damage", "1",
        "Strobl 2011", "economic outcomes", "wind, damage", "1, 1",
        "Dosa et al. 2012", "mortality", "other", "1",
        "Caillouët et al. 2008", "other health", "distance", "1",
        "Kessler et al. 2007", "other health", "damage", "1"
)

a <- exp_metrics %>%
        mutate(metric = purrr::map(metric, str_split, pattern = ","),
               metric = purrr::map(metric, unlist),
               n = purrr::map(n, str_split, pattern = ","),
               n = purrr::map(n, unlist)) %>%
        unnest() %>%
        mutate(n = as.numeric(n),
               metric = str_trim(metric)) %>%
        group_by(study) %>%
        mutate(tot = n()) %>%
        ungroup() %>%
        mutate(area = fct_relevel(area, "tropical cyclone exposure",
                                  "birth outcomes", "mental health",
                                  "mortality", "other health",
                                  "physical damage", "educational outcomes",
                                  "economic outcomes",
                                  "other non-health")) %>%
        arrange(desc(area), tot) %>%
        mutate(study = factor(study, levels = unique(study)),
               metric = fct_relevel(metric, "distance", "wind", "flooding", "damage", "other"),
               metric = factor(metric, labels = str_to_title(levels(metric)))) # %>%
        # filter(area %in% c("birth outcomes", "mental health", "mortality", "other health"))

a_labels <- a %>%
        select(study, area) %>%
        distinct

out <- ggplot(a, aes(x = metric, y = as.numeric(study), fill = metric, alpha = n)) +
        geom_tile(color = "black") +
        scale_fill_viridis(discrete = TRUE, option = "A", guide = "none") +
        scale_alpha_continuous(range = c(0.25, 1),
                               name = "Thresholds considered") +
        theme_few() +
        theme(legend.position = "top") +
        labs(x = "", y = "") +
        scale_y_continuous(breaks = as.numeric(a_labels$study),
                           labels = a_labels$study,
                           sec.axis = sec_axis(~., breaks = as.numeric(a_labels$study),
                                               labels = str_to_title(a_labels$area))) +
        scale_x_discrete(position = "top")


# pdf("figures/previous_exposure_metrics.pdf", height = 4, width = 8)
pdf("figures/previous_exposure_metrics.pdf", height = 6, width = 8)
print(out)
dev.off()
