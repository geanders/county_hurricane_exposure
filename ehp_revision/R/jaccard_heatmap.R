library(hurricaneexposure)
library(hurricaneexposuredata)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(viridis)
library(dendextend)
library(ComplexHeatmap)
library(circlize)

data(closest_dist)

calc_jaccard <- function(x){
        x_mat <- x %>%
                dplyr::select(-fips) %>%
                dplyr::mutate_all(funs(as.numeric(.))) %>%
                as.matrix()
        jaccard <- proxy::simil(t(x_mat), method = "jaccard")
        out <- combn(colnames(x_mat), 2) %>%
                t() %>%
                as_tibble(.name_repair = "unique") %>%
                dplyr::rename(metric_1 = `...1`,
                              metric_2 = `...2`) %>%
                dplyr::mutate(jaccard = as.vector(jaccard))
        return(out)
}

calc_metric_1_not_2 <- function(x){
        x_mat <- x %>%
                dplyr::select(-fips) %>%
                dplyr::mutate_all(funs(as.numeric(.))) %>%
                as.matrix()
        met_1_not_2 <- function(i, j){
                x <- x_mat[ , i]
                y <- x_mat[ , j]
                if(sum(x | y) == 0) return(NA)
                a <- sum(x & y)
                b <- sum(x & !y)
                c <- sum(!x & y)
                metric <- b / (a + b + c)
                return(metric)
        }
        out_metric <- outer(1:5, 1:5, FUN = Vectorize(met_1_not_2))
        out <- combn(colnames(x_mat), 2) %>%
                t() %>%
                as_tibble(.name_repair = "unique") %>%
                dplyr::rename(metric_1 = `...1`,
                              metric_2 = `...2`) %>%
                dplyr::mutate(met_1_not_2 = as.vector(t(out_metric))[t(upper.tri(out_metric))])
        return(out)
}


calc_metric_2_not_1 <- function(x){
        x_mat <- x %>%
                dplyr::select(-fips) %>%
                dplyr::mutate_all(funs(as.numeric(.))) %>%
                as.matrix()
        met_2_not_1 <- function(i, j){
                x <- x_mat[ , i]
                y <- x_mat[ , j]
                if(sum(x | y) == 0) return(NA)
                a <- sum(x & y)
                b <- sum(x & !y)
                c <- sum(!x & y)
                metric <- c / (a + b + c)
                return(metric)
        }
        out_metric <- outer(1:5, 1:5, FUN = Vectorize(met_2_not_1))
        out <- combn(colnames(x_mat), 2) %>%
                t() %>%
                as_tibble(.name_repair = "unique") %>%
                dplyr::rename(metric_1 = `...1`,
                              metric_2 = `...2`) %>%
                dplyr::mutate(met_2_not_1 = as.vector(t(out_metric))[t(upper.tri(out_metric))])
        return(out)
}

my_fips <- unique(closest_dist$fips)

distance_data <- county_distance(counties = my_fips, start_year = 1996, end_year = 2011,
                                 dist_limit = 100) %>%
        select(storm_id, fips) %>%
        dplyr::mutate(distance = TRUE)

rain_data <- county_rain(counties = my_fips, start_year = 1996, end_year = 2011,
                         rain_limit = 75, dist_limit = 500) %>%
        select(storm_id, fips) %>%
        dplyr::mutate(rain = TRUE)

wind_data <- county_wind(counties = my_fips, start_year = 1996, end_year = 2011,
                         wind_limit = 17.4) %>%
        select(storm_id, fips) %>%
        dplyr::mutate(wind = TRUE)

flood_data <- county_events(counties = my_fips, start_year = 1996, end_year = 2011,
                            event_type = "flood") %>%
        select(storm_id, fips) %>%
        dplyr::mutate(flood = TRUE)

tornado_data <- county_events(counties = my_fips, start_year = 1996, end_year = 2011,
                            event_type = "tornado") %>%
        select(storm_id, fips) %>%
        dplyr::mutate(tornado = TRUE)

jaccard_sims <- distance_data %>%
        full_join(rain_data, by = c("storm_id", "fips")) %>%
        full_join(wind_data, by = c("storm_id", "fips")) %>%
        full_join(flood_data, by = c("storm_id", "fips")) %>%
        full_join(tornado_data, by = c("storm_id", "fips")) %>%
        mutate_at(c("distance", "rain", "wind", "flood", "tornado"),
                  funs(ifelse(is.na(.), FALSE, .))) %>%
        dplyr::mutate(any_exposure = any(distance, rain, wind, flood, tornado)) %>%
        dplyr::group_by(storm_id) %>%
        dplyr::mutate(counties_exposed = sum(any_exposure)) %>%
        ungroup() %>%
        dplyr::filter(counties_exposed >= 100) %>%
        dplyr::select(-counties_exposed, -any_exposure) %>%
        nest(data = c(fips, distance, rain, wind, flood, tornado)) %>%
        dplyr::mutate(jaccard = purrr::map(data, calc_jaccard)) %>%
        unnest(jaccard)
jaccard_sims <- jaccard_sims %>%
        dplyr::mutate(which_to_switch = metric_1 == "wind" & metric_2 == "tornado",
               metric_1 = ifelse(which_to_switch, "tornado", metric_1),
               metric_2 = ifelse(which_to_switch, "wind", metric_2)) %>%
        dplyr::select(-which_to_switch)

metric_1_not_2 <- distance_data %>%
        full_join(rain_data, by = c("storm_id", "fips")) %>%
        full_join(wind_data, by = c("storm_id", "fips")) %>%
        full_join(flood_data, by = c("storm_id", "fips")) %>%
        full_join(tornado_data, by = c("storm_id", "fips")) %>%
        mutate_at(c("distance", "rain", "wind", "flood", "tornado"),
                  funs(ifelse(is.na(.), FALSE, .))) %>%
        dplyr::mutate(any_exposure = any(distance, rain, wind, flood, tornado)) %>%
        dplyr::group_by(storm_id) %>%
        dplyr::mutate(counties_exposed = sum(any_exposure)) %>%
        ungroup() %>%
        filter(counties_exposed >= 100) %>%
        select(-counties_exposed, -any_exposure) %>%
        nest(data = c(fips, distance, rain, wind, flood, tornado)) %>%
        dplyr::mutate(met_1_not_2 = purrr::map(data, calc_metric_1_not_2)) %>%
        unnest(met_1_not_2)
metric_2_not_1 <- distance_data %>%
        full_join(rain_data, by = c("storm_id", "fips")) %>%
        full_join(wind_data, by = c("storm_id", "fips")) %>%
        full_join(flood_data, by = c("storm_id", "fips")) %>%
        full_join(tornado_data, by = c("storm_id", "fips")) %>%
        mutate_at(c("distance", "rain", "wind", "flood", "tornado"),
                  funs(ifelse(is.na(.), FALSE, .))) %>%
        dplyr::mutate(any_exposure = any(distance, rain, wind, flood, tornado)) %>%
        dplyr::group_by(storm_id) %>%
        dplyr::mutate(counties_exposed = sum(any_exposure)) %>%
        ungroup() %>%
        filter(counties_exposed >= 100) %>%
        select(-counties_exposed, -any_exposure) %>%
        nest(data = c(fips, distance, rain, wind, flood, tornado)) %>%
        dplyr::mutate(met_2_not_1 = purrr::map(data, calc_metric_2_not_1)) %>%
        unnest(met_2_not_1)
other_metrics <- full_join(metric_1_not_2, metric_2_not_1,
                           by = c("storm_id", "metric_1", "metric_2")) %>%
        mutate(metric_set = paste(str_to_title(metric_1), str_to_title(metric_2), sep = " / ")) %>%
        dplyr::group_by(metric_set) %>%
        dplyr::summarize(met_1_not_2 = mean(met_1_not_2),
                         met_2_not_1 = mean(met_2_not_1))

counties_exposed <- distance_data %>%
        full_join(rain_data, by = c("storm_id", "fips")) %>%
        full_join(wind_data, by = c("storm_id", "fips")) %>%
        full_join(flood_data, by = c("storm_id", "fips")) %>%
        full_join(tornado_data, by = c("storm_id", "fips")) %>%
        mutate_at(c("distance", "rain", "wind", "flood", "tornado"),
                  funs(ifelse(is.na(.), FALSE, .))) %>%
        dplyr::mutate(any_exposure = any(distance, rain, wind, flood, tornado)) %>%
        group_by(storm_id) %>%
        dplyr::summarize(counties_exposed = sum(any_exposure)) %>%
        dplyr::filter(counties_exposed >= 100)

# For Grace in 2003 and Ida in 2009, there are no wind or tornado exposures
jaccard_sims <- jaccard_sims %>% 
        mutate(jaccard = ifelse(storm_id %in% c("Grace-2003", "Ida-2009") & 
                                        metric_1 == "tornado" & metric_2 == "wind",
                                NA, jaccard))

a <- jaccard_sims %>%
        dplyr::mutate(metric_1 = str_to_title(metric_1),
               metric_2 = str_to_title(metric_2)) %>%
        unite(metric_pair, metric_1, metric_2, sep = " / ") %>%
        spread(key = metric_pair, value = jaccard) %>%
        dplyr::mutate(storm_id = str_replace(storm_id, "-", " ("),
               storm_id = paste0(storm_id, ")"))
for_heatmap <- as.matrix(a[ , 3:12])
rownames(for_heatmap) <- a$storm_id

row_dend <- hclust(stats::dist(for_heatmap), method = "complete")
row_dend <- color_branches(row_dend, k = 4)

ha_rot_cn <- HeatmapAnnotation(text = anno_text(colnames(for_heatmap), rot = 45, just = "left",
                                                gp = gpar(fontsize = 10),
                                                location = unit(0.5, "mm")
                                                ))

# metric_type <- data.frame(metrics = colnames(for_heatmap)) %>%
#         separate(metrics, c("first_metric", "second_metric"), sep = " / ")
# bind_cols(metric_type, other_metrics)
# value <- list(c(0.77500000, 0.08000000), # Distance / Flood
#               c(0.39705882, 0.09803922), # Distance / Rain
#               c(0.90957447, 0.02127660), # Distance / Tornado
#               c(0.00000000, 0.35211268), # Distance / Wind
#               c(0.72131148, 0.26229508), # Flood / Tornado
#               c(0.67625899, 0.11510791), # Rain / Flood
#               c(0.87218045, 0.07518797), # Rain / Tornado
#               c(0.01730104, 0.57439446), # Rain / Wind
#               c(0.01045296, 0.94076655), # Tornado / Wind
#               c(0.84429066, 0.01730104)  # Wind / Flood
#               )
# anno_disagree <- function(value) {
#         f <- function(index){
#                 n = length(index)
#                 pushViewport(viewport(xscale = c(0.5, n + 0.5), yscale = c(-1.5, 1.5)))
#                 grid.rect()
#                 grid.yaxis(at = c(0, 0.5, 1.0), label = c("0.0", "0.5", "1.0"),
#                            gp =  gpar(fontsize = 6), main = FALSE)
#                 grid.yaxis(at = c(0, -0.5, -1.0), label = c("0.0", "0.5", "1.0"),
#                            gp =  gpar(fontsize = 6))
#                 grid.rect(x = index, y = 0, height = sapply(value, function(x) x[1])[index],
#                           default.unit = "native", width = 0.6, just = "bottom")
#                 grid.rect(x = index, y = 0, height = sapply(value, function(x) x[2])[index],
#                           default.unit = "native", width = 0.6, just = "top", gp = gpar(fill = "gray"))
#                 upViewport()
#         }
#         attr(f, "fun") = "anno_disagree"
#         return(f)
# }
# ha_col <- HeatmapAnnotation(first_metric = metric_type$first_metric,
#                         bars = anno_disagree(value = value[c(9, 5, 3, 7, 10, 1, 6, 2, 8, 4)]),
#                         second_metric = metric_type$second_metric,
#                         col = list(first_metric = c("Distance" = "#ffffbf",
#                                                     "Rain" = "#2b83ba",
#                                                     "Wind" = "#fdae61",
#                                                     "Flood" = "#abdda4",
#                                                     "Tornado" = "#d7191c"),
#                                    second_metric = c("Distance" = "#ffffbf",
#                                                      "Rain" = "#2b83ba",
#                                                      "Wind" = "#fdae61",
#                                                      "Flood" = "#abdda4",
#                                                      "Tornado" = "#d7191c")),
#                         annotation_height = c(1, 6, 1),
#                         show_legend = c(TRUE, FALSE, FALSE),
#                         annotation_legend_param = list(first_metric = list(nrow = 2, title = "Exposure metric",
#                                                                            title_position = "leftcenter",
#                                                                            title_gp = gpar(fontsize = 9, lineheight = 0.7, font = 2),
#                                                                            labels = c("Distance", "Rain", "Wind", "Flood", "Tornado"),
#                                                                            at = c("Distance", "Rain", "Wind", "Flood", "Tornado"))))

ha_row <- HeatmapAnnotation(counties_exposed = counties_exposed$counties_exposed,
                            show_annotation_name = FALSE,
                            annotation_legend_param = list(counties_exposed = list(title = "\n# of\ncounties\nexposed\nby any\nmetric",
                                                                                   title_gp = gpar(fontsize = 9,
                                                                                                   lineheight = 0.7,
                                                                                                   font = 2))),
                            col = list(counties_exposed = colorRamp2(c(100, 275, 450, 625),
                                                                     viridis::viridis(4, option = "D", direction = -1))),
                            which = "row", width = unit(0.25, "cm"))

heat_map <- Heatmap(for_heatmap, col = viridis(256, option = "A", direction = -1),
        cluster_rows = row_dend, split = 4, show_row_dend = FALSE,
        row_names_side = "left", row_names_gp = gpar(fontsize = 8),
        show_column_names = FALSE, show_column_dend = FALSE, #column_dend_side = "bottom",
        column_dend_height = unit(6, "mm"),
        name = "Jaccard\nindex\nbetween\nmetrics",
        heatmap_legend_param = list(title_gp = gpar(fontsize = 9, lineheight = 0.7, font = 2)),
        # bottom_annotation = ha_col,
        top_annotation = ha_rot_cn,
        rect_gp = gpar(col = "white", lwd = 1),
        row_dend_reorder = TRUE, 
        row_title = NULL)

pdf(file = "figures/jaccard_heatmap.pdf", height = 8.4, width = 5)
draw(heat_map + ha_row, padding = unit(c(2, 2, 18, 2), "mm"), annotation_legend_side = "bottom")
# decorate_annotation("first_metric", {grid.text("Exposure metric #1", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
# decorate_annotation("second_metric", {grid.text("Exposure metric #2", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
dev.off()


### Check for accessibility

library(dichromat)
library(colorspace)

ha_row <- HeatmapAnnotation(counties_exposed = counties_exposed$counties_exposed,
                            show_annotation_name = FALSE,
                            annotation_legend_param = list(counties_exposed = list(title = "\n# of\ncounties\nexposed\nby any\nmetric",
                                                                                   title_gp = gpar(fontsize = 9,
                                                                                                   lineheight = 0.7,
                                                                                                   font = 2))),
                            col = list(counties_exposed = colorRamp2(c(100, 275, 450, 625),
                                                                     viridis::viridis(4, option = "D", direction = -1) %>% 
                                                                             dichromat(type = "deutan"))),
                            which = "row", width = unit(0.25, "cm"))
heat_map <- Heatmap(for_heatmap, col = viridis(256, option = "A", direction = -1) %>% 
                            dichromat(type = "deutan"),
                    cluster_rows = row_dend, split = 4, show_row_dend = FALSE,
                    row_names_side = "left", row_names_gp = gpar(fontsize = 8),
                    show_column_names = FALSE, show_column_dend = FALSE, #column_dend_side = "bottom",
                    column_dend_height = unit(6, "mm"),
                    name = "Jaccard\nindex\nbetween\nmetrics",
                    heatmap_legend_param = list(title_gp = gpar(fontsize = 9, lineheight = 0.7, font = 2)),
                    # bottom_annotation = ha_col,
                    top_annotation = ha_rot_cn,
                    rect_gp = gpar(col = "white", lwd = 1),
                    row_dend_reorder = TRUE, 
                    row_title = NULL, 
                    column_title = "Green-Blind (Deuteranopia)")

pdf(file = "ehp_revision/figures/jaccard_heatmap_check1.pdf", height = 8.4, width = 5)
draw(heat_map + ha_row, padding = unit(c(2, 2, 18, 2), "mm"), annotation_legend_side = "bottom")
# decorate_annotation("first_metric", {grid.text("Exposure metric #1", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
# decorate_annotation("second_metric", {grid.text("Exposure metric #2", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
dev.off()

ha_row <- HeatmapAnnotation(counties_exposed = counties_exposed$counties_exposed,
                            show_annotation_name = FALSE,
                            annotation_legend_param = list(counties_exposed = list(title = "\n# of\ncounties\nexposed\nby any\nmetric",
                                                                                   title_gp = gpar(fontsize = 9,
                                                                                                   lineheight = 0.7,
                                                                                                   font = 2))),
                            col = list(counties_exposed = colorRamp2(c(100, 275, 450, 625),
                                                                     viridis::viridis(4, option = "D", direction = -1) %>% 
                                                                             dichromat(type = "protan"))),
                            which = "row", width = unit(0.25, "cm"))
heat_map <- Heatmap(for_heatmap, col = viridis(256, option = "A", direction = -1) %>% 
                            dichromat(type = "protan"),
                    cluster_rows = row_dend, split = 4, show_row_dend = FALSE,
                    row_names_side = "left", row_names_gp = gpar(fontsize = 8),
                    show_column_names = FALSE, show_column_dend = FALSE, #column_dend_side = "bottom",
                    column_dend_height = unit(6, "mm"),
                    name = "Jaccard\nindex\nbetween\nmetrics",
                    heatmap_legend_param = list(title_gp = gpar(fontsize = 9, lineheight = 0.7, font = 2)),
                    # bottom_annotation = ha_col,
                    top_annotation = ha_rot_cn,
                    rect_gp = gpar(col = "white", lwd = 1),
                    row_dend_reorder = TRUE, 
                    row_title = NULL, 
                    column_title = "Red-Blind (Protanopia)")

pdf(file = "ehp_revision/figures/jaccard_heatmap_check2.pdf", height = 8.4, width = 5)
draw(heat_map + ha_row, padding = unit(c(2, 2, 18, 2), "mm"), annotation_legend_side = "bottom")
# decorate_annotation("first_metric", {grid.text("Exposure metric #1", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
# decorate_annotation("second_metric", {grid.text("Exposure metric #2", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
dev.off()

ha_row <- HeatmapAnnotation(counties_exposed = counties_exposed$counties_exposed,
                            show_annotation_name = FALSE,
                            annotation_legend_param = list(counties_exposed = list(title = "\n# of\ncounties\nexposed\nby any\nmetric",
                                                                                   title_gp = gpar(fontsize = 9,
                                                                                                   lineheight = 0.7,
                                                                                                   font = 2))),
                            col = list(counties_exposed = colorRamp2(c(100, 275, 450, 625),
                                                                     viridis::viridis(4, option = "D", direction = -1) %>% 
                                                                             dichromat(type = "tritan"))),
                            which = "row", width = unit(0.25, "cm"))
heat_map <- Heatmap(for_heatmap, col = viridis(256, option = "A", direction = -1) %>% 
                            dichromat(type = "tritan"),
                    cluster_rows = row_dend, split = 4, show_row_dend = FALSE,
                    row_names_side = "left", row_names_gp = gpar(fontsize = 8),
                    show_column_names = FALSE, show_column_dend = FALSE, #column_dend_side = "bottom",
                    column_dend_height = unit(6, "mm"),
                    name = "Jaccard\nindex\nbetween\nmetrics",
                    heatmap_legend_param = list(title_gp = gpar(fontsize = 9, lineheight = 0.7, font = 2)),
                    # bottom_annotation = ha_col,
                    top_annotation = ha_rot_cn,
                    rect_gp = gpar(col = "white", lwd = 1),
                    row_dend_reorder = TRUE, 
                    row_title = NULL, 
                    column_title = "Blue-Blind (Tritanopia)")

pdf(file = "ehp_revision/figures/jaccard_heatmap_check3.pdf", height = 8.4, width = 5)
draw(heat_map + ha_row, padding = unit(c(2, 2, 18, 2), "mm"), annotation_legend_side = "bottom")
# decorate_annotation("first_metric", {grid.text("Exposure metric #1", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
# decorate_annotation("second_metric", {grid.text("Exposure metric #2", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
dev.off()

ha_row <- HeatmapAnnotation(counties_exposed = counties_exposed$counties_exposed,
                            show_annotation_name = FALSE,
                            annotation_legend_param = list(counties_exposed = list(title = "\n# of\ncounties\nexposed\nby any\nmetric",
                                                                                   title_gp = gpar(fontsize = 9,
                                                                                                   lineheight = 0.7,
                                                                                                   font = 2))),
                            col = list(counties_exposed = colorRamp2(c(100, 275, 450, 625),
                                                                     viridis::viridis(4, option = "D", direction = -1) %>% 
                                                                             desaturate())),
                            which = "row", width = unit(0.25, "cm"))
heat_map <- Heatmap(for_heatmap, col = viridis(256, option = "A", direction = -1) %>% 
                            desaturate(),
                    cluster_rows = row_dend, split = 4, show_row_dend = FALSE,
                    row_names_side = "left", row_names_gp = gpar(fontsize = 8),
                    show_column_names = FALSE, show_column_dend = FALSE, #column_dend_side = "bottom",
                    column_dend_height = unit(6, "mm"),
                    name = "Jaccard\nindex\nbetween\nmetrics",
                    heatmap_legend_param = list(title_gp = gpar(fontsize = 9, lineheight = 0.7, font = 2)),
                    # bottom_annotation = ha_col,
                    top_annotation = ha_rot_cn,
                    rect_gp = gpar(col = "white", lwd = 1),
                    row_dend_reorder = TRUE, 
                    row_title = NULL, 
                    column_title = "Desaturated (Grayscale)")

pdf(file = "ehp_revision/figures/jaccard_heatmap_check4.pdf", height = 8.4, width = 5)
draw(heat_map + ha_row, padding = unit(c(2, 2, 18, 2), "mm"), annotation_legend_side = "bottom")
# decorate_annotation("first_metric", {grid.text("Exposure metric #1", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
# decorate_annotation("second_metric", {grid.text("Exposure metric #2", unit(-1, "mm"), just = "right", gp = gpar(fontsize = 8, font = 2))})
dev.off()





