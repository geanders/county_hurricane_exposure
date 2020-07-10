library(hurricaneexposuredata)
library(hurricaneexposure)
library(dplyr)
library(tidyr)
library(xtable)
library(stringr)
library(psych)

my_fips <- unique(closest_dist$fips)
distance_exposure <- county_distance(counties = my_fips, start_year = 1988,
                                     end_year = 2015, dist_limit = 100) %>%
        select(storm_id, fips) %>%
        mutate(distance_exposed = TRUE,
               storm_id = as.character(storm_id))
rain_exposure <- county_rain(counties = my_fips,
                             start_year = 1988, end_year = 2011,
                             rain_limit = 75, dist_limit = 500) %>%
        select(storm_id, fips) %>%
        mutate(rain_exposed = TRUE,
               storm_id = as.character(storm_id))
wind_exposure <- county_wind(counties = my_fips, start_year = 1988,
                             end_year = 2015, wind_limit = 15) %>%
        select(storm_id, fips) %>%
        mutate(wind_exposed = TRUE,
               storm_id = as.character(storm_id))
flood_exposure <- county_events(counties = my_fips,
                                start_year = 1996, end_year = 2015,
                                event_type = "flood") %>%
        select(storm_id, fips) %>%
        mutate(flood_exposed = TRUE,
               storm_id = as.character(storm_id))
tornado_exposure <- county_events(counties = my_fips,
                                  start_year = 1996, end_year = 2015,
                                  event_type = "tornado") %>%
        select(storm_id, fips) %>%
        mutate(tornado_exposed = TRUE,
               storm_id = as.character(storm_id))
total_exposure <- distance_exposure %>%
        full_join(rain_exposure, by = c("storm_id", "fips")) %>%
        full_join(wind_exposure, by = c("storm_id", "fips")) %>%
        full_join(flood_exposure, by = c("storm_id", "fips")) %>%
        full_join(tornado_exposure, by = c("storm_id", "fips")) %>%
        gather(metric, exposed,
               -storm_id, -fips, na.rm = FALSE) %>%
        mutate(exposed = ifelse(is.na(exposed), FALSE, exposed))

storm_severity <- total_exposure %>%
        dplyr::mutate(metric = str_replace(metric, "_exposed", ""),
               metric = str_to_title(metric)) %>%
        dplyr::group_by(storm_id, metric) %>%
        dplyr::summarize(exposed_counties = sum(exposed)) %>%
        tidyr::spread(metric, exposed_counties) %>%
        dplyr::ungroup()

## The following code is derived from an example at http://shinyapps.org/apps/RGraphCompendium/index.php
.plotMarginalCor <- function(variable, cexYlab = 1.3, lwd = 2, rugs = FALSE, xlim = NULL,
                             yhigh = NULL, year_lab = c("1988 to 2015"),
                             median_lab = "Median: 62", iqr_lab = "11, 150",
                             worst_storm_lab = "Beryl (1994): 330") {

        # histogram with density estimator

        variable <- variable[!is.na(variable)]

        density <- density(variable)
        if(is.null(xlim)){
                xlim <- c(0, max(variable))
        }
        h <- hist(variable, breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
                  plot = FALSE)
        jitVar <- jitter(variable)
        if(is.null(yhigh)){
                yhigh <- max(max(h$density), max(density$y))
        }
        ylow <- 0
        xticks <- pretty(c(variable, h$breaks, xlim), min.n = 3)
        plot(range(xticks), c(ylow, yhigh), type = "n", axes = FALSE, ylab = "",
             xlab = "")
        h <- hist(variable, freq = FALSE, main = "", ylim = c(ylow, yhigh), xlab = "",
                  ylab = " ", axes = FALSE, col = "grey", add = TRUE, breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500))
        ax1 <- axis(1, line = 0.3, at = xticks, lab = xticks)
        par(las = 0)
        ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density),
                                                                            max(density$y))), labels = c("", "Density", ""), lwd.ticks = 0,
                    pos = range(ax1) - 0.08 * diff(range(ax1)), cex.axis = 1.7, mgp = c(3,
                                                                                        0.7, 0))

        if (rugs)
                rug(jitVar)

        #lines(density$x[density$x >= min(ax1) & density$x <= max(ax1)], density$y[density$x >=
        #                                                                                  min(ax1) & density$x <= max(ax1)], lwd = lwd)
        text(0.62 * xlim[2], 0.9 * yhigh, labels = year_lab, cex = 1.2)
        text(0.62 * xlim[2], 0.75 * yhigh, labels = median_lab, cex = 1)
        text(0.62 * xlim[2], 0.6 * yhigh, labels = iqr_lab, cex = 1)
        text(0.62 * xlim[2], 0.45 * yhigh, labels = worst_storm_lab, cex = 1)
}

.poly.pred <- function(fit, line = FALSE, xMin, xMax, lwd) {

        # predictions of fitted model

        # create function formula
        f <- vector("character", 0)

        for (i in seq_along(coef(fit))) {

                if (i == 1) {

                        temp <- paste(coef(fit)[[i]])
                        f <- paste(f, temp, sep = "")
                }

                if (i > 1) {

                        temp <- paste("(", coef(fit)[[i]], ")*", "x^", i - 1, sep = "")
                        f <- paste(f, temp, sep = "+")
                }
        }

        x <- seq(xMin, xMax, length.out = 100)
        predY <- eval(parse(text = f))

        if (line == FALSE) {
                return(predY)
        }

        if (line) {
                lines(x, predY, lwd = lwd)
        }
}


.plotScatter <- function(xVar, yVar, cexPoints = 1.3, cexXAxis = 1.3,
                         cexYAxis = 1.3, lwd = 2, max_val = NULL) {

        # displays scatterplot

        d <- data.frame(xx = xVar, yy = yVar)
        d <- na.omit(d)
        xVar <- d$xx
        yVar <- d$yy

        fit <- lm(yy ~ poly(xx, 1, raw = TRUE), d)

        xlow <- min((min(xVar) - 0.1 * min(xVar)), min(pretty(xVar)))
        if(is.null(max_val)){
                xhigh <- max((max(xVar) + 0.1 * max(xVar)), max(pretty(xVar)))
        } else{
                xhigh <- max_val
        }
        xticks <- pretty(c(xlow, xhigh))
        ylow <- min((min(yVar) - 0.1 * min(yVar)), min(pretty(yVar)), min(.poly.pred(fit,
                                                                                     line = FALSE, xMin = xticks[1], xMax = xticks[length(xticks)],
                                                                                     lwd = lwd)))
        if(is.null(max_val)){
                yhigh <- max((max(yVar) + 0.1 * max(yVar)), max(pretty(yVar)), max(.poly.pred(fit,
                                                                                              line = FALSE, xMin = xticks[1], xMax = xticks[length(xticks)],
                                                                                              lwd = lwd)))
        } else{
                yhigh <- max_val
        }

        yticks <- pretty(c(ylow, yhigh))

        yLabs <- vector("character", length(yticks))

        for (i in seq_along(yticks)) {

                if (yticks[i] < 10^6) {

                        yLabs[i] <- format(yticks[i], digits = 3, scientific = FALSE)

                } else {

                        yLabs[i] <- format(yticks[i], digits = 3, scientific = TRUE)
                }
        }

        plot(xVar, yVar, col = "#00000050", pch = 21, bg = "#d3d3d350", ylab = "",
             xlab = "", axes = FALSE, ylim = range(yticks), xlim = range(xticks),
             cex = cexPoints)
        # .poly.pred(fit, line = TRUE, xMin = xticks[1], xMax = xticks[length(xticks)],
        #            lwd = lwd)

        par(las = 1)

        axis(1, line = 0.4, labels = xticks, at = xticks, cex.axis = cexXAxis)
        axis(2, line = 0.2, labels = yLabs, at = yticks, cex.axis = cexYAxis)

        invisible(max(nchar(yLabs)))
}

.plotCorValue <- function(xVar, yVar, cexText = 2, cexCI = 1.7, hypothesis = "correlated",
                          pearson = TRUE, kendallsTauB = FALSE, spearman = FALSE,
                          confidenceInterval = 0.95, year_lab = "1988 to 2015") {

        # displays correlation value

        CIPossible <- TRUE

        tests <- c()

        if (pearson)
                tests <- c(tests, "pearson")

        if (spearman)
                tests <- c(tests, "spearman")

        if (kendallsTauB)
                tests <- c(tests, "kendall")

        plot(1, 1, type = "n", axes = FALSE, ylab = "", xlab = "")

        lab <- vector("list")

        for (i in seq_along(tests)) {

                if (round(cor.test(xVar, yVar, method = tests[i])$estimate, 8) ==
                    1) {

                        CIPossible <- FALSE

                        if (tests[i] == "pearson") {
                                lab[[i]] <- bquote(italic(r) == "1.000")
                        }

                        if (tests[i] == "spearman") {
                                lab[[i]] <- bquote(italic(rho) == "1.000")
                        }

                        if (tests[i] == "kendall") {
                                lab[[i]] <- bquote(italic(tau) == "1.000")
                        }

                } else if (round(cor.test(xVar, yVar, method = tests[i])$estimate,
                                 8) == -1) {

                        CIPossible <- FALSE

                        if (tests[i] == "pearson") {
                                lab[[i]] <- bquote(italic(r) == "-1.000")
                        }

                        if (tests[i] == "spearman") {
                                lab[[i]] <- bquote(italic(rho) == "-1.000")
                        }

                        if (tests[i] == "kendall") {
                                lab[[i]] <- bquote(italic(tau) == "-1.000")
                        }

                } else {

                        if (tests[i] == "pearson") {
                                lab[[i]] <- bquote(italic(r) == .(formatC(round(cor.test(xVar,
                                                                                         yVar, method = tests[i])$estimate, 3), format = "f",
                                                                          digits = 3)))
                        }

                        if (tests[i] == "spearman") {
                                lab[[i]] <- bquote(rho == .(formatC(round(cor.test(xVar,
                                                                                   yVar, method = tests[i])$estimate, 3), format = "f",
                                                                    digits = 3)))
                        }

                        if (tests[i] == "kendall") {
                                lab[[i]] <- bquote(tau == .(formatC(round(cor.test(xVar,
                                                                                   yVar, method = tests[i])$estimate, 3), format = "f",
                                                                    digits = 3)))
                        }
                }
        }

        if (length(tests) == 1) {
                ypos <- 1
        }

        if (length(tests) == 2) {
                ypos <- c(1, 0.9)
        }

        if (length(tests) == 3) {
                ypos <- c(1.2, 1, 0.8)
        }


        for (i in seq_along(tests)) {

                text(1, ypos[i], labels = lab[[i]], cex = cexText)
        }


        if (hypothesis == "correlated" & length(tests) == 1 & any(tests ==
                                                                  "pearson")) {

                alternative <- "two.sided"
                ctest <- cor.test(xVar, yVar, method = tests, conf.level = confidenceInterval)
        }

        if (hypothesis != "correlated" & length(tests) == 1 & any(tests ==
                                                                  "pearson")) {

                if (hypothesis == "correlatedPositively") {

                        ctest <- cor.test(xVar, yVar, method = tests, alternative = "greater",
                                          conf.level = confidenceInterval)

                } else if (hypothesis == "correlatedNegatively") {

                        ctest <- cor.test(xVar, yVar, method = tests, alternative = "less",
                                          conf.level = confidenceInterval)
                }

        }

        if (any(tests == "pearson") & length(tests) == 1 && CIPossible) {

                CIlow <- formatC(round(ctest$conf.int[1], 3), format = "f", digits = 3)
                CIhigh <- formatC(round(ctest$conf.int[2], 3), format = "f", digits = 3)

                text(1, 0.8, labels = paste(100 * confidenceInterval, "% CI: [",
                                            CIlow, ", ", CIhigh, "]", sep = ""), cex = cexCI)
        }
        text(1, 1.15, year_lab, cex = 1.2 * cexText)

}

variables <- c("Distance", "Rain", "Wind", "Flood", "Tornado")
dataset <- storm_severity
max_x <- max(dataset[ , variables])
year_labs <- data_frame(year_min = c(1988, 1988, 1988, 1996, 1996),
                        year_max = c(2015, 2011, 2015, 2015, 2015))
median_labs <- c("62", "21", "26", "9", "1")
iqr_labs <- c("11, 150", "2, 113", "2, 66", "0, 39", "0, 9")
worst_storm_labs <- c("Beryl (1994): 330", "Frances (2004): 464",
                      "Ike (2008): 335", "Ivan (2004): 317", "Ivan (2004): 91")


l <- length(variables)

pdf(file = "figures/severityagreement.pdf", width = 10, height = 9)
par(mfrow = c(l, l), cex.axis = 1.3, mar = c(3, 4, 2, 1.5) + 0.1, oma = c(0,
                                                                          2.2, 2, 0))

for (row in seq_len(l)) {

        for (col in seq_len(l)) {

                if (row == col) {
                        .plotMarginalCor(dataset[[variables[row]]], xlim = c(0, max_x),
                                         year_lab = paste(year_labs[row, ], collapse = " to "),
                                         median_lab = paste("Median:", median_labs[row]),
                                         iqr_lab = paste("IQR:", iqr_labs[row]),
                                         worst_storm_lab = worst_storm_labs[row])
                }
                if (col > row) {
                        .plotScatter(dataset[[variables[col]]], dataset[[variables[row]]],
                                     max_val = max_x, cexPoints = 0.7)
                }
                if (col < row) {
                        year_lab_pair <- c(max(year_labs[c(col, row), "year_min"]),
                                           min(year_labs[c(col, row), "year_max"]))
                        year_lab_pair <- paste(year_lab_pair, collapse = " to ")
                        if (l < 7) {
                                .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]],
                                              cexCI = 1.2, pearson = FALSE, kendallsTauB = TRUE,
                                              spearman = TRUE,
                                              cexText = 1.2, year_lab = year_lab_pair)
                        }
                        if (l >= 7) {
                                .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]],
                                              cexCI = 1.2, pearson = FALSE, kendallsTauB = TRUE)
                        }
                }
        }
}

textpos <- seq(1/(l * 2), (l * 2 - 1)/(l * 2), 2/(l * 2))
for (t in seq_along(textpos)) {
        mtext(text = variables[t], side = 3, outer = TRUE, at = textpos[t],
              cex = 1.2, line = -0.8)
        mtext(text = variables[t], side = 2, outer = TRUE, at = rev(textpos)[t],
              cex = 1.2, line = -0.1)
}
dev.off()
