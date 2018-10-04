## Map of hurricane tracks
library(ggplot2)
library(hurricaneexposuredata)
library(hurricaneexposure)

data(closest_dist)
storms <- unique(closest_dist$storm_id)
storms <- storms[gsub("*.+-", "", storms) <= 2015 &
                         gsub("*.+-", "", storms) >= 1988]
a <- map_tracks(storms, plot_points = FALSE, alpha = 0.3, color = "darkcyan")
# Add storms whose names were retired
# Source: http://www.nhc.noaa.gov/aboutnames_history.shtml
fig <- map_tracks(c("Hugo-1989", "Bob-1991", "Andrew-1992",
             "Opal-1995", "Fran-1996", "Georges-1998", "Mitch-1998",
             "Floyd-1999", "Allison-2001", "Michelle-2001",
             "Isidore-2002", "Lili-2002", "Isabel-2003",
             "Charley-2004", "Frances-2004",
             "Ivan-2004", "Jeanne-2004",
             "Dennis-2005", "Katrina-2005", "Rita-2005",
             "Wilma-2005", "Noel-2007",
             "Gustav-2008", "Ike-2008", "Paloma-2008",
             "Irene-2011", "Sandy-2012"),
           plot_object = a, plot_points = FALSE, color = "darkcyan",
           padding = 0) +
        theme_void() +
        theme(panel.background = element_rect(fill = "white")) +
        coord_map()

pdf(file = "figures/hurrtracks.pdf", width = 4.8, height = 4.0)
print(fig)
dev.off()

tiff(filename = "figures/hurrtracks.tiff", width = 480, height = 400)
print(fig)
dev.off()
