# Script to clean up EmPower data, so we can match it up by FIPS with the hurricane
# exposure data.

# setwd("writing/pnas_draft/")
library(readxl)
library(readr)
library(dplyr)
library(maps)
library(tidyr)
library(stringr)
library(choroplethr)


county_fips <- read_csv("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
                        col_names = c("abb", "state_fips", "county_fips",
                                      "county_name", "h")) %>%
        select(-h) %>%
        unite(fips, state_fips, county_fips, sep = "") %>%
        mutate(county_name = str_replace(str_to_lower(county_name), " parish| county", ""),
               abb = str_to_lower(abb),
               fips = as.integer(fips)) %>%
        unite(county_name, county_name, abb, sep = ",")

extra_fips <- data_frame(county_name = c("de soto,fl",
                                         "saint johns,fl",
                                         "saint lucie,fl",
                                         "radford,va",
                                         "salem,va",
                                         "prince georges,md",
                                         "queen annes,md",
                                         "saint marys,md",
                                         "saint lawrence,ny",
                                         "saint clair,mi",
                                         "saint joseph,mi",
                                         "saint croix,wi",
                                         "dewitt,il",
                                         "la salle,il",
                                         "saint clair,il",
                                         "de kalb,in",
                                         "la porte,in",
                                         "st joseph,in",
                                         "saint charles,mo",
                                         "saint clair,mo",
                                         "saint francois,mo",
                                         "saint louis,mo",
                                         "saint louis city,mo",
                                         "sainte genevieve,mo",
                                         "obrien,ia",
                                         "de witt,tx",
                                         "saint francis,ar",
                                         "saint bernard,la",
                                         "saint charles,la",
                                         "saint helena,la",
                                         "saint james,la",
                                         "saint landry,la",
                                         "saint martin,la",
                                         "saint mary,la",
                                         "saint tammany,la",
                                         "st john the baptist,la",
                                         "de kalb,al",
                                         "saint clair,al",
                                         "bedford,va"),
                         fips = c("12027",
                                  "12109",
                                  "12111",
                                  "51750",
                                  "51775",
                                  "24033",
                                  "24035",
                                  "24037",
                                  "36089",
                                  "26147",
                                  "26149",
                                  "55109",
                                  "17039",
                                  "17099",
                                  "17163",
                                  "18033",
                                  "18091",
                                  "18141",
                                  "29183",
                                  "29185",
                                  "29187",
                                  "29189",
                                  "29510",
                                  "29186",
                                  "19141",
                                  "48123",
                                  "05123",
                                  "22087",
                                  "22089",
                                  "22091",
                                  "22093",
                                  "22097",
                                  "22099",
                                  "22101",
                                  "22103",
                                  "22095",
                                  "01049",
                                  "01115",
                                  "51515")) %>%
        mutate(fips = as.integer(fips))

county_fips <- bind_rows(county_fips, extra_fips)

# EmPower data as of July 10, 2017. https://empowermap.hhs.gov/
medi <- read_xlsx("empower_data.xlsx") %>%
        dplyr::rename(county = `Geographic Area`,
               beneficiaries = Beneficiaries,
               elec_dependent = `Electricity-Dependent Beneficiaries`,
               state = State) %>%
        mutate_at(c("county", "state"), str_to_lower) %>%
        unite(county_name, county, state, sep = ",", remove = FALSE) %>%
        left_join(county_fips, by = "county_name" )

medi %>%
        filter(!is.na(fips)) %>%
        select(fips, elec_dependent) %>%
        dplyr::rename(region = fips, value = elec_dependent) %>%
        county_choropleth(state_zoom = c("florida", "georgia", "south carolina",
                                         "north carolina", "virginia",
                                         "maryland", "delaware", "west virginia",
                                         "pennsylvania", "new jersey", "new york",
                                         "connecticut", "vermont", "new hampshire",
                                         "district of columbia", "maine",
                                         "massachusetts", "alabama", "mississippi",
                                         "louisiana", "texas", "oklahoma",
                                         "arkansas", "tennessee", "kentucky",
                                         "illinois", "indiana", "ohio", "missouri",
                                         "wisconsin", "iowa", "michigan", "kansas"))

save(medi, file = "clean_empower.RData")
