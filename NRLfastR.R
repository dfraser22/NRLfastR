library(devtools)
library(rsvg)
# devtools::install_github("klutometis/roxygen")
library(roxygen2)

library(grImport2)
library(grid)
library(gridSVG)
library(renv)

# rm(list = ls())
# ls()
# devtools::document()
# renv::snapshot()

setwd("C:/dan.fraser/Users/Documents")
# create("NRLfastR")

getwd()

# usethis::create_package("C:/Users/dan.fraser/Documents/NRLfastR")

# devtools::document()
# get_NRLteam_logo <- function(team_name) {
#   NRL_logos <- data.frame(
#     team = c("Broncos","Bulldogs","Cowboys","Dolphins","Dragons","Eels","Knights","Panthers","Rabbitohs",
#              "Raiders","Roosters","SeaEagles","Sharks","Storm","Titans","Warriors","West Tigers"),
#     url = c("https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Broncos.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Bulldogs.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Cowboys.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Dolphins.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Dragons.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Eels.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Knights.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Panthers.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Rabbitohs.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Raiders.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Roosters.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/SeaEagles.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Sharks.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Storm.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Titans.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Warriors.png",
#             "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/WestTigers.png"),
#             stringsAsFactors = FALSE
#   )
#
#   logo_url <- logos$url[match(team_name, logos$team)]
#
#   if (is.na(logo_url)) {
#     stop("Team not found.")
#   }
#
#   return(logo_url)
# }

# save(NRL_logos, file = "data/NRL_logos.rda")
