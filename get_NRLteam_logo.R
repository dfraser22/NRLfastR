# install.packages(c('devtools','rsvg','roxygen2','grImport2',
#                    'grid','gridSVG','tidyverse','ggplot2',
#                    'broom','rio','Rcurl','readxl','httr',
#                    'plotly','corrplot','ggcorrplot','stringr',
#                    'vip','caTools','rstanarm','tidymodels',
#                    'devtools'))
# install.packages('renv')
devtools::install_github("klutometis/roxygen")
library(renv)
library(broom)
library(rio)
library(RCurl)
library(readxl)
library(httr)
library(tidyverse)
library(plotly)
library(corrplot)
library(ggcorrplot)
library(stringr)
library(vip)
library(caTools)
library(rstanarm)
library(tidymodels)
library(devtools)
library(rsvg)
library(roxygen2)
library(grImport2)
library(grid)
library(gridSVG)

# renv::snapshot()
# setwd("C:/Users/dan.fraser/Documents")
# create("NRLfastR")

#' Get NRL Team Logo URL
#'
#' @param team_name A character string specifying the NRL team name.
#' @return A character string containing the URL of the team logo.
#' @examples
#' get_NRLteam_logo('Broncos')
#' @export
#'
get_NRLteam_logo <- function(team_name) {
  NRL_logos <- data.frame(
    team = c("Broncos","Bulldogs","Cowboys","Dolphins","Dragons","Eels","Knights","Panthers","Rabbitohs",
             "Raiders","Roosters","SeaEagles","Sharks","Storm","Titans","Warriors","West Tigers"),
    url = c("https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Broncos.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Bulldogs.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Cowboys.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Dolphins.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Dragons.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Eels.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Knights.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Panthers.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Rabbitohs.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Raiders.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Roosters.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/SeaEagles.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Sharks.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Storm.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Titans.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Warriors.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/WestTigers.png"),
    stringsAsFactors = FALSE
  )

  logo_url <- NRL_logos$url[match(team_name, NRL_logos$team)]

  if (is.na(logo_url)) {
    stop("Team not found.")
  }

  return(logo_url)
}

NRL_logos
# get_NRLteam_logo('Broncos')
