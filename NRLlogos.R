#' NRL Logos Data Frame
#'
#' A dataframe containing NRL team names and their logo URLs.
#'
#' @format A data frame with 17 rows and 2 columns:
#' \describe{
#'   \item{team}{Team names}
#'   \item{url}{URLs of team logos}
#' }
#' @source \url{https://github.com/dfraser22/NFLfastR}
#' @export
"NRL_logos"

NRL_logos <- data.frame(
  team = c("Broncos", "Bulldogs", "Cowboys", "Dolphins", "Dragons", "Eels",
           "Knights", "Panthers", "Rabbitohs", "Raiders", "Roosters",
           "SeaEagles", "Sharks", "Storm", "Titans", "Warriors", "West Tigers"),
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

# unlink("man/NRL_logos.Rd")
# devtools::document()
# devtools::clean_dll()

# devtools::check()

devtools::document()
