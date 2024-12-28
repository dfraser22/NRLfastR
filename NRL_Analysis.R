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
library(DT)
library(readr)

# renv::snapshot()
# library(devtools)
# uninstall("NRLfastR")
# remove.packages('NRLfastR')
# install("C:/Users/dan.fraser/Documents/NRLfastR", force = TRUE)
# library(NRLfastR)

# Kick % correlation to winning
# Way of analysing risk taking (errors) vs line breaks
# Percentage of try assists that are kicks vs passes
# tree based models
# Percentage of post contact metres that make up total metres
# Play the ball speed's relationship to pre-contact metres (total metres - post contact)?
# Research vip function

# What wins NRL games? +/- on run metres. Total runs per game is normally distributed.
# Can I set the data up as Variables scored ----> Variables conceded ---->
# Tackle busts as indicator of line breaks?

# Loading data/cleaning ####

# NRL_seasons <- read_xlsx('C:/Users/dan.fraser/PycharmProjects/pythonProject/nrl_all_match_stats_2021_2024_wide.xlsx')
NRL_seasons_Original <- read_xlsx("C:/Users/dan.fraser/Downloads/nrl_all_match_stats_2021_2024_wide.xlsx")

NRL_Endof2024 <- read_xlsx("C:/Users/dan.fraser/Downloads/nrl_all_match_stats_2024_wide.xlsx")

NRL_specificurls <- read_xlsx("C:/Users/dan.fraser/Downloads/nrl_match_stats_specific_urls.xlsx") %>% # IDs 2024-8-2 & 2023-25-5
  mutate(ID = c('2023-25-4','2023-25-4','2024-8-2','2024-8-2'))

NRL_2021_2024 <- rbind(NRL_seasons_Original,NRL_Endof2024,NRL_specificurls)

NRL_seasons <- NRL_2021_2024 %>% rename("Set Restarts Awarded" = Awarded) %>%
  mutate(Possession = as.numeric(gsub("%", "",Possession)),
                                      Territory = as.numeric(gsub("%", "",Territory))) %>%
  separate(`Completion Rate`, into = c("numerator", "denominator"), sep = "/", convert = TRUE) %>%
  mutate(`Completion Rate %` = round(numerator / denominator,2) * 100,
          Average_Metres_Per_Run = round(as.numeric(`Run Metres`)/as.numeric(Runs),2)) %>%
  select(-numerator, -denominator) %>%
  mutate(across(c("Scores","Possession","Territory","Runs",
                  "Run Metres","Dummy Half Runs","Tackle Busts","Post Contact Metres","Offloads","Linebreaks",
                  "20m Restarts","Tackled In Opp 20","In Goal Escapes","Set Restarts Awarded","Tackles","Missed Tackles",
                  "Correct","Incorrect","Errors","Penalties Conceded","Kicks","Kick Metres",
                  "40/20s","20/40s","Attacking Kicks","Drop Outs","Forced Drop Outs","Kicks Dead",
                  "Completion Rate %",'Average_Metres_Per_Run'), ~ as.numeric(.))) %>%
  mutate(Season = case_when(
     startsWith(ID, "2024") ~ "2024",
     startsWith(ID, "2023") ~ "2023",
     startsWith(ID, "2022") ~ "2022",
     startsWith(ID, "2021") ~ "2021")) %>%
  mutate('Game Type' = case_when(ID %in% c('2024-31-1','2024-30-1','2024-30-2',
                                           "2024-29-1","2024-29-2","2024-28-1",
                                           "2024-28-2","2024-28-3","2024-28-4",
                                           '2023-31-1','2023-30-2','2023-30-1',
                                           "2023-29-1","2023-29-2","2023-28-1",
                                           "2023-28-2","2023-28-3","2023-28-4",
                                           "2022-26-1","2022-26-2","2022-26-3",
                                           "2022-26-4","2022-27-1","2022-27-2",
                                           "2022-28-1","2022-28-2","2022-29-1",
                                           "2021-26-1","2021-26-2","2021-26-3",
                                           "2021-26-4","2021-27-1","2021-27-2",
                                           "2021-28-1","2021-28-2","2021-29-1") ~ 'Playoff',
                                 TRUE ~ 'Regular Season'))

even_indices <- seq(2, nrow(NRL_seasons), by = 2)
odd_indices <- seq(1, nrow(NRL_seasons), by = 2)

# Subset the original dataframe using the even indices
NRL_away_teams <- NRL_seasons[even_indices, ]
NRL_home_teams <- NRL_seasons[odd_indices, ]

Fast_NRLr <- inner_join(NRL_home_teams,NRL_away_teams,by=c('ID','Season','Game Type'))
view(Fast_NRLr)

New_cols <- c("ID","Home Team","Home Score",
"Home Possession","Home Territory","Home Runs",
"Home Run Metres","Home Dummy Half Runs","Home Tackle Busts",
"Home Post Contact Metres", "Home Offloads", "Home Linebreaks",
"Home 20m Restarts","Home Tackled In Opp 20","Home In Goal Escapes",
"Home Awarded Set Restarts","Home Tackles","Home Missed Tackles",
"Home Correct Reviews","Home Incorrect Reviews","Home Errors",
"Home Penalties Conceded",  "Home Kicks","Home Kick Metres",
"Home 40/20s","Home 20/40s","Home Attacking Kicks",
"Home Drop Outs","Home Forced Drop Outs","Home Kicks Dead",
"Home Completion Rate","Home Average Metres Per Run",
"Season",'Game Type', "Away Team","Away Score",
"Away Possession","Away Territory","Away Runs",
"Away Run Metres","Away Dummy Half Runs","Away Tackle Busts",
"Away Post Contact Metres","Away Offloads","Away Linebreaks",
"Away 20m Restarts","Away Tackled In Opp 20","Away In Goal Escapes",
"Away Awarded Set Restarts", "Away Tackles","Away Missed Tackles",
"Away Correct Reviews","Away Incorrect Reviews","Away Errors",
"Away Penalties Conceded","Away Kicks","Away Kick Metres",
"Away 40/20s","Away 20/40s","Away Attacking Kicks",
"Away Drop Outs","Away Forced Drop Outs","Away Kicks Dead",
"Away Completion Rate","Away Average Metres Per Run") # "Game Type 1")

colnames(Fast_NRLr) <- New_cols
view(Fast_NRLr)

Fast_NRLr <- Fast_NRLr %>%
  relocate("ID","Home Team","Home Score","Away Team","Away Score",
"Home Possession","Home Territory",
"Away Possession","Away Territory",
"Home Runs","Home Run Metres","Away Runs",
"Away Run Metres","Home Dummy Half Runs","Away Dummy Half Runs",
"Home Tackle Busts","Away Tackle Busts",
"Home Post Contact Metres","Away Post Contact Metres","Home Average Metres Per Run",
"Away Average Metres Per Run","Home Offloads","Away Offloads",
"Home Linebreaks","Away Linebreaks",
"Home 20m Restarts","Away 20m Restarts","Home Completion Rate",
"Away Completion Rate","Home Tackled In Opp 20","Away Tackled In Opp 20",
"Home In Goal Escapes","Away In Goal Escapes","Home Awarded Set Restarts","Away Awarded Set Restarts",
"Home Tackles", "Away Tackles","Home Missed Tackles",
"Away Missed Tackles","Home Correct Reviews","Home Incorrect Reviews",
"Away Correct Reviews","Away Incorrect Reviews",
"Home Errors","Away Errors","Home Penalties Conceded","Away Penalties Conceded",
"Home Kicks","Away Kicks",
"Home Kick Metres","Away Kick Metres","Home 40/20s","Home 20/40s",
"Away 40/20s","Away 20/40s",
"Home Attacking Kicks","Away Attacking Kicks","Home Drop Outs",
"Away Drop Outs","Home Forced Drop Outs","Away Forced Drop Outs",
"Home Kicks Dead","Away Kicks Dead","Season") %>%
mutate(across(c("Home Score","Away Score","Home Possession","Home Territory",
                "Away Possession","Away Territory","Home Completion Rate",
                "Away Completion Rate","Home Runs","Home Run Metres","Away Runs",
                "Away Run Metres","Home Dummy Half Runs","Away Dummy Half Runs",
                "Home Tackle Busts","Away Tackle Busts","Home Post Contact Metres",
                "Away Post Contact Metres","Home Offloads","Away Offloads",
                "Home Linebreaks","Away Linebreaks",
                "Home 20m Restarts","Away 20m Restarts","Home Tackled In Opp 20",
                "Away Tackled In Opp 20",
                "Home In Goal Escapes","Away In Goal Escapes","Home Awarded Set Restarts",
                "Away Awarded Set Restarts",
                "Home Tackles", "Away Tackles","Home Missed Tackles",
                "Away Missed Tackles","Home Correct Reviews","Home Incorrect Reviews",
                "Away Correct Reviews","Away Incorrect Reviews",
                "Home Errors","Away Errors","Home Penalties Conceded","Away Penalties Conceded",
                "Home Kicks","Away Kicks",
                "Home Kick Metres","Away Kick Metres","Home 40/20s","Home 20/40s",
                "Away 40/20s","Away 20/40s",
                "Home Attacking Kicks","Away Attacking Kicks","Home Drop Outs",
                "Away Drop Outs","Home Forced Drop Outs","Away Forced Drop Outs",
                "Home Kicks Dead","Away Kicks Dead",
                "Home Average Metres Per Run","Away Average Metres Per Run"),
                 ~ as.numeric(.))) %>%
  mutate('Home result' = case_when(as.numeric(as.character(`Home Score`)) > `Away Score` ~ 'W',
                                   `Home Score` == `Away Score` ~ 'D',
                                   `Away Score` > `Home Score` ~ 'L'),
         'Away result' = case_when(`Home Score` > `Away Score` ~ 'L',
                                   `Home Score` == `Away Score` ~ 'D',
                                   `Away Score` > `Home Score` ~ 'W'),
         'Home Completion Difference' = `Home Completion Rate` - `Away Completion Rate`,
         'Away Completion Difference' = `Away Completion Rate` - `Home Completion Rate`,
         'Home Score Against' = `Away Score`,
         'Away Score Against' = `Home Score`) %>%
  mutate('Total runs' = `Home Runs` + `Away Runs`)
view(Fast_NRLr)

# Home and Away splits
Away_Fast_NRLr <- Fast_NRLr %>% select(ID,Season,`Away Team`,`Away Score`,`Away result`,
                                       `Away Possession`,`Away Territory`,`Away Runs`,
                                       `Away Run Metres`,`Away Dummy Half Runs`,`Away Tackle Busts`,
                                       `Away Post Contact Metres`,`Away Offloads`,`Away Linebreaks`,
                                       `Away 20m Restarts`,`Away Completion Rate`,`Away Tackled In Opp 20`,
                                       `Away In Goal Escapes`,`Away Awarded Set Restarts`,`Away Tackles`,
                                       `Away Missed Tackles`,`Away Correct Reviews`,`Away Incorrect Reviews`,
                                       `Away Errors`,`Away Penalties Conceded`,`Away Kicks`,
                                       `Away Kick Metres`,`Away 40/20s`,`Away 20/40s`,
                                       `Away Attacking Kicks`,`Away Drop Outs`,`Away Forced Drop Outs`,
                                       `Away Forced Drop Outs`,`Away Kicks Dead`,`Away Completion Difference`,
                                       `Away Score Against`,`Away Average Metres Per Run`,`Home Average Metres Per Run`,
                                       `Home Linebreaks`, `Home Run Metres`,`Total runs`) %>%
  mutate('Average run difference' = `Away Average Metres Per Run` - `Home Average Metres Per Run`,
         'Line break rate' = round(`Away Linebreaks`/`Away Runs`,2),
         'Line break differential' = `Away Linebreaks` - `Home Linebreaks`,
         'Run metre differential' = `Away Run Metres` - `Home Run Metres`)

Home_Fast_NRLr <- Fast_NRLr %>% select(ID,Season,`Home Team`,`Home Score`,`Home result`,
                                       `Home Possession`,`Home Territory`,`Home Runs`,
                                       `Home Run Metres`,`Home Dummy Half Runs`,`Home Tackle Busts`,
                                       `Home Post Contact Metres`,`Home Offloads`,`Home Linebreaks`,
                                       `Home 20m Restarts`,`Home Completion Rate`,`Home Tackled In Opp 20`,
                                       `Home In Goal Escapes`,`Home Awarded Set Restarts`,`Home Tackles`,
                                       `Home Missed Tackles`,`Home Correct Reviews`,`Home Incorrect Reviews`,
                                       `Home Errors`,`Home Penalties Conceded`,`Home Kicks`,
                                       `Home Kick Metres`,`Home 40/20s`,`Home 20/40s`,
                                       `Home Attacking Kicks`,`Home Drop Outs`,`Home Forced Drop Outs`,
                                       `Home Forced Drop Outs`,`Home Kicks Dead`,`Home Completion Difference`,
                                       `Home Score Against`,`Home Average Metres Per Run`,
                                       `Away Average Metres Per Run`,`Away Linebreaks`,
                                       `Away Run Metres`,`Total runs`) %>%
  mutate('Average run difference' = `Home Average Metres Per Run` - `Away Average Metres Per Run`,
         'Line break rate' = round(`Home Linebreaks`/`Home Runs`,2),
         'Line break differential' = `Home Linebreaks` - `Away Linebreaks`,
         'Run metre differential' = `Home Run Metres` - `Away Run Metres`)

Full_colnames <- c("ID","Season","Team","Scores","Result","Possession","Territory","Runs",
                   "Run Metres","Dummy Half Runs","Tackle Busts","Post Contact Metres",
                   "Offloads","Linebreaks","20m Restarts","Completion Rate",
                   "Tackled In Opp 20","In Goal Escapes",
                   "Awarded","Tackles","Missed Tackles","Correct","Incorrect","Errors","Penalties Conceded",
                   "Kicks","Kick Metres","40/20s","20/40s","Attacking Kicks","Drop Outs",
                   "Forced Drop Outs","Kicks Dead","Completion Difference","Score Against",
                   "Average Metres Per Run","Average Metres Per Run Conceded",
                   'Line breaks conceded','Run metres conceded', 'Total runs','Average run difference',
                   'Line break rate','Line break differential','Run metre differential')

colnames(Home_Fast_NRLr) <- Full_colnames
colnames(Away_Fast_NRLr) <- Full_colnames

# Creating Full_Fast_NRLr ####
Full_Fast_NRLr <- rbind(Home_Fast_NRLr,Away_Fast_NRLr) %>%
  mutate(Team = str_to_title(Team))

view(Full_Fast_NRLr)

Full_Fast_NRLr <- Full_Fast_NRLr %>%
  mutate('Game Type' = case_when(ID %in% c('2024-31-1','2024-30-2','2024-30-1',
                                           "2024-28-1","2024-28-2","2024-28-3",
                                           "2024-28-4","2024-29-1","2024-29-2",
                                           '2023-31-1','2023-30-2','2023-30-1',
                                           "2023-28-1","2023-28-2","2023-28-3",
                                           "2023-28-4","2023-29-1","2023-29-2",
                                           "2022-26-1","2022-26-2","2022-26-3",
                                           "2022-26-4","2022-27-1","2022-27-2",
                                           "2022-28-1","2022-28-2","2022-29-1",
                                           "2021-26-1","2021-26-2","2021-26-3",
                                           "2021-26-4","2021-27-1","2021-27-2",
                                           "2021-28-1","2021-28-2","2021-29-1") ~ "Playoff",
                                 TRUE ~ 'Regular Season'))

# NRL ladders 2015-2024 ####
NRL_ladders_1524 <- "NRLladders2015-2024.xlsx"

tab_NRL <- excel_sheets(path = NRL_ladders_1524)

NRL_seasons <- lapply(tab_NRL, function(x) read_excel(path = NRL_ladders_1524, sheet = x))
View(NRL_seasons)

NRL_seasons <- lapply(NRL_seasons, function(df)
  df %>% select(1:9)  # Replace with your desired column names
)

names(NRL_seasons) <- tab_NRL

NRL_MergedSeasons <- do.call(rbind, NRL_seasons[order(names(NRL_seasons))]) %>%
  mutate(Season = c(rep(paste0("2015"), times = 16),
                    rep(paste0("2016"), times = 16),
                    rep(paste0("2017"), times = 16),
                    rep(paste0("2018"), times = 16),
                    rep(paste0("2019"), times = 16),
                    rep(paste0("2020"), times = 16),
                    rep(paste0("2021"), times = 16),
                    rep(paste0("2022"), times = 16),
                    rep(paste0("2023"), times = 17),
                    rep(paste0("2024"), times = 17)))
view(NRL_MergedSeasons)

# Player Stats ####
# Check game tally for each season using url, check finals games
NRL_PlayerStats <-
  read_excel("C:/Users/dan.fraser/PycharmProjects/pythonProject/nrl_player_stats_with_urls.xlsx") %>%
  select(-1,-5,-7,-16,-18,-22,-35,-41,-48,-58,-68)
View(NRL_PlayerStats)

NRL_PlayerStats_Finals_Original <-
  read_excel("C:/Users/dan.fraser/PycharmProjects/pythonProject/nrl_finals_player_stats_with_urls.xlsx") %>%
  select(-1,-5,-7,-16,-18,-22,-35,-41,-48,-58,-68)
colnames(NRL_PlayerStats_Finals_Original)

NRL_PlayerStats_2024_Finals <-
  read_csv("C:/Users/dan.fraser/Downloads/nrl_player_stats_2024_rounds_24_to_27.csv",
           locale = locale(encoding = "UTF-8"))
View(NRL_PlayerStats_2024_Finals)

unique(NRL_PlayerStats_2024_Finals$URL)

NRL_PlayerStats_2024_Finals$Player <-
  iconv(NRL_PlayerStats_2024_Finals$Player, from = "latin1", to = "UTF-8", sub = "byte")

# NRL_PlayerStats_2024_Finals$Player <- gsub(" ", "", NRL_PlayerStats_2024_Finals$Player)
# NRL_PlayerStats_2024_Finals$Player <- gsub("\\s+", "", NRL_PlayerStats_2024_Finals$Player)
NRL_PlayerStats_2024_Finals$Player <- str_replace_all(NRL_PlayerStats_2024_Finals$Player, "\\s+", "")

unique(NRL_PlayerStats_2024_Finals$Player)
Player_ColNames <- c("Player","Number","Position","MinsPlayed",
                     "Points","Tries","Conversions","ConversionAttempts",
                     "PenaltyGoals","GoalConversionRate","1PointFieldGoals","2PointFieldGoals",
                     "TotalPoints","AllRuns","AllRunMetres","KickReturnMetres",
                     "PostContactMetres","LineBreaks","LineBreakAssists","TryAssists",
                     "LineEngagedRuns","TackleBreaks","HitUps","PlayTheBall",
                     "AveragePlayTheBallSpeed","DummyHalfRuns","DummyHalfRunMetres","OneonOneSteal",
                     "Offloads","DummyPasses","Passes","Receipts",
                     "PassesToRunRatio","TackleEfficiency","TacklesMade","MissedTackles",
                     "IneffectiveTackles","Intercepts","KicksDefused","Kicks",
                     "KickingMetres","ForcedDropOuts","BombKicks","Grubbers",
                     "40/20","20/40","CrossFieldKicks","KickedDead",
                     "Errors","HandlingErrors","OneonOneLost","Penalties",
                     "RuckInfringements","Inside10Metres","OnReport",
                     "SinBins","SendOffs","StintOne","StintTwo","Team","URL")

colnames(NRL_PlayerStats) <- Player_ColNames
colnames(NRL_PlayerStats_Finals_Original) <- Player_ColNames
colnames(NRL_PlayerStats_2024_Finals) <- Player_ColNames

NRL_PlayerStats_JoinedOriginal <-
  rbind(NRL_PlayerStats,NRL_PlayerStats_Finals_Original,NRL_PlayerStats_2024_Finals)

unique(NRL_PlayerStats_JoinedOriginal$Player)

team_mapping <- c(
  "rabbitohs" = "Rabbitohs",
  "bulldogs" = "Bulldogs",
  "eels" = "Eels",
  "titans" = "Titans",
  "sea-eagles" = "Sea Eagles",
  "cowboys" = "Cowboys",
  "wests-tigers" = "Wests Tigers",
  "sharks" = "Sharks",
  "storm" = "Storm",
  "knights" = "Knights",
  "broncos" = "Broncos",
  "panthers" = "Panthers",
  "dragons" = "Dragons",
  "roosters" = "Roosters",
  "raiders" = "Raiders",
  "warriors" = "Warriors",
  "dolphins" = "Dolphins"
)

# "IzaacTu\u0092itupou Thompson" "mesTedesco" "Gordon ChanKumTong"  "Tevita PangaiJunior"
# "De LaSalleVa'a"   "Tallyn DaSilva"    "Te  MaireMartin" "Joseph- AukusoSuaalii"
NRL_PlayerStats_Joined <- NRL_PlayerStats_JoinedOriginal %>%
  mutate(Player = case_when(
    Player %in% c("TeMaireMartin", "AJBrimson",
                  "Joseph-AukusoSua'ali'i", "JJCollins",
                  "TukuHauTapuha", "mesTedesco",
                  "RaymondTuaimalo Vaega","IzaacTu’itupou Thompson",
                  "GordonChan Kum Tong","IzaacTu\u0092itupouThompson",
                  "Gordon ChanKumTong", "Tevita PangaiJunior",
                  "De LaSalleVa'a", "Tallyn DaSilva",
                  "Te  MaireMartin", "Joseph- AukusoSuaalii") ~ Player,
    TRUE ~ str_replace(Player, "(?<!^)([A-Z])", " \\1")
  )) %>%
  mutate(Player = str_replace(Player, "TeMaireMartin", "Te Maire Martin"),
         Player = str_replace(Player, "AJBrimson", "AJ Brimson"),
         Player = str_replace(Player, "Joseph-AukusoSua'ali'i", "Joseph-Aukuso Sua'ali'i"),
         Player = str_replace(Player, "Joseph- AukusoSuaalii", "Joseph-Aukuso Sua'ali'i"),
         Player = str_replace(Player, "JJCollins", "JJ Collins"),
         Player = str_replace(Player, "TukuHauTapuha", "Tuku Hau Tapuha"),
         Player = str_replace(Player, "mesTedesco", "James Tedesco"),
         Player = str_replace(Player, "RaymondTuaimalo Vaega", "Raymond Tuaimalo Vaega"),
         Player = str_replace(Player, "IzaacTu’itupou Thompson", "Izaac Tu’itupou Thompson"),
         Player = str_replace(Player, "GordonChan Kum Tong", "Gordon Chan Kum Tong"),
         Player = str_replace(Player, "Gordon ChanKumTong", "Gordon Chan Kum Tong"),
         Player = str_replace(Player, "IzaacTu\u0092itupouThompson", "Izaac Tu’itupou Thompson"),
         Player = str_replace(Player, "IzaacTu\u0092itupouThompson", "Izaac Tu’itupou Thompson"),
         Player = str_replace(Player, "Tallyn DaSilva", "Tallyn Da Silva"),
         Player = str_replace(Player, "Te  MaireMartin", "Te Maire Martin"),
         Player = str_replace(Player, "Tevita PangaiJunior", "Tevita Pangai Junior")
         ) %>%
  mutate(Season = case_when(str_detect(URL,'/2021/') ~ '2021',
                            str_detect(URL,'/2022/') ~ '2022',
                            str_detect(URL,'/2023/') ~ '2023',
                            str_detect(URL,'/2024/') ~ '2024')) %>%
  mutate(Game_Type = case_when(str_detect(URL,'finals') ~ 'Playoffs',
                               str_detect(URL,'grand') ~ 'Grand final',
                               TRUE ~ 'Regular season'),
         Opponent = str_extract(URL, "(?<=-v-)[^/]+(?=/)")) %>%
  mutate(Opponent = recode(Opponent, !!!team_mapping)) %>%
  mutate(GoalConversionRate = str_remove_all(GoalConversionRate,"%"),
         TackleEfficiency = str_remove_all(TackleEfficiency,"%"),
         AveragePlayTheBallSpeed = str_remove_all(AveragePlayTheBallSpeed,"s")) %>%
  mutate(across(
    c(Points, Tries, Conversions, ConversionAttempts, PenaltyGoals,
      GoalConversionRate, `1PointFieldGoals`, `2PointFieldGoals`, TotalPoints,
      AllRuns, AllRunMetres, KickReturnMetres, PostContactMetres,
      LineBreaks, LineBreakAssists, TryAssists, LineEngagedRuns,
      TackleBreaks, HitUps, PlayTheBall, AveragePlayTheBallSpeed,
      DummyHalfRuns, DummyHalfRunMetres, OneonOneSteal, Offloads,
      DummyPasses, Passes, Receipts, PassesToRunRatio, TackleEfficiency,
      TacklesMade, MissedTackles,IneffectiveTackles, Intercepts,
      KicksDefused, Kicks, KickingMetres,
      ForcedDropOuts, BombKicks, Grubbers, `40/20`, `20/40`,
      CrossFieldKicks, KickedDead, Errors, HandlingErrors, OneonOneLost,
      Penalties, RuckInfringements, Inside10Metres, OnReport, SinBins, SendOffs),
    ~ as.numeric(str_replace_all(., "-", "0"))
  )) %>%
  mutate(TacklesMade = as.numeric(TacklesMade),
         MissedTackles = as.numeric(MissedTackles),
         IneffectiveTackles = as.numeric(IneffectiveTackles))
str(NRL_PlayerStats_Joined)

unique(NRL_PlayerStats_Joined$Team)
## Seasons & Rounds ####
# NRL_PlayerStats_Joined <- NRL_PlayerStats_Joined %>%
#   mutate(Season = str_extract(URL, "(?<=/nrl-premiership/)[0-9]{4}"))

unique(NRL_PlayerStats_Joined$Player)

unique(NRL_PlayerStats_Joined$URL)
NRL_PlayerStats_Joined %>%
  group_by(URL) %>%
  count() %>%
  print(n = 828)

ids_34x_32 <- c("2021-1-1", "2021-1-2", "2021-1-3","2021-1-4", "2021-1-5", "2021-1-6", "2021-1-7", "2021-1-8",
                "2021-2-1","2021-2-2", "2021-2-3", "2021-2-4", "2021-2-5","2021-2-6", "2021-2-7", "2021-2-8",
                "2021-3-1","2021-3-2", "2021-3-3", "2021-3-4", "2021-3-5", "2021-3-6", "2021-3-7", "2021-3-8",
                "2021-4-1","2021-4-2", "2021-4-3", "2021-4-4", "2021-4-5", "2021-4-6", "2021-4-7", "2021-4-8")
# 32*34 + 796*36
ids_36_796 <- c("2021-5-1","2021-5-2", "2021-5-3", "2021-5-4", "2021-5-5", "2021-5-6", "2021-5-7", "2021-5-8",
                "2021-6-1","2021-6-2", "2021-6-3", "2021-6-4", "2021-6-5", "2021-6-6", "2021-6-7","2021-6-8",
                "2021-7-1","2021-7-2", "2021-7-3", "2021-7-4", "2021-7-5", "2021-7-6", "2021-7-7","2021-7-8",
                "2021-8-1","2021-8-2", "2021-8-3", "2021-8-4", "2021-8-5", "2021-8-6", "2021-8-7","2021-8-8",
                "2021-9-1","2021-9-2", "2021-9-3", "2021-9-4", "2021-9-5", "2021-9-6", "2021-9-7","2021-9-8",
                "2021-10-1","2021-10-2","2021-10-3","2021-10-4","2021-10-5","2021-10-6","2021-10-7","2021-10-8",
                "2021-11-1","2021-11-2","2021-11-3","2021-11-4","2021-11-5","2021-11-6","2021-11-7","2021-11-8",
                "2021-12-1","2021-12-2","2021-12-3","2021-12-4","2021-12-5","2021-12-6","2021-12-7","2021-12-8",
                "2021-13-1","2021-13-2","2021-13-3","2021-13-4",
                "2021-14-1","2021-14-2","2021-14-3","2021-14-4","2021-14-5","2021-14-6","2021-14-7","2021-14-8",
                "2021-15-1","2021-15-2","2021-15-3","2021-15-4","2021-15-5","2021-15-6","2021-15-7","2021-15-8",
                "2021-16-1","2021-16-2","2021-16-3","2021-16-4","2021-16-5","2021-16-6","2021-16-7","2021-16-8",
                "2021-17-1","2021-17-2","2021-17-3","2021-17-4",
                "2021-18-1","2021-18-2","2021-18-3","2021-18-4","2021-18-5","2021-18-6","2021-18-7","2021-18-8",
                "2021-19-1","2021-19-2","2021-19-3","2021-19-4","2021-19-5","2021-19-6","2021-19-7","2021-19-8",
                "2021-20-1","2021-20-2","2021-20-3","2021-20-4","2021-20-5","2021-20-6","2021-20-7","2021-20-8",
                "2021-21-1","2021-21-2","2021-21-3","2021-21-4","2021-21-5","2021-21-6","2021-21-7","2021-21-8",
                "2021-22-1","2021-22-2","2021-22-3","2021-22-4","2021-22-5","2021-22-6","2021-22-7","2021-22-8",
                "2021-23-1","2021-23-2","2021-23-3","2021-23-4","2021-23-5","2021-23-6","2021-23-7","2021-23-8",
                "2021-24-1","2021-24-2","2021-24-3","2021-24-4","2021-24-5","2021-24-6","2021-24-7","2021-24-8",
                "2021-25-1","2021-25-2","2021-25-3","2021-25-4","2021-25-5","2021-25-6","2021-25-7","2021-25-8",
                "2022-1-1","2022-1-2","2022-1-3","2022-1-4","2022-1-5","2022-1-6","2022-1-7","2022-1-8",
                "2022-2-1","2022-2-2","2022-2-3","2022-2-4","2022-2-5","2022-2-6","2022-2-7","2022-2-8",
                "2022-3-1","2022-3-2","2022-3-3","2022-3-4","2022-3-5","2022-3-6","2022-3-7","2022-3-8",
                "2022-4-1","2022-4-2","2022-4-3","2022-4-4","2022-4-5","2022-4-6","2022-4-7","2022-4-8",
                "2022-5-1","2022-5-2","2022-5-3","2022-5-4","2022-5-5","2022-5-6","2022-5-7","2022-5-8",
                "2022-6-1","2022-6-2","2022-6-3","2022-6-4","2022-6-5","2022-6-6","2022-6-7","2022-6-8",
                "2022-7-1","2022-7-2","2022-7-3","2022-7-4","2022-7-5","2022-7-6","2022-7-7","2022-7-8",
                "2022-8-1","2022-8-2","2022-8-3","2022-8-4","2022-8-5","2022-8-6","2022-8-7","2022-8-8",
                "2022-9-1","2022-9-2","2022-9-3","2022-9-4","2022-9-5","2022-9-6","2022-9-7","2022-9-8",
                "2022-10-1","2022-10-2","2022-10-3","2022-10-4","2022-10-5","2022-10-6","2022-10-7","2022-10-8",
                "2022-11-1","2022-11-2","2022-11-3","2022-11-4","2022-11-5","2022-11-6","2022-11-7","2022-11-8",
                "2022-12-1","2022-12-2","2022-12-3","2022-12-4","2022-12-5","2022-12-6","2022-12-7","2022-12-8",
                "2022-13-1","2022-13-2","2022-13-3","2022-13-4",
                "2022-14-1","2022-14-2","2022-14-3","2022-14-4","2022-14-5","2022-14-6","2022-14-7","2022-14-8",
                "2022-15-1","2022-15-2","2022-15-3","2022-15-4","2022-15-5","2022-15-6","2022-15-7","2022-15-8",
                "2022-16-1","2022-16-2","2022-16-3","2022-16-4","2022-16-5","2022-16-6","2022-16-7","2022-16-8",
                "2022-17-1","2022-17-2","2022-17-3","2022-17-4",
                "2022-18-1","2022-18-2","2022-18-3","2022-18-4","2022-18-5","2022-18-6","2022-18-7","2022-18-8",
                "2022-19-1","2022-19-2","2022-19-3","2022-19-4","2022-19-5","2022-19-6","2022-19-7","2022-19-8",
                "2022-20-1","2022-20-2","2022-20-3","2022-20-4","2022-20-5","2022-20-6","2022-20-7","2022-20-8",
                "2022-21-1","2022-21-2","2022-21-3","2022-21-4","2022-21-5","2022-21-6","2022-21-7","2022-21-8",
                "2022-22-1","2022-22-2","2022-22-3","2022-22-4","2022-22-5","2022-22-6","2022-22-7","2022-22-8",
                "2022-23-1","2022-23-2","2022-23-3","2022-23-4","2022-23-5","2022-23-6","2022-23-7","2022-23-8",
                "2022-24-1","2022-24-2","2022-24-3","2022-24-4","2022-24-5","2022-24-6","2022-24-7","2022-24-8",
                "2022-25-1","2022-25-2","2022-25-3","2022-25-4","2022-25-5","2022-25-6","2022-25-7","2022-25-8",
                "2023-1-1","2023-1-2","2023-1-3","2023-1-4","2023-1-5","2023-1-6","2023-1-7","2023-1-8",
                "2023-2-1","2023-2-2","2023-2-3","2023-2-4","2023-2-5","2023-2-6","2023-2-7","2023-2-8",
                "2023-3-1","2023-3-2","2023-3-3","2023-3-4","2023-3-5","2023-3-6","2023-3-7","2023-3-8",
                "2023-4-1","2023-4-2","2023-4-3","2023-4-4","2023-4-5","2023-4-6","2023-4-7","2023-4-8",
                "2023-5-1","2023-5-2","2023-5-3","2023-5-4","2023-5-5","2023-5-6","2023-5-7","2023-5-8",
                "2023-6-1","2023-6-2","2023-6-3","2023-6-4","2023-6-5","2023-6-6","2023-6-7","2023-6-8",
                "2023-7-1","2023-7-2","2023-7-3","2023-7-4","2023-7-5","2023-7-6","2023-7-7","2023-7-8",
                "2023-8-1","2023-8-2","2023-8-3","2023-8-4","2023-8-5","2023-8-6","2023-8-7","2023-8-8",
                "2023-9-1","2023-9-2","2023-9-3","2023-9-4","2023-9-5","2023-9-6","2023-9-7","2023-9-8",
                "2023-10-1","2023-10-2","2023-10-3","2023-10-4","2023-10-5","2023-10-6","2023-10-7","2023-10-8",
                "2023-11-1","2023-11-2","2023-11-3","2023-11-4","2023-11-5","2023-11-6","2023-11-7","2023-11-8",
                "2023-12-1","2023-12-2","2023-12-3","2023-12-4","2023-12-5","2023-12-6","2023-12-7","2023-12-8",
                "2023-13-1","2023-13-2","2023-13-3","2023-13-4","2023-13-5",
                "2023-14-1","2023-14-2","2023-14-3","2023-14-4","2023-14-5","2023-14-6","2023-14-7",
                "2023-15-1","2023-15-2","2023-15-3","2023-15-4","2023-15-5","2023-15-6","2023-15-7","2023-15-8",
                "2023-16-1","2023-16-2","2023-16-3","2023-16-4","2023-16-5",
                "2023-17-1","2023-17-2","2023-17-3","2023-17-4","2023-17-5","2023-17-6","2023-17-7",
                "2023-18-1","2023-18-2","2023-18-3","2023-18-4","2023-18-5","2023-18-6","2023-18-7","2023-18-8",
                "2023-19-1","2023-19-2","2023-19-3","2023-19-4","2023-19-5",
                "2023-20-1","2023-20-2","2023-20-3","2023-20-4","2023-20-5","2023-20-6","2023-20-7",
                "2023-21-1","2023-21-2","2023-21-3","2023-21-4","2023-21-5","2023-21-6","2023-21-7","2023-21-8",
                "2023-22-1","2023-22-2","2023-22-3","2023-22-4","2023-22-5","2023-22-6","2023-22-7","2023-22-8",
                "2023-23-1","2023-23-2","2023-23-3","2023-23-4","2023-23-5","2023-23-6","2023-23-7","2023-23-8",
                "2023-24-1","2023-24-2","2023-24-3","2023-24-4","2023-24-5","2023-24-6","2023-24-7","2023-24-8",
                "2023-25-1","2023-25-2","2023-25-3","2023-25-4","2023-25-5","2023-25-6","2023-25-7","2023-25-8", # "2023-25-4"
                "2023-26-1","2023-26-2","2023-26-3","2023-26-4","2023-26-5","2023-26-6","2023-26-7","2023-26-8",
                "2023-27-1","2023-27-2","2023-27-3","2023-27-4","2023-27-5","2023-27-6","2023-27-7","2023-27-8",
                "2024-1-1","2024-1-2","2024-1-3","2024-1-4","2024-1-5","2024-1-6","2024-1-7","2024-1-8",
                "2024-2-1","2024-2-2","2024-2-3","2024-2-4","2024-2-5","2024-2-6","2024-2-7","2024-2-8",
                "2024-3-1","2024-3-2","2024-3-3","2024-3-4","2024-3-5","2024-3-6","2024-3-7","2024-3-8",
                "2024-4-1","2024-4-2","2024-4-3","2024-4-4","2024-4-5","2024-4-6","2024-4-7","2024-4-8",
                "2024-5-1","2024-5-2","2024-5-3","2024-5-4","2024-5-5","2024-5-6","2024-5-7","2024-5-8",
                "2024-6-1","2024-6-2","2024-6-3","2024-6-4","2024-6-5","2024-6-6","2024-6-7","2024-6-8",
                "2024-7-1","2024-7-2","2024-7-3","2024-7-4","2024-7-5","2024-7-6","2024-7-7","2024-7-8",
                "2024-8-1","2024-8-2","2024-8-3","2024-8-4","2024-8-5","2024-8-6","2024-8-7","2024-8-8",
                "2024-9-1","2024-9-2","2024-9-3","2024-9-4","2024-9-5","2024-9-6","2024-9-7","2024-9-8",
                "2024-10-1", "2024-10-2", "2024-10-3","2024-10-4","2024-10-5","2024-10-6","2024-10-7","2024-10-8",
                "2024-11-1","2024-11-2","2024-11-3","2024-11-4","2024-11-5","2024-11-6","2024-11-7","2024-11-8",
                "2024-12-1","2024-12-2","2024-12-3","2024-12-4","2024-12-5","2024-12-6","2024-12-7","2024-12-8",
                "2024-13-1","2024-13-2","2024-13-3","2024-13-4","2024-13-5",
                "2024-14-1","2024-14-2","2024-14-3","2024-14-4","2024-14-5","2024-14-6","2024-14-7",
                "2024-15-1","2024-15-2","2024-15-3","2024-15-4","2024-15-5","2024-15-6","2024-15-7","2024-15-8",
                "2024-16-1","2024-16-2","2024-16-3","2024-16-4","2024-16-5",
                "2024-17-1","2024-17-2","2024-17-3","2024-17-4","2024-17-5","2024-17-6","2024-17-7",
                "2024-18-1","2024-18-2","2024-18-3","2024-18-4","2024-18-5","2024-18-6","2024-18-7","2024-18-8",
                "2024-19-1","2024-19-2","2024-19-3","2024-19-4","2024-19-5",
                "2024-20-1","2024-20-2","2024-20-3","2024-20-4","2024-20-5","2024-20-6","2024-20-7","2024-20-8",
                "2024-21-1","2024-21-2","2024-21-3","2024-21-4","2024-21-5","2024-21-6","2024-21-7","2024-21-8",
                "2024-22-1","2024-22-2","2024-22-3","2024-22-4","2024-22-5","2024-22-6","2024-22-7","2024-22-8",
                "2024-23-1","2024-23-2","2024-23-3","2024-23-4","2024-23-5","2024-23-6","2024-23-7","2024-23-8",
                "2021-26-1","2021-26-2","2021-26-3","2021-26-4",
                "2021-27-1","2021-27-2","2021-28-1","2021-28-2","2021-29-1",
                "2022-26-1","2022-26-2","2022-26-3","2022-26-4",
                "2022-27-1","2022-27-2","2022-28-1","2022-28-2", "2022-29-1",
              # "2023-27-1","2023-27-2","2023-27-3","2023-27-4","2023-27-5","2023-27-6","2023-27-7","2023-27-8",
                "2023-28-1","2023-28-2","2023-28-3","2023-28-4",
                "2023-29-1","2023-29-2","2023-30-1","2023-30-2","2023-31-1",
              "2024-24-1","2024-24-2","2024-24-3","2024-24-4","2024-24-5","2024-24-6","2024-24-7","2024-24-8",
              "2024-25-1","2024-25-2","2024-25-3","2024-25-4","2024-25-5","2024-25-6","2024-25-7","2024-25-8",
              "2024-26-1","2024-26-2","2024-26-3","2024-26-4","2024-26-5","2024-26-6","2024-26-7","2024-26-8",
              "2024-27-1","2024-27-2","2024-27-3","2024-27-4","2024-27-5","2024-27-6","2024-27-7","2024-27-8",
              "2024-28-1","2024-28-2","2024-28-3","2024-28-4",
              "2024-29-1","2024-29-2","2024-30-1","2024-30-2","2024-31-1"
              )

unique(ids_36_796)
ids_34x_32
# Use rep to repeat IDs
repeated_ids_34x_32 <- rep(ids_34x_32, each = 34)
repeated_ids_36_796 <- rep(ids_36_796, each = 36)

NRL_PlayerStats_Joined %>%
  filter(Season %in% '2024') %>%
  view()

NRL_PlayerStats_Joined %>%
  filter(ID %in% '2024-1-1') %>%
  view()

nrow(NRL_PlayerStats_Joined)
# Combine all into a single list
final_ids <- c(repeated_ids_34x_32, repeated_ids_36_796)
final_ids

all_ids <- unique(final_ids)

duplicates <- all_ids[duplicated(all_ids)]
if (length(duplicates) > 0) {
  print(paste("Duplicate IDs found:", paste(duplicates, collapse = ", ")))
} else {
  print("No duplicate IDs found.")
}

unique(final_ids)

seasons <- substr(all_ids, 1, 4)
seasons
season_counts <- table(seasons)

# Print counts per season
print(season_counts)

ids_2024 <- grep("^2024", all_ids, value = TRUE)
view(ids_2024)
# Check the number of IDs for 2024
length(ids_2024) # Should return 214

NRL_PlayerStats_Joined$ID <- final_ids
nrow(NRL_PlayerStats_Joined)
unique(NRL_PlayerStats_Joined$URL)
#
# colnames(Full_Fast_NRLr)
# colnames(NRL_PlayerStats_Joined)
#
# unique(Full_Fast_NRLr$Team)

NRL_PlayerStats_Joined <- NRL_PlayerStats_Joined %>%
  mutate(Team = str_replace_all(Team, "Wests ", "")) %>%
  mutate(Opponent = str_replace_all(Opponent, "Wests ", ""))

## Adding missing opponents for 2024 finals games ####

Missing_Opponent <- which(is.na(NRL_PlayerStats_Joined$Opponent))
Missing_Opponent

finals_teams_2024 <- c("Roosters","Panthers","Sharks","Storm","Knights","Cowboys","Sea Eagles","Bulldogs",
                       "Cowboys","Sharks","Sea Eagles","Roosters","Roosters","Storm","Sharks","Panthers",
                       "Panthers","Storm")
finals_opponents_2024 <- rep(finals_teams_2024,each = 18)

NRL_PlayerStats_Joined$Opponent[Missing_Opponent] <- finals_opponents_2024

dim(Full_Fast_NRLr)
dim(NRL_PlayerStats_Joined)

Full_Fast_NRLr %>%
  distinct(ID,Team) %>%
  view()

NRL_PlayerStats_Joined %>%
  distinct(ID,Team) %>%
  view()

NRL_PlayerStats_Joined %>%
  filter(ID %in% c('2023-25-1','2023-25-2','2023-25-3','2023-25-4',
                   '2023-25-5','2023-25-6','2023-25-7','2023-25-8',
                   '2023-26-1')) %>%
  view()

NRL_PlayerStats_Joined %>%
  # filter(ID %in% '2023-25-4') %>%
  filter(Team %in% 'Tigers',Opponent %in% 'Dolphins') %>%
  # filter(Team %in% 'Dolphins',Opponent %in% 'Tigers') %>%
  view()

Fast_NRLr_IDs <- Full_Fast_NRLr %>%
  # select(ID) %>%
  distinct(ID,.keep_all = FALSE) %>%
  rename(Fast_IDs = ID)
view(Fast_NRLr_IDs)

NRL_PlayerStats_IDs <- NRL_PlayerStats_Joined %>%
  # select(ID) %>%
  distinct(ID,.keep_all = FALSE) %>%
  rename(Player_IDs = ID)

view(NRL_PlayerStats_IDs)

IDs_Check <- inner_join(Fast_NRLr_IDs,NRL_PlayerStats_IDs)
view(IDs_Check)

IDs_Check <- full_join(Fast_NRLr_IDs, NRL_PlayerStats_IDs, by = "ID", suffix = c(".Fast", ".Stats"),
                       relationship = "many-to-many") %>%
  distinct(ID) %>%
  view()

# Check rows in Full_Fast_NRLr that don't have a match in NRL_PlayerStats_Joined
NRL_Fast_NRLr_Unmatched <- anti_join(Full_Fast_NRLr, NRL_PlayerStats_Joined, by = c('ID', 'Team'))
view(NRL_Fast_NRLr_Unmatched)

NRL_Fast_NRLr_Unmatched %>%
  distinct(ID) %>%
  view()

# Check rows in NRL_PlayerStats_Joined that don't have a match in Full_Fast_NRLr
NRL_PlayerStats_Joined_Unmatched <- anti_join(NRL_PlayerStats_Joined, Full_Fast_NRLr, by = c('ID', 'Team'))
NRL_PlayerStats_Joined_Unmatched %>%
  distinct(ID) %>%
  view()

str(Full_Fast_NRLr)
str(NRL_PlayerStats_Joined)

# Creating NRL_Team_Player_Stats ####
NRL_Team_Player_Stats <- full_join(Full_Fast_NRLr,NRL_PlayerStats_Joined,
                               by = c('ID','Team')) %>%
  select(-Game_Type,-TotalPoints) %>%
  rename('Team Offloads' = Offloads.x,
         'Team Errors' = Errors.x,
         'Team Kicks' = Kicks.x,
         'Player Kicks' = Kicks.y,
         'Player Errors' = Errors.y,
         'Player Offloads' = Offloads.y,
         'Player 40/20s' = `40/20`,
         'Player 20/40s' = `20/40`,
         'Team 40/20s' = `40/20s`,
         'Team 20/40s' = `20/40s`,
         "Player Forced Drop Outs" = ForcedDropOuts)

NRL_Team_Player_Stats %>%
  distinct(ID) %>%
  view()

NRL_Team_Player_Stats_IDs <- tibble(ID = NRL_Team_Player_Stats$ID)
final_ids_df <- tibble(ID = final_ids)

# Use anti_join to find IDs in NRL_Team_Player_Stats$ID that are not in final_ids
missing_ids <- anti_join(NRL_Team_Player_Stats_IDs, final_ids_df, by = "ID")
view(missing_ids) # 2023-25-4

NRL_Team_Player_Stats %>%
  distinct(ID) %>%
  count() %>%
  view()
  # distinct(ID,Team) %>%
  # view()


# Fixing Opponent ####
opponent_mapping <- NRL_Team_Player_Stats %>%
  group_by(ID) %>%
  summarise(
    team1 = unique(Team)[1],
    team2 = unique(Team)[2]
  )
opponent_mapping

NRL_Team_Player_Stats <- NRL_Team_Player_Stats %>%
  left_join(opponent_mapping, by = "ID") %>%
  mutate(
    Opponent = if_else(Team == Opponent,
                       if_else(Team == team1, team2, team1),
                       Opponent)
  ) %>%
  select(-team1, -team2) %>%
  ungroup()

NRL_Team_Player_Stats %>%
  distinct(ID) %>%
  count()

TeamStats_PlayerStats <- NRL_Team_Player_Stats %>%
  group_by(ID,Team) %>%
  mutate('Team Tries' = sum(Tries),
         'Team Conversions' = sum(Conversions),
         'Team Conversion Attempts' = sum(ConversionAttempts),
         'Team Penalties' = sum(PenaltyGoals),
         'Team 1pt Field Goals' = sum(`1PointFieldGoals`),
         'Team 2pt Field Goals' = sum(`2PointFieldGoals`),
         # 'Team LineBreaks' = sum(Linebreaks),
         'Team Line Break Assists' = sum(LineBreakAssists),
         'Team Try Assists' = sum(TryAssists)) %>%
  select(-Correct,-Incorrect,-Awarded,-OnReport) %>%  # %>%
  # select(ID,Team,Scores,Result,Linebreaks,`Team Tries`,`Team Conversions`,
  #        `Team Conversion Attempts`,`Team Penalties`,`Team 1pt Field Goals`,
  #        `Team 2pt Field Goals`,`Team Line Break Assists`,`Team Try Assists`)
# %>% distinct(ID, Team,.keep_all = TRUE)
  ungroup() %>%
  distinct(ID) %>%
  count() %>%
  view()
View(TeamStats_PlayerStats)

## End of prep ====

TeamStats_PlayerStats %>%
  filter(Tries > 0 | TryAssists > 0 |
         LineBreaks > 0 | LineBreakAssists > 0 |
         Intercepts > 0 ) %>%
  select(-c("LineEngagedRuns", "TackleBreaks", "HitUps", "PlayTheBall", "AveragePlayTheBallSpeed",
         "DummyHalfRuns", "DummyHalfRunMetres", "OneonOneSteal", "Player Offloads", "DummyPasses", "Passes",
         "Receipts",  "PassesToRunRatio",  "TackleEfficiency", "TacklesMade",
         "MissedTackles",  "IneffectiveTackles", "KicksDefused" ,
         "Player Kicks", "KickingMetres", "Player Forced Drop Outs","BombKicks",
         "Grubbers", "Player 40/20s","Player 20/40s", "CrossFieldKicks",
         "KickedDead", "Player Errors","HandlingErrors","OneonOneLost",
         "Penalties","RuckInfringements","Inside10Metres",
         "SinBins", "SendOffs", "AllRuns", "AllRunMetres", "KickReturnMetres", "PostContactMetres",
         "Completion Difference", "Score Against","Average Metres Per Run",
         "Average Metres Per Run Conceded", "Line breaks conceded", "Run metres conceded", "Total runs",
         "Average run difference", "Line break rate", "Line break differential", "Run metre differential",
         "Possession", "Territory", "Runs",
         "Run Metres", "Dummy Half Runs", "Tackle Busts","Post Contact Metres",
         "Team Offloads", "Linebreaks","20m Restarts", "Completion Rate",
         "Tackled In Opp 20", "In Goal Escapes","Tackles",
         "Missed Tackles","Team Errors", "Penalties Conceded","Team Kicks","Kick Metres","Team 40/20s",
         "Team 20/40s","Attacking Kicks" ,"Drop Outs","Forced Drop Outs",
         "Kicks Dead",  "Completion Difference","Score Against","Season","Game Type","URL",
         "StintOne","StintTwo", "Opponent", "MinsPlayed")) %>%
  relocate(`Team Tries`,.after = Team) %>%
  relocate(`Team Try Assists`,.after = `Team Tries`) %>%
  relocate(LineBreaks,.after = `Team Try Assists`) %>%
  relocate(`Team Line Break Assists`,.after = LineBreaks) %>%
  view()

# Use this or variations of this below to calculate rate of unassisted tries (ie no assist)
# and those try assists via kicks where there is no line break assist
TeamStats_PlayerStats %>%
  filter(`Team Try Assists` > `Team Line Break Assists`) %>%
  select(-c("LineEngagedRuns", "TackleBreaks", "HitUps", "PlayTheBall", "AveragePlayTheBallSpeed",
            "DummyHalfRuns", "DummyHalfRunMetres", "OneonOneSteal", "Player Offloads", "DummyPasses", "Passes",
            "Receipts",  "PassesToRunRatio",  "TackleEfficiency", "TacklesMade",
            "MissedTackles",  "IneffectiveTackles", "KicksDefused" ,
            "Player Kicks", "KickingMetres", "Player Forced Drop Outs","BombKicks",
            "Grubbers", "Player 40/20s","Player 20/40s", "CrossFieldKicks",
            "KickedDead", "Player Errors","HandlingErrors","OneonOneLost",
            "Penalties","RuckInfringements","Inside10Metres",
            "SinBins", "SendOffs", "AllRuns", "AllRunMetres", "KickReturnMetres", "PostContactMetres",
            "Completion Difference", "Score Against","Average Metres Per Run",
            "Average Metres Per Run Conceded", "Line breaks conceded", "Run metres conceded", "Total runs",
            "Average run difference", "Line break rate", "Line break differential", "Run metre differential",
            "Possession", "Territory", "Runs",
            "Run Metres", "Dummy Half Runs", "Tackle Busts","Post Contact Metres",
            "Team Offloads", "Linebreaks","20m Restarts", "Completion Rate",
            "Tackled In Opp 20", "In Goal Escapes","Tackles",
            "Missed Tackles","Team Errors", "Penalties Conceded","Team Kicks","Kick Metres","Team 40/20s",
            "Team 20/40s","Attacking Kicks" ,"Drop Outs","Forced Drop Outs",
            "Kicks Dead",  "Completion Difference","Score Against","Season","Game Type","URL")) %>%
  view()

# Use this or variations of this below to calculate rate of unassisted tries (ie no assist)
# and those try assists via kicks where there is no line break assist

# Where team has more `Team Tries` than `Team Try Assists`, there has been an unassisted try


TeamStats_PlayerStats %>%
  mutate(Unassisted_Tries = `Team Tries` - `Team Try Assists`,
         Potential_Kick_Assists = `Team Try Assists` - `Team Line Break Assists`) %>%
  # filter(`Team Try Assists` > `Team Line Break Assists`) %>%
  distinct(ID,Team,.keep_all = TRUE) %>%
  select(-c("LineEngagedRuns", "TackleBreaks", "HitUps", "PlayTheBall", "AveragePlayTheBallSpeed",
            "DummyHalfRuns", "DummyHalfRunMetres", "OneonOneSteal", "Player Offloads", "DummyPasses", "Passes",
            "Receipts",  "PassesToRunRatio",  "TackleEfficiency", "TacklesMade",
            "MissedTackles",  "IneffectiveTackles", "KicksDefused" ,
            "Player Kicks", "KickingMetres", "Player Forced Drop Outs","BombKicks",
            "Grubbers", "Player 40/20s","Player 20/40s", "CrossFieldKicks",
            "KickedDead", "Player Errors","HandlingErrors","OneonOneLost",
            "Penalties","RuckInfringements","Inside10Metres",
            "SinBins", "SendOffs", "AllRuns", "AllRunMetres", "KickReturnMetres", "PostContactMetres",
            "Completion Difference", "Score Against","Average Metres Per Run",
            "Average Metres Per Run Conceded", "Line breaks conceded", "Run metres conceded", "Total runs",
            "Average run difference", "Line break rate", "Line break differential", "Run metre differential",
            "Possession", "Territory", "Runs",
            "Run Metres", "Dummy Half Runs", "Tackle Busts","Post Contact Metres",
            "Team Offloads", "Linebreaks","20m Restarts", "Completion Rate",
            "Tackled In Opp 20", "In Goal Escapes","Tackles",
            "Missed Tackles","Team Errors", "Penalties Conceded","Team Kicks","Kick Metres","Team 40/20s",
            "Team 20/40s","Attacking Kicks" ,"Drop Outs","Forced Drop Outs",
            "Kicks Dead",  "Completion Difference","Score Against","Season","Game Type","URL")) %>%
  ungroup() %>%
  distinct(ID) %>%
  count() %>%
  view()

TeamStats_PlayerStats %>%
  filter(ID %in% '2021-1-6',Team %in% 'Panthers') %>%
  view()

# Create 0 & 1 results in Fast_NRLr_long for Logistic Regression
Fast_NRLr_long <- Fast_NRLr %>%
  pivot_longer(cols = c(`Home Score`, `Away Score`),
               names_to = "Location",
               values_to = "Score") %>%
  mutate(Result = ifelse(Location == "Home Score", `Home result`, `Away result`)) %>%
  select(Score, Result,`Home Team`,`Away Team`,ID,`Home result`,
         `Away result`) %>%
  mutate(Result = str_replace(Result,"W","1"),
         Result = str_replace(Result,"L","0")) %>%
  filter(Result %in% c("0","1")) %>%
  mutate(Result = as.numeric(as.character(Result)))
view(Fast_NRLr_long)

# Logistic Regression ####
for_model <- glm(Fast_NRLr_long$Result~Fast_NRLr_long$Score,
             family = "binomial")
summary(for_model)

## Plot ####
Plot <- Fast_NRLr_long %>%
  ggplot(aes(x=Score,y=Result)) +
  geom_point()
ggplotly(Plot)

Log_Regression_Plot <- Fast_NRLr_long %>%
  ggplot(aes(x=Score,y=Result)) +
  geom_point() +
  stat_smooth(method = "glm",se=FALSE,
              method.args = list(family='binomial'))
ggplotly(Log_Regression_Plot)

shapiro.test(Full_Fast_NRLr$Scores)

# Attack correlation/regression ####
Attack <- Full_Fast_NRLr %>%
   select(Scores,Possession,Territory,Runs,`Run Metres`,`Post Contact Metres`,
          `Linebreaks`,`Tackle Busts`,Offloads,Errors,`Tackled In Opp 20`,
          `Run Metres`,`Post Contact Metres`,`Completion Rate`,
          `Completion Difference`,`Average Metres Per Run`,
          `Average run difference`) %>%
   cor(use = "complete.obs")
ggcorrplot(Attack,lab = TRUE,
           lab_size = 3,title = 'Attack Correlation Matrix')

Full_Fast_NRLr %>%
  ggplot(aes(x = Offloads,y= Errors)) +
  geom_point(stat = 'identity') +
  # scale_fill_trc_continuous(palette = blue_shades) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

Full_Fast_NRLr %>%
  ggplot(aes(x = `Tackle Busts`,y = Linebreaks)) +
  geom_point(stat = 'identity') +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

Full_Fast_NRLr %>%
  ggplot(aes(`Completion Difference`,Possession)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

CompDiff_Score <- Full_Fast_NRLr %>%
  filter(!Result=='D') %>%
  ggplot(aes(x=`Completion Difference`,y=Scores,
             fill=Result)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")
ggplotly(CompDiff_Score)

par(mar = c(5, 5, 4, 2) + 0.1)

Full_Fast_NRLr %>%
  ggplot(aes(x = `Completion Difference`, y = Scores,colour = Result)) +
  geom_point(stat = 'identity')

## Attack Multi-linear regression ####
attack_regression_model <-
  lm(Scores ~ Possession + Territory + Runs + `Run Metres` + Awarded +
     `Tackle Busts` + `Post Contact Metres` + Offloads + Linebreaks +
       `Tackled In Opp 20`,
     data = Full_Fast_NRLr)
summary(attack_regression_model)

## Defence correlation/regression ####
colnames(Full_Fast_NRLr)
Defence <- Full_Fast_NRLr %>%
  select(`Score Against`,Possession,Territory,Tackles,`Missed Tackles`,
         Errors,`Penalties Conceded`,`Completion Rate`,
         `Completion Difference`,`Average Metres Per Run Conceded`,
         `Line breaks conceded`) %>%
  cor(use = "complete.obs")
ggcorrplot(Defence,lab = TRUE,
           lab_size = 3,title = 'Defence Correlation Matrix')

defence_regression_model <-
  lm(`Score Against` ~ Possession + Territory + Tackles +
       Errors + `Penalties Conceded`,
     data = Full_Fast_NRLr)
summary(defence_regression_model)

# Expected points regression ####
# Run metres, possession, line breaks

set.seed(101)
sample = sample.split(Full_Fast_NRLr$ID, SplitRatio = .75)
Training_data = subset(Full_Fast_NRLr, sample == TRUE)
Test_data = subset(Full_Fast_NRLr, sample == FALSE)

view(Training_data) # 619
view(Test_data) # 207

Training_data <- Full_Fast_NRLr %>%
  filter(ID %in% Training_data$ID)
view(Training_data)

Test_data <- Full_Fast_NRLr %>%
  filter(ID %in% Test_data$ID)
view(Test_data)

Full_Fast_NRLr %>%
  ggplot(aes(x = `Run Metres`,y = Scores)) +
  geom_point(stat = 'identity')

Full_Fast_NRLr %>%
  ggplot(aes(x = Possession,y = Scores)) +
  geom_point(stat = 'identity')

Full_Fast_NRLr %>%
  ggplot(aes(x = Linebreaks,y = Scores)) +
  geom_point(stat = 'identity')

colnames(Training_data)
Training_data$`Post Contact Metres`

# Training data Expected points Version 1
Exp_points_V1 <- glm(Scores ~ Possession  + Linebreaks + `Tackled In Opp 20`,
                  # + `Completion Difference` + `Tackle Busts` + `Average Metres Per Run`
                  # + `Average run difference` + `Post Contact Metres` + Territory,
                  data = Training_data)
Exp_points_V1
summary(Exp_points_V1)

Exp_points_V1$residuals
plot(Exp_points_V1$residuals)

Exp_points_V1$fitted.values
view(Exp_points_V1$residuals)
shapiro.test(Exp_points_V1$residuals)

mean(Exp_points_V1$residuals)
sd(Exp_points_V1$residuals)

Exp_points_V1 %>%
  ggplot(aes(x = Exp_points_V1$residuals)) +
  geom_density()

Normal_Plot_Exp_points_V1 <- Exp_points_V1 %>%
  ggplot(aes(x = Exp_points_V1$residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(
    fun = dnorm,
    args = list(mean = -8.86425e-14, sd = 7.478569),
    color = "red",
    size = 1
  ) +
  xlim(-25, 25)
ggplotly(Normal_Plot_Exp_points_V1)
Normal_Plot_Exp_points_V1
residuals <- resid(Exp_points_V1)
Exp_points_V1$residuals

qqplot <- ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")
ggplotly(qqplot)

Teams_expected_points <- Training_data %>%
  # filter(!Season == 2024) %>%
  mutate('Predicted points' = Exp_points_V1$fitted.values,
         Over_expected = Scores - `Predicted points`) %>%
  group_by(Team,Season) %>%
  summarise(count = n(),
            actual_points = sum(Scores),
            expected_points = sum(`Predicted points`),
            points_oe = sum(Over_expected)
            )
view(Teams_expected_points)

Teams_plot <- Teams_expected_points %>%
  filter(Season %in% 2024) %>%
  ggplot(aes(x = expected_points,y = actual_points,
             label = Team)) +
  geom_point(stat = 'identity') +
  # geom_text(label = paste(Teams_expected_points$Team),
  #           check_overlap = TRUE,nudge_x = 5,nudge_y = 5) +
  geom_hline(yintercept = mean(Teams_expected_points$actual_points)) +
  geom_vline(xintercept = mean(Teams_expected_points$expected_points)) +
  geom_smooth(method = lm)
ggplotly(Teams_plot)

## Testing V1 on Test Data ====

points_preds_V1 <- data.frame(predict.lm(Exp_points_V1,newdata = Test_data)) %>%
  rename(expected_points = predict.lm.Exp_points_V1..newdata...Test_data.)
points_preds_V1
Test_data_preds_V1 <- cbind(Test_data,points_preds_V1)

Test_data_preds_V1 %>%
  select(Team,Scores,expected_points,ID) %>%
  view()
vip(Exp_points_V1)

# Testing V2 (covariance) on Test Data ====

Exp_points_covar_V2 <- glm(Scores ~ (Possession  + Linebreaks + `Run Metres`)^2,
                     data = Training_data)
summary(Exp_points_covar_V2)
vip(Exp_points_covar_V2)

points_preds_covar_V2 <- data.frame(predict.lm(Exp_points_covar_V2,newdata = Test_data)) %>%
  rename(expected_points = predict.lm.Exp_points_covar_V2..newdata...Test_data.)

Test_data_preds_V2 <- cbind(Test_data,points_preds_covar_V2)

Test_data_preds_V2 %>%
  select(Team,Scores,expected_points,ID) %>%
  view()

# Create training and test data (80 & 20%)

Full_Fast_NRLr %>%
  mutate(Percent_of_postcontact = round(`Post Contact Metres`/`Run Metres`,2)) %>%
  view()

Full_Fast_NRLr %>% group_by(Team) %>%
  summarise(Average_Concede = round(mean(`Score Against`),2),
            Average_Score = round(mean(Scores),2),
            Wins = sum(Result == 'W'),
            Total_Matches = n(),
            Win_Percentage = round(Wins/Total_Matches,2)) %>%
  view()

Full_Fast_NRLr %>% group_by(Result) %>%
  summarise(Average_Concede = round(mean(`Score Against`),2),
            Average_Score = round(mean(Scores),2),
 #          Wins = sum(Result == 'W'),
            Total_Matches = n()) %>%
 #          Win_Percentage = round(Wins/Total_Matches,2)) %>%
  view()

# Normal test for total runs/speed of game ####
Playoffs <- Fast_NRLr %>%
  dplyr::filter(`Game Type` %in% 'Playoff')
Regular_Season <- Fast_NRLr %>%
  filter(`Game Type` == 'Regular Season')

t.test(Regular_Season$`Total runs`,Playoffs$`Total runs`)

Fast_NRLr <- Fast_NRLr %>% mutate('Z score - Total runs' = (`Total runs` - 323.7)/22.09) # (mean - mean)/SD

hist(Fast_NRLr$`Total runs`)

shapiro.test(Fast_NRLr$`Total runs`)
mean(Fast_NRLr$`Total runs`)
sd(Fast_NRLr$`Total runs`)

fit <- lm(Scores ~ `Tackled In Opp 20`,Full_Fast_NRLr)
plot(Full_Fast_NRLr$Scores,Full_Fast_NRLr$`Tackled In Opp 20`)
abline(fit)

colnames(Full_Fast_NRLr)

# Run metre differential ####
Full_Fast_NRLr %>%
  filter(`Run metre differential` > 0) %>%
  count(Result) %>%
  mutate(Percentage = round(n / sum(n) * 100,2)) %>%
  arrange(desc(Result)) %>%
  view()

# Line break differential ####
Full_Fast_NRLr %>%
  filter(`Line break differential` > 0) %>%
  count(Result) %>%
  mutate(Percentage = round(n / sum(n) * 100,2)) %>%
  view()

# Completion difference ####
Full_Fast_NRLr %>%
  filter(`Completion Difference` > 0) %>%
  count(Result) %>%
  mutate(Percentage = round(n / sum(n) * 100,2)) %>%
  view()

Full_Fast_NRLr %>%
  filter(`Completion Difference` < 0,Result=='W') %>%
  view()

Full_Fast_NRLr %>%
  filter(`Line break differential` > 0 & `Completion Difference` > 0) %>%
  count(Result) %>%
  mutate(Percentage = round(n / sum(n) * 100,2)) %>%
  view()

Full_Fast_NRLr %>%
  filter(`Line break differential` > 0 & `Completion Difference` > 0 & `Run metre differential` > 0) %>%
  count(Result) %>%
  mutate(Percentage = round(n / sum(n) * 100,2)) %>%
  view()

## Team_kicking_perc ####
NRL_Team_Player_Stats <- NRL_Team_Player_Stats %>%
  group_by(ID,Team) %>%
  mutate(
  Team_kicking_Perc = round((sum(PenaltyGoals, na.rm = TRUE) + sum(Conversions, na.rm = TRUE)) /
                           (sum(ConversionAttempts, na.rm = TRUE) + sum(PenaltyGoals, na.rm = TRUE)),2)
  ) %>%
  ungroup()

NRL_Team_Player_Stats %>%
  select(ID,Team,Team_kicking_Perc) %>%
  view()

NRL_Team_Player_Stats %>%
  filter(Team_kicking_Perc == 0) %>%
  distinct(Team,ID,.keep_all = TRUE) %>%
  view()

cor(NRL_Team_Player_Stats$Team_kicking_Perc,
    NRL_Team_Player_Stats$Points,use='complete.obs')

# Testing ####

# Season to season Point differential to wins ####
# Do we have to change it to per game averages rather than totals?
# Do regressions on it?

NRL_MergedSeasons <- NRL_MergedSeasons %>%
  mutate(Label = paste(Club,Season,sep = "_")) %>%
  mutate(Win_Percentage = round(Wins/Played,3))
view(NRL_MergedSeasons)

Wins_Diff_Plot <- NRL_MergedSeasons %>%
  ggplot(aes(x = Diff.,y = Win_Percentage,
             label = Label)) +
  geom_point(stat = 'identity') +
  geom_smooth(method = lm)
ggplotly(Wins_Diff_Plot)

Wins_For_Plot <- NRL_MergedSeasons %>%
  ggplot(aes(x = For,y = Wins,
             colour = Club, label = Season)) +
  geom_point(stat = 'identity')
  # geom_smooth(method = lm)
ggplotly(Wins_For_Plot)

Wins_Against_Plot <- NRL_MergedSeasons %>%
  ggplot(aes(x = Against,y = Wins,
             colour = Club, label = Season)) +
  geom_point(stat = 'identity')
# geom_smooth(method = lm)
ggplotly(Wins_Against_Plot)

colnames(NRL_MergedSeasons)

RegModel <-
  lm(Win_Percentage ~ For + Against,
     data = NRL_MergedSeasons)
summary(RegModel)

DiffWinPerc_RegModel <-
  lm(Win_Percentage ~ Diff.,
     data = NRL_MergedSeasons)
summary(DiffWinPerc_RegModel)

Diff_Perc_Plot <- NRL_MergedSeasons %>%
  ggplot(aes(x = Diff.,y = Win_Percentage,
             label = Label)) +
  geom_point(stat = 'identity') +
  geom_smooth(method = lm)
ggplotly(Diff_Perc_Plot)

PointsWins_RegModel <-
  lm(Wins ~ For,
     data = NRL_MergedSeasons)
summary(PointsWins_RegModel)
2# y (wins) = -4.969249 + 0.033185*x (points)
# For each additional point a team scores, the expected number of wins increases by 0.03.

AgainstWins_RegModel <-
  lm(Wins ~ Against,
     data = NRL_MergedSeasons)
summary(AgainstWins_RegModel)
# y (wins) = 27 - 0.03*x (points)
# For each additional point a team concedes, the expected number of wins decreases by 0.03.

colnames(NRL_MergedSeasons)
DiffWins_RegModel <-
  lm(Wins ~ Diff.,
     data = NRL_MergedSeasons)
summary(DiffWins_RegModel)

NRL_reg_seasons <- NRL_MergedSeasons %>%
  filter(!Season %in% '2020')

view(NRL_reg_seasons)
PointsWins_RegModel <-
  lm(Wins ~ For,
     data = NRL_reg_seasons)
summary(PointsWins_RegModel)

Wins_For_Plot <- NRL_reg_seasons %>%
  ggplot(aes(x = For,y = Wins,
             label = Label)) +
  geom_point(stat = 'identity') +
  geom_smooth(method = lm)
ggplotly(Wins_For_Plot)

