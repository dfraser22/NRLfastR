# Team Stats 

Three xlsx files (nrl_all_match_stats_2021_2024_wide.xlsx, nrl_all_match_stats_2024_wide.xlsx & nrl_match_stats_specific_urls.xlsx) 
have been scrapped from foxsports.com.au (see NRL_Games_Scraping.py in NRLscraping repo for code that has nrl_all_match_stats_2021_2024_wide.xlsx output).

nrl_all_match_stats_2024_wide.xlsx is for round 19 onwards in 2024. See NRL_Games_Scraping_24_R19on.py in this repo for code.

nrl_match_stats_specific_urls is at Specific_URLS.py in this repo. These are the two IDs ('2023-25-4','2024-8-2') in that script.

Those three files have imported, cleaned, and merged. Additional variables have then been added and/or calculated.

# Player Stats

Three files (nrl_player_stats_with_urls.xlsx, nrl_finals_player_stats_with_urls & nrl_player_stats_2024_rounds_24_to_27.csv) have been scrapped or pulled together from nrl.com. 

nrl_player_stats_with_urls.xlsx comprises all of 2021, 2022, and 2023 regular seasons, and up to the round 23 of season 2024. Code is at nrl_player_stats.py (1st version in that fike) in the NRLscraping repo.

nrl_finals_player_stats_with_urls covers the 2021, 2022, and 2023 finals series.

nrl_player_stats_2024_rounds_24_to_27.csv was compiled manually using a chrome extension as nrl.com had changed their url format. It covers from Round 24 of 2024 through to the end of the finals.

Those three files have imported, cleaned, and merged. Additional variables have then been added and/or calculated.
