import time
import requests
from bs4 import BeautifulSoup
import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
from webdriver_manager.chrome import ChromeDriverManager

# Initialize the WebDriver
driver = webdriver.Chrome(service=ChromeService(ChromeDriverManager().install()))

# Function to scrape data for a single match with retries
def scrape_match_data(url, season, round_num, game_num, retries=3):
    try:
        for attempt in range(retries):
            try:
                driver.get(url)
                time.sleep(20)  # Wait for the page to load


                page_source = driver.page_source
                soup = BeautifulSoup(page_source, 'html.parser')

                # Extract team names
                team_names = soup.select('.styles__TeamName-sc-1cu7fqd-5')
                if len(team_names) >= 2:
                    team_1_name = team_names[0].text.strip()
                    team_2_name = team_names[1].text.strip()
                else:
                    raise ValueError("Could not find team names")

                # Extract categories and stats
                categories = []
                team_1_stats = []
                team_2_stats = []

                rows = soup.select('.styles__StatsComparisonTable-sc-1lc8go-0 .styles__StatsComparisonRow-sc-2bcjei-3')

                for row in rows:
                    category = row.select_one('.iagDTd').text.strip()
                    stat_team_1 = row.select('figcaption')[0].text.strip()
                    stat_team_2 = row.select('figcaption')[1].text.strip()

                    categories.append(category)
                    team_1_stats.append(stat_team_1)
                    team_2_stats.append(stat_team_2)

                # Default values for scores, possession, and territory
                score_category = 'Scores'
                possession_category = 'Possession'
                territory_category = 'Territory'

                score_team_1, score_team_2 = 'N/A', 'N/A'
                possession_team_1, possession_team_2 = 'N/A', 'N/A'
                territory_team_1, territory_team_2 = 'N/A', 'N/A'

                # Extract Scores
                score_selector = '.styles__ScoreSummary-sc-9364gn-0 .styles__MatchScore-sc-3rdpqd-0'
                scores = soup.select(score_selector)
                if len(scores) >= 2:
                    score_team_1 = scores[0].text.strip()
                    score_team_2 = scores[1].text.strip()

                # Extract Possession
                possession_selector = '.styles__PossessionFigure-sc-zrml0t-3 figcaption .styles__TeamLabel-sc-1cu7fqd-4'
                possession = soup.select(possession_selector)
                if len(possession) >= 2:
                    possession_team_1 = possession[0].text.strip()
                    possession_team_2 = possession[1].text.strip()

                # Extract Territory
                territory_selector = '.styles__StatsComparisonBar-sc-ar67kb-0'
                territory = soup.select(territory_selector)
                if len(territory) >= 2:
                    territory_team_1 = territory[0].text.strip()
                    territory_team_2 = territory[1].text.strip()

                # Create DataFrame
                match_df = pd.DataFrame({
                    'Team': [team_1_name, team_2_name],
                    'Scores': [score_team_1, score_team_2],
                    'Possession': [possession_team_1, possession_team_2],
                    'Territory': [territory_team_1, territory_team_2],
                    'ID': [f"{season}-{round_num}-{game_num}", f"{season}-{round_num}-{game_num}"]
                })

                for category, stat_team_1, stat_team_2 in zip(categories, team_1_stats, team_2_stats):
                    match_df[category] = [stat_team_1, stat_team_2]

                return match_df

            except Exception as e:
                print(f"Attempt {attempt + 1} failed for URL {url}: {e}")
                time.sleep(10)  # Wait before retrying

        print(f"Failed to scrape data for URL {url} after {retries} attempts.")
        return None

    except Exception as e:
        print(f"Error occurred for URL {url}: {e}")
        return None

# Function to get the maximum number of rounds for a season
def get_max_round(season):
    if season == 2023:
        return 31
    elif season in [2021, 2022]:
        return 29
    else:
        return 25

# Function to get the number of games in a round
def get_number_of_games(season, round_num):
    if season in [2021, 2022]:
        if round_num in [27, 28]:
            return 2
        elif round_num == 29:
            return 1
        elif round_num == 26:
            return 4
        else:
            return 8
    elif season == 2023:
        if round_num in [29, 30]:
            return 2
        elif round_num == 31:
            return 1
        elif round_num == 28:
            return 4
        else:
            return 8
    elif season == 2024:
        return 8
    else:
        return 8  # Default case for rounds with less than 8 games

# Main script
all_matches_df = pd.DataFrame()
missing_urls = []

expected_games = {
    2021: 201,
    2022: 201,
    2023: 213,
    2024: 136
}

for season in [2021, 2022, 2023, 2024]:
    max_round = get_max_round(season)
    for round_num in range(1, max_round + 1):  # Adjust rounds based on the season
        num_games = get_number_of_games(season, round_num)
        for game_num in range(1, num_games + 1):  # Iterate through games in each round
            url = f"https://www.foxsports.com.au/nrl/nrl-premiership/match-centre/NRL{season}{round_num:02}{game_num:02}/stats"
            match_data = scrape_match_data(url, season, round_num, game_num)
            if match_data is not None:
                all_matches_df = pd.concat([all_matches_df, match_data], ignore_index=True)
            else:
                missing_urls.append(url)

# Verify the total number of games scraped
total_games_scraped = all_matches_df['ID'].nunique()
expected_total_games = sum(expected_games.values())

if total_games_scraped != expected_total_games:
    print(f"Warning: Expected {expected_total_games} games but only found {total_games_scraped} games.")
    print("Missing URLs:")
    for url in missing_urls:
        print(url)

# Reorder columns to ensure the 'ID' column is first
columns_order = ['ID', 'Team', 'Scores', 'Possession', 'Territory'] + [col for col in all_matches_df.columns if col not in ['ID', 'Team', 'Scores', 'Possession', 'Territory']]
all_matches_df = all_matches_df[columns_order]

# Save the DataFrame to an XLSX file

all_matches_df.to_excel('nrl_all_match_stats_2021_2024_wide.xlsx', index=False)
print("Data saved to nrl_all_match_stats_2021_2024_wide.xlsx")

# Print missing URLs
if missing_urls:
    print("The following URLs could not be scraped:")
    for url in missing_urls:
        print(url)

# Quit the WebDriver
driver.quit()