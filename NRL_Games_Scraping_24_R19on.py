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

# Function to get the number of games in a round for the 2024 season
def get_number_of_games_2024(round_num):
    if round_num == 19:
        return 5
    elif round_num == 20:
        return 7
    elif 21 <= round_num <= 27:
        return 8
    elif round_num == 28:
        return 4
    elif round_num in [29, 30]:
        return 2
    elif round_num == 31:
        return 1
    else:
        return 0  # No games for rounds outside 19-31

# Main script
all_matches_df = pd.DataFrame()
missing_urls = []

# Only process the 2024 season from round 19 onward
for season in [2024]:
    for round_num in range(19, 32):  # From round 19 to round 31
        num_games = get_number_of_games_2024(round_num)
        for game_num in range(1, num_games + 1):  # Iterate through games in each round
            url = f"https://www.foxsports.com.au/nrl/nrl-premiership/match-centre/NRL{season}{round_num:02}{game_num:02}/stats"
            match_data = scrape_match_data(url, season, round_num, game_num)
            if match_data is not None:
                all_matches_df = pd.concat([all_matches_df, match_data], ignore_index=True)
            else:
                missing_urls.append(url)

# Print missing URLs
if missing_urls:
    print("The following URLs could not be scraped:")
    for url in missing_urls:
        print(url)

# Reorder columns to ensure the 'ID' column is first
columns_order = ['ID', 'Team', 'Scores', 'Possession', 'Territory'] + [col for col in all_matches_df.columns if col not in ['ID', 'Team', 'Scores', 'Possession', 'Territory']]
all_matches_df = all_matches_df[columns_order]

# Save the DataFrame to an XLSX file
all_matches_df.to_excel('nrl_all_match_stats_2024_wide.xlsx', index=False)
print("Data saved to nrl_all_match_stats_2024_wide.xlsx")

# Quit the WebDriver
driver.quit()