import asyncio
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
import pandas as pd


async def get_team_data(page, team_name):
    await page.click(f"text={team_name}")
    await page.wait_for_selector('table')
    content = await page.content()
    soup = BeautifulSoup(content, 'html.parser')
    table = soup.find('caption', text=f"{team_name} Player Stats").find_parent('table')

    headers = [th.get_text(strip=True) for th in table.find_all('th')]
    rows = []
    for row in table.find_all('tr')[1:]:
        cells = row.find_all('td')
        row_data = [cell.get_text(strip=True) for cell in cells]
        row_data.insert(0, team_name)  # Add team name as the first column
        rows.append(row_data)

    return headers, rows


async def main():
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True)
        page = await browser.new_page()
        await page.goto("https://www.nrl.com/draw/nrl-premiership/2021/round-1/storm-v-rabbitohs/")

        team_buttons = await page.query_selector_all('button[aria-controls="player-stats"]')
        team_names = [await button.inner_text() for button in team_buttons]

        all_data = []
        headers = None

        for team_name in team_names:
            team_headers, team_data = await get_team_data(page, team_name)
            if headers is None:
                headers = ["Team"] + team_headers  # Include team name as first header
            all_data.extend(team_data)

        await browser.close()

    df = pd.DataFrame(all_data, columns=headers)
    df.to_excel("nrl_player_stats.xlsx", index=False)
    print("Data successfully written to nrl_player_stats.xlsx")

if __name__ == "__main__":
    asyncio.run(main())