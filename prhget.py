#!/usr/bin/python3

# apt install python3-selenium

import time
import random
import json
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException, ElementClickInterceptedException, ElementNotInteractableException
from selenium.webdriver.common.by import By

# Open Crome
chrome_options = webdriver.ChromeOptions()
prefs = {"profile.default_content_setting_values.notifications" : 2}
chrome_options.add_experimental_option("prefs", prefs)
chrome_options.add_argument("--disable-notifications")
#chrome_options.add_argument("--headless")
chrome_options.add_argument("--disable-dev-shm-usage");
#chrome_options.add_argument('--no-sandbox')
driver = webdriver.Chrome(options=chrome_options)

driver.get("https://prhcomics.com/catalog-landing-page/?catalogCode=2025-05")

time.sleep(5)

try:
    button = driver.find_element(By.ID, "consent_prompt_submit")
    button.click()
except NoSuchElementException:
    pass

time.sleep(3)

def scroll_down():
    """A method for scrolling the page."""
    # Get scroll height.
    last_height = driver.execute_script("return document.body.scrollHeight")
    while True:
        # Scroll down to the bottom.
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        # Wait to load the page.
        time.sleep(2)

        # Calculate new scroll height and compare with last scroll height.
        new_height = driver.execute_script("return document.body.scrollHeight")
        if new_height == last_height:
            break

        last_height = new_height

times = 30
# Push "See More" some times.
while times > 0:
    times -= 1
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
    scroll_down()
    print("Scrolled")
    try:
        button = driver.find_element(By.ID, "titlelist-load-more-button")
        try:
            button.click()
        except (ElementClickInterceptedException, ElementNotInteractableException):
            pass
    except NoSuchElementException:
        pass
    time.sleep(5)

html = driver.execute_script("return document.body.innerHTML;")
with open("prh.html", "w") as f:
    f.write(html)
    print("Saved")

#driver.quit()
