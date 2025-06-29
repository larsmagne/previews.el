#!/usr/bin/python3

# apt install python3-selenium

import time
import random
import json
import sys
import os

from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException, ElementClickInterceptedException, ElementNotInteractableException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains

if not os.path.exists("/tmp/prh/"):
    os.mkdir("/tmp/prh/")
    
# Open Crome
chrome_options = webdriver.ChromeOptions()
prefs = {"profile.default_content_setting_values.notifications" : 2,
         "download.default_directory" : "/tmp/prh/"}
chrome_options.add_experimental_option("prefs", prefs)
chrome_options.add_argument("--disable-notifications")
chrome_options.add_argument("--headless")
chrome_options.add_argument("window-size=1920,1080")
chrome_options.add_argument("--disable-dev-shm-usage");
#chrome_options.add_argument('--no-sandbox')
driver = webdriver.Chrome(options=chrome_options)

driver.get("https://prhcomics.com/catalog-landing-page/?catalogCode=" +
           sys.argv[1])

time.sleep(5)

try:
    button = driver.find_element(By.ID, "consent_prompt_submit")
    button.click()
except NoSuchElementException:
    pass

time.sleep(3)

button = driver.find_element(By.CLASS_NAME, "product-list-download")
hover = ActionChains(driver).move_to_element(button)
hover.perform()

time.sleep(3)

button.click()

time.sleep(3)

button = driver.find_element(By.XPATH, "//a[@href='#xls']")
hover = ActionChains(driver).move_to_element(button)
hover.perform()

time.sleep(3)

button.click()

time.sleep(60)

driver.quit()
