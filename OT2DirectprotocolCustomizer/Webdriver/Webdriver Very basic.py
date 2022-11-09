from selenium.webdriver.firefox.service import Service
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By

service = Service(executable_path='C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox webdriver//geckodriver')
driver = webdriver.Firefox(service = service)
driver.get("http://www.python.org")
assert "Python in driver.title"
elem = driver.find_element(By.NAME, "q")
elem.clear()
elem.send_keys("pycon")
elem.send_keys(Keys.RETURN)
assert "Noresult found." not in driver.page_source
driver.close()