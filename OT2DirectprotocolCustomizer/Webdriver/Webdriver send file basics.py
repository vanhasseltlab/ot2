from selenium.webdriver.firefox.service import Service
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import time


service = Service(executable_path='C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox webdriver//geckodriver')
driver = webdriver.Firefox(service = service)
driver.get("https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/MVPlate/")
assert "Multiplate MIC - OT2 Commander.title"
#add file to the browse file
path = r"C:\Users\jornb\Documents\GitHub\ot2new\Execution code for OT2\Incubator\OT2DirectprotocolCustomizer\Webdriver\File to upload"
file = "\MV_InputTemplate.xlsx"
fullpath = path + file
fileinput = driver.find_element(By.ID, "file").send_keys(fullpath)
time.sleep(5)
textFromDiv = driver.find_element(By.XPATH, "//div[@class='shiny-text-output shiny-bound-output']").text
print(textFromDiv)

#important IDs that are needed for full interaction only input-ids (and flowchart)
#download Template = Aid "downloadTemplate"
#Plate map ID: pmid
#First name:f_name
#Last name:l_name
#experiment name: exp_name
#experiment number: exp_num
#button id of confirm = do
#wait for 10 seconds
#Download Robot commands = a id= d_OT2
#download Robot Setup guide = a id guide