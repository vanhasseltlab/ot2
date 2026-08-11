# this is the blue print for the newly rewritten wbdriver
#imports
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium import webdriver
from selenium.webdriver.common.by import By
import time
import os
import FreeSimpleGUI as sg

#function
def filesend(filelocation, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, driver, fillingrobot = False):
    #needed for the later appearence in the GUI
    x = 'NA'
    
    #prepping path elements
    path = os.path.expanduser("~")
    
    #This part searches ID of the HTML and adds the variables to it
    driver.find_element(By.ID, "file").send_keys(filelocation)
    
    #348 functionality
    if (fillingrobot == True):
        driver.find_element(By.ID, "fillOuter").click()
    else:
        print("no outerwell will be filled by OT") 
        
    driver.find_element(By.ID, "pmid").send_keys(pmid_plate)
    driver.find_element(By.ID, "f_name").send_keys(Firstname)
    driver.find_element(By.ID, "l_name").send_keys(Lastname)
    driver.find_element(By.ID, "exp_name").send_keys(Experiment_name)
    driver.find_element(By.ID, "exp_num").send_keys(Experiment_num)
    
    #Sleep timer to not try to click the buttons before the server has loaded everything, about 3 seconds is the most stable
    time.sleep(3)
    driver.find_element(By.ID, "do").click()
    time.sleep(3)
    
    #Gets the file name for later use in transfering the file from downloads to a usefull folder and checking if the file is not overriding things
    textFromDiv = driver.find_element(By.XPATH, "//div[@class='shiny-text-output shiny-bound-output']").text
    file_name = "CommandList_" + textFromDiv + ".csv"
    RSPname = "Robothandler_" + textFromDiv + ".xlsx"
    
    #Checking path for the check to check of orverriding things
    if(simulation == "1"):
        path_cmd = path + "//Documents//GitHub//ot2//Execution code for OT2//Incubator//Test User inputs//" + file_name
        path_rsp = path + "//Documents//GitHub//ot2//Execution code for OT2//Incubator//Test User inputs//" + RSPname
    else: 
        path_cmd = path + "//Desktop//User input (for direct)//" + file_name
        path_rsp = path + "//Desktop//User input (for direct)//" + RSPname
      
    checkfilepresent = os.path.isfile(path_cmd)   
    
    #This checks if the file already is present, with the input from a few variables before.
    if(checkfilepresent == True):
        #If the filename already exists this might lead to problems with the further processing of the file. Also forces people to think about their names (when using the app)
        sg.Popup("The file name is already taken please change this", keep_on_top=True)
        driver.close()
    
    else:
        #if the file does not yet exist on the computer: download first the CMDlist then the Robothandler
        driver.find_element(By.ID, "d_OT2").click()
        time.sleep(3)
        driver.find_element(By.ID, "guide").click()
        time.sleep(10)
        
        #check if the download was succesfull
        downpath = path + "//Downloads//"
        downpathcmd = downpath + file_name
        downpathrsp = downpath + RSPname
        
        #checking download
        checkdownloadcmd = os.path.isfile(downpathcmd)
        checkdownloadrsp = os.path.isfile(downpathrsp)
        
        #now to redownload if it failed
        if(checkdownloadcmd == False):
            driver.find_element(By.ID, "d_OT2").click()
        elif(checkdownloadrsp == False):
            driver.find_element(By.ID, "guide").click()
        
        #now to move it to the correct spot --> if needed, Most will just go to the correct spot since its in the driveroptions
        try:
            os.replace(downpathcmd, path_cmd)
            
            os.replace(downpathrsp, path_rsp)
        except:
            pass
        
        sg.Popup("Files should be downloaded(click OK when ready to close browser and continue)", keep_on_top = True)
        
        x = "CommandList_" + textFromDiv
        driver.close()
        #X is the command list so it appears in the Main window
    return x


# =============================================================================
# #only troubleshooting
# if __name__ == "__main__":
#    options = Options()
#    options.set_preference("browser.download.folderList", 2)
#    options.set_preference("browser.download.manager.showWhenStarting", False)
#    options.set_preference("browser.download.dir", r"C:\Users\User\Desktop\User input (for direct)")
#    service = Service(executable_path= os.path.expanduser("~") + "//Desktop" +'//DO NOT TOUCH THIS FOLDER (webdriver)//geckodriver.exe')
#    driver = webdriver.Firefox(service = service, options = options) 
# =============================================================================
