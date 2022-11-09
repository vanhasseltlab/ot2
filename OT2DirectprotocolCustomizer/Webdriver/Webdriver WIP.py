import PySimpleGUI as sg
import os
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import time

#The main webinteration part for layout includes how the window is suppost to look
def Webinteraction():
    layout = [
        [sg.Text("Please provide all information below")],
        [sg.Text("Choose your file: "), sg.FileBrowse()],                                                   #values['Browse']
        [sg.Text("Platemap PMID"), sg.InputText('')],                                                       #values[0]
        [sg.Text("Your name (First Last)"), sg.InputText('')],                                              #values[1]
        [sg.Text("Experiment Name"), sg.InputText('')],                                                     #values[2]
        [sg.Text("Experiment number"), sg.InputText('')],                                                   #values[3]
        [sg.Text("What type of experiment are you going to do?")],
        [sg.Radio("Checkerboard", "group1"), 
         sg.Radio("Multiplate MIC", "group1"), 
         sg.Radio("384 well plate", "group1"), 
         sg.Radio("M9MixR", "group1")],                                                                     #values4/5/6/7
        [sg.Text("Do you want to fill outer wells in robot? (384 plate only)")],
        [sg.Radio("Yes", "group2"), sg.Radio("No", "group2")],                                              #values8/9
        [sg.Button("Save User Inputs"), sg.Button("Send to Server", disabled = True) , sg.Button("Close")]
        ]
    return sg.Window("Webdriver", layout, finalize= True)

#File sending function
def Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num):
    #it searches per id most of the time if possible
    fileinput = driver.find_element(By.ID, "file").send_keys(fullpath)
    Plate_Map_ID = driver.find_element(By.ID, "pmid").send_keys(values[0])
    Firstname_file = driver.find_element(By.ID, "f_name").send_keys(Firstname)
    Lastname_file = driver.find_element(By.ID, "l_name").send_keys(Lastname)
    Experiment_name_file = driver.find_element(By.ID, "exp_name").send_keys(Experiment_name)
    Experiment_num_file = driver.find_element(By.ID, "exp_num").send_keys(Experiment_num)
    #these sleeptimers are so it doesnt just try to download or click something while this is not possible or might give some problems
    time.sleep(5)
    confirmupload = driver.find_element(By.ID, "do").click()
    time.sleep(3)
    textFromDiv = driver.find_element(By.XPATH, "//div[@class='shiny-text-output shiny-bound-output']").text
    file_name = "CommandList_" + textFromDiv + ".csv"
    path_to_cmd = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + file_name
    check=os.path.isfile(path_to_cmd)
    
    #This is a recommendation --> it checks if the file already is present, with the input from a few variables before.
    if(check == True):
        #If the filename already exists this might lead to problems with the further processing of the file. Also forces people to think about their names (when using the app)
        sg.Popup("The file name is already taken please change this", keep_on_top=True)
        driver.close()
    
    else:
        #if not it will download the files
        DownloadRobot = driver.find_element(By.ID, "d_OT2").click()
        time.sleep(3)
        DownloadSetup = driver.find_element(By.ID, "guide").click()
        #checks another time if the command file exists.
        time.sleep(1)
        check2=os.path.isfile(path_to_cmd)
        RSP = "Robothandler_" + textFromDiv + ".xlsx"
        path_to_RSP = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + RSP
        check3=os.path.isfile(path_to_RSP)
        if(check2 == False):
            DownloadRobot = driver.find_element(By.ID, "d_OT2").click()
        elif(check3 == False):
            DownloadSetup = driver.find_element(By.ID, "guide").click()
        else:
            sg.Popup("Files should be downloaded(click OK when ready to close browser and continue)", keep_on_top = True)
            driver.close()
    return 

def Filesending384(fullpath, fillingrobot, notfillingrobot, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num):
    fileinput = driver.find_element(By.ID, "file").send_keys(fullpath)
    if (fillingrobot == True):
        element = driver.find_element(By.ID, "fillOuter")
        element.click()
    else:
        print("no outerwell will be filled by OT") 
    driver.find_element(By.ID, "pmid").send_keys(values[0])
    driver.find_element(By.ID, "f_name").send_keys(Firstname)
    driver.find_element(By.ID, "l_name").send_keys(Lastname)
    driver.find_element(By.ID, "exp_name").send_keys(Experiment_name)
    driver.find_element(By.ID, "exp_num").send_keys(Experiment_num)
    time.sleep(5)
    confirmupload = driver.find_element(By.ID, "do").click()
    time.sleep(3)
    DownloadRobot = driver.find_element(By.ID, "d_OT2").click()
    time.sleep(3)
    DownloadSetup = driver.find_element(By.ID, "guide").click()
    sg.Popup("Check if files were downloaded (click OK when ready to close browser and continue)", keep_on_top = True)
    driver.close()
    return
    
window2 = Webinteraction()

#time for the loop
while True:
    window, event, values = sg.read_all_windows()
    if event == "Close" or event == sg.WIN_CLOSED:
        window.close()
        break
    
    elif(event == "Save User Inputs"):
        try: 
            if(values['Browse'] == ""):
                sg.Popup("you have not filled in the platemap")
            else:
                #gives all the values its own variable
                filepath = values['Browse']
                pmid_plate = values[0]
                Firstlast = values[1].split(" ")
                Firstname = Firstlast[0]
                Lastname = Firstlast[1]
                Experiment_name = values[2]
                Experiment_num = values[3]
                fullpath = os.path.abspath(filepath)
                
                #set the options for new location for downloaded Robot commands
                options = Options()
                options.set_preference("browser.download.folderList", 2)
                options.set_preference("browser.download.manager.showWhenStarting", False)
                options.set_preference("browser.download.dir", r"C:\Users\jornb\Documents\GitHub\ot2new\Execution code for OT2\Incubator\OT2DirectprotocolCustomizer\Webdriver\Firefox download test")
                options.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-gzip")
                
                
                if(values[6] == True and values[9] == False and values[8] == False):
                    sg.Popup("Please make sure you select if you want to have the robot fill the wells for you")
                elif(values[6] == False and values[7] == False and values[4] == False and values[5] == False):
                    sg.Popup("You seem to have not selected the method")            
                else:
                    sg.Popup("Make sure your platemap is correct")
                    window["Send to Server"].update(disabled=False)
        except:
            sg.Popup ("Some expected fields were not filled in or incorrectly")
        
        
    elif(event == "Send to Server"):
        #service path is for the gecko executable (needs changed if used on the real pc)
        if(values[4] == True):
            options.set_preference("browser.download.folderList", 2)
            options.set_preference("browser.download.manager.showWhenStarting", False)
            options.set_preference("browser.download.dir", r"C:\Users\jornb\Documents\GitHub\ot2new\Execution code for OT2\Incubator\OT2DirectprotocolCustomizer\Webdriver\Firefox download test")
            options.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-gzip")
            service = Service(executable_path='C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox webdriver//geckodriver')
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/CQ_Plate/")
            assert "CQ Plate.title"
            filesending = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num)
        
        elif(values[5] == True):
            service = Service(executable_path='C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox webdriver//geckodriver')
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/MVPlate/")
            assert "Multiplate MIC - OT2 Commander.title"
            filesending = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num)
            
        elif(values[6] == True):
            service = Service(executable_path='C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox webdriver//geckodriver')
            driver = webdriver.Firefox(service = service)
            driver.get("https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/Plate384/")
            assert "MIC - 384 Well Plate.title"
            fillingrobot = values[8]
            notfillingrobot = values[9]
            filesending = Filesending384(fullpath, fillingrobot, notfillingrobot, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num)
        
        elif(values[7]== True):
            service = Service(executable_path='C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox webdriver//geckodriver')
            driver = webdriver.Firefox(service = service)
            driver.get("https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/M9MixR/")
            assert "M9 MixR.title"
            filesending = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num)
            
        else:
            sg.Popup('Please select the method you want to use (the app is going to crash now)')
        