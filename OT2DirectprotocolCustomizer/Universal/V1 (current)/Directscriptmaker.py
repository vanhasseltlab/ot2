"""
uDirectscriptmaker - V1
Author: JB
"""

#imports
import FreeSimpleGUI as sg
import os
from pathlib import Path
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium import webdriver
import shutil
import subprocess

#import modules
from CustomModules import Selector
from CustomModules import Filedriver
from CustomModules import Protocolcustomizer

#Window
def Mainwindow(simulation, protocollist, x):
    x = str(x)
    layout = [
         [sg.B("Make commandlist"), sg.B("Refresh")],
         [sg.T("Please provide information about the run you want to do")],
         [sg.T('Select a file', s=(10,1)), sg.FileBrowse(file_types=(('CSV Files', '*.csv'),), key='Browse')],
         [sg.T('Or use the one you just made'), sg.T(str(x), key='importfilename')],
         [sg.R("Selected", "group1", key="Selected", default = True), sg.R("Made", "group1", key="Made")],
         [sg.T("Experiment Name", s = (15,1)), sg.I(key='ExpName')],
         [sg.T("Your Name", s = (15,1)), sg.I(key = 'Name')],
         [sg.T('Date (yymmdd)', s = (15,1)),sg.I(key = 'date')],
         [sg.T("What experiment are you running?")],
         [sg.Listbox(protocollist, size = (50, 4), key = "protocol")],
         [sg.T('Sarstedt or Greiner (only 48 wellplates)')],
         [sg.R('Sarstedt', 'group6', key = 'brands'), sg.R('Greiner', 'group6', key ='brandg', default=True)],
         [sg.B('Save', s= 16, button_color = 'black on yellow'), sg.B('Send', disabled = True, s= 16), sg.P(), sg.B('Close', s=16, button_color = 'tomato')]
         ]
    return sg.Window('Directscript maker', layout, finalize = True) 

#cmdlistmaker Doesnt function yet
def filesending():
    layout = [
        [sg.Text("Please provide all information below")],
        [sg.Text("Choose your file: "), sg.FileBrowse(key = 'Browsecmd')],                                                   
        [sg.Text("Platemap PMID"), sg.InputText('', size=(56, 1), key = 'PMID')],                                         
        [sg.Text("Your name (First AND Lastname)"), sg.InputText('', size=(41, 1), key = 'Firstlast')],                  
        [sg.Text("Experiment Name"), sg.InputText('', size=(54, 1), key = 'EXPname')],                                       
        [sg.Text("Experiment number"), sg.InputText('', size=(53, 1), key = 'EXPnum')],                                    
        [sg.Text("What type of experiment are you going to do?")],
        [sg.Radio("Checkerboard", "group1", key = 'Checker'), 
         sg.Radio("Multiplate MIC", "group1", key = 'MVP'), 
         sg.Radio("384 well plate", "group1", key = '384p'),
         sg.Radio("48 Well plate", "group1", key = '48w'),
         sg.Radio("SingleplateMIC", "group1", key= 'Sp'),
         sg.Radio("qPCR (Flex Only)", "group1", key= "qPCR")],                                                             
        [sg.Text("Do you want to fill outer wells in robot? (384 plate only)")],
        [sg.Radio("Yes", "group3", key = 'Fill'), sg.Radio("No", "group3", key = 'nFill', default = True)],
        [sg.Button("Save User Inputs", s= 16, button_color = 'black on yellow'), 
         sg.Button("Send to Server", s= 16, disabled = True), sg.Push(), sg.Button("Close", s= 16, button_color = "tomato")]
        ]
    return sg.Window("Webdriver", layout, finalize= True)


#start with checking if the Simconfig file exists.
dir_path = os.path.dirname(os.path.realpath(__file__))
simpath = dir_path + "//CustomModules//"
if os.path.isfile(simpath + "Simconfig.py"):
    from CustomModules.Simconfig import simulation, pc
else:
    simulation = "0"
    
#next check if installation is needed.
if simulation == "0":
    currentpath = dir_path
    currentpath = currentpath.replace('\\', '//')
    if 'C://' not in currentpath:
        print("installing started")
        from CustomModules import SetupDirectscriptmaker
        executeinstalle = SetupDirectscriptmaker
    else:
        pass
else:
    pass


#Getting the details of the robot
robotname, robotip, robot_type = Selector.Robotdetails(simulation)
#getting the protocols for the robot
protocollist = Selector.protocolfinder(simulation, robot_type)

#put the theming in (Sim:Green, Flex:lightPurple, OT2:Darkblue)
if simulation == "1":
    sg.theme("DarkGreen1")
elif robot_type == "Flex":
    sg.theme("LightPurple")
    pc = "Flex"
else:
    sg.theme('DarkBlue3')
    pc = "OT2"

x = ""
window1 = Mainwindow(simulation, protocollist, x)
window2_active = False

#Start window loop
while True:
    window, event, values = sg.read_all_windows()
    
    #Stops everything when user uses the button cancel or closes the window
    if event == 'Close' or  event == sg.WIN_CLOSED:
        if window2_active == True:
            window2_active = False
            window2.close()
            window1.UnHide()
        if window == window1:
            window.close()
            break
    
    #events of window 1
    if event == 'Make commandlist':
        window2_active = True
        window2 = filesending()
        window1.Hide()
        
     #window 1 events refresh
    if event == 'Refresh':
        try:
            if(values['Selected']== True and str(x) == "NA"):
                ok = sg.PopupOKCancel("This function does not do anything when you havent made a command list through this program or you havent selected made" '\n' "(underneath the browse button)")
                if(ok =="OK"):
                    print('Try again')
                else:
                    print('try again')
                    
            elif(values['Selected'] == True and str(x) != "NA"):
                tryagain = sg.PopupOKCancel("Dont forget to set selected --> 'Made' underneath Browse (this should never occur, congrats)")
            
            else:
                x = str(x)
                window['importfilename'].update(str(x))
                longstring = x.split("_")
                pmidstr = longstring[1]
                pmidstr = pmidstr.split("-")
                pmidstr = pmidstr[1]
                
                try: 
                    expnamestr = longstring[2] + longstring[3]
                    expnamestr = expnamestr.removeprefix('EXPID-')
                    namestr = longstring[4]
                    namestr = namestr.split('.')
                    namestr = namestr[1] + namestr[0]
                except:
                    expnamestr = longstring[2]
                    expnamestr = expnamestr.removeprefix('EXPID-')
                    namestr = longstring[3]
                    namestr = namestr.split('.')
                    namestr = namestr[1] + namestr[0]
                try:    
                    window['ExpName'].update(expnamestr)
                    window['Name'].update(namestr)
                except:
                    smting = sg.PopupOKCancel("Name can not be pasted into areas, check if file is ok and fill in yourself")
        except:
            ok = sg.PopupOKCancel("This function does not do anything when you havent made a command list through this program or you havent selected made" '\n' "(underneath the browse button)")
    
       
    if event == 'Save':
        #save event triggers only on window1. Goal: save user input and start preperations for sending the csv
        experimentname = values.get("ExpName")
        usrname = values.get("Name")
        date = values.get("date")
        browsecheck = values['Browse']
        madecheck = x
        
        directscript = values.get("protocol")
        if "" in (experimentname, usrname, date, directscript[0]):
            sg.popup("Please fill all items")
        elif (browsecheck == "" and madecheck == ""):
            sg.popup("You either didnt browse the file or did not select 'Made' (if you made the cmdlist with the cmdlistmaker)")
        else:
            directscript = directscript[0]
            directscriptname = date + "_" + usrname + "_" + experimentname + ".py"
            
            #time to check if the browsefile is actually in the right directory
            if(values['Made'] == True):
                 cmdfilename = x
                 print(cmdfilename)
                 filecheck1 =  os.path.expanduser("~") + "//Desktop//User input (for direct)//" + cmdfilename + ".csv"
    
            else:
                #Browse will have the entire path --> need transformation to only get the .csv file
                cmdfilename = values['Browse']
                #pathfile is the path it is browsed from. Needed for the shuttil operation
                pathfile = cmdfilename
                cmdfilename= Path(cmdfilename)
                cmdfilename = cmdfilename.name
                cmdfilename = cmdfilename.split(".")
                #note: this is only the file name not with extention yet
                cmdfilename = cmdfilename[0]+"."+cmdfilename[1]
                
                #Move from USB or other spot to correct file spot
                filecheck1 =  os.path.expanduser("~") + "//Desktop//User input (for direct)//" + cmdfilename + ".csv"
                check4 = os.path.isfile(filecheck1)
                filemove = os.path.expanduser("~") + "//Desktop//User input (for direct)//" + cmdfilename + ".csv"
                
                #and actually making sure this is not causing the simulation to fail. (not what we would want)
                if(check4 == False and simulation == "0"):
                    print("filecheck correct")
                    shutil.copy(pathfile, filemove, follow_symlinks=True)
                    print("Copy succesfull")
                elif(check4 == True and simulation == "0"):
                    print("Check complete :)")
                else:
                    print("")
            
            brandg = values.get("brandg")
            if brandg == True:
                brand = "Greiner"
            else:
                brand = "Sarstedt"
            
            #customizing the protocol to make a Unique Directscript
            protocol = Protocolcustomizer.Protocolctzer(directscriptname, simulation, cmdfilename, pc, brand, experimentname, robotname, directscript)
            
            #After all thatenable sending
            window['Send'].update(disabled=False)
            window['Send'].update(button_color = 'green')
        
    if event == 'Send':
        #this is the sending action towards the robot. Its only for the cmdlist
        sg.popup("Please wait a few seconds for the file to go to the Robot")
        #first set of commands are to be "building blocks" for the full command
        file_path = filecheck1
        #please note that you have a space after the last ' this is nessesary as otherwise the command breaks
        file_path = "'" + file_path + "' "
        robot_root = "'root@"
        robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
        path_robot = robot_root+robotip+robot_rest
        robot_key = os.path.expanduser("~") + "/robot_ssh_key "
        robot_key = robot_key.replace('\\', '/')
        #dont forget that with the new win (11) you will need the -O in there. Newest change that they wanted to make
        scp = "scp -O -i "
            
        #First constructing the full command as string 
        Full_command = scp + robot_key + file_path + path_robot
        #replacing path markers aka // --> / and \\ --> /
        Full_command = Full_command.replace('\\', '/')
        Full_command = Full_command.replace('//', '/')
        completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
        #printing completed to get the entire response there. 
        print(completed)
    
    #Window 2 events
    if event == 'Save User Inputs':
        #Goal: save all input and space first last name for the webdriver later
        try:
            if values['Browsecmd'] == "":
                sg.popup("You have not selected a platemap yet")
            else:
                #gives all the values its own variable
                filepath = values['Browsecmd']
                pmid_plate = values['PMID']
                Firstlast = values['Firstlast'].split(" ")
                Firstname = Firstlast[0]
                Lastname = Firstlast[1]
                Experiment_name = values['EXPname']
                Experiment_num = values['EXPnum']
                filelocation = os.path.abspath(filepath)
                
                
                #Now only the options and services of the webdriver need to be added
                #set the options for new location for downloaded Robot commands
                options = Options()
                options.set_preference("browser.download.folderList", 2)
                options.set_preference("browser.download.manager.showWhenStarting", False)
                options.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-gzip")

                if (simulation == "1"):
                    path = os.path.expanduser("~")
                    downloadloc = path + r"\Documents\GitHub\ot2\Execution code for OT2\Incubator\Universal Directscriptmaker\Supporting scripts and items\Webdriver\Firefox download test"
                    options.set_preference("browser.download.dir", downloadloc)
                    service = Service(executable_path= path +  "//Documents//GitHub//ot2//Execution code for OT2//Incubator//Universal Directscriptmaker//Supporting scripts and items//Webdriver//Firefox webdriver//geckodriver.exe")

                else:
                    path = os.path.expanduser("~")
                    downloadloc = path + r"\Desktop\User input (for direct)"
                    options.set_preference("browser.download.dir", downloadloc)
                    service = Service(executable_path= path + '//Desktop//DO NOT TOUCH THIS FOLDER (webdriver)//geckodriver.exe')
                
                
                if(values['384p'] == True or values['48w'] == True and values['Fill'] == False and values['nFill'] == False):
                    sg.popup("Please make sure you select if you want to have the robot fill the wells for you")
                elif(values['384p'] == False and values['48w'] == False and values['MVP'] == False and values['Checker'] == False and values['Sp'] == False and values['qPCR'] == False):
                    sg.popup("You seem to have not selected the method")            
                else:
                    sg.popup("Make sure your platemap is correct")
                    
                    window["Send to Server"].update(disabled=False)
                    window['Send to Server'].update(button_color = 'green')
                        
        except:
            sg.popup("Some expected fields were not filled in or incorrectly")
            
    if event == 'Send to Server':
        driver = webdriver.Firefox(service = service, options = options)
        
        if(values['Checker'] == True):
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/CQ_Plate/")
            assert "CQ Plate.title"
            x = Filedriver.filesend(filelocation, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, driver)
        
        elif(values['MVP'] == True):
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/MVPlate/")
            assert "Multiplate MIC - OT2 Commander.title"
            x = Filedriver.filesend(filelocation, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, driver)
            
        elif(values['384p'] == True):
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/Plate384/")
            assert "MIC - 384 Well Plate.title"
            fillingrobot = values['Fill']
            notfillingrobot = values['nFill']
            x = Filedriver.filesend(filelocation, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, driver, fillingrobot)
        
        elif(values['qPCR']== True):
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/qPCR/")
            assert "qPCR - FLEX Commander.title"
            x = Filedriver.filesend(filelocation, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, driver)
        
        elif(values['Sp']== True):
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/SingleplateMIC/")
            assert "Singleplate MIC - OT2 Commander.title"
            x = Filedriver.filesend(filelocation, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, driver)  
        
        elif(values['48w'] == True):
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/Plate48/")
            assert "48 Well Plate.title"
            fillingrobot = values['Fill']
            notfillingrobot = values['nFill']
            x = Filedriver.filesend(filelocation, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, driver, fillingrobot)    
        
        else:
            sg.Popup('Please select the method you want to use (the app is going to crash now)')