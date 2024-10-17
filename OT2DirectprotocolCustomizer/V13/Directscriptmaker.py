#LIBRARIES
import PySimpleGUI as sg
import os
import shutil
import subprocess
import json
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium import webdriver
from selenium.webdriver.common.by import By
import time
from pathlib import Path
from urllib.request import urlopen

#WINDOWS
# Main Window
def Mainwindow(simulation, x):
    if (simulation == "1"):
        layout = [
            [sg.B("Make commandlist"), sg.B("Refresh")],
            [sg.T("Please provide information about the OT2 run you want to do")],
            [sg.T('Select a file', s=(10,1)), sg.FileBrowse(file_types=(('CSV Files', '*.csv'),), key='Browse')],
            [sg.T('Or use the one you just made'), sg.T(str(x), key='importfilename')],
            [sg.R("Selected", "group1", key="Selected", default = True), sg.R("Made", "group1", key="Made")],
            [sg.T("Experiment Name", s = (15,1)), sg.I(key='ExpName')],
            [sg.T("Your Name", s = (15,1)), sg.I(key = 'Name')],
            [sg.T('Date (yymmdd)', s = (15,1)),sg.I(key = 'date')],
            [sg.T('Which OT2 do you want to use?')],
            [sg.R('OT2L', 'group2', key= 'OT2L'), sg.R('OT2R', 'group2', key= 'OT2R'), sg.R('Both', 'group2', key = 'BothOT2')],
            [sg.T("What PC is it running on?")],
            [sg.R('Jorn', 'group3', key = 'PCJ'), sg.R('Sebastian', 'group3', key= 'PCS'), sg.R('OT', 'group3', key= 'PCOT')],
            [sg.T("Do you want to use touchtip? (run will take longer)")],
            [sg.R('Yes', 'group4', key = 'TTy'), sg.R('No', 'group4', key = 'TTn')],
            [sg.T('384 wells / 48 wells?')],
            [sg.R('Yes', 'group5', key = '384wy'), sg.R('No', 'group5', key='384wn', default = True)],
            [sg.T('Sarstedt or Greiner (only 48 wellplates)')],
            [sg.R('Sarstedt', 'group6', key = 'brands'), sg.R('Greiner', 'group6', key ='brandg')],
            [sg.B('Save', s= 16, button_color = 'black on yellow'), sg.B('Send', disabled = True, s= 16), sg.P(), sg.B('Close', s=16, button_color = 'tomato')],
        ]
    else:
        layout = [
            [sg.B("Make commandlist"), sg.B("Refresh")],
            [sg.T("Please provide information about the OT2 run you want to do")],
            [sg.T('Select a file', s=(10,1)), sg.FileBrowse(file_types=(('CSV Files', '*.csv'),), key='Browse')],
            [sg.T('Or use the one you just made'), sg.T(str(x), key='importfilename')],
            [sg.R("Selected", "group1", key="Selected", default = True), sg.R("Made", "group1", key="Made")],
            [sg.T("Experiment Name", s = (15,1)), sg.I(key='ExpName')],
            [sg.T("Your Name", s = (15,1)), sg.I(key = 'Name')],
            [sg.T('Date (yymmdd)', s = (15,1)),sg.I(key = 'date')],
            #[sg.T('Which OT2 do you want to use?')],
            #[sg.R('OT2L', 'group2', key= 'OT2L'), sg.R('OT2R', 'group2', key= 'OT2R'), sg.R('Both', 'group2', key = 'BothOT2')],
            [sg.T("What PC is it running on?")],
            [sg.R('Jorn', 'group3', key = 'PCJ', disabled = True), sg.R('Sebastian', 'group3', key= 'PCS', disabled = True), sg.R('OT', 'group3', key= 'PCOT',  disabled = True, default = True)],
            [sg.T("Do you want to use touchtip? (run will take longer)")],
            [sg.R('Yes', 'group4', key = 'TTy', disabled = True), sg.R('No', 'group4', key = 'TTn', disabled = True, default = True)],
            [sg.T('384 wells / 48 wells?')],
            [sg.R('Yes', 'group5', key = '384wy'), sg.R('No', 'group5', key='384wn', default = True)],
            [sg.T('Sarstedt or Greiner (only 48 wellplates)')],
            [sg.R('Sarstedt', 'group6', key = 'brands'), sg.R('Greiner', 'group6', key ='brandg')],
            [sg.B('Save', s= 16, button_color = 'black on yellow'), sg.B('Send', disabled = True, s= 16), sg.P(), sg.B('Close', s=16, button_color = 'tomato')],
            ]
    
    return sg.Window('Directscript maker', layout, finalize = True)

# Second Window
def Webdriver():
    layout = [
        [sg.Text("Please provide all information below")],
        [sg.Text("Choose your file: "), sg.FileBrowse(key = 'Browse')],                                                   
        [sg.Text("Platemap PMID"), sg.InputText('', size=(56, 1), key = 'PMID')],                                         
        [sg.Text("Your name (First AND Lastname)"), sg.InputText('', size=(41, 1), key = 'Firstlast')],                  
        [sg.Text("Experiment Name"), sg.InputText('', size=(54, 1), key = 'EXPname')],                                       
        [sg.Text("Experiment number"), sg.InputText('', size=(53, 1), key = 'EXPnum')],                                    
        [sg.Text("What type of experiment are you going to do?")],
        [sg.Radio("Checkerboard", "group1", key = 'Checker'), 
         sg.Radio("Multiplate MIC", "group1", key = 'MVP'), 
         sg.Radio("384 well plate", "group1", key = '384p'), 
         sg.Radio("M9MixR", "group1", key = 'M9MixR'),
         sg.Radio("48 Well plate", "group1", key = '48w'),
         sg.Radio("SingleplateMIC", "group1", key= 'Sp')],                                                             
        [sg.Text("Do you want to fill outer wells in robot? (384 plate only)")],
        [sg.Radio("Yes", "group3", key = 'Fill'), sg.Radio("No", "group3", key = 'nFill', default = False)],
        [sg.Button("Save User Inputs", s= 16, button_color = 'black on yellow'), 
         sg.Button("Send to Server", s= 16, disabled = True), sg.Push(), sg.Button("Close", s= 16, button_color = "tomato")]
        ]
    return sg.Window("Webdriver", layout, finalize= True)

#FUNCTIONS------------------
#File sending function Both this one and the other 384: there be dragons
def Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path):
    x = 'NA'
    
    #This part searches ID of the HTML and adds the variables to it
    driver.find_element(By.ID, "file").send_keys(fullpath)
    driver.find_element(By.ID, "pmid").send_keys(pmid_plate)
    driver.find_element(By.ID, "f_name").send_keys(Firstname)
    driver.find_element(By.ID, "l_name").send_keys(Lastname)
    driver.find_element(By.ID, "exp_name").send_keys(Experiment_name)
    driver.find_element(By.ID, "exp_num").send_keys(Experiment_num)
    
    #Sleep timers to not try to click the download button before server is ready
    time.sleep(3)
    driver.find_element(By.ID, "do").click()
    time.sleep(3)
    textFromDiv = driver.find_element(By.XPATH, "//div[@class='shiny-text-output shiny-bound-output']").text
    file_name = "CommandList_" + textFromDiv + ".csv"
    
    if(simulation== "1"):
        path_to_cmd = path + "//Webdriver//Firefox download test" + '//' + file_name
    else:
        path_to_cmd = path + "//Desktop//User input (for direct)" + '//' + file_name
    
    checkfilepresent=os.path.isfile(path_to_cmd)
    
    #This is a recommendation --> it checks if the file already is present, with the input from a few variables before.
    if(checkfilepresent == True):
        #If the filename already exists this might lead to problems with the further processing of the file. Also forces people to think about their names (when using the app)
        sg.Popup("The file name is already taken please change this", keep_on_top=True)
        driver.close()
    
    else:
        #if not it will download the files
        driver.find_element(By.ID, "d_OT2").click()
        time.sleep(3)
        driver.find_element(By.ID, "guide").click()
        
        #checks another time if the command file exists.
        time.sleep(3)
        checkdownload = os.path.isfile(path_to_cmd)
        oldpath = path.replace('//OneDrive', '')
        oldpath = oldpath + "//Downloads//" + file_name
        if (checkdownload == False):
            os.replace(oldpath, path_to_cmd)
        
        RSP = "Robothandler_" + textFromDiv + ".xlsx"
        
        if(simulation == "1"):
            path_to_RSP = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + RSP
        else:
            path_to_RSP = path + "//User input (for direct)//" + RSP
        
        checkdownloadrsp = os.path.isfile(path_to_RSP)
        
        if(checkdownload == False):
            driver.find_element(By.ID, "d_OT2").click()
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = path + "//Downloads//" + RSP

        elif(checkdownloadrsp == False):
            driver.find_element(By.ID, "guide").click()
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = path + "//Downloads//" + RSP
        
        else:
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = path + "//Downloads//" + RSP
        
        try:
            os.replace(path_to_RSP, new_pathRSP)
        except:
            pass
        
        sg.Popup("Files should be downloaded(click OK when ready to close browser and continue)", keep_on_top = True)
        
        x = "CommandList_" + textFromDiv
        driver.close()
        #X is the command list so it appears in the Main window
    return x

#completely the same as the previous file sending with only minor changes
def Filesending384(fullpath, fillingrobot, notfillingrobot, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path):
    x = 'NA'
    fileinput = driver.find_element(By.ID, "file").send_keys(fullpath)
    if (fillingrobot == True):
        driver.find_element(By.ID, "fillOuter").click()
    else:
        print("no outerwell will be filled by OT") 
    
    driver.find_element(By.ID, "file").send_keys(fullpath)
    driver.find_element(By.ID, "pmid").send_keys(pmid_plate)
    driver.find_element(By.ID, "f_name").send_keys(Firstname)
    driver.find_element(By.ID, "l_name").send_keys(Lastname)
    driver.find_element(By.ID, "exp_name").send_keys(Experiment_name)
    driver.find_element(By.ID, "exp_num").send_keys(Experiment_num)
    
    #these sleeptimers are so it doesnt just try to download or click something while this is not possible or might give some problems
    time.sleep(2)
    driver.find_element(By.ID, "do").click()
    time.sleep(3)
    textFromDiv = driver.find_element(By.XPATH, "//div[@class='shiny-text-output shiny-bound-output']").text
    file_name = "CommandList_" + textFromDiv + ".csv"
    
    if(simulation == "1"):
        path_to_cmd = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + file_name
    else:
        path_to_cmd = path + "//Desktop//User input (for direct)//" + file_name
    
    checkfilepresent=os.path.isfile(path_to_cmd)
    
    #This is a recommendation --> it checks if the file already is present, with the input from a few variables before.
    if(checkfilepresent == True):
        #If the filename already exists this might lead to problems with the further processing of the file. Also forces people to think about their names (when using the app)
        sg.Popup("The file name is already taken please change this", keep_on_top=True)
        driver.close()
    
    else:
        #if not it will download the files
        driver.find_element(By.ID, "d_OT2").click()
        time.sleep(3)
        driver.find_element(By.ID, "guide").click()
        #checks another time if the command file exists.
        time.sleep(3)
        checkdownload = os.path.isfile(path_to_cmd)
        oldpath = path.replace('//OneDrive', '')
        oldpath = oldpath + "//Downloads" + '//' + file_name
        if (checkdownload == False):
            os.replace(oldpath, path_to_cmd)
        RSP = "Robothandler_" + textFromDiv + ".xlsx"
        
        if(simulation == "1"):
            path_to_RSP = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + RSP
        else:
            path_to_RSP = path + "//Desktop//User input (for direct)" + '//' + RSP
        checkdownloadrsp = os.path.isfile(path_to_RSP)
        
        if(checkdownload == False):
            driver.find_element(By.ID, "d_OT2").click()
        elif(checkdownloadrsp == False):
            driver.find_element(By.ID, "guide").click()
        else:
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = path + "//Downloads//" + RSP
            
        try:
            os.replace(path_to_RSP, new_pathRSP)
        except:
            pass
 
        sg.Popup("Files should be downloaded(click OK when ready to close browser and continue)", keep_on_top = True)
        x = "CommandList_" + textFromDiv
        driver.close()
    return x

#Supporting functions
def robotgetIPs(simulation):
    # load data
    if (simulation == "1"):
        f = open('C://Users//jornb//AppData//Roaming//Opentrons//discovery.json')
    else:
        try:
            f = open(path + "//AppData//Roaming//Opentrons//discovery.json")
        except: 
            f = open("C://Users//cvhLa//AppData//Roaming//Opentrons//discovery.json")
    json_data = json.load(f)['robots']
    
    # initiate loop
    robot_ip_list = {}
    names = []
    addresses = []
    
    # get IP address for each robot
    for i in range(len(json_data)):
        # subset json data for current robot
        current_data = json_data[i]
        
        # extract relevant info; append
        robot_ip_list[current_data['name']] = current_data['addresses'][0]['ip']
        names.append(current_data['name'])
        addresses.append(current_data['addresses'][0]['ip'])
    
    return robot_ip_list

def prepare():   
    #Troubleshooting paths/ development path        
    simpath = 'C://Users//jornb//OneDrive//Documenten//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer'
    livepathR = 'C://Users//User'
    livepathL = 'C://Users//cvhLa//OneDrive'
    
    x = "NA"
    str(x)
    listofusers = os.listdir('C://Users')
    if ("User" in listofusers):
        simulation = '0'
        path = livepathR
    elif("cvhLa" in listofusers):
        simulation = '0'
        path = livepathL
    else:
        simulation = '1'
        path = simpath
    return simulation, x, path

def test_internet():
    sg.PopupAutoClose("Checking connection with server")
    try:
        urlopen('https://ot2.lacdr.leidenuniv.nl/ot2/home/', timeout = 10)
        return True
    except:
        return False
    
def popup_connecting():
    clicked = sg.PopupOKCancel("This is going to take a bit (might not respond while sending file)\n",
             "Please press OK to continue")
    if (clicked =="OK"):
        return
    else:
       window.close() 
    return

#Initiation
simulation, x, path = prepare()
window1, window2 = None, None
window1 = Mainwindow(simulation, x)        

#Start window loop
while True:
    window, event, values = sg.read_all_windows()
    
    #Stops everything when user uses the button cancel or closes the window
    if event == 'Close' or  event == sg.WIN_CLOSED:
        if window == window2:
            window.close()
        if window == window1:
            window.close()
            break    
        
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
                tryagain = sg.PopupOKCancel("Dont forget to set selected --> made underneath Browse (this should never occur)")
            
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
    
    #Make command list (window2) Launcher
    if event == 'Make commandlist':
        if(test_internet()== True):
            window2 = Webdriver()
            print('connected to the internet')
        else:
            clicked = sg.PopupOKCancel("You are not connected to the internet try to make the cmdlist offline")
            if(clicked == "OK"):
                print("ok")
            else:
                window.close 

    #Window 1 save event
    if event == 'Save':
        expname = values.get("ExpName")
        name = values.get("Name")
        date = values.get("date")
        if (values['brandg']==True and values['384wy']):
            brand = 'Greiner'
        elif(values['brands']==True and values['384wy']):
            brand = 'Sarstedt'
        
        #Needs to store the Directscript into memory for later use
        if(simulation == "1"):    
            os.chdir(path + '//V13')
        else:
            os.chdir(path + "//Desktop" + '//Directscriptmaker')
        lines = []
        if(values['384wy'] == True):
             with open('Directscript384.py') as f:
                lines = f.readlines()
        elif (values['384wy'] == False):
            with open('Directscript.py') as f:
               lines = f.readlines()
        else:
            sg.popup_ok_cancel("It seems you forgot the 384/48 Yes or NO question")
            continue

            
        #put filename = into the script
        if(simulation == "1"):
            os.chdir(path + '//V13' + '//New Direct scripts')
        elif(simulation == "1" and values['PCS'] == True): #change This @sebastian
            os.chdir(path + '//V13' + '//New Direct scripts')
        else:
            os.chdir(path + "//Desktop" + "//New Direct scripts")
        
        if(values['date'] == "" or values['ExpName']== "" or values['Name']== "" or values['Browse'] == "" and values['Selected'] == True):
            sg.Popup("Fill all fields and options", keep_on_top = True)
        else:
            listofusers = os.listdir('C://Users')
            if ("OT2L" in listofusers):
                activeOT2 = "OT2L"
            elif ("OT2R" in listofusers):
                activeOT2 = "OT2R"
            else:
                if (values["OT2L"] == True):
                    activeOT2 = "OT2L"
                else:
                    activeOT2 = "OT2R"

            #change names to needed names    
            Direct_protocol_name = date + name + expname + ' ' + activeOT2
            Truename = date + name + expname
            Truename = (Truename + '.py')
            try:
                fh = open(Truename, 'r+')
            except FileNotFoundError:
                fh = open(Truename, 'w+')
            
            #pull the files apart to make sure that we get the expected values for the metadata
            if(values['Made'] == True):
                file_name_meta = x

            else:
                file_name_meta = values['Browse']
                pathfile = file_name_meta
                file_name_meta= Path(file_name_meta)
                file_name_meta = file_name_meta.name
                file_name_meta = file_name_meta.split(".")
                file_name_metabeta = file_name_meta
                file_name_meta = file_name_meta[0]+"."+file_name_meta[1]
                
                #Move from USB or other spot to correct file spot
                filecheck1 = path + "//Desktop" + '//' + "User input (for direct)//" + file_name_meta + ".csv"
                check4 = os.path.isfile(filecheck1)
                filemove = path + "//Desktop" + '//' + "User input (for direct)//" + file_name_meta + ".csv"
                if(check4 == False and simulation == "0"):
                    print("filecheck correct")
                    shutil.copy(pathfile, filemove, follow_symlinks=True)
                    print("Copy succesfull")
                elif(check4 == True and simulation == "0"):
                    print("Check complete :)")
                else:
                    print("")

            #creates the option to create the possiblity for simulations (does not uncomment the simulation underneath the directscript)
            if(values['PCJ'] == True and simulation == "1" ):
                active_pc ="Jorn"
            elif(values['PCS'] == True and simulation == "1"):
                active_pc = "Sebastian"
            else:
                active_pc = "OT"
            
            #Check Touch tip checkbox
            if(values['TTy'] == True):
                touch_tips = "Yes"
            else:
                touch_tips = "No"
            

            # For the metadata of the script added
            with open(Truename, 'w+') as file:
                    file.write("#" + 'This protocol is made for'+ " " + activeOT2 + "\n")
                    file.write('fileName =' + "\'" + file_name_meta  + '.csv'+ "\'" "\n" + "\n")
                    file.write('pc =' + "\'" +active_pc + "\'" + "\n" + "\n")
                    if(values['384wy'] == True):
                        file.write('brand =' + "\'" + brand + "\'" + "\n" + "\n")
                    else:
                        print("")
                    file.write('touch_tips =' + "\'" + touch_tips + "\'" + "\n" + "\n")
                    file.write('#METADATA----------' "\n" +
                                'metadata = {'+"\n"+"\t"+
                                    "\'"+ 'protocolName'"\'"+":"+  "\'" + Direct_protocol_name + "\'" +","+"\n"+"\t"+
                                    "\'"+'author'"\'"+":" + "\'" +'Sebastian <sebastian.tandar@gmail.com>' +"\'" +"\'"+ 'Jorn <jornbrink@kpnmail.nl>' + "\'"+"," +"\n"+"\t"+
                                    "\'"+'description'"\'"+":" + "\'" +'96 wells plate MIC with p300 possibility'+"\'"+ "\'"+'User customized'+"\'"+","+ "\n"+"\t"+
                                    "\'"+'apiLevel'"\'"+":"+"\'" +'2.15'+"\'"+ "\n"+'}\n')
                    
                    #actually puts the script into the new file
                    for asd in lines:
                        file.write(asd)
                        
                    if(simulation == "1" and values['384wn'] == True):
                        file.write("\n" + "##########Simulation##########" + "\n" "from opentrons import simulate" + "\n" +
                                   "bep = simulate.get_protocol_api('2.15')" + "\n" + 
                                   "bep.home()" + "\n" + "run(bep)" + "\n" + "amtList, cmdList, deckMap = ReadCSV_dat(filename)" + "\n"+
                                   "for line in bep.commands():" + "\n"+"    print(line)")
                    elif(simulation == "1" and values['384wy'] == True):
                        file.write("\n" + "##########Simulation##########" + "\n" "from opentrons import simulate" + "\n" +
                                   "bep = simulate.get_protocol_api('2.15')" + "\n" + 
                                   "bep.home()" + "\n" + "run(bep)" + "\n" + "amtList, cmdList, deckMap = ReadCSV_input(fileName)" + "\n"+
                                   "for line in bep.commands():" + "\n"+"    print(line)")
                    else:
                        print ("Simulation mode inactive")
                    
           #enables the send button
            window['Send'].update(disabled=False)
            window['Send'].update(button_color = 'green')
        
            # check robot IP
            robot_ip = robotgetIPs(simulation)
    
    #Send event
    if (event == 'Send' and simulation == '0') :
        #when value 4 is true then the OT2L is selected and the script tries to send the csv to the jupyter
        if(activeOT2 == "OT2L"):
            popup_connecting()
            fileName_direc = file_name_meta  + '.csv'+ '\'' + " "
            path_to_file = "'C:/Users/cvhLa/Onedrive/Desktop/User input (for direct)/"
            file_path = path_to_file + fileName_direc
            robot_root = "'root@"
            robot_ip_ot2l = robot_ip["OT2L"]
            robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
            path_robot = robot_root+robot_ip_ot2l+robot_rest
            OT2_key = "C:/Users/cvhLa/ot2_ssh_key_OT2L "
            scp = "scp -O -i "
                
            Full_command = scp + OT2_key + file_path + path_robot
            completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
            print(completed)
            
        elif(activeOT2 == "OT2R"):
            #if user OT2R exists
            popup_connecting()
            fileName_direc = file_name_meta + '.csv'+ '\'' + " "
            path_to_file = "'C:/Users/User/Desktop/User input (for direct)/"
            file_path = path_to_file + fileName_direc
            robot_root = "'root@"
            robot_ip_ot2r = robot_ip["OT2R"]
            robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
            path_robot = robot_root+robot_ip_ot2r+robot_rest
            OT2_key_right = "C:/Users/User/ot2_ssh_key_OT2R "
            scp = "scp -O -i "
            
            #scp -i C:/Users/cvhLa/ot2_ssh_key_OT2R 'C:/Users/cvhLa/OneDrive/Desktop/Direct Protocols/README.jpg' root@169.254.212.60:/var/lib/jupyter/notebooks
            Full_command = scp + OT2_key_right + file_path + path_robot
            completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
            print(completed)
        
# =============================================================================
#       Depriciated since new computers of OT2s         
#       elif(values['BothOT2']):
#             popup_connecting()
#             fileName_direc = file_name_meta + '.csv'+ '\'' + " "
#             path_to_file = "'C:/Users/User/Desktop/User input (for direct)/"
#             file_path = path_to_file + fileName_direc
#             robot_root = "'root@"
#             robot_ip_ot2r = robot_ip["OT2R"]
#             robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
#             path_robot = robot_root+robot_ip_ot2r+robot_rest
#             OT2_key_right = "C:/Users/User/ot2_ssh_key_OT2R" + " "
#             scp = "scp -i "
#             
#             #literal command : scp -i C:/Users/cvhLa/ot2_ssh_key_OT2R 'C:/Users/cvhLa/OneDrive/Desktop/Direct Protocols/README.jpg' root@169.254.212.60:/var/lib/jupyter/notebooks
#             Full_command = scp + OT2_key_right + file_path + path_robot
#             completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
#             print(completed)
#             print('Right completed')
#             
#             time.sleep(3)
#        
#             fileName_direc = file_name_meta  + '.csv'+ '\'' + " "
#             path_to_file = "'C:/Users/User/Desktop/User input (for direct)/"
#             file_path = path_to_file + fileName_direc
#             robot_root = "'root@"
#             robot_ip_ot2l = robot_ip["OT2L"]
#             robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
#             path_robot = robot_root+robot_ip_ot2l+robot_rest
#             OT2_key = "C:/Users/User/ot2_ssh_key_OT2L "
#             scp = "scp -i "
#             
#             Full_command = scp + OT2_key + file_path + path_robot
#             completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
#             print(completed)
#             print('Job done, am I finished for today?')
#                 
# =============================================================================
        else:
            sg.Popup("Check one of the options", keep_on_top = True)
    
    #No sending needed (only simulation)
    if (event == 'Send' and simulation == '1') :
        print('normally i would send but simulation')
    
    #Window 2 events
    if(event == "Save User Inputs"):
        try: 
            if(values['Browse'] == ""):
                sg.Popup("you have not filled in the platemap")
            else:
                #gives all the values its own variable
                filepath = values['Browse']
                pmid_plate = values['PMID']
                Firstlast = values['Firstlast'].split(" ")
                Firstname = Firstlast[0]
                Lastname = Firstlast[1]
                Experiment_name = values['EXPname']
                Experiment_num = values['EXPnum']
                fullpath = os.path.abspath(filepath)
                
                #set the options for new location for downloaded Robot commands
                options = Options()
                options.set_preference("browser.download.folderList", 2)
                options.set_preference("browser.download.manager.showWhenStarting", False)
                print(simulation)
                if (simulation == "1"):
                    options.set_preference("browser.download.dir", r"C:\Users\jornb\Documents\GitHub\ot2new\Execution code for OT2\Incubator\OT2DirectprotocolCustomizer\Webdriver\Firefox download test")
                    service = Service(executable_path= path + '//Webdriver//Firefox webdriver//geckodriver.exe')
                else:
                    options.set_preference("browser.download.dir", r"C:\Users\User\Desktop\User input (for direct)")
                    service = Service(executable_path= path + "//Desktop" +'//DO NOT TOUCH THIS FOLDER (webdriver)//geckodriver.exe')
                options.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-gzip")
                #options.headless = True
                
                if(values['384p'] == True or values['48w'] == True and values['Fill'] == False and values['nFill'] == False):
                    sg.Popup("Please make sure you select if you want to have the robot fill the wells for you")
                elif(values['384p'] == False and values['48w'] == False and values['MVP'] == False and values['M9MixR'] == False and values['Checker'] == False and values['Sp'] == False):
                    sg.Popup("You seem to have not selected the method")            
                else:
                    sg.Popup("Make sure your platemap is correct")
                    window["Send to Server"].update(disabled=False)
                    window['Send to Server'].update(button_color = 'green')
        except:
            sg.Popup ("Some expected fields were not filled in or incorrectly")
        
    # Send server action    
    if(event == "Send to Server"):
        #Start webdriver with the executable_paths. If the computer changes then you need to change this in the service section and the options values
        if(values['Checker'] == True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/CQ_Plate/")
            assert "CQ Plate.title"
            x = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path)
        
        elif(values['MVP'] == True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/MVPlate/")
            assert "Multiplate MIC - OT2 Commander.title"
            x = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path)
            
        elif(values['384p'] == True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/Plate384/")
            assert "MIC - 384 Well Plate.title"
            fillingrobot = values['Fill']
            notfillingrobot = values['nFill']
            x = Filesending384(fullpath, fillingrobot, notfillingrobot, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path)
        
        elif(values['M9MixR']== True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/M9MixR/")
            assert "M9 MixR.title"
            x = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path)
        
        elif(values['Sp']== True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/SingleplateMIC/")
            assert "Singleplate MIC - OT2 Commander.title"
            x = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path)  
        
        elif(values['48w'] == True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/Plate48/")
            assert "48 Well Plate.title"
            fillingrobot = values['Fill']
            notfillingrobot = values['nFill']
            x = Filesending384(fullpath, fillingrobot, notfillingrobot, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation, path)    
        
        else:
            sg.Popup('Please select the method you want to use (the app is going to crash now)')
            window.close()