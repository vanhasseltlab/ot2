import PySimpleGUI as sg
import os
import shutil
import subprocess
import json
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import time
from pathlib import Path

x = "NA"
str(x)

def Mainwindow():
    listofusers = os.listdir('C://Users')
    global x
    #print(listofusers)
    if ("cvhLa" in listofusers):
        #print("nonsimulation mode")
        simulation = "0"
    else:
        simulation = "1"
        #print("simulation mode active")
    
    # create the layout with buttons and text
    if (simulation == "1"):
        layout = [
            [sg.B("Make command list"), sg.Button("Refresh")],
            [sg.T("Please provide information about the OT2 run you want to do")],
            [sg.T('Select a file', size=(10,1)), sg.FileBrowse(file_types= (('CSV Files', '*.csv'),))], 
            [sg.T('Or use the one you just send'), sg.T(str(x), key='-importfilename-')],                                   #Browse #values[importfilename]
            [sg.R("Selected", "group 3", key="Miep1", default = True), sg.R("Made", "group 3", key= "Miep2")],              #values[Miep & Miep2]
            [sg.T('Experiment Name', size=(15,1)), sg.I(key='expname')],                                                    #values[expname]       
            [sg.T('Your Name', size=(15, 1)), sg.I(key='name')],                                                            #values[name]   
            [sg.T('Date (yymmdd)', size=(15, 1)), sg.I('', key='-date-')],                                                  #values[date]
            [sg.T('Which OT2 do you want to use?')],                       
            [sg.R('OT2L', "group 1"), sg.R('OT2R', "group 1"), sg.R('Both OT2', "group 1", key='-both OT2s-')],             #Values0&1
            [sg.T("What pc is it running on?")],
            [sg.R('Jorn', "group 2"), 
             sg.R('Sebastian', "group 2"), sg.R('OT', "group 2")],                                                          #Values[2/3/4]
            [sg.T("Do you want to use touchtips? (run will be longer)")],
            [sg.R('Yes', 'group3'), sg.R('No', 'group3')],                                                                  #Values[5/6]
            [sg.T("384 wells?")],
            [sg.R('Yes', 'group4'), sg.R('No', 'group4', default=True)],                                                    #Values[7/8]
            [sg.B("Save", s=16), sg.B("Send", disabled=True, s=16), sg.Push(), sg.B("Close", s= 16, button_color = "tomato")]
        ]
    else:
        layout = [
            [sg.B("Make command list"), sg.Button("Refresh")],
            [sg.Text("Please provide information about the OT2 run you want to do")],
            [sg.Text('Select a file', size=(18,1)), sg.FileBrowse(file_types= (('CSV Files', '*.csv'),))],                  #Browse
            [sg.Text('Or use the one you just send'), sg.Text(str(x), key='-importfilename-')],                             #importfilename
            [sg.Radio("Selected", "group 3", key="Miep1", default = True), sg.Radio("Made", "group 3", key= "Miep2")],      #miep1&2
            [sg.Text('Experiment Name', size=(18,1)), sg.Input(key = 'expname')],                                           #Expname
            [sg.Text('Your Name', size=(18, 1)), sg.Input(key = 'name')],                                                   #Name
            [sg.Text('Date (yymmdd)', size=(18, 1)), sg.InputText('', key='-date-')],                                       #values[date]
            [sg.Text('Which OT2 do you want to use?')],                       
            [sg.Radio('OT2L', "group 1"), sg.Radio('OT2R', "group 1"), sg.R('Both OT2', "group 1", key='-both OT2s-')],     #Values0&1
            [sg.Text("What pc is it running on?")],
            [sg.R('Jorn', "group 2", disabled = True, default = False), 
             sg.R('Sebastian', "group 2", disabled = True, default = False), sg.R('OT', "group 2", default = True)],        #Values2/3/4
            [sg.Text("Do you want to use touchtips? (run will be longer (not entirely tested yet only active on 384))")],
            [sg.Radio('Yes', 'group3', disabled = True), sg.Radio('No', 'group3', default = True, disabled = True)],        #Values[5/6]
            [sg.Text("384 wells?")],
            [sg.Radio('Yes', 'group4'), sg.Radio('No', 'group4', default = True)],                                          #Values[7/8]   
            [sg.Button("Save", s=16), sg.Button("Send", disabled=True, s=16), sg.Push(), sg.Button("Close", s= 16, button_color = "tomato")]
            ]
        
    return sg.Window("Opentron direct protocol maker", layout, finalize = True), simulation

def Webinteraction():
    layout = [
        [sg.Text("Please provide all information below")],
        [sg.Text("Choose your file: "), sg.FileBrowse()],                                                   #values['Browse']
        [sg.Text("Platemap PMID"), sg.InputText('', size=(56, 1))],                                         #values[0]
        [sg.Text("Your name (First Last)"), sg.InputText('', size=(50, 1))],                                #values[1]
        [sg.Text("Experiment Name"), sg.InputText('', size=(54, 1))],                                       #values[2]
        [sg.Text("Experiment number"), sg.InputText('', size=(53, 1))],                                     #values[3]
        [sg.Text("What type of experiment are you going to do?")],
        [sg.Radio("Checkerboard", "group1"), 
         sg.Radio("Multiplate MIC", "group1"), 
         sg.Radio("384 well plate", "group1"), 
         sg.Radio("M9MixR", "group1"),
         sg.Radio("SingleplateMIC", "group1")],                                                             #values4/5/6/7/8
        [sg.Text("Do you want to fill outer wells in robot? (384 plate only)")],
        [sg.Radio("Yes", "group2"), sg.Radio("No", "group2")],                                              #values9/10
        [sg.Button("Save User Inputs", s= 16), sg.Button("Send to Server", s= 16, disabled = True), sg.Push(), sg.Button("Close", s= 16, button_color = "tomato")]
        ]
    return sg.Window("Webdriver", layout, finalize= True)

#File sending function
def Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation):
    #it searches per id most of the time if possible
    fileinput = driver.find_element(By.ID, "file").send_keys(fullpath)
    Plate_Map_ID = driver.find_element(By.ID, "pmid").send_keys(values[0])
    Firstname_file = driver.find_element(By.ID, "f_name").send_keys(Firstname)
    Lastname_file = driver.find_element(By.ID, "l_name").send_keys(Lastname)
    Experiment_name_file = driver.find_element(By.ID, "exp_name").send_keys(Experiment_name)
    Experiment_num_file = driver.find_element(By.ID, "exp_num").send_keys(Experiment_num)
    #these sleeptimers are so it doesnt just try to download or click something while this is not possible or might give some problems
    time.sleep(2)
    confirmupload = driver.find_element(By.ID, "do").click()
    time.sleep(3)
    textFromDiv = driver.find_element(By.XPATH, "//div[@class='shiny-text-output shiny-bound-output']").text
    file_name = "CommandList_" + textFromDiv + ".csv"
    
    if(simulation == "1"):
        path_to_cmd = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + file_name
    else:
        path_to_cmd = "C://Users//cvhLa//Onedrive//Desktop//User input (for direct)" + '//' + file_name
    
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
        time.sleep(3)
        check2 = os.path.isfile(path_to_cmd)
        oldpath = "C://Users//cvhLa//Downloads" + '//' + file_name
        if (check2 == False):
            try:
                os.replace(oldpath, path_to_cmd)
            except:
                pass
        
        RSP = "Robothandler_" + textFromDiv + ".xlsx"
        if(simulation == "1"):
            path_to_RSP = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + RSP
        else:
            path_to_RSP = "C://Users//cvhLa//Onedrive//Desktop//User input (for direct)" + '//' + RSP
        check3 = os.path.isfile(path_to_RSP)
        if(check2 == False):
            DownloadRobot = driver.find_element(By.ID, "d_OT2").click()
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = "C://Users//cvhLa//Downloads//" + RSP
        elif(check3 == False):
            DownloadSetup = driver.find_element(By.ID, "guide").click()
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = "C://Users//cvhLa//Downloads//" + RSP
        else:
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = "C://Users//cvhLa//Downloads//" + RSP
        try:
            os.replace(path_to_RSP, new_pathRSP)
        except:
            pass
        
        sg.Popup("Files should be downloaded(click OK when ready to close browser and continue)", keep_on_top = True)
        global x
        x = "CommandList_" + textFromDiv
        driver.close()
    return

def Filesending384(fullpath, fillingrobot, notfillingrobot, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation):
    fileinput = driver.find_element(By.ID, "file").send_keys(fullpath)
    if (fillingrobot == True):
        element = driver.find_element(By.ID, "fillOuter")
        element.click()
    else:
        print("no outerwell will be filled by OT") 
    
    fileinput = driver.find_element(By.ID, "file").send_keys(fullpath)
    Plate_Map_ID = driver.find_element(By.ID, "pmid").send_keys(values[0])
    Firstname_file = driver.find_element(By.ID, "f_name").send_keys(Firstname)
    Lastname_file = driver.find_element(By.ID, "l_name").send_keys(Lastname)
    Experiment_name_file = driver.find_element(By.ID, "exp_name").send_keys(Experiment_name)
    Experiment_num_file = driver.find_element(By.ID, "exp_num").send_keys(Experiment_num)
    
    #these sleeptimers are so it doesnt just try to download or click something while this is not possible or might give some problems
    time.sleep(2)
    confirmupload = driver.find_element(By.ID, "do").click()
    time.sleep(3)
    textFromDiv = driver.find_element(By.XPATH, "//div[@class='shiny-text-output shiny-bound-output']").text
    file_name = "CommandList_" + textFromDiv + ".csv"
    
    if(simulation == "1"):
        path_to_cmd = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + file_name
    else:
        path_to_cmd = "C://Users//cvhLa//Onedrive//Desktop//User input (for direct)" + '//' + file_name
    
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
        time.sleep(3)
        check2 = os.path.isfile(path_to_cmd)
        oldpath = "C://Users//cvhLa//Downloads" + '//' + file_name
        if (check2 == False):
            try:
                os.replace(oldpath, path_to_cmd)
            except:
                pass
        RSP = "Robothandler_" + textFromDiv + ".xlsx"
        if(simulation == "1"):
            path_to_RSP = "C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox download test" + '//' + RSP
        else:
            path_to_RSP = "C://Users//cvhLa//Onedrive//Desktop//User input (for direct)" + '//' + RSP
        check3 = os.path.isfile(path_to_RSP)
        if(check2 == False):
            DownloadRobot = driver.find_element(By.ID, "d_OT2").click()
        elif(check3 == False):
            DownloadSetup = driver.find_element(By.ID, "guide").click()
        else:
            if(simulation == "1"):
                new_pathRSP = "C://Users//jornb//Downloads//" + RSP
            else:
                new_pathRSP = "C://Users//cvhLa//Downloads//" + RSP
            
        try:
            os.replace(path_to_RSP, new_pathRSP)
        except:
            pass

        sg.Popup("Files should be downloaded(click OK when ready to close browser and continue)", keep_on_top = True)
        global x
        x = "CommandList_" + textFromDiv
        driver.close()
    return


#activate first window
window1, window2 = Mainwindow(), None

    # create popup that is to inform user
def popup_connecting():
    clicked = sg.PopupOKCancel("This is going to take a bit (might not respond while sending file)\n",
             "Please press OK to continue")
    if (clicked =="OK"):
        return
    else:
       window.close() 
    return

listofusers = os.listdir('C://Users')
values = []
if ("cvhLa" in listofusers):
    simulation = "0"
else:
    simulation = "1"

#FUNCTIONS----------

def getIPs():
    # load data
    if (simulation == "1"):
        f = open('C://Users//jornb//AppData//Roaming//Opentrons//discovery.json')
    else:
        f = open('C://Users//cvhLa//AppData//Roaming//Opentrons//discovery.json')
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
        
#creates loop that activates the tracking of events and values in the gui
while True:
    window, event, values = sg.read_all_windows()
    
    #Stops everything when user uses the button cancel or closes the window
    if event =="Close" or event == sg.WIN_CLOSED:
        window.close()
        if window == window2:
            window2 == None
        elif window == window1:
            break
        #small little problem --> this will break everything and stop it from reacting but its the best i can do for now
        
    if event == 'Refresh':
        try:
            if(values['Miep1']== True and str(x) == "NA"):
                ok = sg.PopupOKCancel("This function does not do anything when you havent made a command list through this program or you havent selected made" '\n' "(underneath the browse button)")
                if(ok =="OK"):
                    print('Try again')
                else:
                    print('try again')
            elif(values['Miep1'] == True and str(x) != "NA"):
                tryagain = sg.PopupOKCancel("Dont forget to set selected --> made underneath Browse")
            else:
                x = str(x)
                window['-importfilename-'].update(str(x))
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
                    window['expname'].update(expnamestr)
                    window['name'].update(namestr)
                except:
                    smting = sg.PopupOKCancel("Name can not be pasted into areas, check if file is ok and fill in ourself")
        except:
            ok = sg.PopupOKCancel("This function does not do anything when you havent made a command list through this program or you havent selected made" '\n' "(underneath the browse button)")
            
    #If user wants to make a commandlist open new window
    if event == 'Make command list':
        window2 = Webinteraction()
    
    #If user sets save the file is found and prepared for making the script
    if event == 'Save':
        #Needs to store the Directscript into memory for later use
        if(simulation == "1"):    
            os.chdir("C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//V8")
        else:
            os.chdir("C://Users//cvhLa//OneDrive//Desktop//Direct Protocols")
        lines = []
        if(values[7] == True):
         with open('Directscript384.py') as f:
            lines = f.readlines()
        else:
            with open('Directscript.py') as f:
               lines = f.readlines()

            
        #put filename = into the script
        if(simulation == "1"): #and values[2] == True):
            os.chdir("C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//V8//New Direct scripts")
        elif(simulation == "1" and values[3] == True): #change This @sebastian
            os.chdir("C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//V8//New Direct scripts")
        else:
            os.chdir("C://Users//cvhLa//OneDrive//Desktop//New Direct scripts")
        print(values)
        
        if(values['-date-'] == "" or values['expname']== "" or values['name']== "" or values['Browse'] == "" and 'Miep1' == True):
            sg.Popup("Fill all fields and options", keep_on_top = True)
        else:
            if (values[0] == True):
                activeOT2 = "OT2L"
            elif(values['-both OT2s-']):
                activeOT2 = "both OT2s"
            else:
                activeOT2 = "OT2R"
            
            #change names to needed names    
            Direct_protocol_name = values['-date-']+values['name']+values['expname']+ activeOT2
            Truename = values['-date-']+values['name']+values['expname']
            Truename = (Truename+ '.py')
            try:
                fh = open(Truename, 'r+')
            except FileNotFoundError:
                fh = open(Truename, 'w+')
            
            #pull the files apart to make sure that we get the expected values for the metadata
            if(values['Miep2'] == True):
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
                Miep = "C:\\Users\\cvhLa\\OneDrive\\Desktop\\User input (for direct)\\" + file_name_meta + ".csv"
                check4 = os.path.isfile(Miep)
                print(check4)
                if(check4 == False and simulation == "0"):
                    shutil.copy(pathfile, Miep, follow_symlinks=True)
                    print("print copy succes full")
                else:
                    print("simulation")

            #creates the option to create the possiblity for simulations (does not uncomment the simulation underneath the directscript)
            if(values[2] == True and simulation == "1" ):
                active_pc ="Jorn"
            elif(values[3] == True and simulation == "1"):
                active_pc = "Sebastian"
            else:
                active_pc = "OT"
            
            #Check Touch tip checkbox
            if(values[5] == True):
                touch_tips = "Yes"
            else:
                touch_tips = "No"
            

            # For the metadata of the script added
            with open(Truename, 'w+') as file:
                    file.write("#" + 'This protocol is made for'+ " " + activeOT2 + "\n")
                    file.write('fileName =' + "\'" + file_name_meta  + '.csv'+ "\'" "\n" + "\n")
                    file.write('pc =' + "\'" +active_pc + "\'" + "\n" + "\n")
                    file.write('touch_tips =' + "\'" + touch_tips + "\'" + "\n" + "\n")
                    file.write('#METADATA----------' "\n" +
                                'metadata = {'+"\n"+"\t"+
                                    "\'"+ 'protocolName'"\'"+":"+  "\'" + Direct_protocol_name + "\'" +","+"\n"+"\t"+
                                    "\'"+'author'"\'"+":" + "\'" +'Sebastian <sebastian.tandar@gmail.com>' +"\'" +"\'"+ 'Jorn <jornbrink@kpnmail.nl>' + "\'"+"," +"\n"+"\t"+
                                    "\'"+'description'"\'"+":" + "\'" +'96 wells plate MIC with p300 possibility'+"\'"+ "\'"+'User customized'+"\'"+","+ "\n"+"\t"+
                                    "\'"+'apiLevel'"\'"+":"+"\'" +'2.12'+"\'"+ "\n"+'}\n')
                    #actually puts the script into the new file
                    for asd in lines:
                        file.write(asd)
                        
                    if(simulation == "1"):
                        file.write("\n" + "##########Simulation##########" + "\n" "from opentrons import simulate" +
                                   "bep = simulate.get_protocol_api('2.12')" + "\n" + 
                                   "bep.home()" + "\n" + "run(bep)" + "\n" + "amtList, cmdList, deckMap = ReadCSV_dat(filename)" + "\n"+
                                   "for line in bep.commands():" + "\n"+"    print(line)")
                    else:
                        print ("Simulation mode inactive")
                    
           #enables the send button
            window['Send'].update(disabled=False)
        
            # check robot IP
            robot_ip = getIPs()
    
    if event == 'Send':
        #when value 4 is true then the OT2L is selected and the script tries to send the csv to the jupyter
        if(values[0] == True):
            popup_connecting()
            fileName_direc = file_name_meta  + '.csv'+ '\'' + " "
            path_to_file = "'C:/Users/cvhLa/Onedrive/Desktop/User input (for direct)/"
            file_path = path_to_file + fileName_direc
            robot_root = "'root@"
            robot_ip_ot2l = robot_ip["OT2L"]
            robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
            path_robot = robot_root+robot_ip_ot2l+robot_rest
            OT2_key = "C:/Users/cvhLa/ot2_ssh_key_OT2L "
            scp = "scp -i "
                
            Full_command = scp + OT2_key + file_path + path_robot
            completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
            print(completed)
            
        elif(values[1] == True):
            #when value 4 is true then the OT2R is selected and the script tries to send the csv to the jupyter
            popup_connecting()
            fileName_direc = file_name_meta + '.csv'+ '\'' + " "
            path_to_file = "'C:/Users/cvhLa/Onedrive/Desktop/User input (for direct)/"
            file_path = path_to_file + fileName_direc
            robot_root = "'root@"
            robot_ip_ot2r = robot_ip["OT2R"]
            robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
            path_robot = robot_root+robot_ip_ot2r+robot_rest
            OT2_key_right = "C:/Users/cvhLa/ot2_ssh_key_OT2R "
            scp = "scp -i "
            
            #scp -i C:/Users/cvhLa/ot2_ssh_key_OT2L 'C:/Users/cvhLa/OneDrive/Desktop/Direct Protocols/README.jpg' root@169.254.212.60:/var/lib/jupyter/notebooks
            Full_command = scp + OT2_key_right + file_path + path_robot
            completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
            print(completed)
        
        elif(values['-both OT2s-']):
            popup_connecting()
            fileName_direc = file_name_meta + '.csv'+ '\'' + " "
            path_to_file = "'C:/Users/cvhLa/Onedrive/Desktop/User input (for direct)/"
            file_path = path_to_file + fileName_direc
            robot_root = "'root@"
            robot_ip_ot2r = robot_ip["OT2R"]
            robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
            path_robot = robot_root+robot_ip_ot2r+robot_rest
            OT2_key_right = "C:/Users/cvhLa/ot2_ssh_key_OT2R "
            scp = "scp -i "
            
            #scp -i C:/Users/cvhLa/ot2_ssh_key_OT2L 'C:/Users/cvhLa/OneDrive/Desktop/Direct Protocols/README.jpg' root@169.254.212.60:/var/lib/jupyter/notebooks
            Full_command = scp + OT2_key_right + file_path + path_robot
            completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
            print(completed)
            print('Right completed')
            
            time.sleep(3)
       
            fileName_direc = file_name_meta  + '.csv'+ '\'' + " "
            path_to_file = "'C:/Users/cvhLa/Onedrive/Desktop/User input (for direct)/"
            file_path = path_to_file + fileName_direc
            robot_root = "'root@"
            robot_ip_ot2l = robot_ip["OT2L"]
            robot_rest = ":/var/lib/jupyter/notebooks/UserInputs'"
            path_robot = robot_root+robot_ip_ot2l+robot_rest
            OT2_key = "C:/Users/cvhLa/ot2_ssh_key_OT2L "
            scp = "scp -i "
            
            Full_command = scp + OT2_key + file_path + path_robot
            completed = subprocess.run(["powershell", "-Command", Full_command], capture_output=True)
            print(completed)
            print('Job done, am I finished for today?')
                
        else:
            sg.Popup("Check one of the options", keep_on_top = True)
    
    #events of the webdriver side    
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
                print(simulation)
                if (simulation == "1"):
                    options.set_preference("browser.download.dir", r"C:\Users\jornb\Documents\GitHub\ot2new\Execution code for OT2\Incubator\OT2DirectprotocolCustomizer\Webdriver\Firefox download test")
                    service = Service(executable_path='C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//OT2DirectprotocolCustomizer//Webdriver//Firefox webdriver//geckodriver')
                else:
                    options.set_preference("browser.download.dir", r"C:\Users\cvhLa\OneDrive\Desktop\User input (for direct)")
                    service = Service(executable_path='C://Users//cvhLa//OneDrive//Desktop//DO NOT TOUCH THIS FOLDER (webdriver)//geckodriver')
                options.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-gzip")
                
                
                
                if(values[6] == True and values[9] == False and values[10] == False):
                    sg.Popup("Please make sure you select if you want to have the robot fill the wells for you")
                elif(values[6] == False and values[7] == False and values[4] == False and values[5] == False and values[8] == False):
                    sg.Popup("You seem to have not selected the method")            
                else:
                    sg.Popup("Make sure your platemap is correct")
                    window["Send to Server"].update(disabled=False)
        except:
            sg.Popup ("Some expected fields were not filled in or incorrectly")
        
        
    elif(event == "Send to Server"):
        #service path is for the gecko executable (needs changed if used on the real pc)
        if(values[4] == True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/CQ_Plate/")
            assert "CQ Plate.title"
            filesending = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation)
        
        elif(values[5] == True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/MVPlate/")
            assert "Multiplate MIC - OT2 Commander.title"
            filesending = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation)
            
        elif(values[6] == True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/Plate384/")
            assert "MIC - 384 Well Plate.title"
            fillingrobot = values[9]
            notfillingrobot = values[10]
            filesending = Filesending384(fullpath, fillingrobot, notfillingrobot, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation)
        
        elif(values[7]== True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/M9MixR/")
            assert "M9 MixR.title"
            filesending = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation)
        
        elif(values[8]== True):
            driver = webdriver.Firefox(service = service, options = options)
            driver.get("https://ot2.lacdr.leidenuniv.nl/ot2/SingleplateMIC/")
            assert "Singleplate MIC - OT2 Commander.title"
            filesending = Filesending(fullpath, pmid_plate, Firstname, Lastname, Experiment_name, Experiment_num, simulation)
            
        else:
            sg.Popup('Please select the method you want to use (the app is going to crash now)')
            window.close()
        
window.close()  
