#this is the main installer used as a module 


#needed to do:
    #Make directories:
        #Directscriptmaker (complet copy)
        #Webdriver ( //Desktop//DO NOT TOUCH THIS FOLDER (webdriver)//geckodriver.exe)
            #make directory DO NOT TOUCH THIS FOLDER (webdriver)
            #add geckodriver.exe
        #Userinputs ( \Desktop\User input (for direct))
        #Output directory (//New Direct scripts)
        
# imports
import os
import shutil
import FreeSimpleGUI as sg
import datetime

def installwindow():
    layout = [
        [sg.T("Installation setup started, want to Install? Uninstall? Upgrade?")],
        [sg.T("Ready for setup?")],
        [sg.B("Install/upgrade", button_color="green"), sg.P(), sg.B("Uninstall", button_color="yellow"), sg.P(), sg.B("Cancel", button_color = "tomato")],
    ]
    return sg.Window('Installation window', layout, finalize=True)

def installprogresswindow(step):
    layout = [
        [sg.T("Select installation step:"), sg.R("Upgrade", 'group7', key = "upgrade"), sg.R("Full install", 'group7', key = "Fintall", default=True)],
        [sg.T("Current step"), sg.T(step, key='-step-')],
        [sg.B("GO", button_color="Green"), sg.P(), sg.B("Cancel installation", button_color='tomato')]
    ]
    return sg.Window('Progress window', layout, finalize=True)


def setup(upgrade, fullinstall):
    userpath = os.path.expanduser("~") + "//Desktop"
    os.chdir(userpath)
    
    if upgrade == True:
        os.makedirs("Previous version", exist_ok= True)
        os.chdir(userpath + "//Previous version")
        upgradedate = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        upgradedate = upgradedate.replace(":", "_")
        upgradedate = upgradedate.replace("-", "_")
        move = userpath + "//Previous version//" + upgradedate
        dirpath = os.path.dirname(os.path.realpath(__file__))
        DSMpath = os.path.dirname(os.path.dirname(dirpath))
        os.makedirs(upgradedate, exist_ok= True)
        shutil.copytree(DSMpath, move, dirs_exist_ok=True)
        os.chdir(userpath)
        rm = userpath + "//Directscriptmaker"
        shutil.rmtree(rm)
        
    os.makedirs("Directscriptmaker", exist_ok=True)
    DSMloc = userpath + "//Directscriptmaker"

    os.makedirs("DO NOT TOUCH THIS FOLDER (webdriver)", exist_ok=True)
    Driverloc = userpath + "//DO NOT TOUCH THIS FOLDER (webdriver)"

    os.makedirs("User input (for direct)", exist_ok=True)
    cmdloc = userpath + "//User input (for direct)"

    os.makedirs("New Direct scripts", exist_ok=True)
    NDSloc = userpath + "//New Direct scripts"

    #copy the Directscriptmaker to the folder.
    step = "Step 2: Installing DSM"
    window2['-step-'].update(step)
    dirpath = os.path.dirname(os.path.realpath(__file__))
    DSMpath = os.path.dirname(os.path.dirname(dirpath))
    shutil.copytree(DSMpath, DSMloc, dirs_exist_ok=True)

    #copy items to the webdriver
    step = "Step 3: Installing webdriver"
    window2['-step-'].update(step)
    driverpath = os.path.dirname(dirpath)
    driver = driverpath + "//Webdriver"
    shutil.copytree(driver, Driverloc, dirs_exist_ok=True)
    
    #done
    step = "Installed :)"
    window2['-step-'].update(step)
    
    window.close()
    return

def uninstall():
    userpath = os.path.expanduser("~") + "//Desktop"
    os.chdir(userpath)
    
    #just some destinations that need to be gone
    DSMloc = userpath + "//Directscriptmaker"
    Driverloc = userpath + "//DO NOT TOUCH THIS FOLDER (webdriver)"
    NDSloc = userpath + "//New Direct scripts"
    cmdloc = userpath + "//User input (for direct)"
    
    shutil.rmtree(DSMloc, ignore_errors= True)
    shutil.rmtree(Driverloc, ignore_errors= True)
    shutil.rmtree(DSMloc, ignore_errors= True)
    shutil.rmtree(Driverloc, ignore_errors= True)
    return
    

sg.theme("Gray Gray Gray")
window1 = installwindow()
window1_active = True
window2_active = False

while True:
    window, event, values = sg.read_all_windows()

    if event == sg.WIN_CLOSED or event == 'Cancel' or event == 'Cancel installation':
        window.close()
        if window == window1:
            window1_active = False
        if window2_active and window == window2:
            window2_active = False
        if not window1_active and not window2_active:
            break

    if window == window1 and event == "Uninstall":
        try:
            uninstall()
            print("uninstalled")
            
        except Exception as e:
                sg.popup("Something broke:", str(e))
        
        
    if window == window1 and event == "Install/upgrade":
        window1.close()
        window1_active = False

        step = "Step 1: Selecting install/upgrade"
        window2 = installprogresswindow(step)
        window2_active = True



    if window == window2 and event == "GO":
        upgrade = values.get("upgrade")
        fullinstall = values.get("Finstall")
        try:
            setup(upgrade, fullinstall)
        except Exception as e:
            sg.popup("Something broke:", str(e))