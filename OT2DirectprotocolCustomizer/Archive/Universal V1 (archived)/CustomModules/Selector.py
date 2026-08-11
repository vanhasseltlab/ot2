#protocol selector is the selector for what protocol to run on what robot.
#scope: Find robot on pc, get IP, get name robot (active), load protocol and database it
#import modules
import os
import json
import FreeSimpleGUI as sg

#functions
#function Robots
def Robotdetails(simulation):
    userpath = os.path.expanduser("~")
    robotloc = userpath + "//AppData//Roaming//Opentrons//discovery.json"
    try:
        f = open(robotloc)
        
    except:
        print("Error in the selector module -- Robotdetails")
        
    json_data = json.load(f)['robots']
        
    # initiate loop

    # get IP address for each robot
    for i in range(len(json_data)):
        # subset json data for current robot
        current_data = json_data[i]
        
        #since only one pc can controll 1 robot, remove everything that is not seen. Only simulation gives something
        status = (current_data['addresses'][0]['seen'])
        if status != False and simulation != '1':
            names = (current_data['name'])
            addresses = (current_data['addresses'][0]['ip'])
            status = (current_data['addresses'][0]['seen'])
            robot_type = current_data['health']
            robot_type = robot_type['robot_model']
        
        else:
            if status == False and simulation == "1": 
                names = (current_data['name'])
                addresses = (current_data['addresses'][0]['ip'])
                status = (current_data['addresses'][0]['seen'])
                robot_type = current_data['health']
                robot_type = robot_type['robot_model']
                
            else:
                sg.Popup("The robot is offline -- restart the robot and this program (after the robot is ready)", keep_on_top=True)
                break
    
    if "OT-3" in robot_type:
        robot_type = "Flex"
    else:
        robot_type = "OT2"
        
    return names, addresses, robot_type


def protocolfinder(simulation, robot_type):
    #first 
    userpath = os.path.expanduser("~")
    
    #simulation block
    if simulation == "1":
        #If simulation = 1 all files should be detected
        restpath = os.getcwd() +  '//Directscripts'
        #since simulation gives access to everthing this needs to be done with a bit of an interesting route
        filelist = []
        for root, dirs, files in os.walk(restpath):
            for name in files:
                filelist.append(name)
    else:
        #production version
        restpath = userpath + "//Desktop//Directscriptmaker//_internal//Directscripts//" + robot_type
        filelist = os.listdir(restpath)
     
    return filelist


def protocolselector(simulation, robot_type, selection):
    userpath = os.path.expanduser("~")
    
    if simulation == "1":
        #If simulation = 1 all files should be detected
        restpath = os.path.dirname(os.path.realpath(__file__)) +'//Directscripts//'
        #since simulation gives access to everthing this needs to be done with a bit of an interesting route
        for root, dirs, files in os.walk(restpath):
            for root, dirs, files in os.walk(restpath):
                for file in files:
                    if file == selection:
                        truename = os.path.join(root, file)
                        return truename
    else:
        #production version
        restpath = userpath + "//Desktop//_internal//Directscripts//" + robot_type + "//" + selection
        return None  # If no file matches