#INPUT PROMPT--------------
fileName = input("File Name : ")
print("Left pipette type? - 1) P1000 SingleChannel (default) ; 2=P300 Multichannel")
protocolType = input("Selection : ")

#IMPORTS---------
import os
import opentrons.execute
from opentrons import simulate

#importing custom library
#sourceLoc = "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator\\OT2ControlScript"
sourceLoc = '/var/lib/jupyter/notebooks'
os.chdir(sourceLoc)
if(protocolType==2):
    from OT2_ControllerLib import *
else:
    from OT2_ControllerLib_P1000 import *


#READING COMMAND LIST----------
#save_folder = "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator\\M9MixR"
save_folder = '/var/lib/jupyter/notebooks/User Inputs'
os.chdir(save_folder)
amtList, cmdList, deckMap = ReadCSV_Dat(fileName)

########### SIMULATE ############
#prompt simulation
simOption = input("Perform simulation? (Y/N)")
if(simOption == "Y" or simOption=="y"):
    #define protocol
    bep = simulate.get_protocol_api('2.5')
    
    #Operation
    bep.home()
    try:
        run(bep, cmdList, deckMap, amtList) 
        print("\n=====================================================\nSIMULATION COMPLETE -- EVERYTHING SEEMS OKAY (so far)\n=====================================================")
    except:
        print("Run terminated prematurely (see the last printed step)")

    #prompt continue decision
    continue_dec = input("Continue run? (Y/N)")
else:
    continue_dec = "Y"
    
########### EXECUTE ############
if(continue_dec == "Y" or continue_dec=="y"):
    input("Press ENTER to begin run. DON'T FORGET TO PREPARE THE ROBOT DECK")
    
    #reset values
    os.chdir(save_folder)
    amtList, cmdList, deckMap = ReadCSV_Dat(fileName)
    
    #define protocol
    bep = opentrons.execute.get_protocol_api('2.5')

    #Operation
    bep.home()
    run(bep, cmdList, deckMap, amtList) 