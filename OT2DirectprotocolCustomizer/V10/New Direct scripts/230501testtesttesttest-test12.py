#This protocol is made for both OT2s
fileName ='CommandList_PMID-01_EXPID-testtest-test12_test.test.csv'

pc ='Jorn'

touch_tips ='No'

#METADATA----------
metadata = {
	'protocolName':'230501testtesttesttest-test12both OT2s',
	'author':'Sebastian <sebastian.tandar@gmail.com>''Jorn <jornbrink@kpnmail.nl>',
	'description':'96 wells plate MIC with p300 possibility''User customized',
	'apiLevel':'2.12'
}

#IMPORTS---------
import csv
import numpy as np
from math import pi
from opentrons import protocol_api
import os
import opentrons.execute

####### CUSTOM LIBRARY #########
def ReadCSV_Dat(file_name):
    #save all read info into the variable: command_list
    content_list = np.empty(8)
    with open(file_name, 'r') as file:
        cmdCSV = csv.reader(file, delimiter=',')
        for cmdRow in cmdCSV:
            content_list = np.vstack([content_list, cmdRow])
    
    #Find starting point of amount list and command list
    indices = []
    for a in range(len(content_list)):
        if(">" in content_list[a][0]):
            indices.append(a)
    
    #get amount list
    amt_list = content_list[indices[0]+1:indices[1]]
    amt_list = [x[[0, 1, 2, 4]] for x in amt_list]
    
    #get command list
    cmd_list = content_list[indices[1]+1:indices[2]]
    
    #get input deck map
    inp_deckMap = content_list[indices[2]+1:]
    
    #parse out deck location and fills
    deck_loc = [x[0] for x in inp_deckMap]
    fill = [x[1] for x in inp_deckMap]
    deck_map = {}
    for i in range(len(deck_loc)):
        if('96' in fill[i] or 'eep' in fill[i]):
            if('eep' in fill[i]):
                fill[i] = "nest_96_wellplate_2ml_deep"
            else:
                fill[i] = "nest_96_wellplate_200ul_flat"
        elif('15' in fill[i]):
            fill[i] = "opentrons_15_tuberack_falcon_15ml_conical"
        elif('50' in fill[i] or "olvent" in fill[i] or "nno" in fill[i] or "SOLVENT" in fill[i]):
            fill[i] = "opentrons_6_tuberack_falcon_50ml_conical"
        elif('dorf' in fill[i] or 'tock' in fill[i] or "1.5" in fill[i]):
            fill[i] = "opentrons_24_tuberack_nest_1.5ml_snapcap"
        elif('tip' in fill[i] or 'Tip' in fill[i]):
            fill[i] = "opentrons_96_tiprack_300ul"
        
        deck_map[deck_loc[i]] = fill[i]
        
    return amt_list, cmd_list, deck_map

def Update_Source(amt_list, cmd_line, source_well, current_transAmt):
    #get tube location
    tube_loc = [(x[0]==cmd_line[0] and x[1]==source_well) for x in amt_list]
    tube_loc = [i for i, x in enumerate(tube_loc) if x]
    
    #get source amount after dispensed
    amt_list[tube_loc[0]][3] = float(amt_list[tube_loc[0]][3]) - current_transAmt
    
    return(amt_list)

def Update_Target(amt_list, cmd_line, target_well, deck_map, current_transAmt):
    #get tube location
    tube_loc = [(x[0]==cmd_line[2] and x[1]==target_well) for x in amt_list]
    tube_loc = [i for i, x in enumerate(tube_loc) if x]
    
    #if tube is not yet registered
    if(len(tube_loc)==0):
        #check target ware type
        ware_type = deck_map[cmd_line[2]]
        
        if('96_wellplate' in ware_type):
            type_target = '96-well'
        elif('1.5ml' in ware_type):
            type_target = '1.5ml_eppendorf'
        elif('eep' in ware_type):
            type_target = '96-deepwell'
        else:
            type_target = '15ml_falcon'
            
        #generate next item
        regItem = [cmd_line[2], #target labware
                   target_well, #target slot/well
                   "New_Item", #name
                   current_transAmt, #initial amount in slot/well
                   type_target] #type of well/slot
        #append
        amt_list.append(regItem)
    else:
        #get source amount after dispensed
        amt_list[tube_loc[0]][3] = float(amt_list[tube_loc[0]][3]) + current_transAmt
    
    
    return(amt_list)

def CalTip_Aspirate(solutions_map, cmd_line, source_well):
    #get tube type
    tube_loc = [(x[0]==cmd_line[0] and x[1]==source_well) for x in solutions_map]
    tube_loc = [i for i, x in enumerate(tube_loc) if x]
    tube_type = solutions_map[tube_loc[0]][4]
    
    #get source amount after aspirated
    src_amt = float(solutions_map[tube_loc[0]][3])
    
    #if not 96 well plate
    if('96-well' not in tube_type):
        #get dimensions
        if("50" in tube_type):
            h_bot = 15.88 #mm
            r = 28.14/2 #mm
            minH = 5 #mm
            stab = 7 #mm
        elif("15" in tube_type):
            h_bot = 23.36 #mm
            r = 15.62/2 #mm
            minH = 5 #mm
            stab = 5 #mm changed from 5 to 3
        elif("1.5" in tube_type):
            #Tube Dimensions - Eppendorf
            h_bot = 38.3-20 #mm -- bot cone 17.8 (-20); h 37.8
            r = 8.7/2 #mm
            minH = 1.5
            stab = 5 #mm
        else:
            #deep well dimensions
            h_bot = 0
            r = 8.5/2 #mm
            minH = 2 #mm
            stab = 4 #mm
        
        #calculate height
        Vmax_bot = pi*r**2*h_bot/3
        
        if(src_amt>Vmax_bot):
            if(src_amt > (Vmax_bot + 50) and ("1.5" in tube_type)):
                h_tip = 1 # hard-code location for eppendorfs
            else:
                h_tip = h_bot + (src_amt - Vmax_bot)/(pi*r**2)
        else:
            if("1.5" in tube_type):
                h_tip = 0.1 # hard-code location for eppendorfs
            else:
                h_tip = ((3*src_amt*h_bot**2)/(pi*r**2))**(1/3)
    
    #if source is a 96-well plate
    else:
        #assign well dimensions
        r = 6.45/2 #mm
        minH = 1.5 #mm
        
        h_tip = src_amt/(pi*r**2)
        
    #add stab distance; place minimum height into place
    h_tip = max(h_tip-stab, minH)
    
    return(h_tip)

def CalTip_Dispense(solutions_map, cmd_line, target):
    #get tube type
    tube_loc = [(x[0]==cmd_line[2] and x[1]==target) for x in solutions_map]
    tube_loc = [i for i, x in enumerate(tube_loc) if x]
    tube_type = solutions_map[tube_loc[0]][4]
    
    #get source amount after dispensed
    src_amt = float(solutions_map[tube_loc[0]][3])
    
    #if not 96 well plate
    if('96-well' not in tube_type):
        #get dimensions
        if("50" in tube_type):
            h_bot = 15.88 #mm
            r = 28.14/2 #mm
            minH = 10 #mm
        elif("15" in tube_type):
            h_bot = 23.36 #mm
            r = 15.62/2 #mm
            minH = 10 #mm
        elif("1.5" in tube_type):
            #Tube Dimensions - Eppendorf
            h_bot = 37.8-20 #mm
            r = 8.7/2 #mm
            minH = 10
        else:
            #deep well dimensions
            h_bot = 0
            r = 8.5/2 #mm
            minH = 5 #mm
        
        #calculate height
        Vmax_bot = pi*r**2*h_bot/3
        
        if(src_amt>Vmax_bot):
            h_tip = h_bot + (src_amt - Vmax_bot)/(pi*r**2)
        else:
            h_tip = ((3*src_amt*h_bot**2)/(pi*r**2))**(1/3)
    
    #if source is a 96-well plate
    else:
        #on top of well
        h_tip = 6 #mm
        minH = 6 #mm
    
    #add extra distance; place minimum height into place
    h_tip = max(h_tip+3, minH)
    
    return(h_tip)
def GetSrcVolume(solutions_map, cmd_line, source_well):
    #get tube type
    tube_loc = [(x[0]==cmd_line[0] and x[1]==source_well) for x in solutions_map]
    tube_loc = [i for i, x in enumerate(tube_loc) if x]
    tube_type = solutions_map[tube_loc[0]][4]
    
    #get source amount after aspirated
    src_amt = float(solutions_map[tube_loc[0]][3])
    return src_amt


############# MAIN #############
def run(protocol: protocol_api.ProtocolContext):
    #READ
    global fileName #calling from global
    global pc
    try:
        if(pc =="Jorn" or pc =="jorn"):
            os.chdir("C://Users//jornb//Documents//GitHub//ot2new//Execution code for OT2//Incubator//Test User inputs" )
        elif(pc == "Sebastian" or pc== "sebastian"):
            os.chdir("C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\DownstreamProcessors")
        else:
            os.chdir('C://Users//cvhLa//OneDrive//Desktop//User input (for direct)')
    except:
        os.chdir('/var/lib/jupyter/notebooks/UserInputs')

    amtList, cmdList, deckMap = ReadCSV_Dat(fileName)
    ##############################  SETTINGS  ##############################
    dBottom = 4
    dTop = 2
    aspirateSpeed = 125
    dispenseSpeed = 125
    
    ############ LOAD LABWARES ############
    tipLocs = []
    for i in range(11):
        #load labware
        labware_name = deckMap["labware_"+str(i+1)]
        if(('empty' not in labware_name) and labware_name != 'TRASH'):
            deck_position = int(list(deckMap.keys())[i].split('_')[1])
            globals()[list(deckMap.keys())[i]] = protocol.load_labware(labware_name, deck_position)

            #if labware is a tip rack, assign number to tip location(s)
            if('tiprack' in labware_name):
                tipLocs.append(globals()[list(deckMap.keys())[i]])
                
    #load pipettes
        #single-channel
    right_pipette = protocol.load_instrument(
        'p300_single_gen2', 'right', tip_racks=tipLocs)
    right_pipette.flow_rate.aspirate=aspirateSpeed
    right_pipette.flow_rate.dispense=dispenseSpeed
    
    #Update Amount List
    for i in range(len(amtList)):
        if(float(amtList[i][3])<=50):
            amtList[i][3] = float(amtList[i][3])*1000
        #get tube type
        if('50ml' in deckMap[amtList[i][0]]):
            amtList[i] = np.append(amtList[i], '50ml_falcon')
        elif('15ml' in deckMap[amtList[i][0]]):
            amtList[i] = np.append(amtList[i], '15ml_falcon')
        else:
            amtList[i] = np.append(amtList[i], '1.5ml_eppendorf')
     
    ############ EXECUTE COMMANDS ############
    #iterate through all command lines
    current_tipID = 0 #initiate tip ID
    iterateMarker = -1
    progressMax = len(cmdList)
    
    for i in range(len(cmdList)):
        #subset
        cmdRow = cmdList[i]
        print("Progress\t: " + str(int(round(i/progressMax*100))) + "%")
        print(cmdRow)

        #parse all informations
        source_ware = cmdRow[0]
        source_well = cmdRow[1].split(', ')
        target_ware = cmdRow[2]
        target_well = cmdRow[3].split(', ')
        transfer_amt = float(cmdRow[4]) #only one transfer amount is allowed
        if(float(cmdRow[5]) > 0):
            mix_amt = max(float(cmdRow[5]), 300)
        else:
            mix_amt = 0
        tipID = int(cmdRow[6])
        
        #choose pipette -- multichannel block deprecated
        if(len(target_well)==8 and len(source_well)==8):
            #operations for multichannel pipette
            pipette = "left_pipette"
            
            #pick up tip if needed
            if(tipID != current_tipID):
                left_pipette.pick_up_tip() #pick up tip if tipID changes
                current_tipID = tipID #update tip id
            
            #Main Transfers
            source_well_h = source_well[0]
            target_well_h = target_well[0]
            remV = transfer_amt
            
            while(remV>0):
                cur_transfer = min(300, remV)
                if(remV-cur_transfer < 30 and remV-cur_transfer>0):
                    cur_transfer = cur_transfer/2
                remV = remV - cur_transfer
                
                ########### NOT ANNOTATED ############# / BLOCK1 // MULTIPIPETTE HEIGHT
                aspHs = []
                dspHs = []
                mixVols = []
                for j in range(1, 7):
                    #get mix volumes
                    mixVol = GetSrcVolume(amtList, cmdRow, source_well[j]) #not annotated
                    mixVols.append(mixVol)  #not annotated

                    #update solutions map
                    amtList = Update_Source(amtList, cmdRow, source_well[j], cur_transfer)
                    amtList = Update_Target(amtList, cmdRow, target_well[j], deckMap, cur_transfer)

                    #calculate aspirate and dispense height
                    aspH = CalTip_Aspirate(amtList, cmdRow, source_well[j])
                    dspH = CalTip_Dispense(amtList, cmdRow, target_well[j])
                    aspHs.append(aspH)
                    dspHs.append(dspH)

                    source_col = source_well[0][1:]
                    target_col = target_well[0][1:]
                    
                #selecting aspHs/dspHs
                aspHs = [i for i in aspHs if i>0]
                dspHs = [i for i in dspHs if i>0]
                mixVols = [i for i in mixVols if i>0] #not annotated

                aspHs_selected = min(aspHs)
                dspHs_selected = min(dspHs)
                
                ## temporary measure for deep wells AND 96-well plates -- fix well heights
                aspHs_selected = 3 #3 mm from bottom
                dspHs_selected = 15 #15 mm from bottom
            
                #####
                #not annotated
                if(max(mixVols)==0): 
                    mix_amt = 0
                else:
                    mixVols.append(300)
                    mix_amt = min(mixVols)
                ####################################################
                
                
                if(mix_amt==0):
                    #if no mix
                    left_pipette.transfer(cur_transfer,
                                          globals()[source_ware].wells_by_name()[source_well_h].bottom(aspHs_selected),
                                          globals()[target_ware].wells_by_name()[target_well_h].bottom(dspHs_selected),
                                          new_tip='never', disposal_volume=0)
                else:
                    #if mix
                    left_pipette.transfer(cur_transfer,
                                          globals()[source_ware].wells_by_name()[source_well_h].bottom(aspHs_selected),
                                          globals()[target_ware].wells_by_name()[target_well_h].bottom(dspHs_selected),
                                          new_tip='never', mix_before = (2, cur_transfer), disposal_volume=0)
                
                #blow out on top of the current slot
                left_pipette.blow_out(globals()[target_ware].wells_by_name()[target_well_h].bottom(dspHs_selected))
                
            #check if tip need to be trashed afterwards
            if(i == len(cmdList)-1):
                #if this is the last operation
                left_pipette.drop_tip()
            elif(int(cmdRow[6]) != int(cmdList[i+1][6])):
                #drop if different tip id is detected
                left_pipette.drop_tip()


        else:
            #IF NOT MULTI PIPETTE
            pipette = 'right_pipette'
            cur_source_well = source_well[0] #select only the first source
            
            #pick up tip if needed
            if(tipID != current_tipID):
                right_pipette.pick_up_tip() #pick up tip if tipID changes
                current_tipID = tipID #update tip id

            #iterate through all target wells
            for j in range(len(target_well)):
                if(mix_amt>0):
                    mix_amt = min(GetSrcVolume(amtList, cmdRow, cur_source_well), 300)
                
                #Main Transfers
                remV = transfer_amt
                while(remV>0):
                    #Calculate current transfer amount
                    cur_transfer = min(300, remV)
                    if(remV-cur_transfer < 30 and remV-cur_transfer>0):
                        cur_transfer = cur_transfer/2
                    remV = remV - cur_transfer
                    
                    #update solutions map
                    amtList = Update_Source(amtList, cmdRow, cur_source_well, cur_transfer)
                    amtList = Update_Target(amtList, cmdRow, target_well[j], deckMap, cur_transfer)

                    #calculate aspirate and dispense height
                    aspH = CalTip_Aspirate(amtList, cmdRow, cur_source_well)
                    dspH = CalTip_Dispense(amtList, cmdRow, target_well[j])
                    
                    #Mix boolean
                    if(mix_amt==0):
                        #if no mix
                        right_pipette.transfer(cur_transfer,
                                              globals()[source_ware].wells_by_name()[cur_source_well].bottom(aspH),
                                              globals()[target_ware].wells_by_name()[target_well[j]].bottom(dspH),
                                              new_tip='never', disposal_volume=0)
                    else:
                        #if mix
                        right_pipette.transfer(cur_transfer,
                                              globals()[source_ware].wells_by_name()[cur_source_well].bottom(aspH),
                                              globals()[target_ware].wells_by_name()[target_well[j]].bottom(dspH),
                                              new_tip='never', mix_before = (3, cur_transfer), disposal_volume=0)
                    
                    #blow out on top of the current slot
                    right_pipette.blow_out(globals()[target_ware].wells_by_name()[target_well[j]].bottom(dspH))
                    
                    if(touch_tips == "Yes"):
                        if("384" not in str(target_ware)):
                            right_pipette.touch_tip(globals()[target_ware].well_by_name()[target_well[j]], radius=0.8)
                        else:
                            right_pipette.touch_tip(globals()[target_ware].well_by_name()[target_well[j]], radius=0.5, speed = 15)
                    else:
                        print("not using touch tips")
                
            #check if tip need to be trashed afterwards
            if(i == len(cmdList)-1):
                #if this is the last operation
                right_pipette.drop_tip()
            elif(int(cmdRow[6]) != int(cmdList[i+1][6])):
                #drop if different tip id is detected
                right_pipette.drop_tip()
                
##########Simulation##########
from opentrons import simulatebep = simulate.get_protocol_api('2.12')
bep.home()
run(bep)
amtList, cmdList, deckMap = ReadCSV_dat(filename)
for line in bep.commands():
    print(line)