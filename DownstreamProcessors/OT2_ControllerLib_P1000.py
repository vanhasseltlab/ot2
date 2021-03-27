#IMPORTS---------
import csv
import numpy as np
from math import pi
from opentrons import protocol_api

####### CUSTOM LIBRARY #########
def ReadCSV_Dat(file_name):
    #save all read info into the variable: command_list
    content_list = np.empty(9)
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
            if("1000" in fill[i]):
                fill[i] = "opentrons_96_tiprack_1000ul"
            else:
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
            stab = 5 #mm
        elif("1.5" in tube_type):
            #Tube Dimensions - Eppendorf
            h_bot = 37.8-20 #mm
            r = 8.7/2 #mm
            minH = 2
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
            h_tip = h_bot + (src_amt - Vmax_bot)/(pi*r**2)
        else:
            h_tip = ((3*src_amt*h_bot**2)/(pi*r**2))**(1/3)
    
    #if source is a 96-well plate
    else:
        #assign well dimensions
        r = 6.45/2 #mm
        minH = 1.5 #mm
        
        h_tip = src_amt/(pi*r**2)
        
    #add stab distance; place minimum height into place
    h_tip = max(h_tip-5, minH)
    
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
            minH = 5 #mm
        elif("15" in tube_type):
            h_bot = 23.36 #mm
            r = 15.62/2 #mm
            minH = 4 #mm
        elif("1.5" in tube_type):
            #Tube Dimensions - Eppendorf
            h_bot = 37.8-20 #mm
            r = 8.7/2 #mm
            minH = 2
        else:
            #deep well dimensions
            h_bot = 0
            r = 8.5/2 #mm
            minH = 1 #mm
        
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

##############################   METADATA   ##############################
metadata = {
    'protocolName': 'OT2_CommandExecuter_Dec2020',
    'author': 'Sebastian T. Tandar <sebastian.tandar@gmail.com>',
    'description': 'General translator to Python 2.5 API__patch 20201221',
    'apiLevel': '2.5'
}
##############################  SETTINGS  ##############################
dBottom = 4
dTop = 2
aspirateSpeed = 100
dispenseSpeed = 100

############# MAIN #############
def run(protocol: protocol_api.ProtocolContext, cmdList, deckMap, amtList):
    #global cmdList, deckMap, amtList
    
    ############ LOAD LABWARES ############
    tipLocs_300 = []
    tipLocs_1000 = []
    for i in range(11):
        #load labware
        labware_name = deckMap["labware_"+str(i+1)]
        if(('empty' not in labware_name) and labware_name != 'TRASH'):
            deck_position = int(list(deckMap.keys())[i].split('_')[1])
            globals()[list(deckMap.keys())[i]] = protocol.load_labware(labware_name, deck_position)

            #if labware is a tip rack, assign number to tip location(s)
            if('tiprack' in labware_name):
                if("1000" in labware_name):
                    tipLocs_1000.append(globals()[list(deckMap.keys())[i]])
                else:
                    tipLocs_300.append(globals()[list(deckMap.keys())[i]])
                
    #load pipettes
        #P300 single-channel
    right_pipette = protocol.load_instrument('p300_single', 'right', tip_racks=tipLocs_300)
    right_pipette.flow_rate.aspirate=aspirateSpeed
    right_pipette.flow_rate.dispense=dispenseSpeed

        #p1000 single-channel
    left_pipette = protocol.load_instrument('p1000_single_gen2', 'left', tip_racks=tipLocs_1000)
    left_pipette.flow_rate.aspirate=aspirateSpeed
    left_pipette.flow_rate.dispense=dispenseSpeed
    
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
    progressMax = len(cmdList) #for progress bar
    
    #iterate through all command lines
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
        
    	#OPERATION
        # A | Tip Pick/Drop Decision
        #pick up decision
        current_tipID = cmdRow[8]+cmdRow[6]
        
        if( i == 0 ):
            #operation for first row; skip selection, take new tip
    	    new_tip = True
        else:
    	    #opreation for subsequent rows
    	    new_tip = (current_tipID!=prev_tipID) #decision to pick new tip; ignore warning
    	    
        prev_tipID = current_tipID #update tipID increment; ignore warning
        
        #end drop decision
        if(i == (len(cmdList)-1)): #if final row
            drop_tip = True
        else:
            nex_cmd = cmdList[i+1]
            nex_tipID = nex_cmd[8]+nex_cmd[6]
            drop_tip = (current_tipID != nex_tipID)
        
        # B | Pipette selection
        if(cmdRow[8]=='P1000'):
            current_pipette = left_pipette
            currentMax = 1000
            currentMin = 100
        else:
            current_pipette = right_pipette
            currentMax = 300
            currentMin = 30
        
        # C | Pick up tip    
        if(new_tip):
            current_pipette.pick_up_tip()
        
        # D | MAIN OPERATION
        cur_source_well = source_well[0] #select only the first source  
        for j in range(len(target_well)):
            if(mix_amt>0):
                mix_amt = min(GetSrcVolume(amtList, cmdRow, cur_source_well), currentMax)
            
            #Main Transfers
            remV = transfer_amt
            while(remV>0):
                #Calculate current transfer amount
                cur_transfer = min(currentMax, remV)
                if(remV-cur_transfer < currentMin and remV-cur_transfer>0):
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
                    current_pipette.transfer(cur_transfer,
                                          globals()[source_ware].wells_by_name()[cur_source_well].bottom(aspH),
                                          globals()[target_ware].wells_by_name()[target_well[j]].bottom(dspH),
                                          new_tip='never', disposal_volume=0)
                else:
                    #if mix
                    current_pipette.transfer(cur_transfer,
                                          globals()[source_ware].wells_by_name()[cur_source_well].bottom(aspH),
                                          globals()[target_ware].wells_by_name()[target_well[j]].bottom(dspH),
                                          new_tip='never', mix_before = (3, cur_transfer), disposal_volume=0)
                
                #blow out on top of the current slot
                current_pipette.blow_out(globals()[target_ware].wells_by_name()[target_well[j]].bottom(dspH))
            
        # E | Drop tip decision
        if(drop_tip):
            current_pipette.drop_tip()