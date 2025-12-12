import FreeSimpleGUI as sg
import os

def Protocolctzer(scriptname, simulation, fileName, pc, brand, experimentname, activerobot, directscript):
    #this should take the directscript <- selected in the window
    #start getting pathing
    metaname = scriptname.removesuffix('.py')
    if simulation == "1":
        basepath = os.getcwd() #this will be the same as the main executable
        newpathDS = basepath + "//Newdirectscripts"
        if "qPCR" in directscript or "96wells_Flex_MIC" in directscript:
            directscriptpath = os.getcwd() +  '//Directscripts//Flex'
        else:
            directscriptpath = os.getcwd() +  '//Directscripts//OT2'
        
    else:
        userpath = os.path.expanduser("~")
        directscriptpath = userpath + "//Desktop//Directscriptmaker//_internal//Directscripts//" + pc
        newpathDS = userpath + "//Desktop//New Direct scripts"
    
    if "96wells_qPCR.py" in directscript:
        os.chdir(directscriptpath)
        
        #opening the directscript
        with open('96wells_qPCR.py') as f:
            lines=f.readlines()
        
        #change working directory to the new script spot for making the new script
        os.chdir(newpathDS)
        
        try:
            fh = open(scriptname, 'r+')
        except:
            fh= open(scriptname, 'w+')
        
        with open (scriptname, 'w+') as file:
            file.write("# This protocol is made for " + activerobot + "\n")
            file.write("fileName = '" + fileName + ".csv'" + "\n" + "\n")
            file.write("pc = '" + pc + "'" + "\n" + "\n")
            file.write("brand = 'Greiner'" + "\n" + "\n")
            file.write("touch_tips = 'Yes'" + "\n" + "\n")
            file.write("#METADATA----------" + "\n" + 
                       'metadata = {'+"\n"+"\t"+
                       "\'"+ 'protocolName'"\'"+":"+  "\'" + metaname + "\'" +","+"\n"+"\t"+
                       "'author':'Sebastian <sebastian.tandar@gmail.com>''Jorn <jornbrink@kpnmail.nl>'," + "\n"+"\t"+
                       "'description':'Opentrons Flex custom script'' User customized qPCR'}\n" + "\n")
            file.write("requirements = {'robotType': 'Flex', 'apiLevel': '2.19'}"+ "\n")
            
            for asd in lines:
                file.write(asd)
                
            if(simulation == "1"):
                file.write("\n" + "##########Simulation##########" + "\n" "from opentrons import simulate" + "\n" +
                           "bep = simulate.get_protocol_api('2.19', robot_type = 'Flex')" + "\n" + 
                           "bep.home()" + "\n" + "run(bep)" + "\n" +
                           "for line in bep.commands():" + "\n"+"\t"+"print(line)")
        return
    
    elif "96wells_Flex_MIC.py" in directscript:
            os.chdir(directscriptpath)
            
            #opening the directscript
            with open('96wells_Flex_MIC.py') as f:
                lines=f.readlines()
            
            #change working directory to the new script spot for making the new script
            os.chdir(newpathDS)
            
            try:
                fh = open(scriptname, 'r+')
            except:
                fh= open(scriptname, 'w+')
            
            with open (scriptname, 'w+') as file:
                file.write("# This protocol is made for " + activerobot + "\n")
                file.write("fileName = '" + fileName + ".csv'" + "\n" + "\n")
                file.write("pc = '" + pc + "'" + "\n" + "\n")
                file.write("touch_tips = 'No'" + "\n" + "\n")
                file.write("#METADATA----------" + "\n" + 
                           'metadata = {'+"\n"+"\t"+
                           "\'"+ 'protocolName'"\'"+":"+  "\'" + metaname + "\'" +","+"\n"+"\t"+
                           "'author':'Sebastian <sebastian.tandar@gmail.com>''Jorn <jornbrink@kpnmail.nl>'," + "\n"+"\t"+
                           "'description':'Opentrons Flex custom script ''User customized drugdilution on Flex'}\n" + "\n")
                file.write("requirements = {'robotType': 'Flex', 'apiLevel': '2.19'}"+ "\n")
                
                for asd in lines:
                    file.write(asd)
                    
                if(simulation == "1"):
                    file.write("\n" + "##########Simulation##########" + "\n" "from opentrons import simulate" + "\n" +
                               "bep = simulate.get_protocol_api('2.19', robot_type = 'Flex')" + "\n" + 
                               "bep.home()" + "\n" + "run(bep)" + "\n" +
                               "for line in bep.commands():" + "\n"+"\t"+"print(line)")
            return
        
    elif "96wells_MIC_OT2.py" in directscript:
            
        os.chdir(directscriptpath)
        
        #opening the directscript
        with open('96wells_MIC_OT2.py') as f:
            lines=f.readlines()
        
        #change working directory to the new script spot for making the new script
        os.chdir(newpathDS)
        
        try:
            fh = open(scriptname, 'r+')
        except:
            fh= open(scriptname, 'w+')
        
        with open (scriptname, 'w+') as file:
            file.write("# This protocol is made for " + activerobot + "\n")
            file.write("fileName = '" + fileName + ".csv'" + "\n" + "\n")
            file.write("pc = '" + pc + "'" + "\n" + "\n")
            file.write("touch_tips = 'No'" + "\n" + "\n")
            file.write("#METADATA----------" + "\n" + 
                       'metadata = {'+"\n"+"\t"+
                       "\'"+ 'protocolName'"\'"+":"+  "\'" + metaname + "\'" +","+"\n"+"\t"+
                       "'author':'Sebastian <sebastian.tandar@gmail.com>''Jorn <jornbrink@kpnmail.nl>'," + "\n"+"\t"+
                       "'description':'Opentrons OT2 custom script'' User customized drugdilution 96 wells on OT2'}\n" + "\n")
            file.write("requirements = {'robotType': 'OT-2', 'apiLevel': '2.15'}")
            
            for asd in lines:
                file.write(asd)
                
            if(simulation == "1"):
                file.write("\n" + "##########Simulation##########" + "\n" "from opentrons import simulate" + "\n" +
                           "bep = simulate.get_protocol_api('2.19', robot_type = 'OT2')" + "\n" + 
                           "bep.home()" + "\n" + "run(bep)" + "\n" +
                           "for line in bep.commands():" + "\n"+"\t"+"print(line)")
        return
        
    elif "384wells_48wells_MIC_OT2.py" in directscript:
        os.chdir(directscriptpath)
        
        #opening the directscript
        with open('384wells_48wells_MIC_OT2.py') as f:
            lines=f.readlines()
        
        #change working directory to the new script spot for making the new script
        os.chdir(newpathDS)
        
        try:
            fh = open(scriptname, 'r+')
        except:
            fh= open(scriptname, 'w+')
        
        with open (scriptname, 'w+') as file:
            file.write("# This protocol is made for " + activerobot + "\n")
            file.write("fileName = '" + fileName + ".csv'" + "\n" + "\n")
            file.write("pc = '" + pc + "'" + "\n" + "\n")
            file.write('brand =' + "\'" + brand + "\'" + "\n" + "\n")
            file.write("touch_tips = 'No'" + "\n" + "\n")
            file.write("#METADATA----------" + "\n" + 
                       'metadata = {'+"\n"+"\t"+
                       "\'"+ 'protocolName'"\'"+":"+  "\'" + metaname + "\'" +","+"\n"+"\t"+
                       "'author':'Sebastian <sebastian.tandar@gmail.com>''Jorn <jornbrink@kpnmail.nl>'," + "\n"+"\t"+
                       "'description':'Opentrons OT2 custom script'' User customized drugdilution 384 or 48 wells on OT2'}\n" + "\n")
            file.write("requirements = {'robotType': 'OT-2', 'apiLevel': '2.15'}")
            
            for asd in lines:
                file.write(asd)
                
            if(simulation == "1"):
                file.write("\n" + "##########Simulation##########" + "\n" "from opentrons import simulate" + "\n" +
                           "bep = simulate.get_protocol_api('2.19', robot_type = 'OT2')" + "\n" + 
                           "bep.home()" + "\n" + "run(bep)" + "\n" +
                           "for line in bep.commands():" + "\n"+"\t"+"print(line)")
        return