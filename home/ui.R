library(shiny)
library(shinyBS)
shinyUI(fluidPage(
    
    pageWithSidebar(
        headerPanel("OT2 Supporting Apps"),
        
        sidebarPanel(
            #OT2 protocol designers
            titlePanel("Protocol Designer"),
            tipify(actionButton("singleplateMIC", "MIC : \tSingleplate", width='300px',
                                onclick ="window.open('https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/SingleplateMIC')"),
                   "OT2 protocol processor for MIC with one plate", placement='right', trigger='hover'),
            
            tipify(actionButton("MultiplateMIC", "MIC : \tMultiplate", width='300px',
                                onclick ="window.open('https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/MVPlate')"),
                   "OT2 protocol processor for MIC with multiple plates", placement='right', trigger='hover'),
            
            tipify(actionButton("CheckerboardMIC", "MIC : \tCombination | Checkerboard", width='300px',
                                onclick ="window.open('https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/CQ_Plate')"),
                   "OT2 protocol processor for MIC with combination drugs (applicable for multiple plates)", placement='right', trigger='hover'),
            
            tipify(actionButton("384WellPlateMIC", "MIC : \tMultiplate | 384-well", width='300px',
                                onclick ="window.open('https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/Plate384')"),
                   "OT2 protocol processor for MIC using 384-well plates (applicable for multiple plates)", placement='right', trigger='hover'),
            
            tipify(actionButton("48WellPlate", "MIC : \tMultiplate | 384-well", width='300px',
                                onclick ="window.open('https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/Plate48')"),
                   "OT2 protocol processor for 48-well culture plates (applicable for up to 3 plates)", placement='right', trigger='hover'),
            
            tipify(actionButton("M9Mixer", "Medium Mixer", width='300px',
                                onclick ="window.open('https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/M9MixR')"),
                   "OT2 protocol processor for medium/solution mixing", placement='right', trigger='hover'),
            
            #plate analysis
            titlePanel("Output Analysis"),
            tipify(actionButton("growthCurve", "Growth Data", width='300px',
                                onclick ="window.open('https://vanhasseltlab.lacdr.leidenuniv.nl/ot2/PlateAnalysis/GrowthCurve')"),
                   "Integrates plate reader outputs into a long-format table", placement='right', trigger='hover'),
            
            #User Guide download buttons
            titlePanel("Guides"),
            downloadButton("downloadRobotQuickGuide", label = "OT2 Quick Guide", width='600px'),
            downloadButton("downloadRobotGuide", label = "OT2 General Guide", width='600px'),
            downloadButton("downloadServerGuide", label = "Web Server General Guide", width='600px'),
            
            width = 12
        ),
        mainPanel(
            
        )
    )
))
