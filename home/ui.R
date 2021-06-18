library(shiny)
library(shinyBS)
shinyUI(fluidPage(
    
    pageWithSidebar(
        headerPanel("OT Support Home"),
        
        sidebarPanel(
            #OT2 protocol designers
            titlePanel("Protocol Designer"),
            tipify(actionButton("singleplateMIC", "MIC : \tSingleplate", width='300px',
                                onclick ="window.open('http://132.229.100.197:2222/ot2/SingleplateMIC')"),
                   "OT2 protocol processor for MIC with one plate", placement='right', trigger='hover'),
            
            tipify(actionButton("MultiplateMIC", "MIC : \tMultiplate", width='300px',
                                onclick ="window.open('http://132.229.100.197:2222/ot2/MVPlate')"),
                   "OT2 protocol processor for MIC with multiple plates", placement='right', trigger='hover'),
            
            tipify(actionButton("CheckerboardMIC", "MIC : \tCombination | Checkerboard", width='300px',
                                onclick ="window.open('http://132.229.100.197:2222/ot2/CQ_Plate')"),
                   "OT2 protocol processor for MIC with combination drugs (applicable for multiple plates)", placement='right', trigger='hover'),
            
            tipify(actionButton("M9Mixer", "Medium Mixer", width='300px',
                                onclick ="window.open('http://132.229.100.197:2222/ot2/m9MixR')"),
                   "OT2 protocol processor for medium/solution mixing", placement='right', trigger='hover'),
            
            #plate analysis
            titlePanel("Output Analysis"),
            tipify(actionButton("growthCurve", "Growth Data", width='300px',
                                onclick ="window.open('http://132.229.100.197:2222/ot2/PlateAnalysis/GrowthCurve')"),
                   "Integrates plate reader outputs into a long-format table", placement='right', trigger='hover'),
            width = 12
        ),
        mainPanel(
            
        )
    )
))
