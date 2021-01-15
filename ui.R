# Upload app to shinyapps.io
# library(rsconnect)
# deployApp("/mnt/ElRaid/ifernandez/R/shinyepico_input")

# Load packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    
    # HEADER
    dashboardHeader(title = "SHINY APP"), #title
    
    # SIDEBAR
    dashboardSidebar(
        
        # MENU
        sidebarMenu(
            menuItem("Input", tabName = "input", icon = icon("table")), #input
            menuItem("Plot", tabName = "plot", icon = icon("bar-chart-o"))) #plot
    ),
    
    # BODY
    dashboardBody(
        
        # SECTIONS
        tabItems(
        
        # INPUT 
        tabItem(tabName = "input",
            fluidPage( 
              
              verticalLayout( 
              # Box 1
              box(title = "Input",
                  sidebarPanel(width = 30,   #anchura de sidebarPanel
                               fileInput("inputfile",    #nombre para acceder a este input
                                         "Introducir input", #label que aparece en la app
                                         multiple = FALSE, #solo un file hay que subir
                                         accept = ".zip" #acceptar solo cierto tipo de documentos, ¿?
                               ), 
                               uiOutput("inputbutton")
                  
                )),
              
              # Box 2
              
              box(mainPanel(
                  #withSpinner - loading animation (8 types)
                  withSpinner(dataTableOutput("datatable"), type = 7)
              ))
            ))
        ),
        
        # PLOT
        tabItem(tabName = "plot", h1("Plots"))
        )
    )))
