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
                  sidebarPanel(width = 30, 
                               fileInput("inputfile",
                                         "Introducir input",
                                         multiple = FALSE,
                                         accept = ".zip"
                               ), 
                               uiOutput("inputbutton")
                  
                )),
              
              # Box 2
              
              box(mainPanel(
                  width = 9,
                  withSpinner(DT::DTOutput("datatable"))
              )),
              
              conditionalPanel("typeof output.datatable != 'undefined'",
                               box(
                               selectInput("select_input_samplenamevar", "", c()),
                               selectInput("select_input_groupingvar", "", c()),
                               selectInput("select_input_donorvar", "", c()),
                               pickerInput(
                                   inputId = "selected_samples",
                                   label = "",
                                   choices = c(),
                                   options = list(
                                       `actions-box` = TRUE,
                                       size = 10,
                                       `selected-text-format` = "count > 3"
                                   ),
                                   multiple = TRUE
                               ),
                               actionButton("button_input_next", "Continue")))
            ))
        ),
        
        # PLOT
        tabItem(tabName = "plot", h1("Plots"))
        )
    )))
