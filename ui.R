# Upload app to shinyapps.io
# library(rsconnect)
# deployApp("/mnt/ElRaid/ifernandez/R/shinyepico_input")

# AVAILABLE METHODS
norm_options <- c(
    "Raw",
    "Illumina",
    "Funnorm",
    "Noob",
    "SWAN",
    "Quantile",
    "Noob+Quantile"
)

hclust_methods <- c(
    "single",
    "complete",
    "average",
    "mcquitty",
    "median",
    "centroid"
)


# Load packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    
    # HEADER
    dashboardHeader(title = "SHINY APP"), #title
    
    # SIDEBAR
    dashboardSidebar(
        
        # MENU
        sidebarMenu(id = "menu",
            menuItem("Input", tabName = "input", icon = icon("table")), #input
            menuItem("Normalization", tabName = "normalization", icon = icon("bar-chart-o"))) #plot
    ),
    
    # BODY
    dashboardBody(
        
        # SECTIONS
        tabItems(
        
        # INPUT 
        tabItem(tabName = "input",
            fluidRow( 

              # Box 1: Upload file
              sidebarPanel(width = 12, 
                           fileInput("fileinput_input",
                                     "Introducir input",
                                     multiple = FALSE,
                                     accept = ".zip"
                           ), 
                           uiOutput("ui_button_input_load")),
              
              # Box 3: Select input
              conditionalPanel("typeof output.samples_table != 'undefined'",
                               box(width = 3,
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
                               actionButton("button_input_next", "Continue"))),
              
              # Box 2: Table
              box(width = 9,
                  withSpinner(DT::DTOutput("samples_table")))
              
            )
        ),
        
        # NORMALIZATION
        tabItem(tabName = "Normalization", 
            fluidPage(
                verticalLayout(
                    # Box1
                    sidebarPanel(
                        selectInput("select_minfi_norm", "Select Normalization", norm_options),
                        div(
                            margin_left = "50px",
                            switchInput(
                                inputId = "select_minfi_dropcphs",
                                label = "Drop CpHs",
                                labelWidth = "fit",
                                value = TRUE,
                                inline = TRUE
                            ),
                            
                            switchInput(
                                inputId = "select_minfi_dropsnps",
                                label = "Drop SNPs",
                                labelWidth = "fit",
                                value = TRUE,
                                inline = TRUE
                            )
                        ),
                        
                        conditionalPanel(
                            "input.select_minfi_dropsnps",
                            sliderInput(
                                inputId = "slider_minfi_maf",
                                label = "Minimum MAF to filter",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0,
                                width = "75%"
                            )
                        ),
                        
                        switchInput(
                            inputId = "select_minfi_chromosomes",
                            label = "Drop X/Y Chr.",
                            labelWidth = "fit",
                            value = FALSE
                        ),
                        
                        shinyjs::disabled(actionButton("button_minfi_select", "Select")),
                        h4(),
                        textOutput("text_minfi_probes")
                    )
                )
            )
        ),
        
        # DMPS
        tabItem(tabName = "DMPS", 
                fluidPage(
                    verticalLayout(
                        # Box1
                        sidebarPanel(selectInput("select_minfi_norm", "Select Normalization", norm_options)
                                     
                        )
                    )
                )
        ),
        
        # DMRS
        tabItem(tabName = "DMRS", 
                fluidPage(
                    verticalLayout(
                        # Box1
                        sidebarPanel(selectInput("select_minfi_norm", "Select Normalization", norm_options)
                                     
                        )
                    )
                )
        )
        
        
        
        
    ))))
