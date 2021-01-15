#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$inputbutton <- renderUI({
        if (!is.null(input$inputfile$datapath)) {
            return(actionButton("inputbuttonaction", "Load Data"))
        } else {
            return()
        }
    })
    
    observeEvent(input$inputfile, shinyjs::enable("inputbuttonaction"))
    
    
    # When you press button_input_load, the data is unzipped and the metharray sheet is loaded
    rval_sheet <- eventReactive(input$inputbuttonaction, {
        
        # Check if updated file is .zip
        validate(need(tools::file_ext(input$inputfile$datapath) == "zip", "File extension should be .zip"))
        
        shinyjs::disable("button_input_load") # disable the load button to avoid multiple clicks
        
        if (dir.exists(paste0(tempdir(), "/experiment_data"))) {
            unlink(paste0(tempdir(), "/experiment_data"), recursive = TRUE) # remove current files in target directory
        }
        
        zip::unzip(input$inputfile$datapath,
                   exdir = paste0(tempdir(), "/experiment_data")
        ) # extracting zip
        
        sheet <- minfi::read.metharray.sheet(paste0(tempdir(), "/experiment_data"))
        
        # We check if sheet is correct
        # This is to prevent app crashes when zip updated is not correct.
        validate(
            need(
                is.data.frame(sheet) &
                    any(colnames(sheet) %in% "Slide") &
                    any(colnames(sheet) %in% "Array"),
                "SampleSheet is not correct. Please, check your samplesheet and your zip file."
            )
        )
        
        validate(
            need(
                anyDuplicated(colnames(sheet)) == 0,
                "Repeated variable names are not allowed. Please, modify your sample sheet."
            )
        )
        
        colnames(sheet) <- make.names(colnames(sheet)) # fix possible not-valid colnames
        
        sheet
    })
    
    


    
    output$datatable <- renderDataTable({

        rval_sheet()

    })

})
